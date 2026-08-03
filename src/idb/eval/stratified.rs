use crate::edb::{Attribute, Constant, Predicate, RelationSet};
use crate::error::{
    constraint_violated, language_feature_disabled, relation_does_not_exist, Result,
};
use crate::features::FEATURE_COMPARISONS;
use crate::idb::eval::{strata::StratifiedProgram, Evaluator};
use crate::idb::query::{Queryable, Row, View};
use crate::idb::{Atom, Literal, Rule, RuleForm, Term, Variable};
use crate::{Collection, IndexedCollection, Labeled, Program, ProgramCore};
use std::collections::HashSet;
use tracing::trace;

// Safety net for runaway fixpoint loops. Datalog without function symbols
// always terminates, so hitting this is a bug, not a config knob.
const FIXPOINT_HEADROOM: usize = 1024;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Default)]
pub struct StratifiedEvaluator {}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

const METRIC_LABEL: &str = "stratified";

impl Evaluator for StratifiedEvaluator {
    fn inference(&self, program: &Program) -> Result<RelationSet> {
        for rule in program.rules().iter() {
            check_rule_form(rule)?;
        }

        let stratified = StratifiedProgram::from(program)?;
        trace!(
            "inference > stratified program into {} sub-programs",
            stratified.len()
        );

        // Strata must be evaluated bottom-up, and each stratum must be able to
        // see *all* facts derived in earlier strata — most visibly through
        // negated literals (`NOT t(X,Y)` only makes sense once `t` is fully
        // computed), but also in general for chained non-recursive
        // dependencies.
        //
        // We thread a single growing `RelationSet` through the strata. It
        // starts as a clone of the program's intensional DB (preserving
        // schemas and any pre-asserted IDB facts), each stratum's derived
        // facts are merged in, and the final accumulator is the answer.
        let mut accumulated = program.intensional().clone();
        for sub_program in stratified.iter() {
            let derived = infer_stratum(sub_program, &accumulated)?;
            accumulated.merge_from(derived)?;
        }

        // -- Integrity constraints -----------------------------------------
        // Constraints (`:- body.`) are evaluated AFTER all derivation has
        // settled. If any constraint's body has a satisfying binding in the
        // final model, the spec says the program is *inconsistent* — we
        // surface that as a typed `ConstraintViolated` error so the caller
        // can decide whether to treat it as fatal or as a diagnostic.
        check_constraints(program, &accumulated)?;

        Ok(accumulated)
    }

    fn label(&self) -> &'static str {
        METRIC_LABEL
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

///
/// Run the **semi-naive** fixpoint for a single stratum on top of the IDB
/// facts already known (`prior_idb`).
///
/// # Algorithm
///
/// 1. **Pass 0 (seed)**: evaluate every rule with the full IDB to get the
///    initial delta `Δ₀`.
/// 2. **Pass i ≥ 1 (semi-naive step)**: for each rule, for each body atom
///    referring to a predicate produced *in this stratum* (a "recursive
///    atom"), evaluate the rule once with that atom's view sourced from
///    `Δᵢ₋₁` and every other body atom sourced from the full IDB. Sum,
///    subtract anything already known, and the result is `Δᵢ`.
/// 3. Stop when `Δᵢ` is empty.
///
/// Rules with no recursive atoms reach their fixpoint in pass 0 and are
/// skipped on every subsequent pass. The textbook win on transitive
/// closure: pass `n` does O(|Δₙ₋₁| × |R|) work instead of O(|T| × |R|)
/// — i.e. you don't re-derive every old path on every pass.
///
fn infer_stratum(program: &impl ProgramCore, prior_idb: &RelationSet) -> Result<RelationSet> {
    trace!(
        "infer_stratum > prior_idb.flat_count = {}",
        prior_idb.flat_count()
    );

    // Predicates produced by *this* stratum's rules. A body atom whose
    // predicate is in this set is a candidate for delta-substitution.
    let stratum_preds: HashSet<Predicate> = program
        .rules()
        .iter()
        .flat_map(|r| r.head())
        .map(|a| a.label().clone())
        .collect();

    let mut stratum_delta = prior_idb.clone_with_schema_only();
    let mut working = prior_idb.clone();

    // -- Pass 0: full evaluation, seeds the recursion. ------------------
    let mut last_delta = infer_pass_full(program, &working)?;
    last_delta = subtract_known(last_delta, &working)?;
    trace!(
        "infer_stratum > pass 0 produced {} new fact(s)",
        last_delta.flat_count()
    );
    if last_delta.flat_count() == 0 {
        return Ok(stratum_delta);
    }
    stratum_delta.merge_from(last_delta.clone())?;
    working.merge_from(last_delta.clone())?;

    // -- Passes 1..N: semi-naive deltas. --------------------------------
    let cap = program.extensional().flat_count() + prior_idb.flat_count() + FIXPOINT_HEADROOM;
    for pass in 1..=cap {
        let raw = infer_pass_semi_naive(program, &working, &last_delta, &stratum_preds)?;
        let new_delta = subtract_known(raw, &working)?;
        trace!(
            "infer_stratum > pass {} produced {} new fact(s)",
            pass,
            new_delta.flat_count()
        );
        if new_delta.flat_count() == 0 {
            trace!("infer_stratum > fixpoint reached after {} pass(es)", pass + 1);
            return Ok(stratum_delta);
        }
        stratum_delta.merge_from(new_delta.clone())?;
        working.merge_from(new_delta.clone())?;
        last_delta = new_delta;
    }
    panic!("excessive looping in stratified evaluator (cap={})", cap);
}

///
/// Filter `derived` down to facts that don't already appear in `known`.
///
fn subtract_known(derived: RelationSet, known: &RelationSet) -> Result<RelationSet> {
    let mut out = derived.clone_with_schema_only();
    for relation in derived.iter() {
        let predicate = relation.label_ref();
        let already = known.get(&predicate);
        let out_relation = out
            .get_mut(&predicate)
            .expect("clone_with_schema_only preserves all schemas");
        for fact in relation.iter() {
            let is_new = match already {
                Some(rel) => !rel.contains(fact),
                None => true,
            };
            if is_new {
                out_relation.add(fact.clone())?;
            }
        }
    }
    Ok(out)
}

///
/// Pass 0 of the fixpoint: evaluate every rule with the full IDB.
///
fn infer_pass_full(program: &impl ProgramCore, intensional: &RelationSet) -> Result<RelationSet> {
    trace!(
        "infer_pass_full > intensional.flat_count = {}",
        intensional.flat_count()
    );
    let mut delta = intensional.clone_with_schema_only();
    for rule in program.rules().iter() {
        apply_rule_into(rule, program.extensional(), intensional, None, &mut delta)?;
    }
    Ok(delta)
}

///
/// Semi-naive pass: for each rule, evaluate once per recursive body atom,
/// with that atom's view sourced from `last_delta` instead of `intensional`.
/// Non-recursive rules are skipped — they already produced everything they
/// can in pass 0.
///
fn infer_pass_semi_naive(
    program: &impl ProgramCore,
    intensional: &RelationSet,
    last_delta: &RelationSet,
    stratum_preds: &HashSet<Predicate>,
) -> Result<RelationSet> {
    trace!(
        "infer_pass_semi_naive > intensional.flat_count = {}, last_delta.flat_count = {}",
        intensional.flat_count(),
        last_delta.flat_count()
    );
    let mut delta = intensional.clone_with_schema_only();
    for rule in program.rules().iter() {
        // Find body positions referring to predicates produced by this
        // stratum — these are the recursive atoms we must delta-substitute.
        let recursive_positions: Vec<usize> = rule
            .positive_literals()
            .filter_map(|lit| lit.as_relational())
            .enumerate()
            .filter(|(_, atom)| stratum_preds.contains(atom.label()))
            .map(|(i, _)| i)
            .collect();

        if recursive_positions.is_empty() {
            // Non-recursive within this stratum: produced everything in
            // pass 0, nothing new to derive from delta.
            continue;
        }

        for k in recursive_positions {
            apply_rule_into(
                rule,
                program.extensional(),
                intensional,
                Some((k, last_delta)),
                &mut delta,
            )?;
        }
    }
    Ok(delta)
}

///
/// Apply one (possibly delta-substituted) evaluation of a rule, writing any
/// derived facts into `delta`. Shared between the pass-0 and semi-naive
/// paths so the head-projection logic lives in exactly one place (DRY).
///
fn apply_rule_into(
    rule: &Rule,
    extensional: &RelationSet,
    intensional: &RelationSet,
    delta_substitute: Option<(usize, &RelationSet)>,
    delta: &mut RelationSet,
) -> Result<()> {
    // Constraints have no head — they're not derivation rules. They get
    // checked once, after all stratification, by `check_constraints`.
    if rule.form() == RuleForm::Constraint {
        return Ok(());
    }

    let body = match evaluate_body(rule, extensional, intensional, delta_substitute)? {
        Some(view) if !view.is_empty() => view,
        // Either some positive literal had no matching facts, or every
        // positive match was eliminated by an anti-join. Either way, the
        // rule contributes nothing this pass.
        _ => return Ok(()),
    };
    trace!(
        "apply_rule_into > rule {} produced {} body bindings",
        rule,
        body.len()
    );
    for binding in body.iter() {
        // For disjunctive rules (`H1 ; H2 :- body`) we derive *every*
        // head atom from the same binding. This is the only sound
        // bottom-up reading of inclusive disjunction: the resulting
        // model contains all `Hi` and therefore satisfies the
        // disjunction `H1 ∨ … ∨ Hn`. (Stable-model / minimal-model
        // semantics would let you pick a smaller subset, but that's
        // NP-hard and well out of scope.)
        for head in rule.head() {
            let new_fact: Vec<Constant> = head
                .iter()
                .map(|term| match term {
                    Term::Anonymous => unreachable!("anonymous terms cannot appear in a rule head"),
                    Term::Variable(v) => binding
                        .get(&body.attribute_index(v.clone().into()).unwrap())
                        .unwrap()
                        .clone(),
                    Term::Constant(c) => c.clone(),
                })
                .collect();
            delta
                .get_mut(head.label())
                .expect("head predicate must have a schema entry")
                .add_as_fact(new_fact)?;
        }
    }
    Ok(())
}

fn check_rule_form(rule: &Rule) -> Result<()> {
    // Negation, disjunction, and constraints are all now evaluated
    // correctly:
    //   - negation via anti-join in `evaluate_body`,
    //   - disjunction via deriving every head atom per binding in
    //     `apply_rule_into`,
    //   - constraints via the post-stratum `check_constraints` pass.
    //
    // Feature-gating happens at rule-insertion time in
    // `Program::add_rule_relaxed`, so by the time we get here every form
    // is legitimately in play. The only thing this function still blocks
    // is arithmetic comparisons — those need a `theta-join` that the
    // current `View` algebra doesn't have.
    if rule.literals().any(|lit| lit.is_arithmetic()) {
        return Err(language_feature_disabled(FEATURE_COMPARISONS));
    }
    Ok(())
}

///
/// Evaluate every constraint (`:- body.`) in `program` against the final
/// `EDB ∪ accumulated` model. Returns `Err(ConstraintViolated)` for the
/// first satisfied body found — the spec's `ERR_CONSTRAINT_VIOLATED`
/// semantics says one bad binding suffices for the whole program to be
/// inconsistent, so there's no point burning cycles to find more.
///
fn check_constraints(program: &Program, accumulated: &RelationSet) -> Result<()> {
    for rule in program.rules().iter() {
        if rule.form() != RuleForm::Constraint {
            continue;
        }
        let body = evaluate_body(rule, program.extensional(), accumulated, None)?;
        if let Some(view) = body {
            if !view.is_empty() {
                trace!(
                    "check_constraints > VIOLATION: `{}` had {} satisfying binding(s)",
                    rule,
                    view.len()
                );
                return Err(constraint_violated(rule.to_string()));
            }
        }
    }
    Ok(())
}

///
/// Evaluate the body of a single rule, returning a view whose schema lists
/// every body variable (and whose rows are the satisfying bindings).
///
/// The body is computed as
///     (natural-join of all positive literal views)
///     anti-joined with each negative literal's view in turn.
///
/// If `delta_substitute = Some((k, delta))`, the **k-th positive literal**
/// is resolved against `delta` instead of `intensional`. This is the
/// substitution that drives semi-naive evaluation: it constrains the rule
/// to derivations using at least one fact from the previous pass's delta.
///
/// Returns `Ok(None)` only when a positive literal resolves to an empty
/// view — the body is definitionally unsatisfiable in that case and the
/// caller can short-circuit.
///
fn evaluate_body(
    rule: &Rule,
    extensional: &RelationSet,
    intensional: &RelationSet,
    delta_substitute: Option<(usize, &RelationSet)>,
) -> Result<Option<View>> {
    // -- Positive literals: natural-join them all into a single body view. --
    let mut positive_views: Vec<View> = Vec::new();
    let mut had_any_positive = false;
    for (idx, lit) in rule.positive_literals().enumerate() {
        had_any_positive = true;

        // Special case: a fully-ground positive literal (all terms are
        // constants) contributes no variable bindings — it's purely a
        // membership test. The lower-level `query_atom` returns a
        // 1-row boolean-domain view (`new_true()` / `new_false()`),
        // both of which are *non-empty* under `View::is_empty`, so
        // they'd silently survive the join and pollute results. We
        // intercept here: if the atom is in the DB, skip it from the
        // join (it adds no columns/bindings); if it isn't, the
        // conjunction is unsatisfiable and the rule contributes
        // nothing.
        if let Some(atom) = lit.as_relational() {
            if atom.is_existential() {
                let (edb, idb) = match delta_substitute {
                    Some((k, delta)) if k == idx => (extensional, delta),
                    _ => (extensional, intensional),
                };
                if !ground_atom_present(edb, atom)? && !ground_atom_present(idb, atom)? {
                    return Ok(None);
                }
                continue;
            }
        }

        // If this is the position we've been asked to delta-substitute,
        // resolve from the delta IDB (and only the delta IDB); otherwise
        // resolve normally against EDB ∪ full IDB.
        let view = match delta_substitute {
            Some((k, delta)) if k == idx => view_for_literal(lit, extensional, delta)?,
            _ => view_for_literal(lit, extensional, intensional)?,
        };
        if view.is_empty() {
            // Conjunction with an empty relation is empty.
            return Ok(None);
        }
        positive_views.push(view);
    }

    // A rule with no positive literals is a safety violation that the rule
    // builder rejects, but be defensive: nothing positive ⇒ nothing to keep.
    if !had_any_positive {
        return Ok(None);
    }

    // If every positive literal was a satisfied ground atom (e.g. a
    // constraint like `:- p("x"), q("y").` where both facts are
    // present), we have no per-row views to join but the body *is*
    // satisfied. Hand back a unit view (1 row, 0 columns) so callers
    // who count rows see exactly one binding.
    if positive_views.is_empty() {
        return Ok(Some(View::new_with_facts(
            Vec::<Attribute<Variable>>::new(),
            vec![Row::from(Vec::<Constant>::new())],
        )));
    }

    let mut body = View::natural_join_all(positive_views)?;
    trace!(
        "evaluate_body > positive join produced {} rows over schema {:?}",
        body.len(),
        body.schema()
    );

    // -- Negative literals: anti-join the body with each in turn. --
    for lit in rule.negative_literals() {
        let neg_view = view_for_literal(lit, extensional, intensional)?;
        let before = body.len();
        body = body.anti_join(neg_view)?;
        trace!(
            "evaluate_body > anti-join with {:?} kept {}/{} rows",
            lit,
            body.len(),
            before
        );
        if body.is_empty() {
            return Ok(Some(body));
        }
    }

    Ok(Some(body))
}

/// True iff the *ground* `atom` (all-constant terms) is in `db`.
///
/// `Relation::query_atom` returns a 1-row boolean-domain sentinel view
/// for fully-ground queries — `new_true()` for hit, `new_false()` for
/// miss — so a plain `is_empty()` check would wrongly treat "miss" as
/// "present". This helper inspects the sentinel value directly.
fn ground_atom_present(db: &RelationSet, atom: &Atom) -> Result<bool> {
    let view = match db.query_atom(atom)? {
        None => return Ok(false),
        Some(v) => v,
    };
    let false_row = Row::from(Constant::new_false());
    let any_true = view.iter().any(|row| row != &false_row);
    Ok(any_true)
}

fn view_for_literal(
    lit: &Literal,
    extensional: &RelationSet,
    intensional: &RelationSet,
) -> Result<View> {
    let atom = lit
        .as_relational()
        .ok_or_else(|| language_feature_disabled(FEATURE_COMPARISONS))?;
    resolve_atom(atom, extensional, intensional)
}

fn resolve_atom(
    atom: &Atom,
    extensional: &RelationSet,
    intensional: &RelationSet,
) -> Result<View> {
    if let Some(view) = extensional.query_atom(atom)? {
        trace!("resolve_atom > {} ← extensional", atom.label());
        return Ok(view);
    }
    if let Some(view) = intensional.query_atom(atom)? {
        trace!("resolve_atom > {} ← intensional", atom.label());
        return Ok(view);
    }
    Err(relation_does_not_exist(atom.label_ref()))
}
