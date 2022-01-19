use crate::edb::{Constant, RelationSet};
use crate::error::{language_feature_disabled, Result};
use crate::features::{
    FEATURE_COMPARISONS, FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION, FEATURE_NEGATION,
};
use crate::idb::eval::{Evaluator, StratifiedProgram};
use crate::idb::query::{Queryable, View};
use crate::idb::{Atom, Rule, RuleForm, Term};
use crate::{Collection, IndexedCollection, Labeled, MaybePositive, Program, ProgramCore};
use tracing::{error, trace};

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

        let mut results = RelationSet::default();
        for sub_program in stratified.iter() {
            let relations = infer_from_program(sub_program)?;
            results.merge_from(relations)?;
        }

        Ok(results)
    }

    fn label(&self) -> &'static str {
        METRIC_LABEL
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

fn infer_from_program(program: &impl ProgramCore) -> Result<RelationSet> {
    trace!("infer_from_program");
    let mut idb_all = program.intensional().clone_with_schema_only();
    let mut idb_pass = program.intensional().clone_with_schema_only();
    let mut loop_count = 1;
    loop {
        let idb_pass_count = idb_pass.flat_count();
        idb_pass = infer_from_program_rules(program, &idb_pass)?;

        if idb_pass.flat_count() == idb_pass_count {
            trace!("infer_from_program > no more facts were found, so done");
            break;
        } else {
            trace!("infer_from_program > merge and carry on");
            idb_all.merge(&idb_pass)?;
        }
        if loop_count > program.extensional().flat_count() {
            panic!("Excessive looping");
        } else {
            loop_count += 1;
        }
    }
    Ok(idb_all)
}

fn infer_from_program_rules(
    program: &impl ProgramCore,
    intensional: &RelationSet,
) -> Result<RelationSet> {
    trace!(
        "infer_from_program_rules > intensional.flat_count = {}",
        intensional.flat_count()
    );
    let mut next = intensional.clone_with_schema_only();
    for rule in program.rules().iter() {
        let matches = infer_from_rule(rule, program.extensional(), intensional)?;

        if matches.iter().all(|result| !result.is_empty()) {
            trace!(
                "infer_from_program_rules > joining all ({}) result views",
                matches.len()
            );
            let results = View::join_all(matches)?;
            for fact in results.iter() {
                let head_predicates = rule.head().collect::<Vec<&Atom>>();
                let head = head_predicates.get(0).unwrap();
                let relation = next.get_mut(head.label()).unwrap();
                let new_fact = head
                    .iter()
                    .map(|term| match term {
                        Term::Anonymous => unreachable!(),
                        Term::Variable(v) => fact
                            .get(&results.attribute_index(v.clone().into()).unwrap())
                            .unwrap(),
                        Term::Constant(c) => c,
                    })
                    .cloned()
                    .collect::<Vec<Constant>>();
                relation.add_as_fact(new_fact)?;
            }
            trace!("infer_from_program_rules > found {}", next.flat_count());
        }
    }
    Ok(next)
}

fn check_rule_form(rule: &Rule) -> Result<()> {
    if rule.is_negative() {
        Err(language_feature_disabled(FEATURE_NEGATION))
    } else if rule.literals().any(|lit| lit.is_arithmetic()) {
        Err(language_feature_disabled(FEATURE_COMPARISONS))
    } else {
        match rule.form() {
            RuleForm::Pure => Ok(()),
            RuleForm::Constraint => Err(language_feature_disabled(FEATURE_CONSTRAINTS)),
            RuleForm::Disjunctive => Err(language_feature_disabled(FEATURE_DISJUNCTION)),
        }
    }
}

fn infer_from_rule(
    rule: &Rule,
    extensional: &RelationSet,
    intensional: &RelationSet,
) -> Result<Vec<View>> {
    rule.literals()
        .map(|l| {
            if let Some(atom) = l.as_relational() {
                if let Ok(Some(view)) = extensional.query_atom(atom) {
                    trace!("Atom is an extensional relation");
                    Ok(view)
                } else if let Ok(Some(view)) = intensional.query_atom(atom) {
                    trace!("Atom is an intensional relation");
                    Ok(view)
                } else {
                    error!(
                        "Relation '{}' does not exist, should not get here.",
                        atom.label()
                    );
                    unreachable!()
                }
            } else {
                Err(language_feature_disabled(FEATURE_COMPARISONS))
            }
        })
        .collect()
}
