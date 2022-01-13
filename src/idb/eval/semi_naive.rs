use crate::edb::{Constant, RelationSet};
use crate::error::{language_feature_disabled, Result};
use crate::features::{
    FEATURE_COMPARISONS, FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION, FEATURE_NEGATION,
};
use crate::idb::eval::Evaluator;
use crate::idb::query::View;
use crate::idb::{Atom, Rule, RuleForm, Term};
use crate::{
    relation_does_not_exist, Collection, IndexedCollection, Labeled, MaybePositive, Program,
    ProgramCore, Queryable,
};
use tracing::trace;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Default)]
pub struct SemiNaiveEvaluator {}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Evaluator for SemiNaiveEvaluator {
    fn inference(&self, program: &Program) -> Result<RelationSet> {
        if program.is_positive() {
            let mut new_db = program.intensional().clone_with_schema_only();
            loop {
                trace!(
                    "Starting inference pass with {} facts so far",
                    new_db.flat_count()
                );
                let start = new_db.flat_count();
                for rule in program.rules().iter() {
                    check_rule_form(rule)?;

                    let matches = rule_matches(rule, program)?;

                    if matches.iter().all(|result| !result.is_empty()) {
                        trace!("Joining all ({}) result views", matches.len());
                        let results = View::join_all(matches)?;
                        for fact in results.iter() {
                            let head_predicates = rule.head().collect::<Vec<&Atom>>();
                            let head = head_predicates.get(0).unwrap();
                            let relation = new_db.get_mut(head.label()).unwrap();
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
                    }
                }
                if start == new_db.flat_count() {
                    trace!("No more facts were found, so done");
                    break;
                }
            }
            Ok(new_db)
        } else {
            Err(language_feature_disabled(FEATURE_NEGATION))
        }
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

fn check_rule_form(rule: &Rule) -> Result<()> {
    match rule.form() {
        RuleForm::Pure => Ok(()),
        RuleForm::Constraint => Err(language_feature_disabled(FEATURE_CONSTRAINTS)),
        RuleForm::Disjunctive => Err(language_feature_disabled(FEATURE_DISJUNCTION)),
    }
}

fn rule_matches(rule: &Rule, program: &impl ProgramCore) -> Result<Vec<View>> {
    rule.literals()
        .map(|l| {
            if let Some(atom) = l.as_relational() {
                if let Ok(Some(view)) = program.extensional().query_atom(atom) {
                    trace!("First matches from an extensional relation");
                    Ok(view)
                } else if let Ok(Some(view)) = program.intensional().query_atom(atom) {
                    trace!("First matches from an intensional relation");
                    // if let Some(previous_matches) = new_db.matches(atom) {
                    //     trace!("Adding in any matches from the in-progress relation");
                    //     view.extend(previous_matches)?;
                    // }
                    Ok(view)
                } else {
                    Err(relation_does_not_exist(atom.label_ref()))
                }
            } else {
                Err(language_feature_disabled(FEATURE_COMPARISONS))
            }
        })
        .collect()
}
