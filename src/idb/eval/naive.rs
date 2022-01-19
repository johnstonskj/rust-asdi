use crate::edb::Constant;
use crate::error::{language_feature_disabled, Result};
use crate::features::{FEATURE_COMPARISONS, FEATURE_NEGATION};
use crate::idb::eval::Evaluator;
use crate::idb::query::{Queryable, View};
use crate::idb::{Atom, Term};
use crate::{
    relation_does_not_exist, Collection, IndexedCollection, Labeled, MaybePositive, Program,
    ProgramCore, RelationSet, RuleForm, FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION,
};
use tracing::trace;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// Provides a very naive implementation of the [`Evaluator`] trait.
///
/// # Details
///
/// The following is taken from [Datalog -- Logical Rules
/// Recursion](http://infolab.stanford.edu/~ullman/fcdb/aut07/slides/dlog.pdf).
///
/// In a program, predicates can be either:
///
/// 1. EDB = Extensional Database = stored table.
/// 2. IDB = Intensional† Database = relation defined by rules.
///
/// The head is true for given values of the distinguished variables if there exist values of
/// the non-distinguished variables that make all sub-goals of the body true.
///
/// Naive Approach: consider all combinations of values of the variables.
///
/// * If all sub-goals are true, then evaluate the head.
/// * The resulting head is a tuple in the result.
///
/// Relations are finite sets.
/// * We want rule evaluations to be finite and lead to finite results.
/// * “Unsafe” rules like `p(X) :- q(Y)` have infinite results, even if `q` is finite.
/// * Even `p(X) :- q(Y)` requires examining an infinity of `X`-values.
///
/// † -- an _intension_ is any property or quality connoted by a word, phrase, or another symbol.
///
#[derive(Debug, Default)]
pub struct NaiveEvaluator {}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Evaluator for NaiveEvaluator {
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
                    match rule.form() {
                        RuleForm::Pure => Ok(()),
                        RuleForm::Constraint => Err(language_feature_disabled(FEATURE_CONSTRAINTS)),
                        RuleForm::Disjunctive => {
                            Err(language_feature_disabled(FEATURE_DISJUNCTION))
                        }
                    }?;
                    trace!("infer > rule > {}", rule);
                    let matches: Result<Vec<View>> = rule
                        .literals()
                        .map(|l| {
                            if let Some(atom) = l.as_relational() {
                                if let Some(view) = program.extensional().query_atom(atom)? {
                                    trace!("First matches from an extensional relation");
                                    Ok(view)
                                } else if let Some(mut view) =
                                    program.intensional().query_atom(atom)?
                                {
                                    trace!("First matches from an intensional relation");
                                    if let Some(previous_matches) = new_db.query_atom(atom)? {
                                        trace!(
                                            "Adding in any matches from the in-progress relation"
                                        );
                                        view.extend(previous_matches)?;
                                    }
                                    Ok(view)
                                } else {
                                    Err(relation_does_not_exist(atom.label_ref()))
                                }
                            } else {
                                Err(language_feature_disabled(FEATURE_COMPARISONS))
                            }
                        })
                        .collect();

                    let matches = matches?;

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

    fn label(&self) -> &'static str {
        "naive"
    }
}
