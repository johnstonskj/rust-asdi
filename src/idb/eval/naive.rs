/*!
One-line description.

More detailed description, with

# Example

*/

use crate::edb::Constant;
use crate::error::{Error, Result};
use crate::features::{FEATURE_COMPARISONS, FEATURE_NEGATION};
use crate::idb::{eval::Evaluator, Atom, Term, View};
use crate::{Program, Relations};
use tracing::trace;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Default)]
pub struct NaiveEvaluator {}

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Macros
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Evaluator for NaiveEvaluator {
    fn inference(&self, program: &Program) -> Result<Relations> {
        //
        // The following is taken from [Datalog -- Logical Rules
        // Recursion](http://infolab.stanford.edu/~ullman/fcdb/aut07/slides/dlog.pdf).
        //
        // In a program, predicates can be either:
        //
        // 1. EDB = Extensional Database = stored table.
        // 2. IDB = Intensional† Database = relation defined by rules.
        //
        // The head is true for given values of the distinguished variables if there exist values of
        // the non-distinguished variables that make all sub-goals of the body true.
        //
        // Naive Approach: consider all combinations of values of the variables.
        //
        // * If all sub-goals are true, then evaluate the head.
        // * The resulting head is a tuple in the result.
        //
        // Relations are finite sets.
        // * We want rule evaluations to be finite and lead to finite results.
        // * “Unsafe” rules like `p(X) :- q(Y)` have infinite results, even if `q` is finite.
        // * Even `p(X) :- q(Y)` requires examining an infinity of `X`-values.
        //
        // † -- an _intension_ is any property or quality connoted by a word, phrase, or another symbol.
        //
        if program.is_positive() {
            let mut new_db = program.intensional().clone_with_schema_only();
            loop {
                let start = new_db.all_len();
                for rule in program.rules().iter() {
                    trace!("infer > rule > {}", rule);

                    let matches: Result<Vec<View>> = rule
                        .literals()
                        .map(|l| {
                            if let Some(atom) = l.as_atom() {
                                if let Some(view) = program.extensional().matches(atom) {
                                    trace!("infer > extensional matches > view > {}\n{}", l, view);
                                    Ok(view)
                                } else if let Some(mut view) = program.intensional().matches(atom) {
                                    trace!("infer > intensional matches > view > {}\n{}", l, view);
                                    if let Some(previous_matches) = new_db.matches(atom) {
                                        trace!(
                                            "infer > matches > previous > {}\n{}",
                                            l,
                                            previous_matches
                                        );
                                        view.extend(previous_matches)?;
                                        trace!("infer > matches > view (extended) >\n{}", view);
                                    }
                                    Ok(view)
                                } else {
                                    Err(Error::RelationDoesNotExist(atom.predicate().clone()))
                                }
                            } else {
                                Err(Error::LanguageFeatureDisabled(FEATURE_COMPARISONS))
                            }
                        })
                        .collect();

                    let matches = matches?;

                    if matches.iter().all(|result| !result.is_empty()) {
                        let results = View::join_all(matches)?;
                        trace!("infer > rule > joined table >\n{}", results);
                        for fact in results.iter() {
                            let head_predicates = rule.head().collect::<Vec<&Atom>>();
                            assert_eq!(head_predicates.len(), 1);
                            let head = head_predicates.get(0).unwrap();
                            let relation = new_db.relation_mut(head.predicate()).unwrap();
                            let new_fact = head
                                .terms()
                                .map(|term| {
                                    trace!(
                                        "infer > rule > row > joined term {:?} ? {}",
                                        term,
                                        term.is_ignored()
                                    );
                                    match term {
                                        Term::Variable(v) => fact
                                            .get(results.attribute_index(v.clone().into()).unwrap())
                                            .unwrap(),
                                        Term::Constant(c) => c,
                                    }
                                })
                                .cloned()
                                .collect::<Vec<Constant>>();
                            relation.add_as_fact(new_fact)?;
                        }
                    }
                }
                if start == new_db.all_len() {
                    // no more facts were found, so return
                    break;
                }
            }
            Ok(new_db)
        } else {
            Err(Error::LanguageFeatureDisabled(FEATURE_NEGATION))
        }
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
