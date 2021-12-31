/*!
One-line description.

More detailed description, with

# Example

*/

use crate::edb::Constant;
use crate::error::{Error, Result};
use crate::features::FEATURE_NEGATION;
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
            let mut new_db = program.extensional().clone_with_schema_only();
            trace!("infer > new_db > {:?}", new_db);
            loop {
                let start = new_db.all_len();
                for rule in program.rules().iter() {
                    trace!("infer > rule > {}", rule);
                    let matches: Result<Vec<View>> = rule
                        .literals()
                        .map(|l| {
                            let mut table = program.extensional().matches(l.as_atom().unwrap());
                            trace!("infer > matches > table > {:#?}", table);
                            table.extend(new_db.matches(l.as_atom().unwrap()))?;
                            trace!("infer > matches > table (extended) > {:#?}", table);
                            Ok(table)
                        })
                        .collect();
                    // TODO: Propagate errors!
                    let matches = matches.unwrap();
                    if matches.iter().all(|result| !result.is_empty()) {
                        // else: not all sub-goals satisfied
                        matches.iter().for_each(|in_table| {
                            trace!("infer > rule > matched table >\n{}", in_table)
                        });
                        let results = View::join_all(matches)?;
                        trace!("infer > rule > joined table >\n{}", results);
                        for fact in results.iter() {
                            let head_predicates = rule.head().collect::<Vec<&Atom>>();
                            assert_eq!(head_predicates.len(), 1);
                            let head = head_predicates.get(0).unwrap();
                            trace!("infer > rule > row > {:?}", fact);
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
