/*!
One-line description.

More detailed description, with

# Example

*/

use crate::edb::Constant;
use crate::error::Result;
use crate::eval::{Evaluator, Table};
use crate::features::FEATURE_NEGATION;
use crate::idb::Term;
use crate::{Database, Error, Program, SyntacticFragments};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
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

impl Default for NaiveEvaluator {
    fn default() -> Self {
        Self {}
    }
}

impl Evaluator for NaiveEvaluator {
    fn inference(&self, program: &Program, edb: &Database) -> Result<Database> {
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
            let mut new_db = edb.clone_with_schema_only();
            loop {
                let start = new_db.fact_count();
                for rule in program.rules() {
                    let matches: Vec<Table> = rule
                        .literals()
                        .map(|l| edb.matches(l.as_atom().unwrap()))
                        .collect();
                    if matches.iter().all(|result| !result.is_empty()) {
                        // else: not all sub-goals satisfied
                        // TODO: is join enough?
                        let results = Table::join_all(matches);
                        for row in results.rows() {
                            let relation = new_db.relation_mut(rule.head().predicate()).unwrap();
                            let new_fact = rule
                                .head()
                                .terms()
                                .map(|term| match term {
                                    Term::Variable(v) => row.get(v.clone()).unwrap(),
                                    Term::Constant(c) => c,
                                })
                                .cloned()
                                .collect::<Vec<Constant>>();
                            relation.add(new_fact);
                        }
                    }
                }
                if start == new_db.fact_count() {
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

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
#[cfg(feature = "parser")]
mod tests {
    use crate::eval::naive::NaiveEvaluator;
    use crate::eval::Evaluator;
    use crate::parse::parse_str;
    use crate::{Predicate, Query, Variable};
    use std::str::FromStr;

    #[test]
    fn test_socrates() {
        let mut program = parse_str(
            r#"human("Socrates").
human("Plato").

mortal(X) <- human(X).

?- mortal("Socrates").
"#,
        )
        .unwrap()
        .into_parsed();

        println!("{}", program);
        println!("-------------------------------------------------------------------------------");

        let evaluator = NaiveEvaluator::default();

        let results = evaluator.inference(&program, program.database());

        program.database_mut().merge(results.unwrap());

        println!("{}", program);
        println!("-------------------------------------------------------------------------------");

        let is_socrates_mortal = program.queries().next().unwrap();

        let results = program.database().matches(is_socrates_mortal.as_ref());

        println!("{} ==>\n{}", is_socrates_mortal, results);
        println!("-------------------------------------------------------------------------------");

        let all_mortals = Query::new(
            Predicate::from_str("mortal").unwrap(),
            [Variable::from_str("X").unwrap().into()],
        );

        let results = program.database().matches(all_mortals.as_ref());

        println!("{} ==>\n{}", all_mortals, results);
    }
}
