/*!
One-line description.

More detailed description, with

# Example

*/

use crate::edb::Predicate;
use crate::error::Result;
use crate::eval::Evaluator;
use crate::features::FEATURE_NEGATION;
use crate::idb::Term;
use crate::{Database, Error, Program, SyntacticFragments};
use std::collections::{HashMap, HashSet};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct NaiveEvaluatorFactory {}

pub type Tuple = Vec<Term>;

pub type TupleSet = HashSet<Tuple>;

pub type RelationMap = HashMap<Predicate, TupleSet>;

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
struct NaiveEvaluator {}

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
    fn inference(&self, program: &Program, _initial_edb: &Database) -> Result<Database> {
        if program.is_positive() {
            // let all_inferred: Database = Database::new();
            // // for the first iteration use the set of facts already known in the program.
            // let mut inferred_once: &Database = initial_edb;
            // loop {
            //     // for subsequent iteration only use the facts generated in the previous iteration,
            //     let result = self.infer_once(program, &all_inferred, &inferred_once)?;
            //     inferred_once = &result;
            //     if inferred_once.is_empty() {
            //         // nothing new may be inferred.
            //         break;
            //     }
            // }
            // Ok(all_inferred)
            Ok(Default::default())
        } else {
            Err(Error::LanguageFeatureDisabled(FEATURE_NEGATION))
        }
    }
}

#[allow(single_use_lifetimes)]
impl NaiveEvaluator {
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
    #[allow(dead_code)]
    fn infer_once(
        &self,
        from_program: &Program,
        _all_inferred_facts: &Database,
        _this_iteration_facts: &Database,
    ) -> Result<Database> {
        for rule in from_program.rules() {
            for _sub_goal in rule.literals() {}
        }
        Ok(Default::default())
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
mod tests {
    use super::*;
    use crate::edb::AttributeKind;
    use crate::idb::{Atom, Variable};
    use std::str::FromStr;

    #[test]
    #[ignore]
    fn test_that_syllogism() {
        // See https://en.wikipedia.org/wiki/Syllogism

        let mut syllogism = Program::default();
        let p_human = Predicate::from_str("human").unwrap();

        let mut human = syllogism
            .make_new_relation(p_human.clone(), [AttributeKind::String])
            .unwrap();
        human.add(["Socrates".into()]);

        syllogism.database_mut().add(human);

        let var_x: Term = Variable::from_str("X").unwrap().into();

        syllogism
            .add_new_rule(
                Predicate::from_str("mortal").unwrap(),
                [var_x.clone()],
                [Atom::new(p_human, [var_x]).into()],
            )
            .unwrap();

        assert_eq!(
            syllogism.to_string(),
            r#"human("Socrates").
mortal(X) ⟵ human(X).
"#
        );

        //         syllogism
        //             .perform_inference(&NaiveEvaluator::default())
        //             .unwrap();
        //
        //         assert_eq!(
        //             syllogism.to_string(),
        //             r#"human("Socrates").
        // mortal(X) ⟵ human(X).
        // mortal("Socrates").
        // "#
        //         );
    }
}
