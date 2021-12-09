/*!
One-line description.

More detailed description, with

# Example

*/

use crate::error::Result;
use crate::eval::Evaluator;
use crate::features::FEATURE_NEGATION;
use crate::{Error, Fact, Predicate, Program, SyntacticFragments, Term};
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
    fn inference(&self, from_program: &Program) -> Result<HashSet<Fact>> {
        if from_program.is_positive() {
            let all_inferred: HashSet<Fact> = Default::default();
            // for the first iteration use the set of facts already known in the program.
            let mut inferred_once: HashSet<&Fact> = from_program.facts().collect();
            loop {
                // for subsequent iteration only use the facts generated in the previous iteration,
                inferred_once = self.infer_once(from_program, &all_inferred, &inferred_once)?;
                if inferred_once.is_empty() {
                    // nothing new may be inferred.
                    break;
                }
            }
            Ok(all_inferred)
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
    fn infer_once(
        &self,
        from_program: &Program,
        _all_inferred_facts: &HashSet<Fact>,
        _this_iteration_facts: &HashSet<&Fact>,
    ) -> Result<HashSet<&Fact>> {
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
    use crate::{Atom, Rule, Term};

    #[test]
    #[ignore]
    fn test_that_syllogism() {
        // See https://en.wikipedia.org/wiki/Syllogism

        let mut syllogism = Program::default();

        let mortal = syllogism.make_predicate_relation("mortal").unwrap();
        let human = syllogism.make_predicate_relation("human").unwrap();

        let socrates = syllogism.make_constant_string("Socrates").unwrap();

        let var_x: Term = syllogism.make_term_variable("X").unwrap();

        syllogism
            .push(Fact::new_with_arguments(human.clone(), vec![socrates]))
            .unwrap();

        syllogism
            .push(Rule::new_with_body(
                Atom::new(mortal, vec![var_x.clone()]),
                vec![Atom::new(human, vec![var_x]).into()],
            ))
            .unwrap();

        assert_eq!(
            syllogism.to_string(),
            r#"human("Socrates").
mortal(X) ⟵ human(X).
"#
        );

        syllogism
            .perform_inference(&NaiveEvaluator::default())
            .unwrap();

        assert_eq!(
            syllogism.to_string(),
            r#"human("Socrates").
mortal(X) ⟵ human(X).
mortal("Socrates").
"#
        );
    }
}
