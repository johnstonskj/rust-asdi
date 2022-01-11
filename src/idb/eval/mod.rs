/*!
This module provides the [Evaluator] trait used to evaluate a program and infer new rules.

This module also provides the following implementations of the [Evaluator] trait:

1. [NoopEvaluator] -- TBD
2. [NaiveEvaluator] -- TBD

*/

use crate::edb::RelationSet;
use crate::error::Result;
use crate::Program;
use std::fmt::Debug;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// This trait is implemented by types that are able to evaluate a program and infer IDB
/// relations.
///
pub trait Evaluator: Debug {
    ///
    /// This method will evaluate the intensional and extensional relations in the program as well
    /// as the rules defined and return a new copy of the intensional relations with any new facts
    /// added.
    ///
    fn inference(&self, program: &Program) -> Result<RelationSet>;
}

///
/// This implementation of [Evaluator] does nothing. This allows for testing and cases where you
/// may wish to test that other operations have no side-effects.
///
/// Specifically this implementation will return an empty [RelationSet] instance with the same
/// schema as the intensional relations in the program.
///
#[derive(Debug)]
pub struct NoopEvaluator;

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

impl Default for NoopEvaluator {
    fn default() -> Self {
        NoopEvaluator
    }
}

impl Evaluator for NoopEvaluator {
    fn inference(&self, program: &Program) -> Result<RelationSet> {
        Ok(program.intensional().clone_with_schema_only())
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

mod naive;
pub use naive::NaiveEvaluator;

mod strata;
pub use strata::{PrecedenceGraph, PrecedenceNode, StratifiedRules};
