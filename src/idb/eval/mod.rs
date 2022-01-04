use crate::edb::Relations;
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
    fn inference(&self, program: &Program) -> Result<Relations>;
}

#[derive(Debug)]
pub struct NoopEvaluator;

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Default for NoopEvaluator {
    fn default() -> Self {
        NoopEvaluator
    }
}

impl Evaluator for NoopEvaluator {
    fn inference(&self, _: &Program) -> Result<Relations> {
        Ok(Relations::default())
    }
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

pub(crate) mod naive;
