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

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

pub(crate) mod naive;
