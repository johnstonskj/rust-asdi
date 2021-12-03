/*!
One-line description.

More detailed description, with

# Example

*/

use crate::error::Error;
use crate::{Fact, Program, Query, Term};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub enum Results {
    Singular(Term),
    Tabular(Vec<Vec<Term>>),
}

pub trait Validate {
    fn validate(&self, program: &Program) -> Result<(), Error>;
}

pub trait Derive {
    fn derive_into_new(&self, program: &Program) -> Result<Program, Error> {
        let mut new = program.clone();
        self.derive_in_place(&mut new)?;
        Ok(new)
    }

    fn derive_in_place(&self, program: &mut Program) -> Result<(), Error>;
}

pub trait Evaluate {
    fn evaluate(&self, program: &Program, query: &Query) -> Result<Option<Results>, Error>;

    fn evaluate_all(&self, program: &Program) -> Result<Vec<Option<Results>>, Error> {
        let r: Result<Vec<Option<Results>>, Error> = program
            .queries()
            .map(|q| self.evaluate(program, q))
            .collect();
        r
    }
}

#[derive(Debug)]
pub struct SimpleValidator {}

#[derive(Debug)]
pub struct NaiveDerivation {}

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

impl Default for SimpleValidator {
    fn default() -> Self {
        Self {}
    }
}

impl Validate for SimpleValidator {
    fn validate(&self, _: &Program) -> Result<(), Error> {
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------

impl Default for NaiveDerivation {
    fn default() -> Self {
        Self {}
    }
}

#[allow(unused)]
impl Derive for NaiveDerivation {
    fn derive_in_place(&self, program: &mut Program) -> Result<(), Error> {
        loop {
            let mut new_facts: Vec<Fact> = Default::default();

            for rule in program.rules() {
                // TODO: add in the actual eval here!
            }

            if new_facts.is_empty() {
                break;
            } else {
                program.extend(new_facts);
            }
        }
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
