/*!
One-line description.

More detailed description, with

# Example

*/

use crate::error::{Error, Result};
use bimap::BiBTreeMap;
use paste::paste;
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    counter: u64,
    constants: InternedStrings,
    identifiers: InternedStrings,
    relations: InternedStrings,
    variables: InternedStrings,
}

pub type EnvironmentRef = Rc<RefCell<Environment>>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Interned(u64);

type InternedStrings = BiBTreeMap<String, Interned>;

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Macros
// ------------------------------------------------------------------------------------------------

macro_rules! environment_impl {
    ($lcname:ident, $strname:expr) => {
        paste! {
            pub fn [<new_ $lcname>]<S: Into<String>>(&mut self, s: S) -> Result<Interned> {
                let s = s.into();
                match self.[<$lcname s>].get_by_left(&s) {
                    Some(s) => Ok(*s),
                    None => {
                        if Self::[<is_valid_ $lcname>](&s) {
                            self.counter += 1;
                            let interned = Interned(self.counter);
                            self.[<$lcname s>].insert(s, interned.clone());
                            Ok(interned)
                        } else {
                            Err(Error::InvalidValue(String::from($strname), s))
                        }
                    }
                }
            }

            pub fn [<has_ $lcname>]<S: Into<String>>(&self, s: S) -> bool {
                self.[<$lcname s>].contains_left(&s.into())
            }

            pub fn $lcname<S: Into<String>>(&self, s: S) -> Option<&Interned> {
                self.[<$lcname s>].get_by_left(&s.into())
            }

            pub fn [<$lcname _string>](&self, i: &Interned) -> Option<&String> {
                self.[<$lcname s>].get_by_right(i)
            }
        }
    };
}
// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

const UNDERSCORE: char = '_';

impl Default for Environment {
    fn default() -> Self {
        Self {
            counter: Default::default(),
            constants: Default::default(),
            identifiers: Default::default(),
            relations: Default::default(),
            variables: Default::default(),
        }
    }
}

impl Environment {
    environment_impl!(constant, "Constant");

    environment_impl!(identifier, "Identifier");

    environment_impl!(relation, "Relation");

    environment_impl!(variable, "Variable");

    pub fn is_valid_constant(s: &str) -> bool {
        !s.chars().any(|c| c.is_control())
    }

    pub fn is_valid_identifier(s: &str) -> bool {
        let mut chars = s.chars();
        !s.is_empty()
            && chars.next().map(|c| c.is_lowercase()).is_some()
            && chars.all(|c| c.is_alphanumeric() || c == UNDERSCORE)
    }

    pub fn is_valid_variable(s: &str) -> bool {
        let mut chars = s.chars();
        !s.is_empty()
            && chars
                .next()
                .map(|c| c.is_uppercase() || c == UNDERSCORE)
                .is_some()
            && chars.all(|c| c.is_alphanumeric() || c == UNDERSCORE)
    }

    pub fn is_valid_relation(s: &str) -> bool {
        let mut chars = s.chars();
        !s.is_empty()
            && chars
                .next()
                .map(|c| c.is_alphabetic() || c == UNDERSCORE)
                .is_some()
            && chars.all(|c| c.is_alphanumeric() || c == UNDERSCORE)
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
