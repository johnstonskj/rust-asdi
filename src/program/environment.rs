/*!
One-line description.

More detailed description, with

# Example

*/

use bimap::BiBTreeMap;
use std::fmt::Debug;
use std::rc::Rc;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Environment {
    counter: u64,
    values: BiBTreeMap<String, Interned>,
}

pub type EnvironmentRef = Rc<Environment>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Interned(u64);

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

impl Default for Environment {
    fn default() -> Self {
        Self {
            counter: 1,
            values: Default::default(),
        }
    }
}

impl Environment {
    pub fn insert<S: Into<String>>(&mut self, s: S) -> Interned {
        let s = s.into();
        match self.values.get_by_left(&s) {
            Some(s) => *s,
            None => {
                self.counter += 1;
                let interned = Interned(self.counter);
                self.values.insert(s, interned.clone());
                interned
            }
        }
    }

    pub fn has_value<S: Into<String>>(&self, s: S) -> bool {
        self.values.contains_left(&s.into())
    }

    pub fn interned_from<S: Into<String>>(&self, s: S) -> Option<&Interned> {
        self.values.get_by_left(&s.into())
    }

    pub fn string_from(&self, i: &Interned) -> Option<&String> {
        self.values.get_by_right(i)
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
