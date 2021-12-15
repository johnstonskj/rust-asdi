/*!
One-line description.

More detailed description, with

# Example

*/

use crate::idb::Atom;
use crate::{Predicate, Term, CHAR_PERIOD, QUERY_PREFIX_ASCII};
use std::fmt::{Display, Formatter};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// A query simply wraps a single [_atom_](struct.Atom.html) which acts as the goal for the query.
///
/// # Examples
///
/// It is distinguished in the text representation with either the prefix `?-` and suffix `.`
/// **or** the suffix `?` and no period.
///
/// ```prolog
/// ?- ancestor(xerces, X).
/// ancestor(xerces, X)?
/// ```
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Query(Atom);

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

impl Display for Query {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}{}", QUERY_PREFIX_ASCII, self.0, CHAR_PERIOD)
    }
}

impl From<Atom> for Query {
    fn from(v: Atom) -> Self {
        Self(v)
    }
}

impl AsRef<Atom> for Query {
    fn as_ref(&self) -> &Atom {
        &self.0
    }
}

impl Query {
    pub fn new<T: Into<Vec<Term>>>(predicate: Predicate, terms: T) -> Self {
        Self(Atom::new(predicate, terms))
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
