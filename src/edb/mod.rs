/*!
One-line description.

More detailed description, with

# Example

```datalog
@declare human(string).
@declare age(name: string, integer).
```

```datalog
fact("string").
fact(1234).
fact(@true).
fact(@false).
fact(shortString).
fact(str:Short).
```
*/

use crate::error::{Error, Result};
use crate::idb::Atom;
use crate::query::{Matches, Query, View};
use crate::syntax::{CHAR_UNDERSCORE, TYPE_NAME_PREDICATE};
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Database {
    relations: BTreeMap<Predicate, Relation>,
}

pub trait DbValidation {
    fn validate(&self, against: &mut Database) -> Result<()>;
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Predicate(String);

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

impl Display for Database {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for relation in self.iter() {
            writeln!(f, "{}", relation.to_schema_decl())?;
        }
        writeln!(f)?;

        for relation in self.iter() {
            if !relation.is_empty() {
                for fact in relation.facts() {
                    writeln!(f, "{}", fact)?;
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl Database {
    pub fn is_empty(&self) -> bool {
        self.relations.is_empty()
    }

    pub fn len(&self) -> usize {
        self.relations.len()
    }

    pub fn fact_count(&self) -> usize {
        self.iter().map(|r| r.len()).sum()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Relation> {
        self.relations.values()
    }

    pub fn add(&mut self, relation: Relation) {
        if let Some(existing) = self.relations.get_mut(&relation.name()) {
            existing.update_schema(&relation);
            existing.extend(relation);
        } else {
            self.relations.insert(relation.name().clone(), relation);
        }
    }

    pub fn add_new_relation<V: Into<Vec<Attribute<Predicate>>>>(
        &mut self,
        predicate: Predicate,
        schema: V,
    ) -> Result<&mut Relation> {
        if self.relations.contains_key(&predicate) {
            Error::RelationExists(predicate).into()
        } else {
            self.add(Relation::new(predicate.clone(), schema));
            Ok(self.relation_mut(&predicate).unwrap())
        }
    }

    pub fn add_new_relation_from(
        &mut self,
        predicate: Predicate,
        attributes: &[Constant],
    ) -> Result<&mut Relation> {
        self.add_new_relation(
            predicate,
            attributes
                .iter()
                .map(|c| c.kind().into())
                .collect::<Vec<Attribute<Predicate>>>(),
        )
    }

    pub fn contains(&self, predicate: &Predicate) -> bool {
        self.relations.contains_key(predicate)
    }

    pub fn relation(&self, predicate: &Predicate) -> Option<&Relation> {
        self.relations.get(predicate)
    }

    pub fn relation_mut(&mut self, predicate: &Predicate) -> Option<&mut Relation> {
        self.relations.get_mut(predicate)
    }

    pub fn remove(&mut self, predicate: &Predicate) -> Option<Relation> {
        self.relations.remove(predicate)
    }

    // --------------------------------------------------------------------------------------------

    pub fn query(&self, query: &Query) -> View {
        self.matches(query.as_ref())
    }

    pub fn matches(&self, atom: &Atom) -> View {
        if let Some(relation) = self.relation(atom.predicate()) {
            let results = relation.matches(atom);
            if atom.variables().count() == 0 {
                // if all attributes are constant it is a presence query, not a selection.
                if results.is_empty() {
                    View::new_false()
                } else {
                    View::new_true()
                }
            } else {
                results
            }
        } else {
            // TODO: Is this actually an error?
            View::empty()
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn clone_with_schema_only(&self) -> Self {
        Self {
            relations: self
                .relations
                .iter()
                .map(|(p, r)| (p.clone(), r.clone_with_schema_only()))
                .collect(),
        }
    }

    pub fn merge(&mut self, other: Self) -> Result<()> {
        for (other_predicate, other_relation) in other.relations {
            if let Some(relation) = self.relations.get_mut(&other_predicate) {
                relation.extend(other_relation)?
            } else {
                self.relations.insert(other_predicate, other_relation);
            }
        }
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Predicate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Predicate {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        if Self::is_valid(s) {
            Ok(Self(s.to_owned()))
        } else {
            Error::InvalidValue(TYPE_NAME_PREDICATE.to_owned(), s.to_owned()).into()
        }
    }
}

impl AsRef<str> for Predicate {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<Predicate> for String {
    fn from(v: Predicate) -> Self {
        v.0
    }
}

impl AttributeName for Predicate {}

impl Predicate {
    pub fn is_valid(s: &str) -> bool {
        let mut chars = s.chars();
        (!s.is_empty())
            && chars.next().map(|c| c.is_lowercase()).unwrap()
            && chars.all(|c| c.is_alphanumeric() || c == CHAR_UNDERSCORE)
    }

    pub(crate) fn from_str_unchecked(s: &str) -> Self {
        Self(s.to_owned())
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

mod constant;
pub use constant::Constant;

mod relation;
pub(crate) use relation::BaseRelation;
pub use relation::{
    Attribute, AttributeIndex, AttributeKind, AttributeName, Fact, Relation, Schema,
};

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
#[cfg(feature = "parser")]
mod tests {
    use crate::edb::Constant;

    #[test]
    fn test_is_identifier() {
        assert!(Constant::is_identifier("socrates"));
        assert!(Constant::is_identifier("socrates_and_plato"));
        assert!(Constant::is_identifier("socrates1"));
        assert!(Constant::is_identifier("greek:socrates"));
        assert!(Constant::is_identifier("greek:Socrates"));
        assert!(Constant::is_identifier("greek:socrates_and_plato"));
        assert!(Constant::is_identifier("greek:Socrates_and_Plato"));
        assert!(Constant::is_identifier("greek:socrates1"));
        assert!(Constant::is_identifier("greek:Socrates1"));
    }

    #[test]
    fn test_is_not_identifier() {
        assert!(!Constant::is_identifier("Socrates"));
        assert!(!Constant::is_identifier("_and_plato"));
        assert!(!Constant::is_identifier("1socrates"));
        assert!(!Constant::is_identifier("Greek:socrates"));
        assert!(!Constant::is_identifier("_greek:socrates"));
        assert!(!Constant::is_identifier("greek:_and_plato"));
        assert!(!Constant::is_identifier("greek:socrates:plato"));
        assert!(!Constant::is_identifier(":greekSocrates"));
        assert!(!Constant::is_identifier("greek:"));
    }
}
