/*!
One-line description.

More detailed description, with

# Example

*/

use crate::error::{Error, Result};
use crate::eval::{Column, Table};
use crate::idb::Atom;
use crate::syntax::{
    CHAR_COLON, CHAR_COMMA, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, CHAR_UNDERSCORE,
    RESERVED_BOOLEAN_FALSE, RESERVED_BOOLEAN_TRUE, RESERVED_PRAGMA_DECLARE, RESERVED_PREFIX,
    TYPE_NAME_PREDICATE,
};
use crate::{Term, TYPE_NAME_CONST_UNKNOWN};
use std::collections::{BTreeMap, HashSet};
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

///
/// A fact is a simple statement that is either a [_predicate_](enum.Predicate.html) on it's own,
/// or a _predicate_ with one or more [_constant_](enum.Constant.html) arguments.
///
/// # Examples
///
/// Note that predicate identifiers always start with a lowercase character, and constants may be
/// identifiers, double-quoted string, integer, or boolean values.
///
/// ```prolog
/// predicate.
/// predicate(id, id).
/// predicate("str", 1).
/// predicate(id, "str").
/// ```
///
/// A predicate may be an identifier, or a string, as shown in the following.
///
/// ```prolog
/// name(id, "Socrates").
/// "known as"(id, "Socrates").
/// ```
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Relation {
    predicate: Predicate,
    schema: Vec<Attribute>,
    facts: HashSet<Vec<Constant>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fact {
    predicate: Option<Predicate>,
    values: Vec<Constant>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute {
    name: Option<Predicate>,
    kind: Option<AttributeKind>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AttributeKind {
    String,
    Integer,
    Boolean,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    String(String),
    Integer(i64),
    Boolean(bool),
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
        if let Some(existing) = self.relations.get_mut(&relation.predicate) {
            existing.update_schema(&relation);
            existing.facts.extend(relation.facts);
        } else {
            self.relations.insert(relation.predicate.clone(), relation);
        }
    }

    pub fn make_new_relation<V: Into<Vec<Attribute>>>(
        &mut self,
        predicate: Predicate,
        schema: V,
    ) -> Result<Relation> {
        if self.relations.contains_key(&predicate) {
            Error::RelationExists(predicate).into()
        } else {
            Ok(Relation::new(predicate, schema))
        }
    }

    pub fn make_new_relation_from(
        &mut self,
        predicate: Predicate,
        attributes: &[Constant],
    ) -> Result<Relation> {
        self.make_new_relation(
            predicate,
            attributes
                .iter()
                .map(|c| c.kind().into())
                .collect::<Vec<Attribute>>(),
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

    pub fn matches(&self, atom: &Atom) -> Table {
        if let Some(relation) = self.relation(atom.predicate()) {
            let results = relation.matches(atom);
            if atom.variables().count() == 0 {
                // if all attributes are constant it is a presence query, not a selection.
                if results.is_empty() {
                    Table::new_false()
                } else {
                    Table::new_true()
                }
            } else {
                results
            }
        } else {
            // TODO: Is this actually an error?
            Table::empty()
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

    pub fn merge(&mut self, other: Self) {
        for (other_predicate, other_relation) in other.relations {
            if let Some(relation) = self.relations.get_mut(&other_predicate) {
                relation.merge(other_relation)
            } else {
                self.relations.insert(other_predicate, other_relation);
            }
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl Relation {
    pub fn new<V: Into<Vec<Attribute>>>(predicate: Predicate, schema: V) -> Self {
        let schema = schema.into();
        let named: Vec<&Predicate> = schema
            .iter()
            .filter_map(|a| a.name.as_ref())
            .collect::<Vec<&Predicate>>();
        let hashed: HashSet<&&Predicate> = HashSet::from_iter(named.iter());
        if named.len() == hashed.len() {
            Self {
                predicate,
                schema,
                facts: Default::default(),
            }
        } else {
            // TODO: propagate errors
            panic!()
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn predicate(&self) -> &Predicate {
        &self.predicate
    }

    pub fn schema(&self) -> &Vec<Attribute> {
        &self.schema
    }

    pub fn arity(&self) -> usize {
        self.schema.len()
    }

    // --------------------------------------------------------------------------------------------

    pub fn is_empty(&self) -> bool {
        self.facts.is_empty()
    }

    pub fn len(&self) -> usize {
        self.facts.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Vec<Constant>> {
        self.facts.iter()
    }

    pub fn facts(&self) -> impl Iterator<Item = Fact> + '_ {
        self.facts
            .iter()
            .map(|f| Fact::new(self.predicate.clone(), f.clone()))
    }

    pub fn add<V: Into<Vec<Constant>>>(&mut self, fact: V) {
        let fact: Vec<Constant> = fact.into();
        if self.conforms(&fact) {
            self.facts.insert(fact);
        } else {
            println!("{:?} ?= {:?}", self.schema(), fact);
            // TODO: propagate errors
            panic!();
        }
    }

    pub fn contains(&self, fact: &[Constant]) -> bool {
        self.facts.contains(fact)
    }

    pub fn to_schema_decl(&self) -> String {
        format!(
            "{}{} {}{}{}{}{}",
            RESERVED_PREFIX,
            RESERVED_PRAGMA_DECLARE,
            self.predicate,
            CHAR_LEFT_PAREN,
            self.schema
                .iter()
                .map(Attribute::to_string)
                .collect::<Vec<String>>()
                .join(&format!("{} ", CHAR_COMMA)),
            CHAR_RIGHT_PAREN,
            CHAR_PERIOD
        )
    }

    // --------------------------------------------------------------------------------------------

    pub fn clone_with_schema_only(&self) -> Self {
        Self {
            predicate: self.predicate.clone(),
            schema: self.schema.clone(),
            facts: Default::default(),
        }
    }

    pub fn merge(&mut self, other: Self) {
        assert_eq!(self.predicate, other.predicate);
        assert_eq!(self.schema, other.schema);
        self.facts.extend(other.facts)
    }

    // --------------------------------------------------------------------------------------------

    pub fn matches(&self, atom: &Atom) -> Table {
        assert_eq!(atom.predicate(), self.predicate());

        let terms: Vec<&Term> = atom.terms().collect();

        Table::new_with_rows(
            terms
                .iter()
                .map(|term| match term {
                    Term::Variable(v) => Column::from(v.clone()),
                    Term::Constant(_) => Column::unknown(),
                })
                .collect::<Vec<Column>>(),
            self.facts
                .iter()
                .filter_map(|fact| self.terms_match(&terms, fact))
                .collect::<Vec<Vec<Constant>>>(),
        )
    }

    fn terms_match(&self, terms: &[&Term], fact: &[Constant]) -> Option<Vec<Constant>> {
        if terms
            .iter()
            .enumerate()
            .filter(|(_, term)| term.is_constant())
            .all(|(i, term)| term.as_constant().unwrap() == fact.get(i).unwrap())
        {
            Some(fact.to_vec())
        } else {
            None
        }
    }

    // --------------------------------------------------------------------------------------------

    fn update_schema(&mut self, other: &Self) {
        assert_eq!(self.schema.len(), other.schema.len());
        self.schema
            .iter_mut()
            .zip(other.schema().iter())
            .for_each(|(left, right)| {
                if let (None, Some(v)) = (&left.name, &right.name) {
                    left.name = Some(v.clone())
                }
                if let (None, Some(v)) = (left.kind, right.kind) {
                    left.kind = Some(v)
                }
            })
    }

    fn conforms(&self, fact: &[Constant]) -> bool {
        self.schema
            .iter()
            .map(|a| a.kind())
            .collect::<Vec<Option<AttributeKind>>>()
            == fact
                .iter()
                .map(|c| Some(c.kind()))
                .collect::<Vec<Option<AttributeKind>>>()
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Attribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            match &self.name {
                None => String::new(),
                Some(v) => format!("{}{} ", v, CHAR_COLON),
            },
            match &self.kind {
                None => TYPE_NAME_CONST_UNKNOWN.to_string(),
                Some(k) => k.to_string(),
            }
        )
    }
}

impl From<AttributeKind> for Attribute {
    fn from(kind: AttributeKind) -> Self {
        Self {
            name: None,
            kind: Some(kind),
        }
    }
}

impl Attribute {
    pub fn new(name: Predicate, kind: AttributeKind) -> Self {
        Self {
            name: Some(name),
            kind: Some(kind),
        }
    }

    pub(crate) fn new_inner<P: Into<Option<Predicate>>, K: Into<Option<AttributeKind>>>(
        name: P,
        kind: K,
    ) -> Self {
        Self {
            name: name.into(),
            kind: kind.into(),
        }
    }

    pub(crate) fn unknown() -> Self {
        Self {
            name: None,
            kind: None,
        }
    }

    pub fn name(&self) -> Option<&Predicate> {
        self.name.as_ref()
    }

    pub fn kind(&self) -> Option<AttributeKind> {
        self.kind.as_ref().copied()
    }

    pub fn has_unknown_kind(&self) -> bool {
        self.kind.is_none()
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for AttributeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::String => "string",
                Self::Integer => "integer",
                Self::Boolean => "boolean",
            }
        )
    }
}

impl FromStr for AttributeKind {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "str" | "string" => Ok(Self::String),
            "int" | "integer" => Ok(Self::Integer),
            "bool" | "boolean" => Ok(Self::Boolean),
            _ => Error::InvalidValue("ConstantKind".to_string(), s.to_string()).into(),
        }
    }
}

impl From<Constant> for AttributeKind {
    fn from(v: Constant) -> Self {
        v.kind()
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Fact {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}{}",
            match &self.predicate {
                None => CHAR_UNDERSCORE.to_string(),
                Some(p) => p.to_string(),
            },
            CHAR_LEFT_PAREN,
            self.values
                .iter()
                .map(Constant::to_string)
                .collect::<Vec<String>>()
                .join(&format!("{} ", CHAR_COMMA)),
            CHAR_RIGHT_PAREN,
            CHAR_PERIOD
        )
    }
}

impl From<Vec<Constant>> for Fact {
    fn from(v: Vec<Constant>) -> Self {
        Self {
            predicate: None,
            values: v,
        }
    }
}

impl From<Fact> for Vec<Constant> {
    fn from(v: Fact) -> Self {
        v.values
    }
}

impl Fact {
    pub fn new<V: Into<Vec<Constant>>>(predicate: Predicate, values: V) -> Self {
        Self {
            predicate: Some(predicate),
            values: values.into(),
        }
    }

    pub fn predicate(&self) -> Option<&Predicate> {
        self.predicate.as_ref()
    }

    pub fn values(&self) -> impl Iterator<Item = &Constant> {
        self.values.iter()
    }

    pub fn arity(&self) -> usize {
        self.values.len()
    }
}

// ------------------------------------------------------------------------------------------------

impl From<&str> for Constant {
    fn from(s: &str) -> Self {
        if !s.chars().any(|c| c.is_control()) {
            Self::String(s.to_owned())
        } else {
            panic!();
        }
    }
}

impl From<String> for Constant {
    fn from(v: String) -> Self {
        Self::from(v.as_str())
    }
}

impl From<i64> for Constant {
    fn from(v: i64) -> Self {
        Self::Integer(v)
    }
}

impl From<bool> for Constant {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::String(v) =>
                    if Self::is_identifier(v) {
                        v.to_string()
                    } else {
                        format!("{:?}", v)
                    },
                Self::Integer(v) => v.to_string(),
                Self::Boolean(v) => {
                    if *v {
                        format!("{}{}", RESERVED_PREFIX, RESERVED_BOOLEAN_TRUE)
                    } else {
                        format!("{}{}", RESERVED_PREFIX, RESERVED_BOOLEAN_FALSE)
                    }
                }
            }
        )
    }
}

impl Constant {
    pub fn kind(&self) -> AttributeKind {
        match self {
            Self::String(_) => AttributeKind::String,
            Self::Integer(_) => AttributeKind::Integer,
            Self::Boolean(_) => AttributeKind::Boolean,
        }
    }

    pub fn is_identifier(s: &str) -> bool {
        let parts = s.split(CHAR_COLON).collect::<Vec<&str>>();
        if parts.len() == 1 {
            Predicate::is_valid(s)
        } else if parts.len() == 2 {
            Predicate::is_valid(parts.get(0).unwrap())
                && Self::is_identifier_relaxed(parts.get(1).unwrap())
        } else {
            false
        }
    }

    fn is_identifier_relaxed(s: &str) -> bool {
        let mut chars = s.chars();
        !s.is_empty()
            && chars.next().map(|c| c.is_alphabetic()).is_some()
            && chars.all(|c| c.is_alphanumeric() || c == CHAR_UNDERSCORE)
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

impl Predicate {
    pub fn is_valid(s: &str) -> bool {
        let mut chars = s.chars();
        !s.is_empty()
            && chars.next().map(|c| c.is_lowercase()).is_some()
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

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
#[cfg(feature = "parser")]
mod tests {
    use crate::idb::Variable;
    use crate::parse::parse_str;
    use crate::{Atom, Predicate, Term};
    use std::str::FromStr;

    #[test]
    fn test_matches() {
        let program = parse_str(
            r#"#@declare human(string).
            
human("Socrates").

mortal(X) <- human(X).

?- mortal("Socrates").
"#,
        )
        .unwrap()
        .into_parsed();

        println!("{:#?}", program);
        println!("{}", program.to_string());

        let human = Predicate::from_str("human").unwrap();
        let qterm = Atom::new(human, [Term::Variable(Variable::from_str("X").unwrap())]);
        let results = program.database().matches(&qterm);

        println!("{}", results);
    }
}
