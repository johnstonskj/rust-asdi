/*!
One-line description.

More detailed description, with

# Example

*/

use crate::{
    Error, CHAR_COLON, CHAR_COMMA, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, CHAR_UNDERSCORE,
    RESERVED_BOOLEAN_FALSE, RESERVED_BOOLEAN_TRUE, RESERVED_PRAGMA_DECLARE, RESERVED_PREFIX,
    TYPE_NAME_PREDICATE,
};
use std::collections::{BTreeMap, HashSet};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Database {
    relations: BTreeMap<Predicate, Relation>,
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
    kind: AttributeKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AttributeKind {
    String,
    Integer,
    Boolean,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

impl Default for Database {
    fn default() -> Self {
        Self {
            relations: Default::default(),
        }
    }
}

impl Database {
    // --------------------------------------------------------------------------------------------

    // pub fn environment(&self) -> &Environment {
    //     &self.environment
    // }
    //
    // pub fn environment_mut(&mut self) -> &mut Environment {
    //     &mut self.environment
    // }

    // --------------------------------------------------------------------------------------------

    pub fn is_empty(&self) -> bool {
        self.relations.is_empty()
    }

    pub fn len(&self) -> usize {
        self.relations.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Relation> {
        self.relations.values()
    }

    pub fn add(&mut self, relation: Relation) {
        assert!(!self.relations.contains_key(&relation.predicate));
        self.relations.insert(relation.predicate.clone(), relation);
    }

    pub fn make_new_relation<V: Into<Vec<Attribute>>>(
        &mut self,
        predicate: Predicate,
        schema: V,
    ) -> Result<Relation, Error> {
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
    ) -> Result<Relation, Error> {
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

    pub fn clone_with_schema_only(&self) -> Self {
        Self {
            relations: self
                .relations
                .iter()
                .map(|(p, r)| (p.clone(), r.clone_with_schema_only()))
                .collect(),
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
            panic!();
        }
    }

    pub fn contains(&self, fact: &[Constant]) -> bool {
        self.facts.contains(fact)
    }

    // TODO: remove?

    pub fn conforms(&self, fact: &[Constant]) -> bool {
        self.schema
            .iter()
            .map(|a| a.kind)
            .collect::<Vec<AttributeKind>>()
            == fact
                .iter()
                .map(|c| c.kind())
                .collect::<Vec<AttributeKind>>()
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
            self.kind
        )
    }
}

impl From<AttributeKind> for Attribute {
    fn from(kind: AttributeKind) -> Self {
        Self { name: None, kind }
    }
}

impl From<Attribute> for AttributeKind {
    fn from(a: Attribute) -> Self {
        a.kind
    }
}

impl Attribute {
    pub fn new(name: Predicate, kind: AttributeKind) -> Self {
        Self {
            name: Some(name),
            kind,
        }
    }

    pub(crate) fn new_inner(name: Option<Predicate>, kind: AttributeKind) -> Self {
        Self { name, kind }
    }

    pub fn name(&self) -> Option<&Predicate> {
        self.name.as_ref()
    }

    pub fn kind(&self) -> AttributeKind {
        self.kind
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

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
        let mut chars = s.chars();
        !s.is_empty()
            && chars
                .next()
                .map(|c| c.is_alphabetic() || c == CHAR_UNDERSCORE)
                .is_some()
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

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
            && chars
                .next()
                .map(|c| c.is_lowercase() || c == CHAR_UNDERSCORE)
                .is_some()
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
