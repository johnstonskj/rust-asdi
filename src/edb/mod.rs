/*!
This module provides the set of types that primarily describe the Extensional Database (EDB).

Given the following fact:

```datalog
person("Alice", 49, @true).
```

We can deduce:

1. There exists an extensional [relation](Relation) with the [label](Predicate)
   `person`.
1. This relation has the following [schema](Schema):
    1. The arity of this relation is `3`.
    1. The [types](enum.AttributeKind.html) of the [attributes](Attribute) in this
       relation are `(string, integer, boolean)`.
1. This relation has _at least_ one fact which has the [constant](Constant) values
   `("Alice", 49, @true)`.

If we were to include an extensional relation declaration in our example, as follows:

```datalog
@assert person(name: string, age: integer, funny: boolean).

person("Alice", 49, @true).
```

We can add the [label](Predicate) for each attribute of `person` to the relation's schema.

# Example

The following declares two extensional [relations](Relation), `human` and `age`.

```datalog
@assert human(string).
@assert age(name: string, integer).
```

The following are all valid [facts](Fact) expressed in the text representation.

```datalog
string_fact("string").
string_fact(shortString).
string_fact(str:Short).
integer_fact(1234).
integer_fact(-4321).
boolean_fact(@true).
boolean_fact(@false).
```
*/

use crate::error::{Error, Result};
use crate::idb::{Atom, Row, View};
use crate::syntax::{
    CHAR_COLON, CHAR_COMMA, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, CHAR_UNDERSCORE,
    COLUMN_NAME_UNKNOWN, RESERVED_BOOLEAN_FALSE, RESERVED_BOOLEAN_TRUE, RESERVED_PRAGMA_ASSERT,
    RESERVED_PRAGMA_INFER, RESERVED_PREFIX, TYPE_NAME_PREDICATE,
};
use crate::{Term, Variable};
use std::collections::{BTreeMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use std::str::FromStr;
use tracing::{error, trace};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// Contains a set of named [`Relation`]s.
///
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Relations(BTreeMap<Predicate, Relation>);

///
/// A Relation comprises a [label](struct.Predicate.html), a [schema](struct.Schema.html) that
/// describes the attributes of the relation, and a set of [facts](struct.Fact.html).
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Relation {
    label: Rc<Predicate>,
    schema: Schema<Predicate>,
    facts: HashSet<Fact>,
}

// ------------------------------------------------------------------------------------------------

///
/// Schema attributes are named, however schema are used in places where the name may be of a
/// different type. This trait identifies the minimum set of implementations required for an
/// attribute name.
///
pub trait AttributeName: Clone + Debug + Display + PartialEq + Eq + PartialOrd + Ord {}

///
/// A schema is an ordered list of [attribute descriptions](Attribute).
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Schema<T>
where
    T: AttributeName,
{
    attributes: Vec<Attribute<T>>,
    label_index: BTreeMap<T, usize>,
}

///
/// An Attribute structure provides the label and [type](AttributeKind) of an attribute
/// within a relation.
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute<T>
where
    T: AttributeName,
{
    label: Option<T>,
    kind: Option<AttributeKind>,
}

///
/// This type allows for an attribute in a schema to be looked up either by its index, or attribute
/// [label](AttributeName).
///
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AttributeIndex<T>
where
    T: AttributeName,
{
    Label(T),
    Index(usize),
}

///
/// The currently supported set of types for [attributes](Attribute).
///
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AttributeKind {
    String,
    Integer,
    Boolean,
}

// ------------------------------------------------------------------------------------------------

///
/// A fact is a _ground atom_ and comprises an ordered list of [`Constant`] values within
/// a [`Relation`]. The arity and types for the values **must** comply with the
/// relation's schema. The label of a fact **must** be the same as the label of it's relation.
///  
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Fact {
    label: Rc<Predicate>,
    values: Vec<Constant>,
}

///
/// A constant value, with variants matching those in [AttributeKind](AttributeKind).
///
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    String(String),
    Integer(i64),
    Boolean(bool),
}

///
/// A predicate is a value from the set $\small \mathcal{P}$ and labels [relations](struct.Relation.html),
/// and [atoms](../idb/Atom), and [facts](struct.Fact.html) associated with relations.
///
/// A predicate must start with a Unicode **lower** case letter, followed by any Unicode cased letter or
/// Unicode decimal digit, or the `'_'` underscore character.
///
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

impl<T> From<Attribute<T>> for Schema<T>
where
    T: AttributeName,
{
    fn from(c: Attribute<T>) -> Self {
        Self::new(vec![c])
    }
}

impl<T> From<Vec<Attribute<T>>> for Schema<T>
where
    T: AttributeName,
{
    fn from(cs: Vec<Attribute<T>>) -> Self {
        Self::new(cs)
    }
}

impl<T> Schema<T>
where
    T: AttributeName,
{
    pub fn new<V: Into<Vec<Attribute<T>>>>(attributes: V) -> Self {
        let attributes = attributes.into();
        let named: Vec<(T, usize)> = attributes
            .iter()
            .enumerate()
            .filter_map(|(i, c)| c.label().map(|var| (var.clone(), i)))
            .collect();
        let pre_hash_len = named.len();

        let named = BTreeMap::from_iter(named);

        assert_eq!(named.len(), pre_hash_len);

        Self {
            attributes,
            label_index: named,
        }
    }

    pub fn empty() -> Self {
        Self {
            attributes: Default::default(),
            label_index: Default::default(),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn is_empty(&self) -> bool {
        self.attributes.is_empty()
    }

    pub fn arity(&self) -> usize {
        self.attributes.len()
    }

    // --------------------------------------------------------------------------------------------

    pub fn iter(&self) -> impl Iterator<Item = &Attribute<T>> {
        self.attributes.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Attribute<T>> {
        self.attributes.iter_mut()
    }

    // pub fn contains<I: Into<AttributeIndex<T>>>(&self, index: I) -> bool {
    //     match index.into() {
    //         AttributeIndex::Label(n) => self.named_index.contains_key(&n),
    //         AttributeIndex::Index(i) => i < self.arity(),
    //     }
    // }

    pub fn get<I: Into<AttributeIndex<T>>>(&self, index: I) -> Option<&Attribute<T>> {
        match index.into() {
            AttributeIndex::Label(n) => match self.label_index.get(&n) {
                Some(i) => self.attributes.get(*i),
                None => None,
            },
            AttributeIndex::Index(i) => self.attributes.get(i),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn labels(&self) -> impl Iterator<Item = &T> {
        self.label_index.keys()
    }

    pub fn label_intersection(&self, other: &Self) -> Vec<(&T, usize, usize)> {
        self.label_index
            .iter()
            .filter_map(|(var, i)| {
                other
                    .label_index
                    .get(var)
                    .map(|other_i| (var, *i, *other_i))
            })
            .collect()
    }

    pub fn label_union(&self, other: &Self) -> Vec<T> {
        let mut all: Vec<T> = self.labels().cloned().collect();
        for var in other.labels() {
            if !all.contains(var) {
                all.push(var.clone());
            }
        }
        all
    }

    pub fn label_to_index(&self, n: &T) -> Option<usize> {
        self.label_index.get(n).copied()
    }

    // --------------------------------------------------------------------------------------------

    pub fn conforms(&mut self, row: &[Constant]) -> bool {
        row.len() == self.arity()
            && row.iter().zip(self.iter()).all(|(v, c)| {
                if let Some(kind) = c.kind {
                    v.kind() == kind
                } else {
                    true
                }
            })
    }
    pub fn to_column_decl(&self, emit_unknown: bool) -> String {
        self.attributes
            .iter()
            .map(|a| a.to_column_decl(emit_unknown))
            .collect::<Vec<String>>()
            .join(&format!("{} ", CHAR_COMMA))
    }
}

// ------------------------------------------------------------------------------------------------

impl<T> From<AttributeKind> for Attribute<T>
where
    T: AttributeName,
{
    fn from(kind: AttributeKind) -> Self {
        Self::typed(kind)
    }
}

impl<T> From<T> for Attribute<T>
where
    T: AttributeName,
{
    fn from(name: T) -> Self {
        Self::labeled(name)
    }
}

impl<T> Attribute<T>
where
    T: AttributeName,
{
    pub fn anonymous() -> Self {
        Self::new_inner(None, None)
    }

    pub fn labeled(name: T) -> Self {
        Self::new_inner(name, None)
    }

    pub fn typed(kind: AttributeKind) -> Self {
        Self::new_inner(None, kind)
    }

    pub fn string() -> Self {
        Self::new_inner(None, AttributeKind::String)
    }

    pub fn string_labeled(name: T) -> Self {
        Self::new_inner(name, AttributeKind::String)
    }

    pub fn integer() -> Self {
        Self::new_inner(None, AttributeKind::Integer)
    }

    pub fn integer_labeled(name: T) -> Self {
        Self::new_inner(name, AttributeKind::Integer)
    }

    pub fn boolean() -> Self {
        Self::new_inner(None, AttributeKind::Boolean)
    }

    pub fn boolean_labeled(name: T) -> Self {
        Self::new_inner(name, AttributeKind::Boolean)
    }

    pub fn new(name: T, kind: AttributeKind) -> Self {
        Self::new_inner(name, kind)
    }

    pub(crate) fn new_inner<P: Into<Option<T>>, K: Into<Option<AttributeKind>>>(
        name: P,
        kind: K,
    ) -> Self {
        Self {
            label: name.into(),
            kind: kind.into(),
        }
    }

    pub fn label(&self) -> Option<&T> {
        self.label.as_ref()
    }

    pub fn is_anonymous(&self) -> bool {
        self.label.is_none()
    }

    pub fn kind(&self) -> Option<AttributeKind> {
        self.kind.as_ref().copied()
    }

    pub(crate) fn override_kind(&mut self, kind: AttributeKind) -> Option<AttributeKind> {
        let old_kind = self.kind;
        self.kind = Some(kind);
        old_kind
    }

    pub fn is_untyped(&self) -> bool {
        self.kind.is_none()
    }

    pub fn update_from(&mut self, other: &Self) {
        if let (None, Some(v)) = (&self.label, &other.label) {
            self.label = Some(v.clone())
        }
        if let (None, Some(v)) = (self.kind, other.kind) {
            self.kind = Some(v)
        }
    }

    pub fn to_column_decl(&self, emit_unknown: bool) -> String {
        match (&self.label, &self.kind) {
            (None, None) => String::new(),
            (Some(n), Some(k)) => format!("{}{} {}", n, CHAR_COLON, k),
            (Some(n), None) => format!("{}", n),
            (None, Some(k)) => {
                if emit_unknown {
                    format!("{}{} {}", COLUMN_NAME_UNKNOWN, CHAR_COLON, k)
                } else {
                    k.to_string()
                }
            }
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl<T> From<usize> for AttributeIndex<T>
where
    T: AttributeName,
{
    fn from(i: usize) -> Self {
        Self::Index(i)
    }
}

impl<T> From<T> for AttributeIndex<T>
where
    T: AttributeName,
{
    fn from(v: T) -> Self {
        Self::Label(v)
    }
}

impl<T> AttributeIndex<T>
where
    T: AttributeName,
{
    pub fn is_named(&self) -> bool {
        matches!(self, Self::Label(_))
    }

    pub fn as_name(&self) -> Option<&T> {
        match self {
            Self::Label(v) => Some(v),
            _ => None,
        }
    }

    pub fn is_indexed(&self) -> bool {
        matches!(self, Self::Label(_))
    }

    pub fn as_index(&self) -> Option<&usize> {
        match self {
            Self::Index(v) => Some(v),
            _ => None,
        }
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

impl Relations {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn all_len(&self) -> usize {
        self.iter().map(|r| r.len()).sum()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Relation> {
        self.0.values()
    }

    pub fn schema(&self) -> impl Iterator<Item = (&Predicate, &Schema<Predicate>)> {
        self.0.values().map(|r| (r.name(), r.schema()))
    }

    pub fn add(&mut self, relation: Relation) {
        if let Some(existing) = self.0.get_mut(relation.name()) {
            existing.update_schema(&relation);
            existing.extend(relation).unwrap();
        } else {
            self.0.insert(relation.name().clone(), relation);
        }
    }

    pub fn add_new_relation<V: Into<Schema<Predicate>> + Debug>(
        &mut self,
        predicate: Predicate,
        schema: V,
    ) -> Result<&mut Relation> {
        trace!(
            "add_new_relation > predicate: {}, schema: {:?}",
            predicate,
            schema
        );
        let predicate = predicate;
        if self.0.contains_key(&predicate) {
            Error::RelationExists(predicate).into()
        } else {
            let relation = Relation::new(predicate.clone(), schema);
            trace!("add_new_relation < relation: {:?}", relation);
            self.add(relation);
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
        self.0.contains_key(predicate)
    }

    pub fn relation(&self, predicate: &Predicate) -> Option<&Relation> {
        self.0.get(predicate)
    }

    pub fn relation_mut(&mut self, predicate: &Predicate) -> Option<&mut Relation> {
        self.0.get_mut(predicate)
    }

    pub fn matches(&self, atom: &Atom) -> Option<View> {
        if let Some(relation) = self.0.get(atom.label()) {
            trace!(
                "matches > predicate: {}, relation: {:?}",
                atom.label(),
                relation.name()
            );
            let results = relation.matches(atom);
            if atom.variables().count() == 0 {
                // if all attributes are constant it is a presence query, not a selection.
                if results.is_empty() {
                    Some(View::new_false())
                } else {
                    Some(View::new_true())
                }
            } else {
                Some(results)
            }
        } else {
            None
        }
    }

    pub fn clone_with_schema_only(&self) -> Self {
        Self(
            self.0
                .iter()
                .map(|(p, r)| (p.clone(), r.clone_with_schema_only()))
                .collect(),
        )
    }

    pub fn merge(&mut self, other: Self) -> Result<()> {
        for (other_predicate, other_relation) in other.0 {
            if let Some(relation) = self.0.get_mut(&other_predicate) {
                relation.extend(other_relation)?
            } else {
                self.0.insert(other_predicate, other_relation);
            }
        }
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------

impl Relation {
    pub fn new<V: Into<Schema<Predicate>>>(name: Predicate, schema: V) -> Self {
        Self {
            label: Rc::from(name),
            schema: schema.into(),
            facts: Default::default(),
        }
    }

    pub fn clone_with_schema_only(&self) -> Self {
        Self {
            label: self.label.clone(),
            schema: self.schema.clone(),
            facts: Default::default(),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn name(&self) -> &Predicate {
        &self.label
    }

    // --------------------------------------------------------------------------------------------

    pub fn schema(&self) -> &Schema<Predicate> {
        &self.schema
    }

    pub(crate) fn update_schema(&mut self, other: &Self) {
        assert_eq!(self.schema.arity(), other.schema.arity());
        self.schema
            .iter_mut()
            .zip(other.schema().iter())
            .for_each(|(left, right)| left.update_from(right))
    }

    pub fn conforms(&self, fact: &[Constant]) -> bool {
        self.schema
            .iter()
            .map(|a| a.kind())
            .zip(fact.iter())
            .all(|(attribute, constant)| match attribute {
                None => true,
                Some(kind) => kind == constant.kind(),
            })
    }

    // --------------------------------------------------------------------------------------------

    pub fn is_empty(&self) -> bool {
        self.facts.is_empty()
    }

    pub fn len(&self) -> usize {
        self.facts.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Fact> {
        self.facts.iter()
    }

    pub fn add_as_fact<V: Into<Vec<Constant>>>(&mut self, values: V) -> Result<()> {
        let values: Vec<Constant> = values.into();
        if self.schema.conforms(&values) {
            self.add(Fact::new(self.label.clone(), values))?;
        } else {
            error!("Provided row does not conform to schema.");
            // TODO: propagate errors
            panic!();
        }
        Ok(())
    }

    pub fn add(&mut self, fact: Fact) -> Result<()> {
        // TODO: propagate errors
        assert_eq!(fact.label, self.label);
        self.facts.insert(fact);
        Ok(())
    }

    pub fn extend(&mut self, other: Self) -> Result<()> {
        trace!("extend > name {:?} == {:?}", self.label, other.label);
        assert_eq!(self.label, other.label);
        trace!("extend > schema {:?} == {:?}", self.schema, other.schema);
        assert_eq!(self.schema, other.schema);
        for fact in other.facts.into_iter() {
            self.add(fact)?;
        }
        Ok(())
    }

    // --------------------------------------------------------------------------------------------

    pub(crate) fn matches(&self, atom: &Atom) -> View {
        let terms: Vec<&Term> = atom.terms().collect();
        self.match_terms(terms)
    }

    #[allow(single_use_lifetimes)]
    fn match_terms<'a, V: Into<Vec<&'a Term>>>(&self, terms: V) -> View {
        let terms = terms.into();

        View::new_with_facts(
            terms
                .iter()
                .enumerate()
                .map(|(i, term)| {
                    let mut term = match term {
                        Term::Variable(v) => Attribute::from(v.clone()),
                        Term::Constant(v) => Attribute::typed(v.kind()),
                    };
                    if term.kind().is_none() {
                        if let Some(kind) = self.schema().get(i).unwrap().kind() {
                            term.override_kind(kind);
                        }
                    }
                    term
                })
                .collect::<Vec<Attribute<Variable>>>(),
            self.facts
                .iter()
                .filter_map(|fact| self.match_terms_inner(&terms, fact.values()))
                .collect::<Vec<Row>>(),
        )
    }

    fn match_terms_inner(&self, terms: &[&Term], fact: &[Constant]) -> Option<Row> {
        if terms
            .iter()
            .enumerate()
            .filter(|(_, term)| term.is_constant())
            .all(|(i, term)| term.as_constant().unwrap() == fact.get(i).unwrap())
        {
            Some(Row::from(fact.to_vec()))
        } else {
            None
        }
    }

    // --------------------------------------------------------------------------------------------

    pub(crate) fn to_schema_decl(&self, extensional: bool, emit_unknown: bool) -> String {
        format!(
            "{}{} {}{}{}{}{}",
            RESERVED_PREFIX,
            if extensional {
                RESERVED_PRAGMA_ASSERT
            } else {
                RESERVED_PRAGMA_INFER
            },
            self.name(),
            CHAR_LEFT_PAREN,
            self.schema().to_column_decl(emit_unknown),
            CHAR_RIGHT_PAREN,
            CHAR_PERIOD
        )
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Fact {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}{}",
            self.label,
            CHAR_LEFT_PAREN,
            self.iter()
                .map(Constant::to_string)
                .collect::<Vec<String>>()
                .join(&format!("{} ", CHAR_COMMA)),
            CHAR_RIGHT_PAREN,
            CHAR_PERIOD
        )
    }
}

impl From<Fact> for Vec<Constant> {
    fn from(v: Fact) -> Self {
        v.values
    }
}

impl Fact {
    pub fn new<V: Into<Vec<Constant>>>(name: Rc<Predicate>, values: V) -> Self {
        Self {
            label: name,
            values: values.into(),
        }
    }

    pub fn name(&self) -> &Predicate {
        &self.label
    }

    pub fn iter(&self) -> impl Iterator<Item = &Constant> {
        self.values.iter()
    }

    fn values(&self) -> &Vec<Constant> {
        &self.values
    }

    pub fn get(&self, index: usize) -> Option<&Constant> {
        self.values.get(index)
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
    pub fn new_true() -> Self {
        Self::Boolean(true)
    }

    pub fn new_false() -> Self {
        Self::Boolean(false)
    }

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
        (!s.is_empty())
            && chars.next().map(|c| c.is_alphabetic()).unwrap()
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

pub mod io;

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
#[cfg(feature = "parser")]
mod tests {
    use crate::edb::Constant;
    use crate::edb::Predicate;
    use crate::idb::{Atom, Term, Variable};
    use crate::parse::parse_str;
    use std::str::FromStr;

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

    #[test]
    fn test_matches() {
        let program = parse_str(
            r#"#@assert human(string).
            
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

        let qterm = Atom::new(
            human.clone(),
            [Term::Variable(Variable::from_str("X").unwrap())],
        );
        let results = program.extensional().matches(&qterm).unwrap();
        println!("{}", results);

        let qterm = Atom::new(human, [Term::Constant(Constant::from("Socrates"))]);
        let results = program.extensional().matches(&qterm).unwrap();
        println!("{}", results);
    }
}
