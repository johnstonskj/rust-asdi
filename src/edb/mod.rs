/*!
This module provides the set of types that primarily describe the Extensional Database (EDB).

![module UML](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/book/src/model/edb.svg)

Given the following fact:

```datalog
person("Alice", 49, true).
```

We can deduce:

1. There exists an extensional [relation](Relation) with the [label](Predicate)
   `person`.
1. This relation has the following [schema](Schema):
    1. The arity of this relation is `3`.
    1. The [types](enum.AttributeKind.html) of the [attributes](Attribute) in this
       relation are `(string, integer, boolean)`.
1. This relation has _at least_ one fact which has the [constant](Constant) values
   `("Alice", 49, true)`.

If we were to include an extensional relation declaration, using the `assert` pragma, in our
example from above:

```datalog
.assert person(name: string, age: integer, funny: boolean).

person("Alice", 49, true).
```

we may now add the [label](Predicate) for each attribute of `person` to the relation's schema.

Sometimes, where the set of facts for an extensional relation comes from an external source, _or_
is somewhat large it is useful to place the data in an external file. The `input` pragma allows
us to reference a single file that contains data for a relation.

```datalog
.assert person(name: string, age: integer, funny: boolean).
.input(person, "data/people.csv", "csv").
```

Note that any use of `input` requires that the relation exists in the extensional database, either
by means of the `assert` pragma, or by having previously asserted facts for this relation.

# Examples

The following declares two extensional [relations](Relation), `human` and `age`.

```datalog
.assert human(string).
.assert age(name: string, integer).
```

The following is the API equivalent of the Datalog program above.

```rust
use asdi::edb::{Attribute, Predicate, Relation, RelationSet};
use std::str::FromStr;
use asdi::{NameReferenceSet, Program};

let mut program = Program::default();

let human = program.predicates().fetch("human").unwrap();
program.extensional_mut().add_new_relation(
    human,
    vec![Attribute::string()]);

let relation = Relation::new(
    program
        .predicates()
        .fetch("age")
        .unwrap(),
    vec![
        Attribute::string_labeled(program
            .predicates()
            .fetch("name")
            .unwrap()),
        Attribute::integer()
    ]);
program.extensional_mut().add(relation);
```

The following are all valid [facts](Fact) expressed in the text representation.

```datalog
string_fact("string").
string_fact(shortString).
string_fact(str:Short).
integer_fact(1234).
integer_fact(-4321).
boolean_fact(true).
boolean_fact(false).
```
*/

use crate::error::{
    attribute_does_not_exist, attribute_index_invalid, fact_does_not_correspond_to_schema,
    invalid_value, nullary_facts_not_allowed, relation_exists, Error, Result,
};
use crate::idb::query::relational::{FactOps, Projection, Selection};
use crate::idb::query::{Queryable, Row, View};
use crate::idb::Atom;
use crate::syntax::{
    ANONYMOUS_COLUMN_NAME, BOOLEAN_LITERAL_FALSE, BOOLEAN_LITERAL_TRUE, CHAR_COLON,
    CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, CHAR_UNDERSCORE, COMMA_SEPARATOR,
    FUNCTIONAL_DEPENDENCY_UNICODE_SYMBOL, PRAGMA_ID_ASSERT, PRAGMA_ID_INFER, RESERVED_PREFIX,
    TYPE_NAME_PREDICATE,
};
use crate::{
    AttributeName, AttributeNameRef, Collection, IndexedCollection, Labeled, MaybeAnonymous, Term,
    Variable,
};
use ordered_float::NotNan;
use paste::paste;
use std::collections::{BTreeMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Sub;
use std::rc::Rc;
use std::str::FromStr;
use tracing::{error, trace};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// A Set-like collection of [`Relation`]s, the collection is indexed by the relation's label.
///
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RelationSet(BTreeMap<Predicate, Relation>);

///
/// A Relation comprises a [label](struct.Predicate.html), a [schema](struct.Schema.html) that
/// describes the attributes of the relation, and a set of [facts](struct.Fact.html).
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Relation {
    label: PredicateRef,
    schema: Schema<Predicate>,
    facts: HashSet<Fact>,
    pragma: Option<io::FilePragma>,
}

// ------------------------------------------------------------------------------------------------

///
/// A schema is an ordered list of [attribute descriptions](Attribute).
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Schema<T>
where
    T: AttributeName,
{
    attributes: Vec<Attribute<T>>,
    label_index: BTreeMap<AttributeNameRef<T>, usize>,
    functional_dependencies: HashSet<FunctionalDependency>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionalDependency {
    determinant: Vec<usize>,
    dependent: Vec<usize>,
}

///
/// An Attribute structure provides the label and [type](AttributeKind) of an attribute
/// within a relation.
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Attribute<T>
where
    T: AttributeName,
{
    index: Option<usize>,
    label: Option<AttributeNameRef<T>>,
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
    Label(AttributeNameRef<T>),
    Index(usize),
}

///
/// The currently supported set of types for [attributes](Attribute).
///
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AttributeKind {
    String,
    Integer,
    Float,
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
    label: PredicateRef,
    values: Vec<Constant>,
}

///
/// A constant value, with variants matching those in [AttributeKind](AttributeKind).
///
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    String(String),
    Number(Number),
    Boolean(bool),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Number(NumberInner);

// ------------------------------------------------------------------------------------------------

///
/// A predicate is a value from the set $\small \mathcal{P}$ and labels [relations](struct.Relation.html),
/// and [atoms](../idb/Atom), and [facts](struct.Fact.html) associated with relations.
///
/// A predicate must start with a Unicode **lower** case letter, followed by any Unicode cased letter or
/// Unicode decimal digit, or the `'_'` underscore character.
///
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Predicate(String);

///
/// We use a reference-counted type for predicates to try and reduce the number of instances of a
/// commonly used value.
///
/// The type [PredicateSet](../struct.PredicateSet.html), and the program's
/// shared set via [predicates](../struct.PredicateSet.html#method.predicates) allows for common
/// memory usage within a program.
///
pub type PredicateRef = Rc<Predicate>;

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NumberInner {
    Integer(i64),
    Float(NotNan<f64>),
}

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

impl<T> IntoIterator for Schema<T>
where
    T: AttributeName,
{
    type Item = Attribute<T>;
    type IntoIter = std::vec::IntoIter<Attribute<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.attributes.into_iter()
    }
}

impl<T> FromIterator<Attribute<T>> for Schema<T>
where
    T: AttributeName,
{
    fn from_iter<I: IntoIterator<Item = Attribute<T>>>(iter: I) -> Self {
        Self::new(iter.into_iter().collect::<Vec<Attribute<T>>>())
    }
}

impl<T> Collection<Attribute<T>> for Schema<T>
where
    T: AttributeName,
{
    delegate!(is_empty, attributes -> bool);

    delegate!(len, attributes -> usize);

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Attribute<T>> + '_> {
        Box::new(self.attributes.iter())
    }

    fn contains(&self, value: &Attribute<T>) -> bool {
        self.attributes.contains(value)
    }
}

impl<T> IndexedCollection<AttributeIndex<T>, Attribute<T>> for Schema<T>
where
    T: AttributeName,
{
    fn get(&self, index: &AttributeIndex<T>) -> Option<&Attribute<T>> {
        match index {
            AttributeIndex::Label(n) => match self.label_index.get(n) {
                Some(i) => self.attributes.get(*i),
                None => None,
            },
            AttributeIndex::Index(i) => self.attributes.get(*i),
        }
    }

    fn contains_index(&self, index: &AttributeIndex<T>) -> bool {
        match index {
            AttributeIndex::Label(n) => self.label_index.get(n).is_some(),
            AttributeIndex::Index(i) => *i < self.len(),
        }
    }
}

impl<T> Schema<T>
where
    T: AttributeName,
{
    pub fn new<V: Into<Vec<Attribute<T>>>>(attributes: V) -> Self {
        let attributes = attributes.into();
        let named: Vec<(AttributeNameRef<T>, usize)> = attributes
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
            functional_dependencies: Default::default(),
        }
    }

    pub fn empty() -> Self {
        Self {
            attributes: Default::default(),
            label_index: Default::default(),
            functional_dependencies: Default::default(),
        }
    }

    // --------------------------------------------------------------------------------------------

    // This does not need to be public at this time.
    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut Attribute<T>> {
        self.attributes.iter_mut()
    }

    // --------------------------------------------------------------------------------------------

    pub fn labels(&self) -> impl Iterator<Item = &AttributeNameRef<T>> {
        self.label_index.keys()
    }

    pub fn label_intersection(&self, other: &Self) -> Vec<(&AttributeNameRef<T>, usize, usize)> {
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

    pub fn label_union(&self, other: &Self) -> Vec<AttributeNameRef<T>> {
        let mut all: Vec<AttributeNameRef<T>> = self.labels().cloned().collect();
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
        row.len() == self.len()
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
            .join(COMMA_SEPARATOR)
    }

    // --------------------------------------------------------------------------------------------

    pub fn attribute_index_to_index(&self, index: &AttributeIndex<T>) -> Result<usize> {
        match index {
            AttributeIndex::Label(label) => {
                if let Some(index) = self.label_to_index(label) {
                    Ok(index)
                } else {
                    Err(attribute_does_not_exist(label.to_string()))
                }
            }
            AttributeIndex::Index(index) => {
                if *index < self.attributes.len() {
                    Ok(*index)
                } else {
                    Err(attribute_index_invalid(*index))
                }
            }
        }
    }

    pub fn has_functional_dependencies(&self) -> bool {
        !self.functional_dependencies.is_empty()
    }

    pub fn functional_dependencies(&self) -> &HashSet<FunctionalDependency> {
        &self.functional_dependencies
    }

    pub fn add_functional_attribute(&mut self, determinant: AttributeIndex<T>) -> Result<bool> {
        let determinant = self.attribute_index_to_index(&determinant)?;
        let dependents: Vec<usize> = (0..self.attributes.len().sub(determinant)).collect();

        Ok(self
            .functional_dependencies
            .insert(FunctionalDependency::new(vec![determinant], dependents)))
    }

    pub fn add_functional_dependency<V: Into<Vec<AttributeIndex<T>>>>(
        &mut self,
        determinant: V,
        dependent: V,
    ) -> Result<bool> {
        let determinant = self.attribute_indices_to_index(&determinant.into())?;
        let determinant_hash: HashSet<usize> = HashSet::from_iter(determinant.iter().copied());
        assert_eq!(determinant.len(), determinant_hash.len());

        let dependent = self.attribute_indices_to_index(&dependent.into())?;
        let dependent_hash: HashSet<usize> = HashSet::from_iter(dependent.iter().cloned());
        assert_eq!(dependent.len(), dependent_hash.len());

        assert_eq!(determinant_hash.intersection(&dependent_hash).count(), 0);

        Ok(self
            .functional_dependencies
            .insert(FunctionalDependency::new(determinant, dependent)))
    }

    fn attribute_indices_to_index(&self, indices: &[AttributeIndex<T>]) -> Result<Vec<usize>> {
        indices
            .iter()
            .map(|i| self.attribute_index_to_index(i))
            .collect()
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for FunctionalDependency {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.determinants()
                .map(|d| (d + 1).to_string())
                .collect::<Vec<String>>()
                .join(COMMA_SEPARATOR),
            FUNCTIONAL_DEPENDENCY_UNICODE_SYMBOL,
            self.dependents()
                .map(|d| (d + 1).to_string())
                .collect::<Vec<String>>()
                .join(COMMA_SEPARATOR),
        )
    }
}

impl FunctionalDependency {
    fn new<V: Into<Vec<usize>>>(determinants: V, dependents: V) -> Self {
        Self {
            determinant: determinants.into(),
            dependent: dependents.into(),
        }
    }

    pub fn determinants(&self) -> impl Iterator<Item = &usize> {
        self.determinant.iter()
    }

    pub fn dependents(&self) -> impl Iterator<Item = &usize> {
        self.dependent.iter()
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

impl<T> From<AttributeNameRef<T>> for Attribute<T>
where
    T: AttributeName,
{
    fn from(label: AttributeNameRef<T>) -> Self {
        Self::labeled(label)
    }
}

impl<T> MaybeAnonymous for Attribute<T>
where
    T: AttributeName,
{
    fn anonymous() -> Self {
        Self::new_inner(None, None)
    }

    fn is_anonymous(&self) -> bool {
        self.label.is_none()
    }
}

impl<T> Attribute<T>
where
    T: AttributeName,
{
    pub fn labeled(name: AttributeNameRef<T>) -> Self {
        Self::new_inner(name, None)
    }

    pub fn typed(kind: AttributeKind) -> Self {
        Self::new_inner(None, kind)
    }

    pub fn string() -> Self {
        Self::new_inner(None, AttributeKind::String)
    }

    pub fn string_labeled(name: AttributeNameRef<T>) -> Self {
        Self::new_inner(name, AttributeKind::String)
    }

    pub fn integer() -> Self {
        Self::new_inner(None, AttributeKind::Integer)
    }

    pub fn integer_labeled(name: AttributeNameRef<T>) -> Self {
        Self::new_inner(name, AttributeKind::Integer)
    }

    pub fn boolean() -> Self {
        Self::new_inner(None, AttributeKind::Boolean)
    }

    pub fn boolean_labeled(name: AttributeNameRef<T>) -> Self {
        Self::new_inner(name, AttributeKind::Boolean)
    }

    pub fn new(name: AttributeNameRef<T>, kind: AttributeKind) -> Self {
        Self::new_inner(name, kind)
    }

    pub(crate) fn new_inner<
        P: Into<Option<AttributeNameRef<T>>>,
        K: Into<Option<AttributeKind>>,
    >(
        name: P,
        kind: K,
    ) -> Self {
        Self {
            index: None,
            label: name.into(),
            kind: kind.into(),
        }
    }

    pub fn index(&self) -> Option<usize> {
        self.index
    }

    pub fn set_index(&mut self, index: usize) {
        self.index = Some(index)
    }

    ///
    /// Returns this value's label, or `None` if anonymous.
    ///
    pub fn label(&self) -> Option<&AttributeNameRef<T>> {
        self.label.as_ref()
    }

    ///
    /// Returns `true` if this value has a label, else `false`.
    ///
    pub fn is_labeled(&self) -> bool {
        !self.is_anonymous()
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
                    format!("{}{} {}", ANONYMOUS_COLUMN_NAME, CHAR_COLON, k)
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

impl<T> From<AttributeNameRef<T>> for AttributeIndex<T>
where
    T: AttributeName,
{
    fn from(v: AttributeNameRef<T>) -> Self {
        Self::Label(v)
    }
}

impl<T> AttributeIndex<T>
where
    T: AttributeName,
{
    self_is_as!(named, Label, T);

    self_is_as!(indexed, Index, usize);
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
                Self::Float => "float",
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
            "float" => Ok(Self::Float),
            "bool" | "boolean" => Ok(Self::Boolean),
            _ => invalid_value("ConstantKind", s).into(),
        }
    }
}

impl From<Constant> for AttributeKind {
    fn from(v: Constant) -> Self {
        v.kind()
    }
}

// ------------------------------------------------------------------------------------------------

impl<'a> IntoIterator for &'a mut RelationSet {
    type Item = (&'a Predicate, &'a mut Relation);
    type IntoIter = std::collections::btree_map::IterMut<'a, Predicate, Relation>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl FromIterator<Relation> for RelationSet {
    fn from_iter<T: IntoIterator<Item = Relation>>(iter: T) -> Self {
        Self(BTreeMap::from_iter(
            iter.into_iter().map(|r| (r.label().clone(), r)),
        ))
    }
}

impl Queryable for RelationSet {
    fn query_atom(&self, query: &Atom) -> Result<Option<View>> {
        if let Some(relation) = self.0.get(query.label()) {
            relation.query_atom(query)
        } else {
            Ok(None)
        }
    }
}

impl Collection<Relation> for RelationSet {
    delegate!(is_empty -> bool);

    delegate!(len -> usize);

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Relation> + '_> {
        Box::new(self.0.values())
    }

    fn contains(&self, value: &Relation) -> bool {
        self.0.values().any(|r| r == value)
    }
}

impl IndexedCollection<Predicate, Relation> for RelationSet {
    fn get(&self, index: &Predicate) -> Option<&Relation> {
        self.0.get(index)
    }

    fn contains_index(&self, index: &Predicate) -> bool {
        self.0.contains_key(index)
    }
}

impl RelationSet {
    pub fn flat_count(&self) -> usize {
        self.iter().map(|r| r.len()).sum()
    }

    pub fn schema(&self) -> impl Iterator<Item = (&Predicate, &Schema<Predicate>)> {
        self.0.values().map(|r| (r.label(), r.schema()))
    }

    pub fn add(&mut self, relation: Relation) {
        if let Some(existing) = self.0.get_mut(relation.label()) {
            existing.update_schema(&relation);
            existing.extend(relation).unwrap();
        } else {
            self.0.insert(relation.label().clone(), relation);
        }
    }

    pub fn add_new_relation<V: Into<Schema<Predicate>> + Debug>(
        &mut self,
        predicate: PredicateRef,
        schema: V,
    ) -> Result<&mut Relation> {
        trace!(
            "add_new_relation > predicate: {}, schema: {:?}",
            predicate,
            schema
        );
        let predicate = predicate;
        if self.0.contains_key(&predicate) {
            Err(relation_exists(predicate))
        } else {
            let relation = Relation::new(predicate.clone(), schema);
            trace!("add_new_relation < relation: {:?}", relation);
            self.add(relation);
            Ok(self.get_mut(&predicate).unwrap())
        }
    }

    pub fn add_new_relation_from(
        &mut self,
        predicate: PredicateRef,
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

    pub fn iter_mut(&mut self) -> Box<dyn Iterator<Item = &'_ mut Relation> + '_> {
        Box::new(self.0.values_mut())
    }

    pub fn contains(&self, predicate: &Predicate) -> bool {
        self.0.contains_key(predicate)
    }

    pub fn get(&self, predicate: &Predicate) -> Option<&Relation> {
        self.0.get(predicate)
    }

    pub fn get_mut(&mut self, predicate: &Predicate) -> Option<&mut Relation> {
        self.0.get_mut(predicate)
    }

    pub fn clone_with_schema_only(&self) -> Self {
        Self(
            self.0
                .iter()
                .map(|(p, r)| (p.clone(), r.clone_with_schema_only()))
                .collect(),
        )
    }

    pub fn merge_from(&mut self, other: Self) -> Result<()> {
        for (other_predicate, other_relation) in other.0 {
            if let Some(relation) = self.0.get_mut(&other_predicate) {
                relation.extend(other_relation)?;
            } else {
                self.0.insert(other_predicate, other_relation);
            }
        }
        Ok(())
    }

    pub fn merge(&mut self, other: &Self) -> Result<()> {
        for (other_predicate, other_relation) in &other.0 {
            if let Some(relation) = self.0.get_mut(other_predicate) {
                for fact in other_relation.facts.iter() {
                    relation.add(fact.clone())?;
                }
            } else {
                self.0
                    .insert(other_predicate.clone(), other_relation.clone());
            }
        }
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------

impl_labeled! {Relation}

impl Collection<Fact> for Relation {
    delegate!(is_empty, facts -> bool);

    delegate!(len, facts -> usize);

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Fact> + '_> {
        Box::new(self.facts.iter())
    }

    fn contains(&self, value: &Fact) -> bool {
        self.facts.contains(value)
    }
}

impl Queryable for Relation {
    fn query_atom(&self, query: &Atom) -> Result<Option<View>> {
        let schema = self.make_view_schema(&query.iter().collect::<Vec<&Term>>());

        let view = if query.is_universal() {
            // all terms are variables, so all facts match
            View::new_with_facts(
                schema,
                self.facts
                    .iter()
                    .map(|fact| Row::from(fact.values().clone()))
                    .collect::<Vec<Row>>(),
            )
        } else if query.is_existential() {
            // all terms are constants, so only one term should match
            let fact = Fact::new(
                query.label_ref(),
                query.constants().cloned().collect::<Vec<Constant>>(),
            )?;
            if self.contains(&fact) {
                View::new_true()
            } else {
                View::new_false()
            }
        } else {
            let selection = Selection::try_from(query)?;
            // let projection = Projection::try_from(query)?;
            let rows: Result<Vec<Row>> = self
                .iter()
                .filter_map(|fact| fact.clone().select(&selection).transpose())
                // TODO: (ISSUE/rust-asdi/8) Need to update the view join logic
                // .map(|fact| match fact {
                //     Ok(fact) => fact.project(&projection),
                //     Err(e) => Err(e),
                // })
                .collect();
            View::new_with_facts(schema, rows?)
        };

        Ok(Some(view))
    }
}

impl Relation {
    pub fn new<V: Into<Schema<Predicate>>>(name: PredicateRef, schema: V) -> Self {
        Self {
            label: name,
            schema: schema.into(),
            facts: Default::default(),
            pragma: None,
        }
    }

    pub fn clone_with_schema_only(&self) -> Self {
        Self {
            label: self.label.clone(),
            schema: self.schema.clone(),
            facts: Default::default(),
            pragma: None,
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn schema(&self) -> &Schema<Predicate> {
        &self.schema
    }

    pub(crate) fn schema_mut(&mut self) -> &mut Schema<Predicate> {
        &mut self.schema
    }

    pub(crate) fn update_schema(&mut self, other: &Self) {
        assert_eq!(self.schema.len(), other.schema.len());
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

    pub fn add_as_fact<V: Into<Vec<Constant>>>(&mut self, values: V) -> Result<bool> {
        let values: Vec<Constant> = values.into();
        if self.schema.conforms(&values) {
            Ok(self.add(Fact::new(self.label.clone(), values)?)?)
        } else {
            Err(fact_does_not_correspond_to_schema(
                self.label_ref(),
                values
                    .iter()
                    .map(Constant::to_string)
                    .collect::<Vec<String>>()
                    .join(COMMA_SEPARATOR),
            ))
        }
    }

    pub fn add(&mut self, fact: Fact) -> Result<bool> {
        if fact.label() == self.label() {
            Ok(self.facts.insert(fact))
        } else {
            Err(fact_does_not_correspond_to_schema(
                fact.label_ref(),
                fact.iter()
                    .map(Constant::to_string)
                    .collect::<Vec<String>>()
                    .join(COMMA_SEPARATOR),
            ))
        }
    }

    pub fn extend(&mut self, other: Self) -> Result<usize> {
        if self.label() == other.label() && self.schema() == other.schema() {
            let mut added = 0;
            for fact in other.facts.into_iter() {
                added += if self.add(fact)? { 1 } else { 0 };
            }
            Ok(added)
        } else {
            panic!()
        }
    }

    // --------------------------------------------------------------------------------------------

    fn make_view_schema(&self, terms: &[&Term]) -> Vec<Attribute<Variable>> {
        terms
            .iter()
            .enumerate()
            .map(|(i, term)| {
                let mut term = match term {
                    Term::Anonymous => Attribute::anonymous(),
                    Term::Variable(v) => Attribute::from(v.clone()),
                    Term::Constant(v) => Attribute::typed(v.kind()),
                };
                if term.kind().is_none() {
                    if let Some(kind) = self.schema().get(&AttributeIndex::Index(i)).unwrap().kind()
                    {
                        term.override_kind(kind);
                    }
                }
                term
            })
            .collect()
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// Returns `true` if a [io::FilePragma] (`.input` or `.output`) is attached to this relation.
    ///
    pub fn has_file_pragma(&self) -> bool {
        self.pragma.is_some()
    }

    ///
    /// Returns a [io::FilePragma] if one is attached to this relation, else `None`.
    ///
    pub fn file_pragma(&self) -> Option<&io::FilePragma> {
        self.pragma.as_ref()
    }

    ///
    /// Attach a [io::FilePragma] to this relation, if one exists already it is overwritten.
    ///
    pub fn set_file_pragma(&mut self, pragma: io::FilePragma) {
        self.pragma = Some(pragma)
    }

    ///
    /// If a [io::FilePragma] is attached to this relation then read data from the file into this
    /// relation's set of facts.
    ///
    pub fn load_from_file(&mut self) -> Result<usize> {
        if let Some(pragma) = &self.pragma {
            let new_facts = io::read_relation(self, pragma)?;
            self.extend(new_facts)
        } else {
            Ok(0)
        }
    }

    ///
    /// If a [io::FilePragma] is attached to this relation then store this relation's set of facts
    /// into the file.
    ///
    pub fn store_to_file(&mut self) -> Result<()> {
        if let Some(pragma) = &self.pragma {
            io::write_relation(self, pragma)
        } else {
            Ok(())
        }
    }

    // --------------------------------------------------------------------------------------------

    pub(crate) fn to_schema_decl(&self, extensional: bool, emit_unknown: bool) -> String {
        format!(
            "{}{} {}{}{}{}{}",
            RESERVED_PREFIX,
            if extensional {
                PRAGMA_ID_ASSERT
            } else {
                PRAGMA_ID_INFER
            },
            self.label(),
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
                .join(COMMA_SEPARATOR),
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

impl_labeled! {Fact}

impl_collection! {Fact, Constant, values}

impl_indexed_collection! { Fact, Constant, usize, values}

impl FactOps for Fact {
    fn project(self, projection: &Projection) -> Result<Row> {
        Ok(if projection.is_all() {
            Row::from(self)
        } else {
            Row::from(
                self.values
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, c)| {
                        if projection.contains_index(&i) {
                            Some(c)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<Constant>>(),
            )
        })
    }

    fn select(self, criteria: &Selection) -> Result<Option<Row>> {
        Ok(if criteria.is_all() || criteria.is_match(self.values())? {
            Some(Row::from(self))
        } else {
            None
        })
    }
}

impl Fact {
    pub fn new<V: Into<Vec<Constant>>>(label: PredicateRef, values: V) -> Result<Self> {
        let values = values.into();
        if values.is_empty() {
            Err(nullary_facts_not_allowed())
        } else {
            Ok(Self { label, values })
        }
    }

    pub fn values(&self) -> &Vec<Constant> {
        &self.values
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

impl_enum_from!(Constant, Number, Number);

impl From<i64> for Constant {
    fn from(v: i64) -> Self {
        Self::Number(Number::from(v))
    }
}

impl TryFrom<f64> for Constant {
    type Error = Error;

    fn try_from(v: f64) -> std::result::Result<Self, Self::Error> {
        Ok(Self::Number(Number::try_from(v)?))
    }
}

impl_enum_from!(Constant, bool, Boolean);

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
                Self::Number(v) => v.to_string(),
                Self::Boolean(v) => {
                    if *v {
                        BOOLEAN_LITERAL_TRUE
                    } else {
                        BOOLEAN_LITERAL_FALSE
                    }
                    .to_string()
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

    pub fn new_size(v: usize) -> Self {
        Self::Number(Number::from(v))
    }

    self_is_as!(string, String);

    self_is_as!(number, Number, Number);

    self_is_as!(boolean, Boolean, bool);

    pub fn kind(&self) -> AttributeKind {
        match self {
            Self::String(_) => AttributeKind::String,
            Self::Number(v) => v.kind(),
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

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self.0 {
                NumberInner::Integer(v) => v.to_string(),
                NumberInner::Float(v) => v.to_string(),
            }
        )
    }
}

impl From<usize> for Number {
    fn from(v: usize) -> Self {
        Self(NumberInner::Integer(v as i64))
    }
}

impl From<u8> for Number {
    fn from(v: u8) -> Self {
        Self(NumberInner::Integer(v as i64))
    }
}

impl From<i8> for Number {
    fn from(v: i8) -> Self {
        Self(NumberInner::Integer(v as i64))
    }
}

impl From<u16> for Number {
    fn from(v: u16) -> Self {
        Self(NumberInner::Integer(v as i64))
    }
}

impl From<i16> for Number {
    fn from(v: i16) -> Self {
        Self(NumberInner::Integer(v as i64))
    }
}

impl From<u32> for Number {
    fn from(v: u32) -> Self {
        Self(NumberInner::Integer(v as i64))
    }
}

impl From<i32> for Number {
    fn from(v: i32) -> Self {
        Self(NumberInner::Integer(v as i64))
    }
}

impl From<i64> for Number {
    fn from(v: i64) -> Self {
        Self(NumberInner::Integer(v))
    }
}

impl TryFrom<f32> for Number {
    type Error = Error;

    fn try_from(value: f32) -> std::result::Result<Self, Self::Error> {
        Self::try_from(value as f64)
    }
}

impl TryFrom<f64> for Number {
    type Error = Error;

    fn try_from(value: f64) -> std::result::Result<Self, Self::Error> {
        match NotNan::new(value) {
            Ok(float) => Ok(Self(NumberInner::Float(float))),
            Err(e) => {
                error!("Invalid float, cannot be a NaN: {:?}", e);
                Err(invalid_value("float", &value.to_string()))
            }
        }
    }
}

impl Number {
    pub fn from_i64(v: i64) -> Self {
        Self(NumberInner::Integer(v))
    }

    pub fn is_integer(&self) -> bool {
        matches!(&self.0, NumberInner::Integer(_))
    }

    pub fn as_integer(&self) -> Option<&i64> {
        match &self.0 {
            NumberInner::Integer(v) => Some(v),
            _ => None,
        }
    }

    pub fn from_f64(v: f64) -> Option<Self> {
        let v = NotNan::new(v);
        match v {
            Ok(v) => Some(Self(NumberInner::Float(v))),
            Err(e) => {
                error!("Could not convert to a float, error: {:?}", e);
                None
            }
        }
    }

    pub fn is_float(&self) -> bool {
        matches!(&self.0, NumberInner::Float(_))
    }

    pub fn as_float(&self) -> Option<&f64> {
        match &self.0 {
            NumberInner::Float(v) => Some(v.as_ref()),
            _ => None,
        }
    }

    pub fn kind(&self) -> AttributeKind {
        match &self.0 {
            NumberInner::Integer(_) => AttributeKind::Integer,
            NumberInner::Float(_) => AttributeKind::Float,
        }
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
            invalid_value(TYPE_NAME_PREDICATE, s).into()
        }
    }
}

impl AsRef<str> for Predicate {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AttributeName for Predicate {
    fn is_valid(s: &str) -> bool {
        let mut chars = s.chars();
        (!s.is_empty())
            && chars.next().map(|c| c.is_lowercase()).unwrap()
            && chars.all(|c| c.is_alphanumeric() || c == CHAR_UNDERSCORE)
    }

    fn type_name() -> &'static str {
        TYPE_NAME_PREDICATE
    }
}

impl Predicate {
    into_inner_fn!(String);
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

pub mod io;
