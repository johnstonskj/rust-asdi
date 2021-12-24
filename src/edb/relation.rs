/*!
One-line description.

More detailed description, with

# Example

*/

use crate::edb::{Constant, Predicate};
use crate::error::{Error, Result};
use crate::idb::{Atom, Comparison};
use crate::idb::{Term, Variable};
use crate::query::{Matches, View};
use crate::syntax::{
    CHAR_COLON, CHAR_COMMA, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, CHAR_UNDERSCORE,
    RESERVED_PRAGMA_DECLARE, RESERVED_PREFIX, TYPE_NAME_CONST_UNKNOWN,
};
use std::borrow::Cow;
use std::collections::{BTreeMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;
use tracing::{error, trace};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

pub trait AttributeName: Clone + Debug + Display + PartialEq + Eq + PartialOrd + Ord {}

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
pub struct Relation(BaseRelation<Predicate>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Schema<T>
where
    T: AttributeName,
{
    attributes: Vec<Attribute<T>>,
    named: BTreeMap<T, usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute<T>
where
    T: AttributeName,
{
    name: Option<T>,
    kind: Option<AttributeKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AttributeIndex<T>
where
    T: AttributeName,
{
    Name(T),
    Index(usize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AttributeKind {
    String,
    Integer,
    Boolean,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(single_use_lifetimes)] // <-- this appears to be a false positive
pub struct Fact<'a, T>
where
    T: AttributeName,
{
    from: &'a BaseRelation<T>,
    values: Cow<'a, Vec<Constant>>,
}

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct BaseRelation<T>
where
    T: AttributeName,
{
    name: Option<Predicate>,
    schema: Schema<T>,
    facts: HashSet<Vec<Constant>>,
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

impl Display for Relation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Matches for Relation {
    fn matches(&self, atom: &Atom) -> View {
        assert_eq!(atom.predicate(), self.name());
        self.0.matches(atom)
    }

    #[allow(single_use_lifetimes)]
    fn match_terms<'a, V: Into<Vec<&'a Term>>>(&self, terms: V) -> View {
        self.0.match_terms(terms)
    }
}

impl Relation {
    pub fn new<V: Into<Schema<Predicate>>>(name: Predicate, schema: V) -> Self {
        Self(BaseRelation::new_named(name, schema))
    }

    pub fn clone_with_schema_only(&self) -> Self {
        Self(BaseRelation::clone_with_schema_only(&self.0))
    }

    // --------------------------------------------------------------------------------------------

    pub fn name(&self) -> &Predicate {
        self.0.name().unwrap()
    }

    pub fn is_anonymous(&self) -> bool {
        self.0.is_anonymous()
    }

    // --------------------------------------------------------------------------------------------

    pub fn schema(&self) -> &Schema<Predicate> {
        self.0.schema()
    }

    pub fn arity(&self) -> usize {
        self.0.arity()
    }

    pub fn has_attribute<I: Into<AttributeIndex<Predicate>>>(&self, index: I) -> bool {
        self.0.has_attribute(index)
    }

    pub(crate) fn update_schema(&mut self, other: &Self) {
        self.0.update_schema(&other.0)
    }

    // --------------------------------------------------------------------------------------------

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn facts(&self) -> impl Iterator<Item = Fact<'_, Predicate>> + '_ {
        self.0.facts()
    }

    pub fn add<V: Into<Vec<Constant>>>(&mut self, fact: V) -> Result<()> {
        self.0.add(fact)
    }

    pub fn extend(&mut self, other: Self) -> Result<()> {
        self.0.extend(other.0)
    }

    // --------------------------------------------------------------------------------------------

    pub fn to_schema_decl(&self) -> String {
        format!(
            "{}{} {}{}{}{}{}",
            RESERVED_PREFIX,
            RESERVED_PRAGMA_DECLARE,
            self.0.name().map(|n| n.to_string()).unwrap_or_default(),
            CHAR_LEFT_PAREN,
            self.0.schema().to_string(),
            CHAR_RIGHT_PAREN,
            CHAR_PERIOD
        )
    }
}

// ------------------------------------------------------------------------------------------------

#[cfg(feature = "tabular")]
impl<T> Display for BaseRelation<T>
where
    T: AttributeName,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use prettytable::format::Alignment;
        use prettytable::Table;
        use prettytable::{Attr, Cell};

        let mut table = Table::new();

        table.set_titles(
            self.schema
                .iter()
                .map(|c| Cell::new_align(&c.to_string(), Alignment::CENTER).with_style(Attr::Bold))
                .collect(),
        );

        for row in self.iter() {
            table.add_row(row.iter().map(|c| Cell::new(&c.to_string())).collect());
        }

        write!(f, "{}", table)
    }
}

#[cfg(not(feature = "tabular"))]
impl<T> Display for BaseRelation<T>
where
    T: AttributeName,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "| {} |",
            self.schema
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<String>>()
                .join(" | ")
        )?;

        for row in self.iter() {
            writeln!(
                f,
                "| {} |",
                row.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(" | ")
            )?;
        }
        Ok(())
    }
}

impl<T> BaseRelation<T>
where
    T: AttributeName,
{
    pub(crate) fn empty() -> Self {
        Self {
            name: None,
            schema: Schema::empty(),
            facts: Default::default(),
        }
    }

    pub(crate) fn new<V: Into<Schema<T>>>(schema: V) -> Self {
        Self {
            name: None,
            schema: schema.into(),
            facts: Default::default(),
        }
    }

    pub(crate) fn new_named<V: Into<Schema<T>>>(name: Predicate, schema: V) -> Self {
        Self {
            name: Some(name),
            schema: schema.into(),
            facts: Default::default(),
        }
    }

    pub(crate) fn new_with_facts<V: Into<Schema<T>>, C: Into<Vec<Vec<Constant>>>>(
        schema: V,
        facts: C,
    ) -> Self {
        Self {
            name: None,
            schema: schema.into(),
            facts: HashSet::from_iter(facts.into()),
        }
    }

    pub(crate) fn clone_with_schema_only(&self) -> Self {
        Self {
            name: self.name.clone(),
            schema: self.schema.clone(),
            facts: Default::default(),
        }
    }

    // --------------------------------------------------------------------------------------------
    // Name, if present
    // --------------------------------------------------------------------------------------------

    pub(crate) fn name(&self) -> Option<&Predicate> {
        self.name.as_ref()
    }

    pub(crate) fn is_anonymous(&self) -> bool {
        self.name.is_none()
    }

    // --------------------------------------------------------------------------------------------
    // Schema related
    // --------------------------------------------------------------------------------------------

    pub(crate) fn schema(&self) -> &Schema<T> {
        &self.schema
    }

    pub(crate) fn arity(&self) -> usize {
        self.schema.arity()
    }

    pub(crate) fn has_attribute<I: Into<AttributeIndex<T>>>(&self, index: I) -> bool {
        self.schema().contains(index)
    }

    pub(crate) fn update_schema(&mut self, other: &Self) {
        assert_eq!(self.schema.arity(), other.schema.arity());
        self.schema
            .iter_mut()
            .zip(other.schema().iter())
            .for_each(|(left, right)| left.update_from(right))
    }

    pub(crate) fn conforms(&self, fact: &[Constant]) -> bool {
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
    // Facts
    // --------------------------------------------------------------------------------------------

    pub(crate) fn is_empty(&self) -> bool {
        self.facts.is_empty()
    }

    pub(crate) fn len(&self) -> usize {
        self.facts.len()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Vec<Constant>> {
        self.facts.iter()
    }

    pub(crate) fn facts(&self) -> impl Iterator<Item = Fact<'_, T>> + '_ {
        self.facts.iter().map(|f| Fact::new(self, Cow::Borrowed(f)))
    }

    pub(crate) fn add<V: Into<Vec<Constant>>>(&mut self, fact: V) -> Result<()> {
        let fact: Vec<Constant> = fact.into();
        if self.conforms(&fact) {
            self.facts.insert(fact);
        } else {
            error!("Provided row does not conform to schema.");
            // TODO: propagate errors
            panic!();
        }
        Ok(())
    }

    pub(crate) fn extend(&mut self, other: Self) -> Result<()> {
        trace!("extend > name {:?} == {:?}", self.name, other.name);
        assert_eq!(self.name, other.name);
        trace!("extend > schema {:?} == {:?}", self.schema, other.schema);
        assert_eq!(self.schema, other.schema);
        for fact in other.facts.into_iter() {
            self.add(fact)?;
        }
        Ok(())
    }

    pub(crate) fn contains(&self, fact: &[Constant]) -> bool {
        self.facts.contains(fact)
    }

    // --------------------------------------------------------------------------------------------
    // Query
    // --------------------------------------------------------------------------------------------

    pub(crate) fn matches(&self, atom: &Atom) -> View {
        let terms: Vec<&Term> = atom.terms().collect();
        self.match_terms(terms)
    }

    #[allow(single_use_lifetimes)]
    pub(crate) fn match_terms<'a, V: Into<Vec<&'a Term>>>(&self, terms: V) -> View {
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
                    if term.kind.is_none() {
                        term.kind = self.schema().get(i).unwrap().kind();
                    }
                    term
                })
                .collect::<Vec<Attribute<Variable>>>(),
            self.facts
                .iter()
                .filter_map(|fact| self.match_terms_inner(&terms, fact))
                .collect::<Vec<Vec<Constant>>>(),
        )
    }

    fn match_terms_inner(&self, terms: &[&Term], fact: &[Constant]) -> Option<Vec<Constant>> {
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

    pub(crate) fn select(&self, _comparisons: &[Comparison]) -> Self {
        todo!()
    }

    pub(crate) fn select_attributes(&self, attributes: HashSet<AttributeIndex<T>>) -> Self {
        let selection: Vec<usize> = attributes
            .iter()
            .map(|a| {
                assert!(self.schema().contains(a.clone()));
                match a {
                    AttributeIndex::Name(n) => self.schema().name_to_index(n).unwrap(),
                    AttributeIndex::Index(i) => *i,
                }
            })
            .collect();
        self.select_attributes_by_index(&selection)
    }

    pub(crate) fn select_attributes_by_index(&self, attributes: &[usize]) -> Self {
        let schema = Schema::new(
            self.schema
                .iter()
                .enumerate()
                .filter_map(|(i, attr)| {
                    if attributes.contains(&i) {
                        Some(attr)
                    } else {
                        None
                    }
                })
                .cloned()
                .collect::<Vec<Attribute<T>>>(),
        );

        let facts: Vec<Vec<Constant>> = self
            .iter()
            .map(|row| {
                row.iter()
                    .enumerate()
                    .filter_map(|(i, attr)| {
                        if attributes.contains(&i) {
                            Some(attr)
                        } else {
                            None
                        }
                    })
                    .cloned()
                    .collect::<Vec<Constant>>()
            })
            .collect();

        Self::new_with_facts(schema, facts)
    }

    pub(crate) fn select_only_named(&self) -> Self {
        let selection: Vec<usize> = self
            .schema
            .iter()
            .enumerate()
            .filter_map(|(i, c)| if !c.is_anonymous() { Some(i) } else { None })
            .collect();
        self.select_attributes_by_index(&selection)
    }

    pub(crate) fn project<A: Into<AttributeIndex<T>>>(&self, attributes: Vec<A>) -> Self {
        let attributes: HashSet<usize> = HashSet::from_iter(attributes.into_iter().map(|a| {
            let a: AttributeIndex<T> = a.into();
            match a {
                AttributeIndex::Name(n) => self.schema().name_to_index(&n).unwrap(),
                AttributeIndex::Index(i) => i,
            }
        }));
        Self {
            name: None,
            schema: Schema::new(
                self.schema()
                    .iter()
                    .enumerate()
                    .filter_map(|(i, a)| {
                        if attributes.contains(&i) {
                            Some(a)
                        } else {
                            None
                        }
                    })
                    .cloned()
                    .collect::<Vec<Attribute<T>>>(),
            ),
            facts: self
                .iter()
                .map(|f| {
                    f.iter()
                        .enumerate()
                        .filter_map(|(i, c)| {
                            if attributes.contains(&i) {
                                Some(c)
                            } else {
                                None
                            }
                        })
                        .cloned()
                        .collect()
                })
                .collect(),
        }
    }

    pub(crate) fn join_all<V: Into<Vec<BaseRelation<T>>>>(views: V) -> Result<Self> {
        let mut views = views.into();
        assert!(!views.is_empty());
        if views.len() == 1 {
            Ok(views.remove(0))
        } else {
            let mut views = views.into_iter();
            let mut result = views.next().unwrap();
            for next in views {
                result = result.join(&next)?;
            }
            Ok(result)
        }
    }

    pub(crate) fn join(&self, other: &Self) -> Result<Self> {
        let mut new_table: Self = Self::new(
            self.schema()
                .name_union(other.schema())
                .into_iter()
                .map(|n| Attribute::from(n))
                .collect::<Vec<Attribute<T>>>(),
        );
        let common_variables = self.schema().name_intersection(other.schema());

        // TODO: infer attribute types for new results!
        for left_row in self.iter() {
            for right_row in other.filter(
                common_variables
                    .iter()
                    .map(|(_, left_i, right_i)| (left_row.get(*left_i).unwrap().clone(), *right_i))
                    .collect(),
            ) {
                let mut new_row: Vec<Constant> = Vec::with_capacity(new_table.schema().arity());
                for (i, column) in new_table.schema().iter().enumerate() {
                    if let Some(index) = self.schema().name_to_index(column.name().unwrap()) {
                        new_row.insert(i, left_row.get(index).unwrap().clone())
                    } else if let Some(index) = other.schema().name_to_index(column.name().unwrap())
                    {
                        new_row.insert(i, right_row.get(index).unwrap().clone())
                    } else {
                        error!(
                            "The column {:?} ({}) was found in neither table.",
                            column, i
                        );
                        unreachable!()
                    }
                }
                new_table.add(new_row)?;
            }
        }
        Ok(new_table)
    }
    //
    // fn remove_anonymous_attributes(&mut self) {
    //     let mut remove: Vec<usize> = self
    //         .schema
    //         .iter()
    //         .enumerate()
    //         .filter_map(|(i, c)| if c.is_anonymous() { Some(i) } else { None })
    //         .collect();
    //     trace!("reduce > remove (before)\n{:?}", remove);
    //     remove.sort_by(|a, b| b.cmp(a));
    //     trace!("reduce > remove (after)\n{:?}", remove);
    //
    //     let mut result: HashSet<Vec<Constant>> = HashSet::with_capacity(self.facts.len());
    //     for mut row in self.facts.drain() {
    //         mutator(&mut row)?;
    //         result.insert(row);
    //     }
    //     self.facts = result;
    //
    //
    //     self.mutate(|row| {
    //         for col in &remove {
    //             row.remove(*col);
    //         }
    //         Ok(())
    //     })
    //     .unwrap();
    //     self.schema = Schema::new(
    //         self.schema
    //             .iter()
    //             .filter(|col| !col.is_anonymous())
    //             .cloned()
    //             .collect::<Vec<Attribute<T>>>(),
    //     );
    // }

    fn filter(&self, values: Vec<(Constant, usize)>) -> impl Iterator<Item = &Vec<Constant>> {
        self.iter()
            .filter(move |row| values.iter().all(|(v, i)| row.get(*i).unwrap() == v))
    }
}

// ------------------------------------------------------------------------------------------------

impl<T> Display for Schema<T>
where
    T: AttributeName + Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.attributes
                .iter()
                .map(Attribute::to_string)
                .collect::<Vec<String>>()
                .join(&format!("{} ", CHAR_COMMA))
        )
    }
}

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

// impl<T> From<Vec<AttributeKind>> for Schema<T>
// where
//     T: AttributeName,
// {
//     fn from(cs: Vec<AttributeKind>) -> Self {
//         Self::new(
//             cs.into_iter()
//                 .map(|k| Attribute::from(k))
//                 .collect::<Vec<Attribute<T>>>(),
//         )
//     }
// }
//
// impl<T> From<T> for Schema<T>
// where
//     T: AttributeName,
// {
//     fn from(v: T) -> Self {
//         Self::new(vec![Attribute::from(v)])
//     }
// }
//
// impl<T> From<Vec<T>> for Schema<T>
// where
//     T: AttributeName,
// {
//     fn from(vs: Vec<T>) -> Self {
//         Self::new(
//             vs.into_iter()
//                 .map(Attribute::from)
//                 .collect::<Vec<Attribute<T>>>(),
//         )
//     }
// }

impl<T> Schema<T>
where
    T: AttributeName,
{
    pub fn new<V: Into<Vec<Attribute<T>>>>(attributes: V) -> Self {
        let attributes = attributes.into();
        let named: Vec<(T, usize)> = attributes
            .iter()
            .enumerate()
            .filter_map(|(i, c)| c.name().map(|var| (var.clone(), i)))
            .collect();
        let pre_hash_len = named.len();

        let named = BTreeMap::from_iter(named);

        assert_eq!(named.len(), pre_hash_len);

        Self { attributes, named }
    }

    pub fn empty() -> Self {
        Self {
            attributes: Default::default(),
            named: Default::default(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.attributes.is_empty()
    }

    pub fn arity(&self) -> usize {
        self.attributes.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Attribute<T>> {
        self.attributes.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Attribute<T>> {
        self.attributes.iter_mut()
    }

    pub fn has_named(&self) -> bool {
        !self.named.is_empty()
    }

    pub fn contains<I: Into<AttributeIndex<T>>>(&self, index: I) -> bool {
        match index.into() {
            AttributeIndex::Name(n) => self.named.contains_key(&n),
            AttributeIndex::Index(i) => i < self.arity(),
        }
    }

    pub fn get<I: Into<AttributeIndex<T>>>(&self, index: I) -> Option<&Attribute<T>> {
        match index.into() {
            AttributeIndex::Name(n) => match self.named.get(&n) {
                Some(i) => self.attributes.get(*i),
                None => None,
            },
            AttributeIndex::Index(i) => self.attributes.get(i),
        }
    }

    pub fn names(&self) -> impl Iterator<Item = &T> {
        self.named.keys()
    }

    pub fn name_intersection(&self, other: &Self) -> Vec<(&T, usize, usize)> {
        self.named
            .iter()
            .filter_map(|(var, i)| other.named.get(var).map(|other_i| (var, *i, *other_i)))
            .collect()
    }

    pub fn name_union(&self, other: &Self) -> Vec<T> {
        let mut all: Vec<T> = self.names().cloned().collect();
        for var in other.names() {
            if !all.contains(var) {
                all.push(var.clone());
            }
        }
        all
    }

    pub fn name_to_index(&self, n: &T) -> Option<usize> {
        self.named.get(n).copied()
    }

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
}

// ------------------------------------------------------------------------------------------------

impl<T> Display for Attribute<T>
where
    T: AttributeName,
{
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
        Self::named(name)
    }
}

impl<T> Attribute<T>
where
    T: AttributeName,
{
    pub fn anonymous() -> Self {
        Self::new_inner(None, None)
    }

    pub fn named(name: T) -> Self {
        Self::new_inner(name, None)
    }

    pub fn typed(kind: AttributeKind) -> Self {
        Self::new_inner(None, kind)
    }

    pub fn string() -> Self {
        Self::new_inner(None, AttributeKind::String)
    }

    pub fn string_named(name: T) -> Self {
        Self::new_inner(name, AttributeKind::String)
    }

    pub fn integer() -> Self {
        Self::new_inner(None, AttributeKind::Integer)
    }

    pub fn integer_named(name: T) -> Self {
        Self::new_inner(name, AttributeKind::Integer)
    }

    pub fn boolean() -> Self {
        Self::new_inner(None, AttributeKind::Boolean)
    }

    pub fn boolean_named(name: T) -> Self {
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
            name: name.into(),
            kind: kind.into(),
        }
    }

    pub fn name(&self) -> Option<&T> {
        self.name.as_ref()
    }

    pub fn is_anonymous(&self) -> bool {
        self.name.is_none()
    }

    pub fn kind(&self) -> Option<AttributeKind> {
        self.kind.as_ref().copied()
    }

    pub fn is_untyped(&self) -> bool {
        self.kind.is_none()
    }

    pub fn update_from(&mut self, other: &Self) {
        if let (None, Some(v)) = (&self.name, &other.name) {
            self.name = Some(v.clone())
        }
        if let (None, Some(v)) = (self.kind, other.kind) {
            self.kind = Some(v)
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
        Self::Name(v)
    }
}

impl<T> AttributeIndex<T>
where
    T: AttributeName,
{
    pub fn is_named(&self) -> bool {
        matches!(self, Self::Name(_))
    }

    pub fn as_name(&self) -> Option<&T> {
        match self {
            Self::Name(v) => Some(v),
            _ => None,
        }
    }

    pub fn is_indexed(&self) -> bool {
        matches!(self, Self::Name(_))
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

// ------------------------------------------------------------------------------------------------

#[allow(single_use_lifetimes)]
impl<'a, T> Display for Fact<'a, T>
where
    T: AttributeName,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}{}",
            match &self.from.name() {
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
//
// impl<'a, T> From<Vec<Constant>> for Fact<'a, T> {
//     fn from(v: Vec<Constant>) -> Self {
//         Self {
//             predicate: None,
//             values: v,
//         }
//     }
// }
//
// impl From<Fact> for Vec<Constant> {
//     fn from(v: Fact) -> Self {
//         v.values
//     }
// }

impl<'a, T> Fact<'a, T>
where
    T: AttributeName,
{
    pub(crate) fn new(from: &'a BaseRelation<T>, values: Cow<'a, Vec<Constant>>) -> Self {
        Self { from, values }
    }

    pub fn arity(&self) -> usize {
        self.values.len()
    }

    pub fn name(&self) -> Option<&Predicate> {
        self.from.name()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Constant> {
        self.values.iter()
    }

    pub fn schema(&self) -> &Schema<T> {
        self.from.schema()
    }

    pub fn get<I: Into<AttributeIndex<T>>>(&self, index: I) -> Option<&Constant> {
        match index.into() {
            AttributeIndex::Name(n) => {
                let i = self.schema().name_to_index(&n).unwrap();
                self.values.get(i)
            }
            AttributeIndex::Index(i) => self.values.get(i),
        }
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
    use crate::edb::Predicate;
    use crate::idb::{Atom, Term, Variable};
    use crate::parse::parse_str;
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
