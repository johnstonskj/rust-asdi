/*!
This module provides the [Query] type that represents a query goal as well as the [View] and [Row].
types used to return query results.
*/

use crate::edb::{Attribute, AttributeIndex, AttributeKind, Constant, Fact, Schema};
use crate::error::{attribute_index_invalid, nullary_facts_not_allowed, Error, Result};
use crate::idb::{Atom, Variable};
use crate::idb::{ComparisonOperator, Term};
use crate::syntax::{CHAR_PERIOD, QUERY_PREFIX_ASCII};
use crate::{Collection, IndexedCollection, MaybeAnonymous, MaybeLabeled, PredicateRef};
use paste::paste;
use regex::Regex;
use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use tracing::{error, trace};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// A query, or goal, is an atom to be matched against [relations](../edb/struct.Relation.html)
/// known to the program.
///
/// # Examples
///
/// ```datalog
/// .assert car(make: string, model: string, year: integer).
///
/// ## Is there a car model Fiesta, from Ford, with a model year 2010?
/// car(ford, focus, 2010)?
///
/// ## Return all the models Ford made with a model year 2010.
/// car(ford, X, 2010)?
///
/// ## Return all the model years for Ford Fiesta.
/// car(ford, focus, X)?
/// ```
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Query(Atom);

///
/// A view is an intermediate structure, a selection/projection of a [relation](../edb/struct.Relation.html),
/// or the join of two or more views.
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct View {
    schema: Schema<Variable>,
    facts: HashSet<Row>,
}

///
/// A row within a view corresponds to a [`Fact`] within a [`Relation`](../edb/struct.Relation.html),
/// except that it is not labeled.
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Row(Vec<Constant>);

pub trait RelationOps {
    fn project(self, indices: &Projection) -> Result<View>
    where
        Self: Sized;

    fn select(self, criteria: &Selection) -> Result<View>
    where
        Self: Sized;

    fn exists(self, criteria: &Selection) -> Result<bool>;
}

pub trait FactOps {
    fn project(self, indices: &Projection) -> Result<Row>
    where
        Self: Sized;

    fn select(self, criteria: &Selection) -> Result<Option<Row>>
    where
        Self: Sized;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Projection(Vec<(usize, Attribute<Variable>)>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Selection(Vec<Criteria>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Criteria {
    index: usize,
    op: ComparisonOperator,
    value: CriteriaValue,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CriteriaValue {
    Value(Constant),
    Index(usize),
}

pub trait Queryable {
    fn query(&self, query: &Query) -> Result<Option<View>> {
        self.query_atom(query.as_ref())
    }

    fn query_atom(&self, query: &Atom) -> Result<Option<View>>;
}

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
    pub fn new<T: Into<Vec<Term>>>(predicate: PredicateRef, terms: T) -> Self {
        Self(Atom::new(predicate, terms))
    }
}

// ------------------------------------------------------------------------------------------------

#[cfg(feature = "tabular")]
impl Display for View {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use prettytable::format::Alignment;
        use prettytable::Table;
        use prettytable::{Attr, Cell};

        let mut table = Table::new();

        table.set_titles(
            self.schema
                .iter()
                .map(|c| {
                    Cell::new_align(&c.to_column_decl(false), Alignment::CENTER)
                        .with_style(Attr::Bold)
                })
                .collect(),
        );

        for row in self.iter() {
            table.add_row(row.iter().map(|c| Cell::new(&c.to_string())).collect());
        }

        write!(f, "{}", table)
    }
}

#[cfg(not(feature = "tabular"))]
impl Display for View {
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

impl Collection<Row> for View {
    fn is_empty(&self) -> bool {
        self.facts.is_empty()
    }

    fn len(&self) -> usize {
        self.facts.len()
    }

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Row> + '_> {
        Box::new(self.facts.iter())
    }

    fn contains(&self, value: &Row) -> bool {
        self.facts.contains(value)
    }
}

impl RelationOps for View {
    fn project(self, projection: &Projection) -> Result<View> {
        Ok(if projection.is_all() {
            self
        } else {
            let results: Result<Vec<Row>> = self
                .facts
                .into_iter()
                .map(|row| row.project(projection))
                .collect();
            Self::new_with_facts(self.schema, results?)
        })
    }

    fn select(self, selection: &Selection) -> Result<View> {
        Ok(if selection.is_all() {
            self
        } else {
            let results: Result<Vec<Row>> = self
                .facts
                .into_iter()
                .filter_map(|row| row.select(selection).transpose())
                .collect();
            Self::new_with_facts(self.schema, results?)
        })
    }

    fn exists(self, criteria: &Selection) -> Result<bool> {
        for row in self.facts.into_iter() {
            if criteria.is_all() || criteria.matches(row.values())? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl View {
    pub fn empty() -> Self {
        Self {
            schema: Schema::empty(),
            facts: Default::default(),
        }
    }

    pub fn new<V: Into<Schema<Variable>>>(schema: V) -> Self {
        Self {
            schema: schema.into(),
            facts: Default::default(),
        }
    }

    pub fn new_with_facts<V: Into<Schema<Variable>>, C: Into<Vec<Row>>>(
        schema: V,
        facts: C,
    ) -> Self {
        Self {
            schema: schema.into(),
            facts: HashSet::from_iter(facts.into()),
        }
    }

    pub fn new_true() -> Self {
        Self::new_with_facts(
            vec![Attribute::from(AttributeKind::Boolean)],
            vec![Row::from(Constant::new_true())],
        )
    }

    pub fn new_false() -> Self {
        Self::new_with_facts(
            vec![Attribute::from(AttributeKind::Boolean)],
            vec![Row::from(Constant::new_false())],
        )
    }

    // --------------------------------------------------------------------------------------------

    pub fn schema(&self) -> &Schema<Variable> {
        &self.schema
    }

    pub fn attribute_index(&self, index: AttributeIndex<Variable>) -> Option<usize> {
        let index = match &index {
            AttributeIndex::Label(n) => self.schema.label_to_index(n),
            AttributeIndex::Index(i) => Some(*i),
        };
        index.map(|index| {
            assert!(index < self.schema.len());
            index
        })
    }

    // --------------------------------------------------------------------------------------------

    pub fn add(&mut self, row: Row) -> Result<()> {
        self.facts.insert(row);
        Ok(())
    }

    pub fn extend(&mut self, other: Self) -> Result<()> {
        trace!("extend > schema {:?} == {:?}", self.schema, other.schema);
        assert_eq!(self.schema, other.schema);
        for fact in other.facts.into_iter() {
            self.add(fact)?;
        }
        Ok(())
    }

    // --------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------------------------------

    pub fn join_all<V: Into<Vec<View>>>(views: V) -> Result<Self> {
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
                .label_union(other.schema())
                .into_iter()
                .map(Attribute::from)
                .collect::<Vec<Attribute<Variable>>>(),
        );
        let common_variables = self.schema().label_intersection(other.schema());

        // TODO: infer attribute types for new results!
        for left_row in self.iter() {
            for right_row in other.filter(
                common_variables
                    .iter()
                    .map(|(_, left_i, right_i)| (left_row.get(left_i).unwrap().clone(), *right_i))
                    .collect(),
            ) {
                let mut new_row: Vec<Constant> = Vec::with_capacity(new_table.schema().len());
                for (i, column) in new_table.schema().iter().enumerate() {
                    if let Some(index) = self.schema().label_to_index(column.label().unwrap()) {
                        new_row.insert(i, left_row.get(&index).unwrap().clone())
                    } else if let Some(index) =
                        other.schema().label_to_index(column.label().unwrap())
                    {
                        new_row.insert(i, right_row.get(&index).unwrap().clone())
                    } else {
                        error!(
                            "The column {:?} ({}) was found in neither table.",
                            column, i
                        );
                        unreachable!()
                    }
                }
                new_table.add(new_row.into())?;
            }
        }
        Ok(new_table)
    }

    fn filter(&self, values: Vec<(Constant, usize)>) -> impl Iterator<Item = &Row> {
        self.iter()
            .filter(move |row| values.iter().all(|(v, i)| row.get(i).unwrap() == v))
    }
}

// ------------------------------------------------------------------------------------------------

impl From<Vec<Constant>> for Row {
    fn from(v: Vec<Constant>) -> Self {
        Self(v)
    }
}

impl From<Constant> for Row {
    fn from(v: Constant) -> Self {
        Self(vec![v])
    }
}

impl From<Row> for Vec<Constant> {
    fn from(v: Row) -> Self {
        v.0
    }
}

impl From<Fact> for Row {
    fn from(v: Fact) -> Self {
        Self(v.into())
    }
}

impl Collection<Constant> for Row {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Constant> + '_> {
        Box::new(self.0.iter())
    }

    fn contains(&self, value: &Constant) -> bool {
        self.0.contains(value)
    }
}

impl IndexedCollection<usize, Constant> for Row {
    fn get(&self, index: &usize) -> Option<&Constant> {
        self.0.get(*index)
    }

    fn contains_index(&self, index: &usize) -> bool {
        *index < self.len()
    }
}

impl FactOps for Row {
    fn project(self, projection: &Projection) -> Result<Row> {
        Ok(if projection.is_all() {
            self
        } else {
            Self::from(
                self.0
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
        Ok(if criteria.is_all() {
            Some(self)
        } else if criteria.matches(self.values())? {
            Some(self)
        } else {
            None
        })
    }
}

impl Row {
    pub fn values(&self) -> &Vec<Constant> {
        &self.0
    }
}

// ------------------------------------------------------------------------------------------------

impl TryFrom<&Atom> for Projection {
    type Error = Error;

    fn try_from(atom: &Atom) -> std::result::Result<Self, Self::Error> {
        println!("WTF? {:?}", atom);
        let projections: Vec<(usize, Attribute<Variable>)> = atom
            .iter()
            .enumerate()
            .filter(|(_, term)| !term.is_anonymous())
            .map(|(i, term)| {
                (
                    i,
                    match term {
                        Term::Variable(v) => Attribute::from(v.clone()),
                        Term::Constant(v) => Attribute::typed(v.kind()),
                        Term::Anonymous => unreachable!(),
                    },
                )
            })
            .collect();

        println!("WTF??? {:?}", projections);

        if projections.len() == atom.len() {
            println!("ATF!");
            Ok(Self::project_all())
        } else if projections.is_empty() {
            println!("NTF!");
            Err(nullary_facts_not_allowed())
        } else {
            println!("STF!");
            Ok(Self(projections))
        }
    }
}

impl From<Projection> for Schema<Variable> {
    fn from(p: Projection) -> Self {
        Self::from(
            p.0.into_iter()
                .map(|(_, a)| a)
                .collect::<Vec<Attribute<Variable>>>(),
        )
    }
}

#[allow(clippy::len_without_is_empty)]
impl Projection {
    pub fn project_all() -> Self {
        Self(Default::default())
    }

    pub fn is_all(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ (usize, Attribute<Variable>)> {
        self.0.iter()
    }

    pub fn contains_index(&self, index: &usize) -> bool {
        self.0.iter().any(|(i, _)| i == index)
    }
}

// ------------------------------------------------------------------------------------------------

impl TryFrom<&Atom> for Selection {
    type Error = Error;

    fn try_from(atom: &Atom) -> std::result::Result<Self, Self::Error> {
        Ok(Self(
            atom.iter()
                .enumerate()
                .filter_map(|(i, term)| term.as_constant().map(|c| (i, c)))
                .map(|(i, constant)| Criteria {
                    index: i,
                    op: ComparisonOperator::Equal,
                    value: CriteriaValue::Value(constant.clone()),
                })
                .collect(),
        ))
    }
}

#[allow(clippy::len_without_is_empty)]
impl Selection {
    pub fn select_all() -> Self {
        Self(Default::default())
    }

    pub fn is_all(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &'_ Criteria> {
        self.0.iter()
    }

    pub fn contains(&self, value: &Criteria) -> bool {
        self.0.contains(value)
    }

    pub fn matches(&self, fact: &[Constant]) -> Result<bool> {
        for criteria in &self.0 {
            if !self.matches_one(fact, criteria)? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

impl Selection {
    #[inline]
    fn matches_one(&self, fact: &[Constant], criteria: &Criteria) -> Result<bool> {
        let lhs = fact
            .get(criteria.index)
            .ok_or_else(|| attribute_index_invalid(criteria.index))?;
        let rhs = match &criteria.value {
            CriteriaValue::Value(v) => v,
            CriteriaValue::Index(i) => fact.get(*i).ok_or_else(|| attribute_index_invalid(*i))?,
        };
        assert_eq!(lhs.kind(), rhs.kind());
        Ok(match criteria.op {
            ComparisonOperator::Equal => lhs == rhs,
            ComparisonOperator::NotEqual => lhs != rhs,
            ComparisonOperator::LessThan => lhs < rhs,
            ComparisonOperator::LessThanOrEqual => lhs <= rhs,
            ComparisonOperator::GreaterThan => lhs > rhs,
            ComparisonOperator::GreaterThanOrEqual => lhs >= rhs,
            ComparisonOperator::StringMatch => {
                let lhs = lhs.as_string().unwrap();
                let rhs = rhs.as_string().unwrap();
                let regex: Regex = Regex::new(rhs).unwrap();
                regex.is_match(lhs)
            }
        })
    }
}

// ------------------------------------------------------------------------------------------------

impl Criteria {
    pub fn attribute_index(&self) -> usize {
        self.index
    }

    pub fn operator(&self) -> ComparisonOperator {
        self.op
    }

    pub fn compare_to(&self) -> &CriteriaValue {
        &self.value
    }
}

// ------------------------------------------------------------------------------------------------

impl From<usize> for CriteriaValue {
    fn from(v: usize) -> Self {
        Self::Index(v)
    }
}

impl From<Constant> for CriteriaValue {
    fn from(v: Constant) -> Self {
        Self::Value(v)
    }
}

impl CriteriaValue {
    self_is_as!(value, Value, Constant);

    self_is_as!(index, Index, usize);
}
