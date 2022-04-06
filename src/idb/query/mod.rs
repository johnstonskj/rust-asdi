/*!
This module provides the [Query] type that represents a query goal as well as the [View] and [Row].
types used to return query results.

![module UML](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/book/src/model/idb_query.svg)

 */

use self::relational::{FactOps, Projection, RelationOps, Selection};
use crate::edb::{Attribute, AttributeIndex, AttributeKind, Constant, Fact, Schema};
use crate::error::Result;
use crate::idb::{Atom, Term, Variable};
use crate::syntax::{CHAR_PERIOD, QUERY_ASCII_PREFIX};
use crate::{Collection, IndexedCollection, PredicateRef};
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

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct QuerySet(HashSet<Query>);

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
        write!(f, "{} {}{}", QUERY_ASCII_PREFIX, self.0, CHAR_PERIOD)
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

impl Display for QuerySet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for query in self.iter() {
            writeln!(f, "{}", query)?;
        }
        Ok(())
    }
}

impl Collection<Query> for QuerySet {
    delegate!(is_empty -> bool);

    delegate!(len -> usize);

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Query> + '_> {
        Box::new(self.0.iter())
    }

    fn contains(&self, value: &Query) -> bool {
        self.0.contains(value)
    }
}

impl QuerySet {
    pub fn add(&mut self, rule: Query) -> bool {
        self.0.insert(rule)
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
    delegate!(is_empty, facts -> bool);

    delegate!(len, facts -> usize);

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Row> + '_> {
        Box::new(self.facts.iter())
    }

    fn contains(&self, value: &Row) -> bool {
        self.facts.contains(value)
    }
}

impl RelationOps for View {
    fn union(self, _other: Self) -> Result<View>
    where
        Self: Sized,
    {
        todo!()
    }

    fn intersection(self, _other: Self) -> Result<View>
    where
        Self: Sized,
    {
        todo!()
    }

    fn difference(self, _other: Self) -> Result<View>
    where
        Self: Sized,
    {
        todo!()
    }

    fn cartesian_product(self, _other: Self) -> Result<View>
    where
        Self: Sized,
    {
        todo!()
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

    fn natural_join(self, other: Self) -> Result<Self>
    where
        Self: Sized,
    {
        let mut new_table: Self = Self::new(
            self.schema()
                .label_union(other.schema())
                .into_iter()
                .map(Attribute::from)
                .collect::<Vec<Attribute<Variable>>>(),
        );
        let common_variables = self.schema().label_intersection(other.schema());

        // TODO: (ISSUE/rust-asdi/5) infer attribute types for new results!
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

    fn exists(self, criteria: &Selection) -> Result<bool> {
        for row in self.facts.into_iter() {
            if criteria.is_all() || criteria.is_match(row.values())? {
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

    get!(pub schema -> Schema<Variable>);

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

    pub fn natural_join_all<V: Into<Vec<View>>>(views: V) -> Result<Self> {
        let mut views = views.into();
        assert!(!views.is_empty());
        if views.len() == 1 {
            Ok(views.remove(0))
        } else {
            let mut views = views.into_iter();
            let mut result = views.next().unwrap();
            for next in views {
                result = result.natural_join(next)?;
            }
            Ok(result)
        }
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
    delegate!(is_empty -> bool);

    delegate!(len -> usize);

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
        Ok(if criteria.is_all() || criteria.is_match(self.values())? {
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
// Modules
// ------------------------------------------------------------------------------------------------

pub mod relational;
