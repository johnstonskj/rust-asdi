/*!
This module provides the [Query] type that represents a query goal as well as the [View] and [Row].
types used to return query results.
*/

use crate::edb::{Attribute, AttributeIndex, AttributeKind, Constant, Fact, Schema};
use crate::error::{attribute_index_invalid, nullary_facts_not_allowed, Error, Result};
use crate::idb::{Atom, Comparison, ComparisonOperator, Literal, Rule, Term, Variable};
use crate::syntax::{CHAR_PERIOD, QUERY_ASCII_PREFIX};
use crate::{Collection, IndexedCollection, Labeled, MaybeAnonymous, MaybeLabeled, PredicateRef};
use paste::paste;
use regex::Regex;
use std::collections::{HashMap, HashSet};
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
    fn join(self, other: Self) -> Result<Self>
    where
        Self: Sized;

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RelationalOp {
    Relation(PredicateRef),
    Selection(Selection),
    Projection(Projection),
    NaturalJoin(NaturalJoin),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NaturalJoin {
    lhs: Box<RelationalOp>,
    rhs: Box<RelationalOp>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Projection {
    source: Box<RelationalOp>,
    attributes: Vec<Attribute<Variable>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Selection {
    source: Box<RelationalOp>,
    criteria: Vec<Criteria>,
}

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
    fn join(self, other: Self) -> Result<Self>
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

    pub fn join_all<V: Into<Vec<View>>>(views: V) -> Result<Self> {
        let mut views = views.into();
        assert!(!views.is_empty());
        if views.len() == 1 {
            Ok(views.remove(0))
        } else {
            let mut views = views.into_iter();
            let mut result = views.next().unwrap();
            for next in views {
                result = result.join(next)?;
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
        Ok(if criteria.is_all() || criteria.matches(self.values())? {
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

impl Display for RelationalOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Relation(v) => v.to_string(),
                Self::Selection(v) => v.to_string(),
                Self::Projection(v) => v.to_string(),
                Self::NaturalJoin(v) => v.to_string(),
            }
        )
    }
}

impl From<PredicateRef> for RelationalOp {
    fn from(v: PredicateRef) -> Self {
        Self::Relation(v)
    }
}

impl From<Selection> for RelationalOp {
    fn from(v: Selection) -> Self {
        Self::Selection(v)
    }
}

impl From<Projection> for RelationalOp {
    fn from(v: Projection) -> Self {
        Self::Projection(v)
    }
}

impl From<NaturalJoin> for RelationalOp {
    fn from(v: NaturalJoin) -> Self {
        Self::NaturalJoin(v)
    }
}

impl RelationalOp {
    pub fn compile_atom(atom: &Atom, project_constants: bool) -> Result<Self> {
        Self::compile_atom_with(atom, project_constants, Default::default())
    }

    pub fn compile_atom_with(
        atom: &Atom,
        project_constants: bool,
        critera: Vec<Criteria>,
    ) -> Result<Self> {
        let projections: Vec<Attribute<Variable>> = atom
            .iter()
            .enumerate()
            .filter(|(_, term)| {
                if project_constants {
                    !term.is_anonymous()
                } else {
                    term.is_variable()
                }
            })
            .map(|(i, term)| {
                let mut attribute = match term {
                    Term::Variable(v) => Attribute::from(v.clone()),
                    Term::Constant(v) => Attribute::typed(v.kind()),
                    Term::Anonymous => unreachable!(),
                };
                attribute.set_index(i);
                attribute
            })
            .collect();

        if projections.is_empty() {
            Err(nullary_facts_not_allowed())
        } else {
            let mut static_criteria: Vec<Criteria> = atom
                .iter()
                .enumerate()
                .filter_map(|(i, term)| term.as_constant().map(|c| (i, c)))
                .map(|(i, constant)| Criteria {
                    index: i,
                    op: ComparisonOperator::Equal,
                    value: CriteriaValue::Value(constant.clone()),
                })
                .collect();
            static_criteria.extend(critera.into_iter());
            Ok(
                match (
                    project_constants,
                    projections.len() == atom.len(), // true if we projection is complete
                    static_criteria.is_empty(),
                ) {
                    (_, true, true) => RelationalOp::Relation(atom.label_ref()),
                    (_, true, false) => RelationalOp::Selection(Selection::new(
                        static_criteria,
                        RelationalOp::Relation(atom.label_ref()),
                    )),
                    (true, false, false) => RelationalOp::Selection(Selection::new(
                        static_criteria,
                        RelationalOp::Projection(Projection::new(
                            projections,
                            RelationalOp::Relation(atom.label_ref()),
                        )),
                    )),
                    (false, false, false) => RelationalOp::Projection(Projection::new(
                        projections,
                        RelationalOp::Selection(Selection::new(
                            static_criteria,
                            RelationalOp::Relation(atom.label_ref()),
                        )),
                    )),
                    state => {
                        eprintln!("Unexpected state: {:?}", state);
                        unreachable!()
                    }
                },
            )
        }
    }

    pub fn compile_rule(rule: &Rule) -> Result<Self> {
        let arithmetic: Vec<&Comparison> =
            rule.literals().filter_map(Literal::as_arithmetic).collect();
        let relational: Vec<&Atom> = rule.literals().filter_map(Literal::as_relational).collect();

        let mut ops: Vec<RelationalOp> = Default::default();
        for atom in relational {
            let mut criteria: Vec<Criteria> = Default::default();
            for comparison in &arithmetic {
                match (comparison.lhs(), comparison.operator(), comparison.rhs()) {
                    (Term::Variable(lhs), op, Term::Constant(rhs)) => {
                        if let Some(index) = atom.variable_index(lhs) {
                            criteria.push(Criteria::new(
                                index,
                                *op,
                                CriteriaValue::Value(rhs.clone()),
                            ))
                        }
                    }
                    (Term::Constant(lhs), op, Term::Variable(rhs)) => {
                        if let Some(index) = atom.variable_index(rhs) {
                            criteria.push(Criteria::new(
                                index,
                                op.inverse(),
                                CriteriaValue::Value(lhs.clone()),
                            ));
                        }
                    }
                    (Term::Variable(lhs), op, Term::Variable(rhs)) => {
                        if let Some(lhs_index) = atom.variable_index(lhs) {
                            if let Some(rhs_index) = atom.variable_index(rhs) {
                                criteria.push(Criteria::new(
                                    lhs_index,
                                    *op,
                                    CriteriaValue::Index(rhs_index),
                                ));
                            }
                        }
                    }
                    _ => unreachable!(),
                }
            }
            ops.push(Self::compile_atom_with(atom, false, criteria)?);
        }

        // TODO: cross-atom criteria

        let mut ops = ops.into_iter().rev();
        let last = ops.next().unwrap();
        let joined = ops.fold(last, |left, right| {
            NaturalJoin::new(
                // Simplify if a simple relation reference
                if let Some(left) = left.as_relation() {
                    RelationalOp::Relation(left.clone())
                } else {
                    left
                },
                // Simplify if a simple relation reference
                if let Some(right) = right.as_relation() {
                    RelationalOp::Relation(right.clone())
                } else {
                    right
                },
            )
            .into()
        });

        // These need to preserve their order.
        // TODO: may need rework for disjunction.
        let distinguished_terms = rule.distinguished_terms_in_order();
        if distinguished_terms.len() < rule.terms().len() {
            Ok(Projection::new(
                distinguished_terms
                    .iter()
                    .filter_map(|t| t.as_variable())
                    .map(|v| Attribute::labeled(v.clone()))
                    .collect::<Vec<Attribute<Variable>>>(),
                joined,
            )
            .into())
        } else {
            Ok(joined)
        }
    }

    self_is_as!(relation, Relation, PredicateRef);

    self_is_as!(selection, Selection, Selection);

    self_is_as!(projection, Projection, Projection);

    self_is_as!(natural_join, NaturalJoin, NaturalJoin);

    pub fn to_graphviz_string(&self) -> String {
        let (_, nodes, edges) = self.graphviz_one(1);
        format!(
            "digraph G {{\n{}\n{}\n}}",
            nodes
                .into_iter()
                .map(|(_, (_, string))| string)
                .collect::<Vec<String>>()
                .join("\n"),
            edges
                .into_iter()
                .map(|(lhs, rhs)| format!("  node{} -> node{};", lhs, rhs))
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }

    #[allow(clippy::type_complexity)]
    fn graphviz_one(&self, index: u32) -> (u32, HashMap<&Self, (u32, String)>, Vec<(u32, u32)>) {
        let mut node_map: HashMap<&Self, (u32, String)> = Default::default();
        let mut edge_vec: Vec<(u32, u32)> = Default::default();

        let next_index = match self {
            RelationalOp::Relation(op) => {
                if !node_map.contains_key(self) {
                    node_map.insert(
                        self,
                        (index, format!("  node{} [label=\"{}\"];\n", index, op)),
                    );
                }
                index + 1
            }
            RelationalOp::Selection(op) => {
                let (next_index, nodes, edges) = op.source.graphviz_one(index + 1);
                node_map.extend(nodes.into_iter());
                edge_vec.extend(edges.into_iter());
                if let Some((source_index, _)) = node_map.get(op.source.as_ref()) {
                    edge_vec.push((index, *source_index));
                } else {
                    unreachable!()
                }

                node_map.insert(
                    self,
                    (
                        index,
                        format!(
                            "  node{} [label=\"σ\\n[{}]\"];",
                            index,
                            op.criteria
                                .iter()
                                .map(|c| c.to_string())
                                .collect::<Vec<String>>()
                                .join(", "),
                        ),
                    ),
                );
                next_index + 1
            }
            RelationalOp::Projection(op) => {
                let (next_index, nodes, edges) = op.source.graphviz_one(index + 1);
                node_map.extend(nodes.into_iter());
                edge_vec.extend(edges.into_iter());
                if let Some((source_index, _)) = node_map.get(op.source.as_ref()) {
                    edge_vec.push((index, *source_index));
                } else {
                    unreachable!()
                }

                node_map.insert(
                    self,
                    (
                        index,
                        format!(
                            "  node{} [label=\"Π\\n[{}]\"];",
                            index,
                            op.attributes
                                .iter()
                                .map(|v| if let Some(index) = v.index() {
                                    index.to_string()
                                } else if let Some(label) = v.label() {
                                    label.to_string()
                                } else {
                                    todo!()
                                })
                                .collect::<Vec<String>>()
                                .join(", "),
                        ),
                    ),
                );
                next_index + 1
            }
            RelationalOp::NaturalJoin(op) => {
                let (first_next_index, nodes, edges) = op.lhs.graphviz_one(index + 1);
                node_map.extend(nodes.into_iter());
                edge_vec.extend(edges.into_iter());
                if let Some((source_index, _)) = node_map.get(op.lhs.as_ref()) {
                    edge_vec.push((index, *source_index));
                } else {
                    unreachable!()
                }

                let (next_index, nodes, edges) = op.rhs.graphviz_one(first_next_index + 1);
                node_map.extend(nodes.into_iter());
                edge_vec.extend(edges.into_iter());
                if let Some((source_index, _)) = node_map.get(op.rhs.as_ref()) {
                    edge_vec.push((index, *source_index));
                } else {
                    unreachable!()
                }

                node_map.insert(self, (index, format!("  node{} [label=\"⨝\"];", index,)));
                next_index + 1
            }
        };
        (next_index, node_map, edge_vec)
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for NaturalJoin {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) ⨝ ({})", self.lhs, self.rhs)
    }
}

impl NaturalJoin {
    pub fn new<S: Into<RelationalOp>>(lhs: S, rhs: S) -> Self {
        Self {
            lhs: Box::new(lhs.into()),
            rhs: Box::new(rhs.into()),
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Projection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_all() {
            write!(f, "{}", self.source)
        } else {
            write!(
                f,
                "Π[{}]({})",
                self.attributes
                    .iter()
                    .map(|attribute| if let Some(index) = attribute.index() {
                        index.to_string()
                    } else if let Some(label) = attribute.label() {
                        label.to_string()
                    } else {
                        unreachable!()
                    })
                    .collect::<Vec<String>>()
                    .join(", "),
                self.source
            )
        }
    }
}

impl TryFrom<&Atom> for Projection {
    type Error = Error;

    fn try_from(atom: &Atom) -> std::result::Result<Self, Self::Error> {
        let projections: Vec<Attribute<Variable>> = atom
            .iter()
            .enumerate()
            .filter(|(_, term)| !term.is_anonymous())
            .map(|(i, term)| {
                let mut attribute = match term {
                    Term::Variable(v) => Attribute::from(v.clone()),
                    Term::Constant(v) => Attribute::typed(v.kind()),
                    Term::Anonymous => unreachable!(),
                };
                attribute.set_index(i);
                attribute
            })
            .collect();

        if projections.len() == atom.len() {
            Ok(Self::all(RelationalOp::Relation(atom.label_ref())))
        } else if projections.is_empty() {
            Err(nullary_facts_not_allowed())
        } else {
            Ok(Self::new(
                projections,
                RelationalOp::Relation(atom.label_ref()),
            ))
        }
    }
}

impl From<Projection> for Schema<Variable> {
    fn from(p: Projection) -> Self {
        Self::from(
            p.attributes
                .into_iter()
                .collect::<Vec<Attribute<Variable>>>(),
        )
    }
}

#[allow(clippy::len_without_is_empty)]
impl Projection {
    pub fn new<V: Into<Vec<Attribute<Variable>>>, S: Into<RelationalOp>>(
        attributes: V,
        from: S,
    ) -> Self {
        Self {
            source: Box::new(from.into()),
            attributes: attributes.into(),
        }
    }

    pub fn all<S: Into<RelationalOp>>(from: S) -> Self {
        Self {
            source: Box::new(from.into()),
            attributes: Default::default(),
        }
    }

    pub fn is_all(&self) -> bool {
        self.attributes.is_empty()
    }

    delegate!(pub len, attributes -> usize);

    delegate!(pub iter, attributes -> impl Iterator<Item = &'_ Attribute<Variable>>);

    pub fn contains_index(&self, index: &usize) -> bool {
        self.attributes
            .iter()
            .any(|attribute| attribute.index() == Some(*index))
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Selection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_all() {
            write!(f, "{}", self.source)
        } else {
            write!(
                f,
                "σ[{}]({})",
                self.criteria
                    .iter()
                    .map(Criteria::to_string)
                    .collect::<Vec<String>>()
                    .join(", "),
                self.source
            )
        }
    }
}

impl TryFrom<&Atom> for Selection {
    type Error = Error;

    fn try_from(atom: &Atom) -> std::result::Result<Self, Self::Error> {
        Ok(Self {
            source: Box::new(RelationalOp::Relation(atom.label_ref())),
            criteria: atom
                .iter()
                .enumerate()
                .filter_map(|(i, term)| term.as_constant().map(|c| (i, c)))
                .map(|(i, constant)| Criteria {
                    index: i,
                    op: ComparisonOperator::Equal,
                    value: CriteriaValue::Value(constant.clone()),
                })
                .collect(),
        })
    }
}

#[allow(clippy::len_without_is_empty)]
impl Selection {
    pub fn new<V: Into<Vec<Criteria>>, S: Into<RelationalOp>>(criteria: V, from: S) -> Self {
        Self {
            source: Box::new(from.into()),
            criteria: criteria.into(),
        }
    }

    pub fn all<S: Into<RelationalOp>>(from: S) -> Self {
        Self {
            source: Box::new(from.into()),
            criteria: Default::default(),
        }
    }

    pub fn is_all(&self) -> bool {
        self.criteria.is_empty()
    }

    delegate!(pub len, criteria -> usize);

    delegate!(pub iter, criteria -> impl Iterator<Item = &'_ Criteria>);

    pub fn contains(&self, value: &Criteria) -> bool {
        self.criteria.contains(value)
    }

    pub fn add(&mut self, criteria: Criteria) {
        self.criteria.push(criteria);
    }

    pub fn matches(&self, fact: &[Constant]) -> Result<bool> {
        for criteria in &self.criteria {
            if !self.matches_one(fact, criteria)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

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

impl Display for Criteria {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.index, self.op, self.value)
    }
}

impl Criteria {
    pub fn new(index: usize, op: ComparisonOperator, value: CriteriaValue) -> Self {
        Self { index, op, value }
    }

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

impl Display for CriteriaValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CriteriaValue::Value(v) => v.to_string(),
                CriteriaValue::Index(v) => v.to_string(),
            }
        )
    }
}

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
