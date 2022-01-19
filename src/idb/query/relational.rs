/*!
This module provides a simplified model of the Relational Algebra and includes the capability to
compile rules and atoms into [relational operations](RelationalOp).
*/

use super::{Row, View};
use crate::edb::{Attribute, Constant, Schema};
use crate::error::{attribute_index_invalid, nullary_facts_not_allowed, Error, Result};
use crate::idb::eval::ToGraphViz;
use crate::idb::{Atom, Comparison, ComparisonOperator, Literal, Rule, Term, Variable};
use crate::{Collection, Labeled, MaybeAnonymous, MaybeLabeled, PredicateRef, ProgramCore};
use paste::paste;
use regex::Regex;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use tracing::warn;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

pub trait RelationOps {
    fn natural_join(self, other: Self) -> Result<Self>
    where
        Self: Sized;

    fn theta_join<V: Into<Vec<Criteria>>>(self, _other: Self, _criteria: V) -> Result<Self>
    where
        Self: Sized,
    {
        unimplemented!()
    }

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
    Join(Join),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Join {
    lhs: Box<RelationalOp>,
    criteria: Vec<Criteria>,
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

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

pub fn program_to_graphviz(program: &impl ProgramCore) -> String {
    format!(
        "digraph G {{\n{}\n}}\n",
        program
            .rules()
            .iter()
            .enumerate()
            .map(|(index, rule)| {
                let expr = RelationalOp::compile_rule(rule).unwrap();
                expr.to_graphviz_graph((index + 1) as u32)
            })
            .collect::<Vec<String>>()
            .join("\n")
    )
}

// ------------------------------------------------------------------------------------------------
// Implementations
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
                Self::Join(v) => v.to_string(),
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

impl From<Join> for RelationalOp {
    fn from(v: Join) -> Self {
        Self::Join(v)
    }
}

impl RelationalOp {
    pub fn compile_atom(atom: &Atom, project_constants: bool) -> Result<Self> {
        Self::compile_atom_with(atom, project_constants, Default::default())
    }

    pub fn compile_atom_with(
        atom: &Atom,
        project_constants: bool,
        criteria: Vec<Criteria>,
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
            static_criteria.extend(criteria.into_iter());
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
                    (false, false, true) => RelationalOp::Projection(Projection::new(
                        projections,
                        RelationalOp::Relation(atom.label_ref()),
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

        // TODO: negation

        let mut ops: Vec<RelationalOp> = Default::default();
        let mut theta: Vec<&Comparison> = Default::default();
        for atom in relational {
            let mut criteria: Vec<Criteria> = Default::default();
            for comparison in &arithmetic {
                if let Err(e) = comparison.sanity_check() {
                    warn!(
                        "Ignoring arithmetic literal '{}', sanity check failed: {}",
                        comparison, e
                    );
                } else {
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
                                } else {
                                    theta.push(comparison);
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
            ops.push(Self::compile_atom_with(atom, false, criteria)?);
        }

        warn!(
            "Found comparisons for theta join, which is not yet implemented: {:?}",
            theta
        );

        let mut ops = ops.into_iter().rev();
        let last = ops.next().unwrap();
        let joined = ops.fold(last, |left, right| Join::natural(left, right).into());

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

    self_is_as!(join, Join, Join);
}

#[cfg(feature = "graphviz")]
impl ToGraphViz for RelationalOp {
    fn to_graphviz_string(&self) -> Result<String> {
        Ok(self.to_graphviz_graph(1))
    }
}

#[cfg(feature = "graphviz")]
impl RelationalOp {
    fn to_graphviz_graph(&self, graph_index: u32) -> String {
        let (_, nodes, edges) = self.graphviz_one(1 + (graph_index * 100));
        format!(
            "{}{}\n{}\n}}",
            if graph_index == 0 {
                "digraph G {{\n".to_string()
            } else {
                format!(
                    "subgraph cluster_{} {{\n  color=gray;\n  label=\"Rule {}\";\n\n",
                    graph_index, graph_index
                )
            },
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
            RelationalOp::Join(op) => {
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

                if op.is_natural() {
                    node_map.insert(self, (index, format!("  node{} [label=\"⨝\"];", index,)));
                } else {
                    node_map.insert(
                        self,
                        (
                            index,
                            format!(
                                "  node{} [label=\"⨝𝞱\\n[{}]\"];",
                                index,
                                op.criteria
                                    .iter()
                                    .map(|c| c.to_string())
                                    .collect::<Vec<String>>()
                                    .join(", "),
                            ),
                        ),
                    );
                }
                next_index + 1
            }
        };
        (next_index, node_map, edge_vec)
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Join {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_natural() {
            write!(f, "({}) ⨝ ({})", self.lhs, self.rhs)
        } else {
            write!(
                f,
                "({}) ⨝𝞱[{}] ({})",
                self.lhs,
                self.criteria
                    .iter()
                    .map(Criteria::to_string)
                    .collect::<Vec<String>>()
                    .join(", "),
                self.rhs
            )
        }
    }
}

impl Join {
    pub fn natural<S: Into<RelationalOp>>(lhs: S, rhs: S) -> Self {
        Self {
            lhs: Box::new(lhs.into()),
            criteria: Default::default(),
            rhs: Box::new(rhs.into()),
        }
    }

    pub fn theta<S: Into<RelationalOp>, V: Into<Vec<Criteria>>>(
        lhs: S,
        criteria: V,
        rhs: S,
    ) -> Self {
        let criteria = criteria.into();
        assert!(!criteria.is_empty());
        Self {
            lhs: Box::new(lhs.into()),
            criteria,
            rhs: Box::new(rhs.into()),
        }
    }

    pub fn is_natural(&self) -> bool {
        self.criteria.is_empty()
    }

    pub fn is_theta(&self) -> bool {
        !self.criteria.is_empty()
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

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::edb::Predicate;
    use crate::idb::eval::ToGraphViz;
    use crate::idb::query::relational::RelationalOp;
    use crate::idb::{Atom, Comparison, ComparisonOperator, Literal, Rule, Term, Variable};
    use std::str::FromStr;

    #[test]
    #[ignore]
    fn test_compile_rule_with_theta_join() {
        let head = Atom::new(
            Predicate::from_str("baz").unwrap().into(),
            [
                Term::Variable(Variable::from_str("X").unwrap().into()),
                Term::Variable(Variable::from_str("Y").unwrap().into()),
            ],
        );
        let body_1 = Atom::new(
            Predicate::from_str("foo").unwrap().into(),
            [
                Term::Variable(Variable::from_str("X").unwrap().into()),
                Term::Variable(Variable::from_str("A").unwrap().into()),
            ],
        );
        let body_2 = Atom::new(
            Predicate::from_str("bar").unwrap().into(),
            [
                Term::Variable(Variable::from_str("B").unwrap().into()),
                Term::Variable(Variable::from_str("Y").unwrap().into()),
            ],
        );
        let body_3 = Comparison::new(
            Term::Variable(Variable::from_str("A").unwrap().into()),
            ComparisonOperator::NotEqual,
            Term::Variable(Variable::from_str("B").unwrap().into()),
        )
        .unwrap();
        let rule: Rule = Rule::new(
            [head],
            [
                Literal::from(body_1),
                Literal::from(body_2),
                Literal::from(body_3),
            ],
        );

        println!(">>> {}", rule);
        let relational = RelationalOp::compile_rule(&rule).unwrap();
        println!("<<< {}", relational);
        println!("{}", relational.to_graphviz_string().unwrap());
        assert_eq!(
            relational.to_string(),
            "Π[X, Y]((path) ⨝𝞱[A=B] (edge))".to_string()
        );
    }
}
