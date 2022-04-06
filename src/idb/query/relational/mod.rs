/*!
This module provides a simplified model of the Relational Algebra and includes the capability to
compile rules and atoms into [relational operations](RelationalOp).

![module UML](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/book/src/model/idb_query_relational.svg)

 */

use super::{Row, View};
use crate::edb::{Attribute, Constant, Schema};
use crate::error::{
    attribute_index_invalid, incompatible_types, nullary_facts_not_allowed, Error, Result,
};
use crate::idb::{Atom, Comparison, ComparisonOperator, Rule, Term, Variable};
use crate::{Collection, Labeled, MaybeAnonymous, MaybePositive, PredicateRef};
use paste::paste;
use regex::Regex;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use tracing::warn;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

pub trait RelationOps {
    fn union(self, other: Self) -> Result<View>
    where
        Self: Sized;

    fn intersection(self, other: Self) -> Result<View>
    where
        Self: Sized;

    fn difference(self, other: Self) -> Result<View>
    where
        Self: Sized;

    fn cartesian_product(self, other: Self) -> Result<View>
    where
        Self: Sized;

    fn select(self, criteria: &Selection) -> Result<View>
    where
        Self: Sized;

    fn project(self, indices: &Projection) -> Result<View>
    where
        Self: Sized;

    fn natural_join(self, other: Self) -> Result<View>
    where
        Self: Sized;

    fn theta_join<V: Into<Vec<Criteria>>>(self, _other: Self, _criteria: V) -> Result<View>
    where
        Self: Sized,
    {
        unimplemented!()
    }

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
    Relation(RelationSource),
    SetOperation(SetOperation),
    Selection(Selection),
    Projection(Projection),
    Join(Join),
    Sink(RelationSink),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RelationSource {
    source: PredicateRef,
    is_extensional: Option<bool>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RelationSink {
    source: Box<RelationalOp>,
    sink: PredicateRef,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SetOperation {
    lhs: Box<RelationalOp>,
    op: SetOperator,
    rhs: Box<RelationalOp>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SetOperator {
    Union,
    Intersection,
    Difference,
    CartesianProduct,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Join {
    lhs: Box<RelationalOp>,
    criteria: Vec<Criteria>,
    rhs: Box<RelationalOp>,
}

// TODO: (ISSUE/rust-asdi/12) Need to support constants in the final projection.enhancement
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Projection {
    source: Box<RelationalOp>,
    attributes: Vec<Attribute<Variable>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Selection {
    source: Box<RelationalOp>,
    criteria: Vec<Criteria>,
    negated: bool,
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
// Implementations
// ------------------------------------------------------------------------------------------------

impl Display for RelationalOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Relation(v) => v.to_string(),
                Self::SetOperation(v) => v.to_string(),
                Self::Selection(v) => v.to_string(),
                Self::Projection(v) => v.to_string(),
                Self::Join(v) => v.to_string(),
                Self::Sink(v) => v.to_string(),
            }
        )
    }
}

impl From<PredicateRef> for RelationalOp {
    fn from(v: PredicateRef) -> Self {
        Self::Relation(v.into())
    }
}

impl From<(PredicateRef, bool)> for RelationalOp {
    fn from(v: (PredicateRef, bool)) -> Self {
        Self::Relation(RelationSource::new(v.0, v.1))
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
        println!(
            "compile_atom_with > {} ({}) {:?}",
            atom,
            if project_constants {
                "project constants"
            } else {
                "drop constants"
            },
            criteria
        );
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
        println!("compile_atom_with > project {:?}", projections);

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
            println!("compile_atom_with > static_criteria {:?}", static_criteria);
            static_criteria.extend(criteria.into_iter());
            Ok(
                match (
                    project_constants,
                    projections.len() == atom.len(), // true if we projection is complete
                    static_criteria.is_empty(),
                ) {
                    (_, true, true) => RelationalOp::Relation(atom.label_ref().into()),
                    (_, true, false) => RelationalOp::Selection(Selection::new(
                        static_criteria,
                        RelationalOp::Relation(atom.label_ref().into()),
                        false,
                    )),
                    (true, false, false) => RelationalOp::Selection(Selection::new(
                        static_criteria,
                        RelationalOp::Projection(Projection::new(
                            projections,
                            RelationalOp::Relation(atom.label_ref().into()),
                        )),
                        false,
                    )),
                    (false, false, false) => RelationalOp::Projection(Projection::new(
                        projections,
                        RelationalOp::Selection(Selection::new(
                            static_criteria,
                            RelationalOp::Relation(atom.label_ref().into()),
                            false,
                        )),
                    )),
                    (false, false, true) => RelationalOp::Projection(Projection::new(
                        projections,
                        RelationalOp::Relation(atom.label_ref().into()),
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
        println!("----------------------------------------------------------------------");
        let arithmetic: Vec<(&Comparison, bool)> = rule
            .literals()
            .filter_map(|lit| lit.as_arithmetic().map(|atom| (atom, lit.is_negative())))
            .collect();
        let relational: Vec<(&Atom, bool)> = rule
            .literals()
            .filter_map(|lit| lit.as_relational().map(|comp| (comp, lit.is_negative())))
            .collect();

        // TODO: (ISSUE/rust-asdi/3) negation

        let mut ops: Vec<RelationalOp> = Default::default();
        let mut theta: Vec<&Comparison> = Default::default();
        for (atom, atom_negated) in relational {
            println!("compile_rule > atom {} (negated {})", atom, atom_negated);
            let mut criteria: Vec<Criteria> = Default::default();
            for (comparison, comparison_negated) in &arithmetic {
                println!(
                    "compile_rule > atom > comparison {:?} (negated {})",
                    comparison, comparison_negated
                );
                if let Err(e) = comparison.sanity_check() {
                    warn!(
                        "Ignoring arithmetic literal '{:?}', sanity check failed: {}",
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
            let atom_op = Self::compile_atom_with(atom, false, criteria)?;
            println!("compile_rule > atom >> {}", atom_op);
            ops.push(atom_op);
        }

        warn!(
            "Found comparisons for theta join, which is not yet implemented: {:?}",
            theta
        );

        let mut ops = ops.into_iter().rev();
        let last = ops.next().unwrap();
        let joined = ops.fold(last, |left, right| Join::natural(left, right).into());
        println!("compile_rule > joined {:?}", joined);

        // TODO: (ISSUE/rust-asdi/4) may need rework for disjunction.

        let distinguished_terms = rule.distinguished_terms_in_order();
        let joined = if distinguished_terms.len() < rule.variables().len() {
            // TODO: (ISSUE/rust-asdi/12) Need to support constants in the final projection.
            let joined = RelationalOp::from(Projection::new(
                distinguished_terms
                    .iter()
                    .filter_map(|t| t.as_variable())
                    .map(|v| Attribute::labeled(v.clone()))
                    .collect::<Vec<Attribute<Variable>>>(),
                joined,
            ));
            println!("compile_rule > joined {:?}", joined);
            joined
        } else {
            joined
        };
        Ok(RelationalOp::Sink(RelationSink::new(
            joined,
            rule.head.get(0).unwrap().label_ref(),
        )))
    }

    self_is_as!(relation, Relation, RelationSource);

    self_is_as!(selection, Selection);

    self_is_as!(projection, Projection);

    self_is_as!(join, Join);

    self_is_as!(sink, Sink, RelationSink);
}

// ------------------------------------------------------------------------------------------------

impl Display for RelationSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.source)
    }
}

impl From<PredicateRef> for RelationSource {
    fn from(source: PredicateRef) -> Self {
        Self {
            source,
            is_extensional: None,
        }
    }
}

impl RelationSource {
    pub fn new(source: PredicateRef, is_extensional: bool) -> Self {
        Self {
            source,
            is_extensional: Some(is_extensional),
        }
    }

    pub fn extensional(source: PredicateRef) -> Self {
        Self::new(source, true)
    }

    pub fn intensional(source: PredicateRef) -> Self {
        Self::new(source, false)
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for RelationSink {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ≔ {}", self.sink, self.source)
    }
}

impl RelationSink {
    pub fn new(source: RelationalOp, sink: PredicateRef) -> Self {
        Self {
            source: Box::new(source),
            sink,
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for SetOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) {} ({})", self.lhs, self.op, self.rhs)
    }
}

impl SetOperation {
    pub fn new<S: Into<RelationalOp>>(lhs: S, op: SetOperator, rhs: S) -> Self {
        Self {
            lhs: Box::new(lhs.into()),
            op,
            rhs: Box::new(rhs.into()),
        }
    }

    pub fn union<S: Into<RelationalOp>>(lhs: S, rhs: S) -> Self {
        Self {
            lhs: Box::new(lhs.into()),
            op: SetOperator::Union,
            rhs: Box::new(rhs.into()),
        }
    }

    pub fn intersection<S: Into<RelationalOp>>(lhs: S, rhs: S) -> Self {
        Self {
            lhs: Box::new(lhs.into()),
            op: SetOperator::Intersection,
            rhs: Box::new(rhs.into()),
        }
    }

    pub fn difference<S: Into<RelationalOp>>(lhs: S, rhs: S) -> Self {
        Self {
            lhs: Box::new(lhs.into()),
            op: SetOperator::Difference,
            rhs: Box::new(rhs.into()),
        }
    }

    pub fn cartesian_product<S: Into<RelationalOp>>(lhs: S, rhs: S) -> Self {
        Self {
            lhs: Box::new(lhs.into()),
            op: SetOperator::CartesianProduct,
            rhs: Box::new(rhs.into()),
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for SetOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Union => "∪",
                Self::Intersection => "∩",
                Self::Difference => "∖",
                Self::CartesianProduct => "⨯",
            }
        )
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Join {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_natural() {
            write!(f, "({}) ⨝  ({})", self.lhs, self.rhs)
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
            Ok(Self::all(RelationalOp::Relation(atom.label_ref().into())))
        } else if projections.is_empty() {
            Err(nullary_facts_not_allowed())
        } else {
            Ok(Self::new(
                projections,
                RelationalOp::Relation(atom.label_ref().into()),
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

    fn try_from(value: &Atom) -> std::result::Result<Self, Self::Error> {
        Ok(Self {
            source: Box::new(RelationalOp::Relation(value.label_ref().into())),
            criteria: value
                .iter()
                .enumerate()
                .filter_map(|(i, term)| term.as_constant().map(|c| (i, c)))
                .map(|(i, constant)| Criteria {
                    index: i,
                    op: ComparisonOperator::Equal,
                    value: CriteriaValue::Value(constant.clone()),
                })
                .collect(),
            negated: false,
        })
    }
}

impl TryFrom<&Rule> for Selection {
    type Error = Error;

    fn try_from(_value: &Rule) -> std::result::Result<Self, Self::Error> {
        unimplemented!()
    }
}

#[allow(clippy::len_without_is_empty)]
impl Selection {
    pub fn new<V: Into<Vec<Criteria>>, S: Into<RelationalOp>>(
        criteria: V,
        from: S,
        negated: bool,
    ) -> Self {
        Self {
            source: Box::new(from.into()),
            criteria: criteria.into(),
            negated,
        }
    }

    pub fn all<S: Into<RelationalOp>>(from: S) -> Self {
        Self {
            source: Box::new(from.into()),
            criteria: Default::default(),
            negated: false,
        }
    }

    pub fn is_all(&self) -> bool {
        self.criteria.is_empty()
    }

    pub fn is_negated(&self) -> bool {
        self.negated
    }

    delegate!(pub len, criteria -> usize);

    delegate!(pub iter, criteria -> impl Iterator<Item = &'_ Criteria>);

    pub fn contains(&self, value: &Criteria) -> bool {
        self.criteria.contains(value)
    }

    pub fn add(&mut self, criteria: Criteria) {
        self.criteria.push(criteria);
    }

    pub fn is_match(&self, fact: &[Constant]) -> Result<bool> {
        for criteria in &self.criteria {
            if !criteria.is_match(fact)? {
                return Ok(false);
            }
        }
        Ok(true)
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

    pub fn is_match(&self, fact: &[Constant]) -> Result<bool> {
        let lhs = fact
            .get(self.index)
            .ok_or_else(|| attribute_index_invalid(self.index))?;
        let rhs = match &self.value {
            CriteriaValue::Value(v) => v,
            CriteriaValue::Index(i) => fact.get(*i).ok_or_else(|| attribute_index_invalid(*i))?,
        };
        if lhs.kind() != rhs.kind() {
            Err(incompatible_types(
                lhs.kind().to_string(),
                rhs.kind().to_string(),
            ))
        } else {
            Ok(match self.op {
                ComparisonOperator::Equal => lhs == rhs,
                ComparisonOperator::NotEqual => lhs != rhs,
                ComparisonOperator::LessThan => lhs < rhs,
                ComparisonOperator::LessThanOrEqual => lhs <= rhs,
                ComparisonOperator::GreaterThan => lhs > rhs,
                ComparisonOperator::GreaterThanOrEqual => lhs >= rhs,
                ComparisonOperator::StringMatch => {
                    // TODO: cache regex
                    let lhs = lhs.as_string().unwrap();
                    let rhs = rhs.as_string().unwrap();
                    let regex: Regex = Regex::new(rhs).unwrap();
                    regex.is_match(lhs)
                }
            })
        }
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
// Modules
// ------------------------------------------------------------------------------------------------

pub mod ops;

#[cfg(feature = "graphviz")]
pub mod graph;

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::edb::Predicate;
    use crate::idb::query::relational::RelationalOp;
    use crate::idb::{Atom, Comparison, ComparisonOperator, Literal, Rule, Term, Variable};
    use std::str::FromStr;

    #[test]
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
        println!("<<< {:#?}", relational);

        #[cfg(feature = "graphviz")]
        println!("{}", relational.to_graphviz_string().unwrap());

        assert_eq!(
            relational.to_string(),
            "baz ≔ Π[X, Y]((bar) ⨝  (foo))".to_string()
        );
    }
}
