/*!
One-line description.

More detailed description, with

# Example

*/

use crate::edb::{Constant, DbValidation, Predicate};
use crate::error::Result;
use crate::parse::SourceLocation;
use crate::{
    Database, Error, FeatureSet, Relation, SyntacticFragments, CHAR_LEFT_PAREN, CHAR_PERIOD,
    CHAR_RIGHT_PAREN, CHAR_UNDERSCORE, CONJUNCTION_UNICODE_SYMBOL, EMPTY_STR,
    IMPLICATION_UNICODE_ARROW, NOT_UNICODE_SYMBOL, OPERATOR_ASCII_EQUAL,
    OPERATOR_ASCII_GREATER_THAN, OPERATOR_ASCII_GREATER_THAN_OR_EQUAL, OPERATOR_ASCII_LESS_THAN,
    OPERATOR_ASCII_LESS_THAN_OR_EQUAL, OPERATOR_ASCII_NOT_EQUAL, OPERATOR_ASCII_NOT_EQUAL_ALT,
    OPERATOR_UNICODE_GREATER_THAN_OR_EQUAL, OPERATOR_UNICODE_LESS_THAN_OR_EQUAL,
    OPERATOR_UNICODE_NOT_EQUAL, TYPE_NAME_COMPARISON_OPERATOR, TYPE_NAME_VARIABLE,
};
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// A rule has a head, and a body where the head and each element of the body is an
/// [_atom_](struct.Atom.html). All atoms in the body are joined by conjunction, so _atom-1 and
/// atom-2, ..._. Each Atom has one or more arguments that are [_terms_](enum.Term.html), which
/// are either constant values or _variables_.
///
/// # Examples
///
/// Note that predicate identifiers always start with a lowercase character, and constants may be
/// identifiers, double-quoted string, integer, or boolean values. Variable identifiers always
/// start with an uppercase character.
///
/// ```prolog
/// ancestor(X, Y) :- parent(X, Y).
/// ancestor(X, Y) <- parent(X, "Socrates").
/// ancestor(X, Y) ⟵ parent(X, Y).
/// ```
///
/// A predicate may be an identifier, or a string, as shown in the following.
///
/// ```prolog
/// ancestor(X, Y).
/// "an ancestor"(X, Y).
/// ```
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Rule {
    head: Atom,
    body: Vec<Literal>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Atom {
    predicate: Predicate,
    terms: Vec<Term>,
    src_loc: Option<SourceLocation>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Literal {
    negative: bool,
    inner: LiteralInner,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralInner {
    Atom(Atom),
    Comparison(Comparison),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Comparison {
    lhs: Term,
    operator: ComparisonOperator,
    rhs: Term,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ComparisonOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Term {
    Variable(Variable),
    Constant(Constant),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable(String);

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

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.head.to_string(),
            if self.body.is_empty() {
                unreachable!("Rule body is empty!")
            } else {
                format!(
                    " {} {}",
                    IMPLICATION_UNICODE_ARROW,
                    self.body
                        .iter()
                        .map(Literal::to_string)
                        .collect::<Vec<String>>()
                        .join(&format!(" {} ", CONJUNCTION_UNICODE_SYMBOL))
                )
            },
            CHAR_PERIOD,
        )
    }
}

impl SyntacticFragments for Rule {
    fn is_positive(&self) -> bool {
        self.body.iter().all(Literal::is_positive)
    }

    fn is_linear(&self) -> bool {
        self.body.len() == 1
    }

    fn is_guarded(&self) -> bool {
        let all_variables = self.variables();
        self.literals().any(|lit| {
            let lit_variables: HashSet<&Variable> = HashSet::from_iter(lit.variables().into_iter());
            lit_variables == all_variables
        })
    }

    fn is_frontier_guarded(&self) -> bool {
        let frontier_variables: HashSet<&Variable> = self
            .head_variables()
            .intersection(&self.variables())
            .copied()
            .collect();
        self.literals().any(|lit| {
            let lit_variables: HashSet<&Variable> = HashSet::from_iter(lit.variables().into_iter());
            lit_variables == frontier_variables
        })
    }

    fn is_non_recursive(&self) -> bool {
        // TODO: this is only direct recursion, need to check for mutual recursive rules.
        let head_predicate = self.head().predicate();
        !self
            .literals()
            .filter_map(|lit| {
                if let LiteralInner::Atom(atom) = lit.inner() {
                    Some(atom.predicate())
                } else {
                    None
                }
            })
            .any(|predicate| predicate == head_predicate)
    }
}

impl DbValidation for Rule {
    fn validate(&self, against: &mut Database) -> Result<()> {
        Atom::validate(self.head(), against)?;
        for literal in self.literals() {
            Literal::validate(literal, against)?;
        }
        Ok(())
    }
}

impl Rule {
    pub fn new(head: Atom) -> Self {
        Self {
            head,
            body: Default::default(),
        }
    }

    pub fn new_with_body<V: Into<Vec<Literal>>>(head: Atom, body: V) -> Self {
        Self {
            head,
            body: body.into(),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn head(&self) -> &Atom {
        &self.head
    }

    // --------------------------------------------------------------------------------------------

    pub fn has_body(&self) -> bool {
        !self.body.is_empty()
    }

    pub fn push<L: Into<Literal>>(&mut self, literal: L) -> &mut Self {
        self.body.push(literal.into());
        self
    }

    pub fn extend<V: Into<Vec<Literal>>>(&mut self, literals: V) -> &mut Self {
        self.body.append(&mut literals.into());
        self
    }

    pub fn remove<L: Into<Literal>>(&mut self, literal: L) -> &mut Self {
        let literal = literal.into();
        self.body.retain(move |p| p != &literal);
        self
    }

    pub fn literals(&self) -> impl Iterator<Item = &Literal> {
        self.body.iter()
    }

    pub fn positive_literals(&self) -> impl Iterator<Item = &Literal> {
        self.body.iter().filter(|lit| lit.is_positive())
    }

    pub fn negative_literals(&self) -> impl Iterator<Item = &Literal> {
        self.body.iter().filter(|lit| !lit.is_positive())
    }

    // --------------------------------------------------------------------------------------------

    pub fn distinguished_terms(&self) -> HashSet<&Term> {
        self.head().terms().collect()
    }

    pub fn non_distinguished_terms(&self) -> HashSet<&Term> {
        let distinguished = self.distinguished_terms();
        self.terms()
            .into_iter()
            .filter(|term| !distinguished.contains(term))
            .collect()
    }

    pub fn terms(&self) -> HashSet<&Term> {
        self.body
            .iter()
            .map(|lit| lit.terms())
            .flatten()
            .collect::<HashSet<&Term>>()
            .intersection(&self.head.terms().collect())
            .copied()
            .collect()
    }

    pub fn positive_terms(&self) -> HashSet<&Term> {
        self.body
            .iter()
            .filter(|lit| lit.is_positive())
            .map(|lit| lit.terms())
            .flatten()
            .collect()
    }

    pub fn negative_terms(&self) -> HashSet<&Term> {
        self.body
            .iter()
            .filter(|lit| !lit.is_positive())
            .map(|lit| lit.terms())
            .flatten()
            .collect()
    }

    // --------------------------------------------------------------------------------------------

    pub fn head_variables(&self) -> HashSet<&Variable> {
        self.head().variables().collect()
    }

    pub fn variables(&self) -> HashSet<&Variable> {
        self.body
            .iter()
            .map(|lit| lit.variables())
            .flatten()
            .collect()
    }

    pub fn positive_variables(&self) -> HashSet<&Variable> {
        self.body
            .iter()
            .filter(|lit| lit.is_positive())
            .map(|lit| lit.variables())
            .flatten()
            .collect()
    }

    pub fn negative_variables(&self) -> HashSet<&Variable> {
        self.body
            .iter()
            .filter(|lit| !lit.is_positive())
            .map(|lit| lit.variables())
            .flatten()
            .collect()
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// # Well-formedness
    ///
    /// A Datalog program is well‐formed if all of its rules are well‐formed.
    ///
    /// A rule is well‐formed iff:
    ///
    /// 1. all variables that appear in the head also appear in a positive literal in
    ///    the body of the clause.
    /// 2. all variables that appear in a negative literal in the body of a clause also appears in some
    ///    positive literal in the body of the clause.
    ///
    pub fn check_well_formed(&self, _features: &FeatureSet) -> Result<()> {
        if !self.body.is_empty() {
            let body_positive_terms = self.positive_terms();

            let missing: Vec<&Term> = self
                .distinguished_terms()
                .into_iter()
                .filter(|term| !body_positive_terms.contains(term))
                .collect();
            if !missing.is_empty() {
                return Err(Error::HeadVariablesMissingInBody(
                    self.head().predicate().to_string(),
                    self.head().source_location().cloned(),
                    missing
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>(),
                ));
            }

            let missing: Vec<&Term> = self
                .negative_terms()
                .into_iter()
                .filter(|term| !body_positive_terms.contains(term))
                .collect();
            if !missing.is_empty() {
                return Err(Error::NegativeVariablesNotPositive(
                    self.head().predicate().to_string(),
                    self.head().source_location().cloned(),
                    missing
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>(),
                ));
            }
        }
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.predicate,
            CHAR_LEFT_PAREN,
            if self.terms.is_empty() {
                unreachable!("Atom terms are empty")
            } else {
                self.terms
                    .iter()
                    .map(Term::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            },
            CHAR_RIGHT_PAREN,
        )
    }
}

impl DbValidation for Atom {
    fn validate(&self, _db: &mut Database) -> Result<()> {
        // if !db.contains(self.predicate()) {
        //     db.make_new_relation_from(self.predicate().clone(), )
        // }
        Ok(())
    }
}

impl Atom {
    pub fn new<T: Into<Vec<Term>>>(predicate: Predicate, terms: T) -> Self {
        let terms = terms.into();
        assert!(!terms.is_empty());
        Self {
            predicate,
            terms,
            src_loc: None,
        }
    }

    pub fn new_from<T: Into<Vec<Term>>>(relation: &Relation, terms: T) -> Result<Self> {
        let terms = terms.into();
        let schema = relation.schema();
        assert_eq!(terms.len(), schema.len());
        for (i, t) in terms.iter().enumerate() {
            if let Term::Constant(c) = t {
                if c.kind() != schema.get(i).unwrap().kind().unwrap() {
                    return Error::FactDoesNotConformToSchema(
                        relation.predicate().clone(),
                        terms
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<String>>()
                            .join(", "),
                    )
                    .into();
                }
            }
        }
        Ok(Self {
            predicate: relation.predicate().clone(),
            terms,
            src_loc: None,
        })
    }

    pub fn new_at_location(predicate: Predicate, terms: &[Term], location: SourceLocation) -> Self {
        let terms: Vec<Term> = terms.into();
        assert!(!terms.is_empty());
        Self {
            predicate,
            terms,
            src_loc: Some(location),
        }
    }

    pub fn predicate(&self) -> &Predicate {
        &self.predicate
    }

    pub fn push<V: Into<Term>>(&mut self, argument: V) -> &mut Self {
        self.terms.push(argument.into());
        self
    }

    pub fn extend<T: Into<Vec<Term>>>(&mut self, arguments: T) -> &mut Self {
        self.terms.append(&mut arguments.into());
        self
    }

    pub fn remove<V: Into<Term>>(&mut self, argument: V) -> &mut Self {
        assert!(self.terms.len() > 1);
        let argument: Term = argument.into();
        self.terms.retain(move |p| p != &argument);
        self
    }

    pub fn terms(&self) -> impl Iterator<Item = &Term> {
        self.terms.iter()
    }

    pub fn arity(&self) -> usize {
        self.terms.len()
    }

    pub fn variables(&self) -> impl Iterator<Item = &Variable> {
        self.terms.iter().filter_map(|t| {
            if let Term::Variable(v) = t {
                Some(v)
            } else {
                None
            }
        })
    }

    pub fn source_location(&self) -> Option<&SourceLocation> {
        self.src_loc.as_ref()
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            if self.negative {
                NOT_UNICODE_SYMBOL
            } else {
                EMPTY_STR
            },
            self.inner,
        )
    }
}

impl From<Atom> for Literal {
    fn from(v: Atom) -> Self {
        Self::atom(v)
    }
}

impl From<Comparison> for Literal {
    fn from(v: Comparison) -> Self {
        Self::comparison(v)
    }
}

impl DbValidation for Literal {
    fn validate(&self, against: &mut Database) -> Result<()> {
        LiteralInner::validate(self.inner(), against)
    }
}

impl Literal {
    pub fn atom(atom: Atom) -> Self {
        Self {
            negative: false,
            inner: atom.into(),
        }
    }

    pub fn negative_atom(atom: Atom) -> Self {
        Self {
            negative: true,
            inner: atom.into(),
        }
    }

    pub fn comparison(comparison: Comparison) -> Self {
        Self {
            negative: false,
            inner: comparison.into(),
        }
    }

    pub fn negative_comparison(comparison: Comparison) -> Self {
        Self {
            negative: true,
            inner: comparison.into(),
        }
    }

    pub fn is_positive(&self) -> bool {
        !self.negative
    }

    pub fn inner(&self) -> &LiteralInner {
        &self.inner
    }

    pub fn terms(&self) -> Vec<&Term> {
        match &self.inner {
            LiteralInner::Atom(v) => v.terms().collect(),
            LiteralInner::Comparison(v) => v.terms(),
        }
    }

    pub fn variables(&self) -> Vec<&Variable> {
        self.terms()
            .iter()
            .filter_map(|t| {
                if let Term::Variable(v) = t {
                    Some(v)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn is_atom(&self) -> bool {
        self.inner.is_atom()
    }

    pub fn as_atom(&self) -> Option<&Atom> {
        self.inner.as_atom()
    }

    pub fn is_comparison(&self) -> bool {
        self.inner.is_comparison()
    }

    pub fn as_comparison(&self) -> Option<&Comparison> {
        self.inner.as_comparison()
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for LiteralInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Atom(v) => v.to_string(),
                Self::Comparison(v) => v.to_string(),
            }
        )
    }
}

impl From<Atom> for LiteralInner {
    fn from(v: Atom) -> Self {
        Self::Atom(v)
    }
}

impl From<Comparison> for LiteralInner {
    fn from(v: Comparison) -> Self {
        Self::Comparison(v)
    }
}

impl DbValidation for LiteralInner {
    fn validate(&self, against: &mut Database) -> Result<()> {
        match self {
            LiteralInner::Atom(a) => Atom::validate(a, against),
            LiteralInner::Comparison(_) => {
                // TODO: not sure
                Ok(())
            }
        }
    }
}

impl LiteralInner {
    pub fn is_atom(&self) -> bool {
        matches!(self, LiteralInner::Atom(_))
    }

    pub fn as_atom(&self) -> Option<&Atom> {
        match self {
            LiteralInner::Atom(a) => Some(a),
            _ => None,
        }
    }

    pub fn is_comparison(&self) -> bool {
        matches!(self, LiteralInner::Comparison(_))
    }

    pub fn as_comparison(&self) -> Option<&Comparison> {
        match self {
            LiteralInner::Comparison(c) => Some(c),
            _ => None,
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Comparison {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.operator, self.rhs,)
    }
}

impl Comparison {
    pub fn new<L: Into<Term>, R: Into<Term>>(lhs: L, operator: ComparisonOperator, rhs: R) -> Self {
        Self {
            lhs: lhs.into(),
            operator,
            rhs: rhs.into(),
        }
    }

    pub fn eq<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Self {
        Self::new(left, ComparisonOperator::Equal, right)
    }

    pub fn ne<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Self {
        Self::new(left, ComparisonOperator::NotEqual, right)
    }

    pub fn lt<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Self {
        Self::new(left, ComparisonOperator::LessThan, right)
    }

    pub fn lte<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Self {
        Self::new(left, ComparisonOperator::LessThanOrEqual, right)
    }

    pub fn gt<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Self {
        Self::new(left, ComparisonOperator::GreaterThan, right)
    }

    pub fn gte<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Self {
        Self::new(left, ComparisonOperator::GreaterThanOrEqual, right)
    }

    pub fn terms(&self) -> Vec<&Term> {
        vec![&self.lhs, &self.rhs]
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Equal => OPERATOR_ASCII_EQUAL,
                Self::NotEqual => OPERATOR_ASCII_NOT_EQUAL,
                Self::LessThan => OPERATOR_ASCII_LESS_THAN,
                Self::LessThanOrEqual => OPERATOR_ASCII_LESS_THAN_OR_EQUAL,
                Self::GreaterThan => OPERATOR_ASCII_GREATER_THAN,
                Self::GreaterThanOrEqual => OPERATOR_ASCII_GREATER_THAN_OR_EQUAL,
            }
        )
    }
}

impl FromStr for ComparisonOperator {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            OPERATOR_ASCII_EQUAL => Ok(Self::Equal),
            OPERATOR_ASCII_NOT_EQUAL
            | OPERATOR_ASCII_NOT_EQUAL_ALT
            | OPERATOR_UNICODE_NOT_EQUAL => Ok(Self::NotEqual),
            OPERATOR_ASCII_LESS_THAN => Ok(Self::LessThan),
            OPERATOR_ASCII_LESS_THAN_OR_EQUAL | OPERATOR_UNICODE_LESS_THAN_OR_EQUAL => {
                Ok(Self::LessThanOrEqual)
            }
            OPERATOR_ASCII_GREATER_THAN => Ok(Self::GreaterThan),
            OPERATOR_ASCII_GREATER_THAN_OR_EQUAL | OPERATOR_UNICODE_GREATER_THAN_OR_EQUAL => {
                Ok(Self::GreaterThanOrEqual)
            }
            _ => Err(Error::InvalidValue(
                TYPE_NAME_COMPARISON_OPERATOR.to_string(),
                s.to_string(),
            )),
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Term::Variable(v) => v.to_string(),
                Term::Constant(v) => v.to_string(),
            }
        )
    }
}

impl From<Variable> for Term {
    fn from(v: Variable) -> Self {
        Self::Variable(v)
    }
}

impl From<Constant> for Term {
    fn from(v: Constant) -> Self {
        Self::Constant(v)
    }
}

impl From<&str> for Term {
    fn from(s: &str) -> Self {
        Constant::from(s).into()
    }
}

impl From<String> for Term {
    fn from(v: String) -> Self {
        Constant::from(v).into()
    }
}

impl From<i64> for Term {
    fn from(v: i64) -> Self {
        Constant::from(v).into()
    }
}

impl From<bool> for Term {
    fn from(v: bool) -> Self {
        Constant::from(v).into()
    }
}

impl Term {
    pub fn is_variable(&self) -> bool {
        matches!(self, Term::Variable(_))
    }

    pub fn as_variable(&self) -> Option<&Variable> {
        match self {
            Term::Variable(v) => Some(v),
            _ => None,
        }
    }

    pub fn is_constant(&self) -> bool {
        matches!(self, Term::Constant(_))
    }

    pub fn as_constant(&self) -> Option<&Constant> {
        match self {
            Term::Constant(c) => Some(c),
            _ => None,
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Variable {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        if Self::is_valid(s) {
            Ok(Self(s.to_owned()))
        } else {
            Error::InvalidValue(TYPE_NAME_VARIABLE.to_owned(), s.to_owned()).into()
        }
    }
}

impl AsRef<str> for Variable {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<Variable> for String {
    fn from(v: Variable) -> Self {
        v.0
    }
}

impl Variable {
    pub fn is_valid(s: &str) -> bool {
        let mut chars = s.chars();
        !s.is_empty()
            && chars
                .next()
                .map(|c| c.is_uppercase() || c == CHAR_UNDERSCORE)
                .is_some()
            && chars.all(|c| c.is_alphanumeric() || c == CHAR_UNDERSCORE)
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
