/*!
This module provides the set of types that primarily describe the Intensional Database (IDB).

![module UML](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/book/src/model/idb.svg)

Given the following rule:

```datalog
ancestor(X, Y) :- parent(X, Y).
```

We can deduce:

1. There exists an intensional [`Relation`] with the [label](../edb/struct.Predicate.html)
   `ancestor`.
1. This relation has the following [schema](../edb/struct.Schema.html):
    1. The arity of this relation is `2`.
    1. The [types](enum.AttributeKind.html) of the [attributes](struct.Attribute.html) in this
       relation are as yet unknown.

All head atoms **must** be from an intensional relation , so if `ancestor` were to exist in the EDB
the rule above would be invalid.

**IFF** any atom in the rule body **is not** in the EDB, **then** we may use the same deduction
process above to add it to the IDB.

If we were to include an intensional relation declaration in our example, as follows:

```datalog
.infer ancestor(child: string, parent: string).

ancestor(X, Y) :- parent(X, Y).
```

We can add the [label](../edb/struct.Predicate.html) and [type](../edb/struct.AttributeKind.html)
for each attribute of `ancestor` to the relation's schema.

# Example

TBD

The following declares two intensional [relations](struct.Relation.html), `mortal` and `age`.

```datalog
.assert human(string).

.infer mortal from human.
.infer age(name: string, integer).
```

The following are valid [rules](struct.Rule.html).

```datalog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z), ancestor(Z, Y).
```
*/

use crate::edb::{AttributeIndex, Constant, Predicate, Relation};
use crate::error::{
    anonymous_variable_not_allowed, comparison_is_always_false, comparison_is_always_true,
    fact_does_not_correspond_to_schema, head_variables_missing_in_body, incompatible_types,
    invalid_head_atom_count, invalid_value, negative_variables_not_also_positive, Error, Result,
    SourceLocation,
};
use crate::features::{FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION};
use crate::syntax::{
    ANONYMOUS_TERM, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, CHAR_UNDERSCORE,
    CONJUNCTION_UNICODE_SYMBOL, DISJUNCTION_UNICODE_SYMBOL, EMPTY_STR, FALSE_UNICODE_SYMBOL,
    IMPLICATION_UNICODE_ARROW, NEGATION_UNICODE_SYMBOL, OPERATOR_EQUAL_ASCII,
    OPERATOR_GREATER_THAN_ASCII, OPERATOR_GREATER_THAN_OR_EQUAL_ASCII,
    OPERATOR_GREATER_THAN_OR_EQUAL_UNICODE, OPERATOR_LESS_THAN_ASCII,
    OPERATOR_LESS_THAN_OR_EQUAL_ASCII, OPERATOR_LESS_THAN_OR_EQUAL_UNICODE,
    OPERATOR_NOT_EQUAL_ASCII, OPERATOR_NOT_EQUAL_ASCII_ALT, OPERATOR_NOT_EQUAL_UNICODE,
    OPERATOR_STRING_MATCH_ASCII, OPERATOR_STRING_MATCH_ASCII_WORD, OPERATOR_STRING_MATCH_UNICODE,
    TYPE_NAME_COMPARISON_OPERATOR, TYPE_NAME_VARIABLE,
};
use crate::{
    AttributeName, Collection, FeatureSet, IndexedCollection, Labeled, MaybeAnonymous,
    MaybePositive, PredicateRef,
};
use paste::paste;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// Implemented by elements of the IDB that need to distinguish between values containing only
/// constants and those that contain at least one variable.
///
pub trait MaybeGround {
    ///
    /// Returns `true` if this value is ground; defined as containing only constant values, else
    /// `false`.
    ///
    fn is_ground(&self) -> bool;
}

///
/// This is the set of rules that comprise part of the intensional database along with an instance
/// of [Relations](../edb/struct.Relations.html).
///
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RuleSet(HashSet<Rule>);

///
/// An individual rule consists of a set of head [atoms](Atom) and a set of body [literals](Literal).
///
/// The head is a set of atoms, with the cardinality having meaning. In the default  
/// language $\small\text{Datalog}$ there may only be one head atom; in $\small\text{Datalog}^{\Leftarrow}$
/// the head atom is optional, and in $\small\text{Datalog}^{\lor}$ there may be more than one head
/// atom.
///
/// Safety...
///  
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Rule {
    head: Vec<Atom>,
    body: Vec<Literal>,
}

///
/// This enumeration represents the three forms a rule can take. This is defined by the predicate
/// $\small form(r)$ in the description of [rules](../index.html#rules), formula (vi).
///
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RuleForm {
    ///
    /// A pure rule is one where there is only a single atom in the head, regardless of the body and
    /// is the only form allowed in the strict language $\small\text{Datalog}$.
    /// $$\small |head\(r\)| = 1$$
    ///
    Pure,
    ///
    /// A constraint rule, or contradiction, does not allow any consequence to be determined
    /// from evaluation of its body, it is only available in the language $\small\text{Datalog}^{\lnot}$.
    /// $$\small |head\(r\)| = 0 \land \text{Datalog}^{\lnot}$$
    ///
    Constraint,
    ///
    /// A disjunctive rule is one where there is more than one atom, and any one (inclusive
    /// disjunction) may be true if the body is true, it is only available in the language
    /// $\small\text{Datalog}^{\lor}$.
    /// $$\small |head\(r\)| > 1  \land \text{Datalog}^{\lor}$$
    ///
    Disjunctive,
}

// ------------------------------------------------------------------------------------------------

///
/// An Atom comprises an ordered list of [`Term`] values within a [`Relation`]. The arity and
/// types for the values **must** comply with the relation's schema. The label of a fact **must**
/// be the same as the label of it's relation.
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Atom {
    label: PredicateRef,
    terms: Vec<Term>,
    src_loc: Option<SourceLocation>,
}

///
/// A literal is either an [relational literal](Atom) or if using the language $\small\text{Datalog}^{\theta}$
/// an [arithmetic literal](Comparison). Additionally, if using the language $\small\text{Datalog}^{\lnot}$
/// a literal may be negated.
///
/// # Correspondence Relation ($\global\def\correq{\small\overset{\raisebox{-0.75em}{$\tiny\frown$}}{=}}\correq$)
///
/// To perform matching of literals it is important to be able to describe not only the equality
/// between their terms, but a correspondence between literals that defines whether they match.
/// We use the $\small \correq$ symbol (Unicode ≘, _corresponds to_, `\u{e28998}`) to denote such a
/// relationship. For example, the relational literal $\small p\(X\)$ is not equal to
/// $\small p\(Y\)$, however they may correspond in terms of how they match against existing facts;
/// as such we can assert that $\small p\(X\) \correq p\(Y\)$.
///
/// Correspondence between two relational literals asserts that both share the same predicate, they
/// have the same arity, and that their terms also correspond.
///
/// $$\tag{i}\small l \correq r \coloneqq
///   label\(l\) = label\(r\) \land
///   arity\(l\) = arity\(r\) \land
///   \forall{i} \in \lbrace 1, \ldots, arity\(l\)\rbrace \medspace \( t_{l_i}  \correq t_{r_i} \)$$
///
/// $$\tag{ii}\small t_l \correq t_r \coloneqq
/// \begin{cases}
///   t_l = t_r, &\text{if } constant\(t_l\) \land constant\(t_r\) \\\\
///   true, &\text{if } variable\(t_l\) \land variable\(t_r\) \\\\
///   false, &\text{otherwise}
/// \end{cases}$$
///
/// Given the definitions i and ii, it should be clear that:
///
/// $$\small
/// \begin{alignat*}{3}
/// & p\(12\) = p\(12\)         \quad && \land   \quad && p\(12\) \correq p\(12\)      \\\\
/// & p\(12\) \not = p\(21\)    \quad && \land   \quad && p\(12\) \not \correq p\(21\) \\\\
/// & p\(12\) \not = p\(Y\)     \quad && \land   \quad && p\(12\) \not \correq p\(Y\)  \\\\
/// & p\(X\) \not = p\(Y\)      \quad && \land   \quad && p\(X\) \correq p\(Y\)        \\\\
/// & p\(X\) = p\(X\)           \quad && \land   \quad && p\(X\) \correq p\(X\)
/// \end{alignat*}$$
///
/// Correspondence between two arithmetic literals is defined in a similar manner, with the addition
/// that the operators, $\small\theta$ must be the same.
///
/// $$\tag{iii}\small l \correq r \coloneqq
///   \theta_{l} = \theta_{r} \land
///   t_{l} \correq t_{r} $$
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Literal {
    negative: bool,
    inner: LiteralInner,
}

///
/// Contains the actual content of a [`Literal`].
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralInner {
    /// A relational literal
    Relational(Atom),
    /// An arithmetic literal, if using the language $\small\text{Datalog}^{\theta}$.
    Arithmetic(Comparison),
}

// ------------------------------------------------------------------------------------------------

///
/// This describes an arithmetic literal, i.e. a comparison operation between to [`Term`]s.
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Comparison {
    lhs: Term,
    operator: ComparisonOperator,
    rhs: Term,
}

///
/// The supported set of operations within an arithmetic literal.
///
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ComparisonOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    StringMatch,
}

// ------------------------------------------------------------------------------------------------

///
/// A term, $\small t \in \mathcal{T}$, is either a [`Variable`]  or a [`Constant`]  value.
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Term {
    /// denoted with the character `_` in a literal.
    Anonymous,
    /// a value such that term is $\small \in \mathcal{V}$
    Variable(VariableRef),

    /// a value such that term is $\small \in \mathcal{C}$
    Constant(Constant),
}

///
/// A value from the set $\small \mathcal{V}$.
///
/// A variable must start with a Unicode **upper** case letter, followed by any Unicode cased letter or
/// Unicode decimal digit, or the `'_'` underscore character.
///
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable(String);

///
/// We use a reference-counted type for predicates to try and reduce the number of instances of a
/// commonly used value.
///
// The type [PredicateSet](../struct.PredicateSet.html), and the program's
// shared set via [predicates](../struct.PredicateSet.html#method.predicates) allows for common
// memory usage within a program.
//
pub type VariableRef = Rc<Variable>;

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

impl Display for RuleSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for rule in self.iter() {
            writeln!(f, "{}", rule)?;
        }
        Ok(())
    }
}

impl Collection<Rule> for RuleSet {
    delegate!(is_empty -> bool);

    delegate!(len -> usize);

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Rule> + '_> {
        Box::new(self.0.iter())
    }

    fn contains(&self, value: &Rule) -> bool {
        self.0.contains(value)
    }
}

impl RuleSet {
    pub fn add(&mut self, rule: Rule) -> bool {
        self.0.insert(rule)
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            if self.head.is_empty() {
                FALSE_UNICODE_SYMBOL.to_string()
            } else if self.head.len() == 1 {
                self.head.get(0).unwrap().to_string()
            } else {
                self.head
                    .iter()
                    .map(|atom| atom.to_string())
                    .collect::<Vec<String>>()
                    .join(&format!(" {} ", DISJUNCTION_UNICODE_SYMBOL))
            },
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

impl MaybeGround for Rule {
    fn is_ground(&self) -> bool {
        self.head().all(|atom| atom.is_ground()) && self.literals().all(|lit| lit.is_ground())
    }
}

impl MaybePositive for Rule {
    fn is_positive(&self) -> bool {
        self.body.iter().all(Literal::is_positive)
    }
}

impl Rule {
    pub fn new<A: Into<Vec<Atom>>, B: Into<Vec<Literal>>>(head: A, body: B) -> Self {
        let body = body.into();
        assert!(!body.is_empty());
        Self {
            head: head.into(),
            body,
        }
    }

    pub fn new_pure<B: Into<Vec<Literal>>>(head: Atom, body: B) -> Self {
        Self::new(vec![head], body)
    }

    pub fn new_constraint<B: Into<Vec<Literal>>>(body: B) -> Self {
        Self::new(Vec::default(), body)
    }

    pub fn new_disjunctive<A: Into<Vec<Atom>>, B: Into<Vec<Literal>>>(head: A, body: B) -> Self {
        let head = head.into();
        assert!(head.len() > 1);
        Self::new(head, body)
    }

    // --------------------------------------------------------------------------------------------

    pub fn form(&self) -> RuleForm {
        match self.head.len() {
            0 => RuleForm::Constraint,
            1 => RuleForm::Pure,
            _ => RuleForm::Disjunctive,
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn head(&self) -> impl Iterator<Item = &Atom> {
        self.head.iter()
    }

    // --------------------------------------------------------------------------------------------

    pub fn has_body(&self) -> bool {
        !self.body.is_empty()
    }

    pub fn add<L: Into<Literal>>(&mut self, literal: L) -> &mut Self {
        self.body.push(literal.into());
        self
    }

    pub fn extend<V: Into<Vec<Literal>>>(&mut self, literals: V) -> &mut Self {
        self.body.append(&mut literals.into());
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
        self.head().map(|atom| atom.iter()).flatten().collect()
    }

    pub fn distinguished_terms_in_order(&self) -> Vec<&Term> {
        let terms: Vec<&Term> = self.head().map(|atom| atom.iter()).flatten().collect();
        dedup_in_place(terms)
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
            .union(&self.distinguished_terms())
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

    pub fn head_variables(&self) -> HashSet<&VariableRef> {
        self.head().map(|atom| atom.variables()).flatten().collect()
    }

    pub fn variables(&self) -> HashSet<&VariableRef> {
        self.body
            .iter()
            .map(|lit| lit.variables())
            .flatten()
            .collect()
    }

    pub fn positive_variables(&self) -> HashSet<&VariableRef> {
        self.body
            .iter()
            .filter(|lit| lit.is_positive())
            .map(|lit| lit.variables())
            .flatten()
            .collect()
    }

    pub fn negative_variables(&self) -> HashSet<&VariableRef> {
        self.body
            .iter()
            .filter(|lit| !lit.is_positive())
            .map(|lit| lit.variables())
            .flatten()
            .collect()
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// Guarded $\small\text{Datalog}$ is defined where for every rule, all the variables that occur
    /// in the rule bodies must occur together in at least one atom, called a guard atom.
    ///
    pub fn is_guarded(&self) -> bool {
        let all_variables = self.variables();
        self.literals().any(|lit| {
            let lit_variables: HashSet<&VariableRef> =
                HashSet::from_iter(lit.variables().into_iter());
            lit_variables == all_variables
        })
    }

    ///
    /// Frontier-Guarded $\small\text{Datalog}$ is defined where for every rule, all the variables
    /// that are shared between the rule body and the rule head (called the frontier variables) must
    /// all occur together in a guard atom.
    ///
    pub fn is_frontier_guarded(&self) -> bool {
        let frontier_variables: HashSet<&VariableRef> = self
            .head_variables()
            .intersection(&self.variables())
            .copied()
            .collect();
        self.literals().any(|lit| {
            let lit_variables: HashSet<&VariableRef> =
                HashSet::from_iter(lit.variables().into_iter());
            lit_variables == frontier_variables
        })
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// # Rules Safety
    ///
    /// A Datalog program is well‐formed if all of its rules are well‐formed.
    ///
    /// A rule is well‐formed iff:
    ///
    /// 1. It complies with any restrictions implied by the passed set of features.
    /// 2. It meets the rules for safety, define as:
    ///    1. all variables that appear in the head also appear in a positive literal in
    ///       the body of the clause, and
    ///    2. all variables that appear in a negative literal in the body of a clause also appears
    ///       in some positive literal in the body of the clause.
    ///
    /// # Example unsafe rules
    ///
    /// ```datalog
    /// [1]  invalid(X, Y) :- movie(X, Y, "1940"), Y > "1910".
    /// [2]  invalid(X)    :- movie(X, Y, "1940"), NOT cast(U, X).
    /// ```
    ///
    pub fn safety_check(&self, features: &FeatureSet) -> Result<()> {
        let (min, max) = if features.supports(&FEATURE_DISJUNCTION) {
            (1, usize::MAX)
        } else if features.supports(&FEATURE_CONSTRAINTS) {
            (0, usize::MAX)
        } else {
            (1, 1)
        };
        let head_len = self.head.len();
        if head_len < min || head_len > max {
            println!("HC: {:#?}", self);
            return Err(invalid_head_atom_count(
                head_len,
                min,
                max,
                match self.head.get(0) {
                    None => None,
                    Some(atom) => atom.source_location().cloned(),
                },
            ));
        }

        let body_positive_terms = self.positive_terms();

        for atom in self.head() {
            let missing: Vec<&Term> = atom
                .iter()
                .into_iter()
                .filter(|term| {
                    if term.is_variable() {
                        !body_positive_terms.contains(term)
                    } else {
                        false
                    }
                })
                .collect();
            if !missing.is_empty() {
                return Err(head_variables_missing_in_body(
                    atom.label_ref(),
                    missing
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>(),
                    atom.source_location().cloned(),
                ));
            }

            let missing: Vec<&Term> = self
                .negative_terms()
                .into_iter()
                .filter(|term| {
                    if term.is_variable() {
                        !body_positive_terms.contains(term)
                    } else {
                        false
                    }
                })
                .collect();
            if !missing.is_empty() {
                return Err(negative_variables_not_also_positive(
                    atom.label_ref(),
                    missing
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>(),
                    atom.source_location().cloned(),
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
            self.label,
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

impl Labeled for Atom {
    get!(label -> Predicate);

    get_cloned!(label_ref, label -> PredicateRef);
}

impl MaybeGround for Atom {
    fn is_ground(&self) -> bool {
        self.terms.iter().all(Term::is_constant)
    }
}

impl Collection<Term> for Atom {
    delegate!(is_empty, terms -> bool);

    delegate!(len, terms -> usize);

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Term> + '_> {
        Box::new(self.terms.iter())
    }

    fn contains(&self, value: &Term) -> bool {
        self.terms.contains(value)
    }
}

impl Atom {
    pub fn new<T: Into<Vec<Term>>>(label: PredicateRef, terms: T) -> Self {
        let terms = terms.into();
        assert!(!terms.is_empty());
        Self {
            label,
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
                if c.kind()
                    != schema
                        .get(&AttributeIndex::Index(i))
                        .unwrap()
                        .kind()
                        .unwrap()
                {
                    return Err(fact_does_not_correspond_to_schema(
                        relation.label_ref(),
                        terms
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<String>>()
                            .join(", "),
                    ));
                }
            }
        }
        Ok(Self {
            label: relation.label_ref(),
            terms,
            src_loc: None,
        })
    }

    pub fn new_at_location(label: PredicateRef, terms: &[Term], location: SourceLocation) -> Self {
        let terms: Vec<Term> = terms.into();
        assert!(!terms.is_empty());
        Self {
            label,
            terms,
            src_loc: Some(location),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn add<V: Into<Term>>(&mut self, argument: V) -> &mut Self {
        self.terms.push(argument.into());
        self
    }

    pub fn extend<T: Into<Vec<Term>>>(&mut self, arguments: T) -> &mut Self {
        self.terms.append(&mut arguments.into());
        self
    }

    // --------------------------------------------------------------------------------------------

    pub fn variables(&self) -> impl Iterator<Item = &VariableRef> {
        self.terms.iter().filter_map(|t| {
            if let Term::Variable(v) = t {
                Some(v)
            } else {
                None
            }
        })
    }

    pub fn variable_index(&self, variable: &VariableRef) -> Option<usize> {
        self.terms
            .iter()
            .enumerate()
            .filter_map(|(i, term)| term.as_variable().map(|v| (i, v)))
            .find(|(_, var)| var == &variable)
            .map(|(i, _)| i)
    }

    pub fn constants(&self) -> impl Iterator<Item = &Constant> {
        self.terms.iter().filter_map(|t| {
            if let Term::Constant(v) = t {
                Some(v)
            } else {
                None
            }
        })
    }

    pub fn source_location(&self) -> Option<&SourceLocation> {
        self.src_loc.as_ref()
    }

    pub fn is_existential(&self) -> bool {
        self.constants().count() == self.len()
    }

    pub fn is_universal(&self) -> bool {
        self.variables().count() == self.len()
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            if self.negative {
                NEGATION_UNICODE_SYMBOL
            } else {
                EMPTY_STR
            },
            self.inner,
        )
    }
}

impl From<Atom> for Literal {
    fn from(v: Atom) -> Self {
        Self::relational(v)
    }
}

impl From<Comparison> for Literal {
    fn from(v: Comparison) -> Self {
        Self::arithmetic(v)
    }
}

impl MaybeGround for Literal {
    fn is_ground(&self) -> bool {
        self.inner.is_ground()
    }
}

impl MaybePositive for Literal {
    fn is_positive(&self) -> bool {
        !self.negative
    }
}

impl AsRef<LiteralInner> for Literal {
    fn as_ref(&self) -> &LiteralInner {
        &self.inner
    }
}

impl Literal {
    pub fn new(negative: bool, inner: LiteralInner) -> Self {
        Self { negative, inner }
    }

    pub fn relational(atom: Atom) -> Self {
        Self::new(false, atom.into())
    }

    pub fn negative_relational(atom: Atom) -> Self {
        Self::new(true, atom.into())
    }

    pub fn arithmetic(comparison: Comparison) -> Self {
        Self::new(false, comparison.into())
    }

    pub fn negative_arithmetic(comparison: Comparison) -> Self {
        Self::new(true, comparison.into())
    }

    // --------------------------------------------------------------------------------------------

    delegate!(pub is_relational, inner -> bool);

    delegate!(pub as_relational, inner -> Option<&Atom>);

    delegate!(pub is_arithmetic, inner -> bool);

    delegate!(pub as_arithmetic, inner -> Option<&Comparison>);

    // --------------------------------------------------------------------------------------------

    pub fn terms(&self) -> Vec<&Term> {
        match &self.inner {
            LiteralInner::Relational(v) => v.iter().collect(),
            LiteralInner::Arithmetic(v) => v.terms(),
        }
    }

    pub fn variables(&self) -> Vec<&VariableRef> {
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

    into_inner_fn!(LiteralInner, inner);
}

// ------------------------------------------------------------------------------------------------

impl Display for LiteralInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Relational(v) => v.to_string(),
                Self::Arithmetic(v) => v.to_string(),
            }
        )
    }
}

impl_enum_from!(LiteralInner, Atom, Relational);

impl_enum_from!(LiteralInner, Comparison, Arithmetic);

impl MaybeGround for LiteralInner {
    fn is_ground(&self) -> bool {
        match self {
            LiteralInner::Relational(a) => a.is_ground(),
            LiteralInner::Arithmetic(c) => c.is_ground(),
        }
    }
}

impl LiteralInner {
    self_is_as!(relational, Relational, Atom);

    self_is_as!(arithmetic, Arithmetic, Comparison);
}

// ------------------------------------------------------------------------------------------------

impl Display for Comparison {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.operator, self.rhs,)
    }
}

impl MaybeGround for Comparison {
    fn is_ground(&self) -> bool {
        self.lhs.is_constant() && self.rhs.is_constant()
    }
}

impl Comparison {
    pub fn new<L: Into<Term>, R: Into<Term>>(
        lhs: L,
        operator: ComparisonOperator,
        rhs: R,
    ) -> Result<Self> {
        let new_self = Self::new_unchecked(lhs, operator, rhs);
        new_self.sanity_check()?;
        Ok(new_self)
    }

    pub fn new_unchecked<L: Into<Term>, R: Into<Term>>(
        lhs: L,
        operator: ComparisonOperator,
        rhs: R,
    ) -> Self {
        Self {
            lhs: lhs.into(),
            operator,
            rhs: rhs.into(),
        }
    }

    pub fn sanity_check(&self) -> Result<()> {
        match (
            &self.lhs,
            self.operator == ComparisonOperator::Equal,
            &self.rhs,
        ) {
            (Term::Anonymous, _, _) | (_, _, Term::Anonymous) => {
                Err(anonymous_variable_not_allowed())
            }
            (Term::Constant(lhs), true, Term::Constant(rhs)) => {
                if lhs.kind() != rhs.kind() {
                    Err(incompatible_types(
                        lhs.kind().to_string(),
                        rhs.kind().to_string(),
                    ))
                } else if lhs == rhs {
                    Err(comparison_is_always_true(self.to_string()))
                } else {
                    Err(comparison_is_always_false(self.to_string()))
                }
            }
            (Term::Constant(lhs), false, Term::Constant(rhs)) => {
                if lhs.kind() != rhs.kind() {
                    Err(incompatible_types(
                        lhs.kind().to_string(),
                        rhs.kind().to_string(),
                    ))
                } else if lhs == rhs {
                    Err(comparison_is_always_false(self.to_string()))
                } else {
                    Err(comparison_is_always_true(self.to_string()))
                }
            }
            (Term::Variable(lhs), true, Term::Variable(rhs)) => {
                if lhs == rhs {
                    Err(comparison_is_always_true(self.to_string()))
                } else {
                    Ok(())
                }
            }
            (Term::Variable(lhs), false, Term::Variable(rhs)) => {
                if lhs == rhs {
                    Err(comparison_is_always_false(self.to_string()))
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }

    pub fn eq<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Result<Self> {
        Self::new(left, ComparisonOperator::Equal, right)
    }

    pub fn ne<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Result<Self> {
        Self::new(left, ComparisonOperator::NotEqual, right)
    }

    pub fn lt<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Result<Self> {
        Self::new(left, ComparisonOperator::LessThan, right)
    }

    pub fn lte<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Result<Self> {
        Self::new(left, ComparisonOperator::LessThanOrEqual, right)
    }

    pub fn gt<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Result<Self> {
        Self::new(left, ComparisonOperator::GreaterThan, right)
    }

    pub fn gte<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Result<Self> {
        Self::new(left, ComparisonOperator::GreaterThanOrEqual, right)
    }

    // --------------------------------------------------------------------------------------------

    get!(pub lhs -> Term);

    get!(pub rhs -> Term);

    get!(pub operator -> ComparisonOperator);

    // --------------------------------------------------------------------------------------------

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
                Self::Equal => OPERATOR_EQUAL_ASCII,
                Self::NotEqual => OPERATOR_NOT_EQUAL_ASCII,
                Self::LessThan => OPERATOR_LESS_THAN_ASCII,
                Self::LessThanOrEqual => OPERATOR_LESS_THAN_OR_EQUAL_ASCII,
                Self::GreaterThan => OPERATOR_GREATER_THAN_ASCII,
                Self::GreaterThanOrEqual => OPERATOR_GREATER_THAN_OR_EQUAL_ASCII,
                Self::StringMatch => OPERATOR_STRING_MATCH_ASCII,
            }
        )
    }
}

impl FromStr for ComparisonOperator {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            OPERATOR_EQUAL_ASCII => Ok(Self::Equal),
            OPERATOR_NOT_EQUAL_ASCII
            | OPERATOR_NOT_EQUAL_ASCII_ALT
            | OPERATOR_NOT_EQUAL_UNICODE => Ok(Self::NotEqual),
            OPERATOR_LESS_THAN_ASCII => Ok(Self::LessThan),
            OPERATOR_LESS_THAN_OR_EQUAL_ASCII | OPERATOR_LESS_THAN_OR_EQUAL_UNICODE => {
                Ok(Self::LessThanOrEqual)
            }
            OPERATOR_GREATER_THAN_ASCII => Ok(Self::GreaterThan),
            OPERATOR_GREATER_THAN_OR_EQUAL_ASCII | OPERATOR_GREATER_THAN_OR_EQUAL_UNICODE => {
                Ok(Self::GreaterThanOrEqual)
            }
            OPERATOR_STRING_MATCH_ASCII
            | OPERATOR_STRING_MATCH_UNICODE
            | OPERATOR_STRING_MATCH_ASCII_WORD => Ok(Self::StringMatch),
            _ => Err(invalid_value(TYPE_NAME_COMPARISON_OPERATOR, s)),
        }
    }
}

impl ComparisonOperator {
    pub fn inverse(&self) -> Self {
        match self {
            Self::Equal => Self::Equal,
            Self::NotEqual => Self::NotEqual,
            Self::LessThan => Self::GreaterThanOrEqual,
            Self::LessThanOrEqual => Self::GreaterThan,
            Self::GreaterThan => Self::LessThanOrEqual,
            Self::GreaterThanOrEqual => Self::LessThan,
            Self::StringMatch => Self::StringMatch,
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
                Term::Anonymous => ANONYMOUS_TERM.to_string(),
                Term::Variable(v) => v.to_string(),
                Term::Constant(v) => v.to_string(),
            }
        )
    }
}

impl_enum_from!(Term, VariableRef, Variable);

impl_enum_from!(Term, Constant, Constant);

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

impl MaybeAnonymous for Term {
    fn anonymous() -> Self {
        Self::Anonymous
    }

    fn is_anonymous(&self) -> bool {
        matches!(self, Self::Anonymous)
    }
}

impl Term {
    self_is_as!(variable, Variable, VariableRef);

    self_is_as!(constant, Constant);
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
            invalid_value(TYPE_NAME_VARIABLE, s).into()
        }
    }
}

impl AsRef<str> for Variable {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AttributeName for Variable {
    fn is_valid(s: &str) -> bool {
        let mut chars = s.chars();
        s != ANONYMOUS_TERM
            && !s.is_empty()
            && chars.next().map(|c| c.is_uppercase()).unwrap()
            && chars.all(|c| c.is_alphanumeric() || c == CHAR_UNDERSCORE)
    }

    fn type_name() -> &'static str {
        TYPE_NAME_VARIABLE
    }
}

impl Variable {
    into_inner_fn!(pub String);
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

//
// TODO: (ISSUE/rust-asdi/6) make this better, it's horrible.
//
fn dedup_in_place<T: Clone + Eq + Hash, V: Into<Vec<T>>>(v: V) -> Vec<T> {
    let v = v.into();
    if v.len() <= 1 {
        v
    } else {
        let mut vs: HashSet<T> = Default::default();
        let mut v2: Vec<T> = Default::default();
        for a in v.into_iter() {
            if vs.insert(a.clone()) {
                v2.push(a)
            }
        }
        v2
    }
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

pub mod eval;

pub mod query;
