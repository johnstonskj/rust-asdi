/*!
This module provides the set of types that primarily describe the Intensional Database (IDB).

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
@infer ancestor(child: string, parent: string).

ancestor(X, Y) :- parent(X, Y).
```

We can add the [label](../edb/struct.Predicate.html) and [type](../edb/struct.AttributeKind.html)
for each attribute of `ancestor` to the relation's schema.

# Example

TBD

The following declares two intensional [relations](struct.Relation.html), `mortal` and `age`.

```datalog
@assert human(string).

@infer mortal from human.
@infer age(name: string, integer).
```

The following are valid [rules](struct.Rule.html).

```datalog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z), ancestor(Z, Y).
```

# Datalog and Relational Algebra

Every expression in the _basic_ relational algebra can be expressed as a $\small\text{Datalog}$
query. However, operations in the extended relational algebra (grouping, aggregation, and sorting)
have no corresponding capability in $\small\text{Datalog}$. Similarly, $\small\text{Datalog}$ can
express recursion, which the relational algebra cannot. The following describes the foundational
relational operations and their $\small\text{Datalog}$ equivalent.

The semantics of $\small\text{Datalog}$ relations are primarily set based we would expect common
set operations to work in $\small\text{Datalog}$ rules. Given the relations R, $\small r(a,b,c)$,
and S, $\small s(d,e,f)$, we can easily describe the common set operations.

## Union ($\small\cup$)

Union is a binary operator that is written as $\small R \cup S$, where
$\small R$ and $\small S$ are relations.

For set union and set difference, the two relations involved must be **union-compatible** -—
that is, the two relations must have the same set of attributes. A strict form of this rule does not
allow for same-named attributes whereas a looser form allows duplicate names IFF they have identical
types. Because set intersection is defined in terms of set union and set difference, the two
relations involved in set intersection must also be union-compatible.

Consider the expression $\small U \coloneqq R \cup S$, which can be expressed as follows:

```datalog
u(X, Y, Z) :- r(X, Y, Z).
u(X, Y, Z) :- s(X, Y, Z).
```

## Difference ($\small\setminus$)

Difference is a binary operator that is written as $\small R \setminus S$,
where $\small R$ and $\small S$ are relations. This cannot be
implemented in $\small\text{Datalog}$ as it required negation.

Consider the expression $\small D \coloneqq R \setminus S$ which can be expressed in
$\small\text{Datalog}^{\lnot}$ as follows:

```datalog
d(X,Y,Z) :- r(X,Y,Z) AND NOT s(X,Y,Z).
```

## Intersection ($\small\cap$)

Intersection is a binary operator that is written as $\small R \cap S$, where
$\small R$ and $\small S$ are relations.

Consider the expression $\small I \coloneqq R \cap S$, which can be expressed as follows:

```datalog
i(X,Y,Z) :- r(X,Y,Z) AND s(X,Y,Z).
```

Intersection may be defined in terms of the difference operation, as below.

$$\small R \cap S \enspace\equiv\medspace R \setminus (R \setminus S)$$

## Cartesian Product ($\small\times$)

Cartesian Product is a binary operator that is written as $\small R \times S$,
where $\small R$ and $\small S$ are relations. In relational algebra it is required that the two
relations involved must have disjoint headers—that is, they must not have a common attribute name.

The following is a valid expression of $\small P \coloneqq R \times S$:

```datalog
p(A, B, C, D, E, F) :- r(A, B, C) AND s(D, E, F).
```

## Projection ($\small\Pi$)

Projection is a unary operation written as
$\small\Pi_{a_{1},\ldots ,a_{n}}(R)$ where $a_{1},\ldots ,a_{n}$ is a set of attribute names. The
result of such projection is defined as the set that is obtained when all tuples in $R$ are
restricted to the set $\small\lbrace a_{1},\ldots ,a_{n}\rbrace$. In some literature the lower case
$\small\pi$ is used instead of $\small\Pi$.

For example, the projection $\small P \coloneqq \Pi_{X} \(R\)$, a projection of the first attribute in R only,
can be expressed in $\small\text{Datalog}$ as either of the following equivalent rules.

```datalog
p(X) :- r(X, Y, Z).
p(X) :- r(X, _, _).
```

## Generalized Selection ($\small\sigma$)

As defined by Codd, selection is written as $\small \sigma_{a\theta b}(R)$ or
$\small \sigma_{a\theta v}(R)$ where:

* $\small a$ and $\small b$ are attribute names,
* $\small\theta$ is a binary operation in the set $\small\lbrace =, \neq, <, \leq, >, \geq \rbrace$,
* $\small v$ is a constant value,
* $\small R$ is a relation,

The selection $\small\sigma_{a\theta b}(R)$ denotes all tuples in $\small R$ for which $\small\theta$
holds between the $\small a$ and the $\small b$ attribute.

The selection $\small\sigma_{a\theta v}(R)$ denotes all tuples in $\small R$ for which $\small\theta$
holds between the $\small a$ attribute and the value $\small v$.

Selection requires arithmetic literals and therefore the languages $\small\text{Datalog}^{\theta}$
and $\small\text{Datalog}^{\lnot}$ for negation.

**Generalized Selection** is a unary operation written as $\small\sigma_{\varphi}(R)$ where
$\small\varphi$ is a propositional formula that consists of conditions as allowed in the normal selection
and the logical operators $\small\land$ (and), $\small\lor$ (or) and $\small\lnot$ (negation). This selection
selects all those tuples in $\small R$ for which $\small\varphi$ holds.

In $\small\text{Datalog}^{\theta}$ the selection $\small L \coloneqq \sigma_{X>100 \land Y=‘something’} \(R\)$
can be expressed as follows, where both rules are equivalent.

```datalog
l(X, Y, Z) :- r(X, Y, Z) AND X > 100 AND Y = something.
l(X, Y, Z) :- r(X, Y, something) AND X > 100.
```

## Rename ($\small\rho$)

Rename is a unary operation written as $\small\rho_{a/b}\(R\)$ where the result is identical to $\small R$
except that the $\small b$ attribute in all tuples is renamed to an $\small a$ attribute. This is
simply used to rename the attribute of a relation or the relation itself. There is also the
$\small\rho_{x(A_{1},\ldots, A_{n})}\(R\)$ notation, where $\small R$ is renamed to $\small x$ and the
attributes $\small\lbrace a_{1},\ldots, a_{n}\rbrace$ are renamed to $\small\lbrace A_{1}, \ldots, A_{n}\rbrace$.

TBD

## Theta Join ($\small\Join_{\theta}$)

Theta Join is a binary operator that is written as $\small{R \Join S} \atop {a \theta b}$ and
$\small{R \Join S} \atop {a \theta v}$ or alternatively as $\small R \Join_{a \theta b} S$ and
$\small R \Join_{a \theta v} S$, where $\small R$ and $\small S$ are relations,
and the expressions $\small a\theta b$ and $\small a\theta v$ should be interpreted in the same way
as for selection.

A **generalized Theta Join** can be described following the convention of generalized selection where
$\small\theta$ expands into a propositional formula in the same manner as $\small\varphi$. Unfortunately
there is no notational alignment where a generalized theta join might be signified as
$\small\Join_{\varphi}$

For example, the natural join $\small J \coloneqq R \Join_{S.X>100} S$, a join conditional join on an attribute
in S can be expressed in $\small\text{Datalog}^{\theta}$ as follows.

```datalog
j(X,Y,Z) :- r(X,Y,Z) AND s(Xs,Y,Z) AND Xs > 100.
```

It is possible to dispense with the theta join in most cases as it can be expressed in terms of
the selection and cartesian product operators described above.

$$\small R \Join_{\theta} \enspace\equiv\enspace \sigma_{\theta}\(R \times S\)$$

An **Equi-Join** is a special case of the Theta Join where $\small\theta$ contains only equalities.

## Natural Join ($\small\Join$)

Natural Join is a binary operator that is written as $\small R \Join S$, where $\small R$ and
$\small S$ are relations. The result of the natural join is the set of all combinations of tuples
in $\small R$ and $\small S$ that are equal on their common attribute names. The natural join is
arguably one of the most important operators since it is the relational counterpart of the logical
AND operator.

For example, the natural join $\small J = R \Join S$, a join on the first two attributes of R and S,
can be expressed in $\small\text{Datalog}$ as follows. Note that the common attributes are defined
by the names of their variables, not the underlying names of the attribute schema.

```datalog
j(X,Y,Z,Q) :- r(X,Y,Z) AND s(X,Y,Q).
```

A Natural Join is a special case of the Equi-Join where equality operations are performed on all
common attributes.

## Complex Expressions

More complex examples can the be made from combining relational operators. The relational
query $\small A \coloneqq \Pi_{X}\(\sigma_{Y = 3} \(R\) \)$ becomes

```datalog
a(X) :- r(X, 3, _).
```

Similarly, the relational query $\small A \coloneqq \Pi_{X}\(\sigma_{Y = 3} \(R\) \Join_{R.X=S.X}\sigma_{Y = 5} \(S\)\)$
becomes

```datalog
a(X) :- r(X, 3, _) AND s(X, 5, _).
```

## Recursion

Although relational algebra seems powerful enough for most practical purposes, there are some
simple and natural operators on relations that cannot be expressed by relational algebra.
One of them is the transitive closure of a binary relation. Given a domain $\small D$, let binary
relation $\small R$, $\small \text{Datalog } r(x, y)$, be a subset of $\small D\times D$. The transitive closure $\small R^{+}$ of $R$ is the smallest
subset of $\small D\times D$ that contains $R$ and satisfies the following condition:

$$\small\forall x\forall y\forall z\left((x,y)\in R^{+}\land (y,z)\in R^{+}\Rightarrow (x,z)\in R^{+}\right)$$

In Datalog this form of recursion is naturally expressed, as follows.

```datalog
r_plus(X, Z) :- r(X, Y), r_plus(Y, Z).
```

_Acknowledgements_. Examples in this section are taken from [_Introduction to Data Management CSE 344, Lecture 10:
Datalog_](https://courses.cs.washington.edu/courses/cse344/12au/lectures/lecture10-datalog.pdf),
Magda Balazinska, Fall 2012. Definitions for the relational algebra are taken from
[_Relational algebra_](https://en.wikipedia.org/wiki/Relational_algebra), Wikipedia, fetched
January 2022. The [Relational Algebra Query Converter](http://www.grammaticalframework.org/qconv/qconv-a.html)
is an interesting way to take a SQL query and convert to relational algebra which can then be
converted to Datalog using the examples above.
*/

use crate::edb::{AttributeIndex, Constant, Predicate, Relation};
use crate::error::{
    fact_does_not_correspond_to_schema, head_variables_missing_in_body, invalid_head_atom_count,
    invalid_value, negative_variables_not_also_positive, Error, Result, SourceLocation,
};
use crate::features::{FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION};
use crate::syntax::{
    ANONYMOUS_TERM, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, CHAR_UNDERSCORE,
    CONJUNCTION_UNICODE_SYMBOL, DISJUNCTION_UNICODE_SYMBOL, EMPTY_STR, FALSE_UNICODE_SYMBOL,
    IMPLICATION_UNICODE_ARROW, NOT_UNICODE_SYMBOL, OPERATOR_ASCII_EQUAL,
    OPERATOR_ASCII_GREATER_THAN, OPERATOR_ASCII_GREATER_THAN_OR_EQUAL, OPERATOR_ASCII_LESS_THAN,
    OPERATOR_ASCII_LESS_THAN_OR_EQUAL, OPERATOR_ASCII_NOT_EQUAL, OPERATOR_ASCII_NOT_EQUAL_ALT,
    OPERATOR_UNICODE_GREATER_THAN_OR_EQUAL, OPERATOR_UNICODE_LESS_THAN_OR_EQUAL,
    OPERATOR_UNICODE_NOT_EQUAL, TYPE_NAME_COMPARISON_OPERATOR, TYPE_NAME_VARIABLE,
};
use crate::{
    AttributeName, Collection, FeatureSet, IndexedCollection, Labeled, MaybeAnonymous, MaybeGround,
    MaybePositive, PredicateRef, SyntaxFragments,
};
use paste::paste;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// This is the set of rules that comprise part of the intensional database along with an instance
/// of [Relations](../edb/struct.Relations.html).
///
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Rules(HashSet<Rule>);

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RuleForm {
    Pure,
    Constraint,
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
    Atom(Atom),
    /// An arithmetic literal, if using the language $\small\text{Datalog}^{\theta}$.
    Comparison(Comparison),
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ComparisonOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
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
    Variable(Variable),

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

impl Display for Rules {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for rule in self.iter() {
            writeln!(f, "{}", rule)?;
        }
        Ok(())
    }
}

impl Collection<Rule> for Rules {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ Rule> + '_> {
        Box::new(self.0.iter())
    }

    fn contains(&self, value: &Rule) -> bool {
        self.0.contains(value)
    }
}

impl Rules {
    pub fn add(&mut self, rule: Rule) {
        self.0.insert(rule);
    }
}

// ------------------------------------------------------------------------------------------------

impl SyntaxFragments for Rule {
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
        let head_predicates = self
            .head()
            .map(|atom| atom.label())
            .collect::<Vec<&Predicate>>();
        !self
            .literals()
            .filter_map(|lit| {
                if let LiteralInner::Atom(atom) = lit.as_ref() {
                    Some(atom.label())
                } else {
                    None
                }
            })
            .any(|predicate| head_predicates.contains(&predicate))
    }
}

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
            .intersection(&self.distinguished_terms())
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
        self.head().map(|atom| atom.variables()).flatten().collect()
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
    /// 1. It complies with any restrictions implied by the passed set of features.
    /// 2. It meets the rules for safety, define as:
    ///    1. all variables that appear in the head also appear in a positive literal in
    ///       the body of the clause, and
    ///    2. all variables that appear in a negative literal in the body of a clause also appears
    ///       in some positive literal in the body of the clause.
    ///
    pub fn well_formed_check(&self, features: &FeatureSet) -> Result<()> {
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
                    atom.label_ref().clone(),
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
                .filter(|term| !body_positive_terms.contains(term))
                .collect();
            if !missing.is_empty() {
                return Err(negative_variables_not_also_positive(
                    atom.label_ref().clone(),
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
    fn label(&self) -> &Predicate {
        &self.label
    }

    fn label_ref(&self) -> PredicateRef {
        self.label.clone()
    }
}

impl MaybeGround for Atom {
    fn is_ground(&self) -> bool {
        self.terms.iter().all(Term::is_constant)
    }
}

impl Collection<Term> for Atom {
    fn is_empty(&self) -> bool {
        self.terms.is_empty()
    }

    fn len(&self) -> usize {
        self.terms.len()
    }

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
                        relation.label_ref().clone(),
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

    pub fn atom(atom: Atom) -> Self {
        Self::new(false, atom.into())
    }

    pub fn negative_atom(atom: Atom) -> Self {
        Self::new(true, atom.into())
    }

    pub fn comparison(comparison: Comparison) -> Self {
        Self::new(false, comparison.into())
    }

    pub fn negative_comparison(comparison: Comparison) -> Self {
        Self::new(true, comparison.into())
    }

    // --------------------------------------------------------------------------------------------

    delegate!(is_atom, inner -> bool);

    delegate!(as_atom, inner -> Option<&Atom>);

    delegate!(is_comparison, inner -> bool);

    delegate!(as_comparison, inner -> Option<&Comparison>);

    // --------------------------------------------------------------------------------------------

    pub fn terms(&self) -> Vec<&Term> {
        match &self.inner {
            LiteralInner::Atom(v) => v.iter().collect(),
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

impl MaybeGround for LiteralInner {
    fn is_ground(&self) -> bool {
        match self {
            LiteralInner::Atom(a) => a.is_ground(),
            LiteralInner::Comparison(c) => c.is_ground(),
        }
    }
}

impl LiteralInner {
    self_is_as!(atom, Atom);

    self_is_as!(comparison, Comparison);
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

    // --------------------------------------------------------------------------------------------

    get!(lhs -> Term);

    get!(rhs -> Term);

    get!(operator -> ComparisonOperator);

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
            _ => Err(invalid_value(TYPE_NAME_COMPARISON_OPERATOR, s)),
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

impl MaybeAnonymous for Term {
    fn anonymous() -> Self {
        Self::Anonymous
    }

    fn is_anonymous(&self) -> bool {
        matches!(self, Self::Anonymous)
    }
}

impl Term {
    self_is_as!(variable, Variable);

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

impl From<Variable> for String {
    fn from(v: Variable) -> Self {
        v.0
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
}

impl Variable {
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

mod eval;
pub use eval::{naive::NaiveEvaluator, Evaluator, NoopEvaluator};

mod query;
pub use query::{Query, Row, View};
