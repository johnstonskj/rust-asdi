/*!
One-line description.

More detailed description, with

# Example

# Query Forms

**Natural join** ($\Join$) is a binary operator that is written as $R \Join S$ where $R$ and $S$ are
relations. The result of the natural join is the set of all combinations of tuples in $R$
and $S$ that are equal on their common attribute names.

**Projection** ($\Pi$) is a unary operation written as
$\Pi_{a_{1},\ldots ,a_{n}}(R)$ where $a_{1},\ldots ,a_{n}$ is a set of attribute names. The
result of such projection is defined as the set that is obtained when all tuples in $R$ are
restricted to the set $\lbrace a_{1},\ldots ,a_{n}\rbrace$.

> Note: when implemented in SQL standard the "default projection" returns a multiset instead
> of a set, and the $\Pi$ projection to eliminate duplicate data is obtained by the addition
> of the `DISTINCT` keyword.

**Generalized Selection** ($\sigma$) is a unary operation written as
$\sigma_{\varphi}(R)$ where $\varphi$ is a propositional formula that consists
of atoms as allowed in the normal selection and the logical operators $\land$ (and), $\lor$
(or) and $\lnot$ (negation). This selection selects all those tuples in $R$ for which $\varphi$
holds.

Although relational algebra seems powerful enough for most practical purposes, there are some
simple and natural operators on relations that cannot be expressed by relational algebra.
One of them is the transitive closure of a binary relation. Given a domain $D$, let binary
relation $R$ be a subset of $D\times D$. The transitive closure $R^{+}$ of $R$ is the smallest
subset of $D\times D$ that contains $R$ and satisfies the following condition:

$$\forall x\forall y\forall z\left((x,y)\in R^{+}\land (y,z)\in R^{+}\Rightarrow (x,z)\in R^{+}\right)$$

In Datalog

```prolog
r_plus(X, Z) :- r(X, Y), r_plus(Y, Z).
```

*/

use crate::edb::{
    Attribute, AttributeIndex, AttributeKind, BaseRelation, Constant, Database, DbValidation, Fact,
    Predicate, Schema,
};
use crate::error::Result;
use crate::idb::Term;
use crate::idb::{Atom, Variable};
use crate::syntax::{CHAR_PERIOD, QUERY_PREFIX_ASCII};
use std::fmt::{Debug, Display, Formatter};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// A query simply wraps a single [_atom_](struct.Atom.html) which acts as the goal for the query.
///
/// # Examples
///
/// It is distinguished in the text representation with either the prefix `?-` and suffix `.`
/// **or** the suffix `?` and no period.
///
/// ```prolog
/// ?- ancestor(xerces, X).
/// ancestor(xerces, X)?
/// ```
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Query(Atom);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct View(BaseRelation<Variable>);

pub trait Matches {
    fn query(&self, query: &Query) -> View {
        self.matches(query.as_ref())
    }

    fn matches(&self, atom: &Atom) -> View;

    #[allow(single_use_lifetimes)]
    fn match_terms<'a, V: Into<Vec<&'a Term>>>(&self, terms: V) -> View;
}

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

impl Display for View {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Matches for View {
    fn matches(&self, atom: &Atom) -> View {
        self.0.matches(atom)
    }

    #[allow(single_use_lifetimes)]
    fn match_terms<'a, V: Into<Vec<&'a Term>>>(&self, terms: V) -> View {
        self.0.match_terms(terms)
    }
}

impl View {
    pub fn new<V: Into<Schema<Variable>>>(schema: V) -> Self {
        Self(BaseRelation::new(schema))
    }

    pub fn new_with_facts<V: Into<Schema<Variable>>, C: Into<Vec<Vec<Constant>>>>(
        schema: V,
        facts: C,
    ) -> Self {
        Self(BaseRelation::new_with_facts(schema, facts))
    }

    pub fn new_true() -> Self {
        Self(BaseRelation::new_with_facts(
            vec![Attribute::from(AttributeKind::Boolean)],
            vec![vec![true.into()]],
        ))
    }

    pub fn new_false() -> Self {
        Self(BaseRelation::new_with_facts(
            vec![Attribute::from(AttributeKind::Boolean)],
            vec![vec![false.into()]],
        ))
    }

    pub fn new_count(v: usize) -> Self {
        Self::new_integer(v as i64)
    }

    pub fn new_integer(v: i64) -> Self {
        Self(BaseRelation::new_with_facts(
            vec![Attribute::from(AttributeKind::Integer)],
            vec![vec![v.into()]],
        ))
    }

    pub fn new_string(s: &str) -> Self {
        Self(BaseRelation::new_with_facts(
            vec![Attribute::from(AttributeKind::String)],
            vec![vec![s.to_string().into()]],
        ))
    }

    pub fn empty() -> Self {
        Self(BaseRelation::empty())
    }

    pub fn is_anonymous(&self) -> bool {
        true
    }

    // --------------------------------------------------------------------------------------------

    pub fn schema(&self) -> &Schema<Variable> {
        self.0.schema()
    }

    pub fn arity(&self) -> usize {
        self.0.arity()
    }

    pub fn has_attribute<I: Into<AttributeIndex<Variable>>>(&self, index: I) -> bool {
        self.0.has_attribute(index)
    }

    // --------------------------------------------------------------------------------------------

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn facts(&self) -> impl Iterator<Item = Fact<'_, Variable>> + '_ {
        self.0.facts()
    }

    pub fn add<V: Into<Vec<Constant>>>(&mut self, fact: V) -> Result<()> {
        self.0.add(fact)
    }

    pub fn extend(&mut self, other: Self) -> Result<()> {
        self.0.extend(other.0)
    }

    // --------------------------------------------------------------------------------------------

    pub fn join_all<V: Into<Vec<Self>> + Debug>(views: V) -> Result<Self> {
        Ok(Self(BaseRelation::join_all(
            views
                .into()
                .into_iter()
                .map(|v| v.0)
                .collect::<Vec<BaseRelation<Variable>>>(),
        )?))
    }
}

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

impl DbValidation for Query {
    fn validate(&self, against: &mut Database) -> Result<()> {
        Atom::validate(self.as_ref(), against)
    }
}

impl Query {
    pub fn new<T: Into<Vec<Term>>>(predicate: Predicate, terms: T) -> Self {
        Self(Atom::new(predicate, terms))
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
