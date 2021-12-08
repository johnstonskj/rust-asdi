/*!
Another Simplistic Datalog Implementation (ASDI), in Rust.

This package provides a data model to represent Datalog programs in memory, a parser for common
syntactic expression, and some evaluation implementations. The text representation parser is a separate
feature, so if you only need to construct and evaluate programs using the API you do not pull
in the [Pest](https://pest.rs) parser and support.

Tools that operate on the model to _validate_, _derive_, and _evaluate_ the program are
implementations of traits defined in the [`eval`](eval/index.html) module. This makes it easier to
integrate new more sophisticated implementations over time.

# Source Syntax

The text representation that the parser feature accepts is intended to be flexible. While the core program
elements are fixed, there are a number of different operator styles in common usage and where
possible the parser will accept them all.

For example, the following is a simple yet complete program and written using Unicode characters
for an expressive representation.

```prolog
parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ⋀ parent(Z, Y).

?- ancestor(xerces, X).
```

## Facts

Facts are introduced in the form _predicate_ followed by a period (end of statement marker) or a
set of _arguments_ within parenthesis. A predicate is either an

```prolog
parent("Xerces", brooke).
```

### Constants

* Identifier
* String
* Integer
* Float
* Boolean `@true` or `@false`

## Rules

* The ASCII string `":-"` (colon `\u{3a}` and hyphen-minus `\u{2d}`).
* The ASCII string `"<-"` (less-than `\u{3c}` and hyphen-minus `\u{2d}`).
* The Unicode character `⟵` (long/full-width leftwards arrow `\u{27f5}`).

```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) <- parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Y).
```

* The ASCII character `,` (comma `\u{2c}`).
* The ASCII character `&` (comma `\u{26}`).
* The Unicode character `⋀` (n-ary logical and `\u{22c0}`).
* The case-sensitive ASCII string `"AND"`.


```prolog
ancestor(X, Y) ⟵ parent(X, Z), parent(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) & parent(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) ⋀ parent(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) AND parent(Z, Y).
```

## Queries

question mark `\u{3f}`

```prolog
?- ancestor(xerces, X).
ancestor(xerces, X)?
```

## Pragmas

commercial at `\u{40}`

```prolog
@include("file").
```

# Example

TBD

*/

#![warn(
    unknown_lints,
    // ---------- Stylistic
    absolute_paths_not_starting_with_crate,
    elided_lifetimes_in_paths,
    explicit_outlives_requirements,
    macro_use_extern_crate,
    nonstandard_style, /* group */
    noop_method_call,
    rust_2018_idioms,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    // ---------- Future
    future_incompatible, /* group */
    rust_2021_compatibility, /* group */
    // ---------- Public
    missing_debug_implementations,
    // missing_docs,
    unreachable_pub,
    // ---------- Unsafe
    unsafe_code,
    unsafe_op_in_unsafe_fn,
    // ---------- Unused
    unused, /* group */
)]
#![deny(
    // ---------- Public
    exported_private_dependencies,
    private_in_public,
    // ---------- Deprecated
    anonymous_parameters,
    bare_trait_objects,
    ellipsis_inclusive_range_patterns,
    // ---------- Unsafe
    deref_nullptr,
    drop_bounds,
    dyn_drop,
)]

use crate::environment::{Environment, EnvironmentRef, Interned};
use crate::error::{Error, Result};
use crate::eval::NaiveEvaluator;
use crate::features::FeatureSet;
use crate::syntax::*;
use std::cell::{Ref, RefMut};
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// A program is a complete world from which we can derive new facts, or query. A program consists
/// therefore an [_environment_](environment/struct.Environment.html) that captures identifiers
/// and string constants, and a set of program elements.
///
/// While some literature requires that a program's text representation list all [_facts_](struct.Fact.html), then
/// all [_rules_](struct.Rule.html), and then any [_queries_](struct.Query.html), this model
/// makes no such distinction.
///
#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    features: FeatureSet,
    environment: EnvironmentRef,
    elements: Vec<ProgramElement>,
}

pub trait SyntacticFragments {
    fn is_positive(&self) -> bool;

    /// **linear Datalog**, where rule bodies must consist of a single atom.
    fn is_linear(&self) -> bool;

    /// **guarded Datalog**, where for every rule, all the variables that occur in the rule bodies
    /// must occur together in at least one atom, called a _guard atom_.
    fn is_guarded(&self) -> bool;

    /// **frontier-guarded Datalog**, where for every rule, all the variables that are shared
    /// between the rule body and the rule head (called the _frontier variables_) must all occur
    /// together in a guard atom.
    fn is_frontier_guarded(&self) -> bool;

    /// **non-recursive Datalog** is defined by disallowing recursion in the definition of Datalog
    /// programs.
    fn is_non_recursive(&self) -> bool;
}

///
/// A program element is either a [_fact_](struct.Fact.html), [_rule_](struct.Rule.html),
/// [_query_](struct.Query.html), or  [_pragma_](enum.Pragma.html). A pragma is generally
/// not a part of the processing of the model, rather it affects the parsing and construction of a
/// model from text representation.
///
#[derive(Clone, Debug, PartialEq)]
pub enum ProgramElement {
    Fact(Fact),
    Rule(Rule),
    Pragma(Pragma),
    Query(Query),
}

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
#[derive(Clone, Debug, PartialEq)]
pub struct Fact {
    predicate: Predicate,
    arguments: Vec<Constant>,
}

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
#[derive(Clone, Debug, PartialEq)]
pub struct Rule {
    head: Atom,
    body: Vec<Literal>,
}

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
#[derive(Clone, Debug, PartialEq)]
pub struct Query(Atom);

///
/// A pragma, in general, directs the parser behavior, and while they therefore have little use
/// parsed into the model they allow for generation of new textual representations that retain
/// them.
///
/// # Examples
///
/// Currently, only the following pragma is recognized, `include` will insert the contents of the
/// file identified by the one string constant parameter into the current source location.
///
/// ```prolog
/// @include("file/path")
/// ```
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pragma {
    Include(PathBuf),
    // Input(Interned),
    // Output(Interned),
    // Decl(Name, Type-1, ... Type-n)
    // Feature(sym-1, ... sym-n)
}

///
///  An Atom has a similar structure to a [_fact_](struct.Fact.html)
///
#[derive(Clone, Debug, PartialEq)]
pub struct Atom {
    predicate: Predicate,
    arguments: Vec<Term>,
    src_loc: Option<SourceLocation>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Literal {
    negative: bool,
    inner: LiteralInner,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralInner {
    Atom(Atom),
    Expression(LiteralExpression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LiteralExpression {
    left: Term,
    rest: Option<(ComparisonOperator, Term)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComparisonOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Constant {
    Identifier(Interned),
    String(Interned),
    Integer(i64),
    //    Float(f64),
    Boolean(bool),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Term {
    Variable(Interned),
    Constant(Constant),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Predicate {
    Relation(Interned),
    String(Interned),
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct SourceLocation {
    line: usize,
    column: usize,
}

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

trait DisplayExt {
    fn to_extern_string(&self, environment: &Environment) -> String;
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

impl Default for Program {
    fn default() -> Self {
        Self {
            features: Default::default(),
            environment: Default::default(),
            elements: Default::default(),
        }
    }
}

impl From<FeatureSet> for Program {
    fn from(v: FeatureSet) -> Self {
        Self {
            features: v,
            environment: Default::default(),
            elements: Default::default(),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for element in &self.elements {
            writeln!(f, "{}", element.to_extern_string(&self.environment()))?;
        }
        Ok(())
    }
}

impl SyntacticFragments for Program {
    fn is_positive(&self) -> bool {
        self.rules().all(|rule| rule.is_positive())
    }

    fn is_linear(&self) -> bool {
        self.rules().all(|rule| rule.is_linear())
    }

    fn is_guarded(&self) -> bool {
        self.rules().all(|rule| rule.is_guarded())
    }

    fn is_frontier_guarded(&self) -> bool {
        self.rules().all(|rule| rule.is_frontier_guarded())
    }

    fn is_non_recursive(&self) -> bool {
        self.rules().all(|rule| rule.is_non_recursive())
    }
}

impl Program {
    pub fn new<E>(elements: E) -> Self
    where
        E: Into<Vec<ProgramElement>>,
    {
        Self::new_with_environment_and_features(Default::default(), elements, Default::default())
    }

    pub fn new_with_features<E>(elements: E, features: FeatureSet) -> Self
    where
        E: Into<Vec<ProgramElement>>,
    {
        Self::new_with_environment_and_features(Default::default(), elements, features)
    }

    pub fn new_with_environment<E>(environment: EnvironmentRef, elements: E) -> Self
    where
        E: Into<Vec<ProgramElement>>,
    {
        Self::new_with_environment_and_features(environment, elements, Default::default())
    }

    pub fn new_with_environment_and_features<E>(
        environment: EnvironmentRef,
        elements: E,
        features: FeatureSet,
    ) -> Self
    where
        E: Into<Vec<ProgramElement>>,
    {
        Self {
            features,
            environment,
            elements: elements.into(),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn features(&self) -> &FeatureSet {
        &self.features
    }

    // --------------------------------------------------------------------------------------------

    pub fn environment(&self) -> Ref<'_, Environment> {
        self.environment.borrow()
    }

    pub fn environment_mut(&mut self) -> RefMut<'_, Environment> {
        self.environment.borrow_mut()
    }

    pub fn make_constant_identifier<S>(&mut self, s: S) -> Result<Constant>
    where
        S: Into<String>,
    {
        Ok(Constant::Identifier(
            self.environment_mut().new_identifier(s)?,
        ))
    }

    pub fn make_constant_string<S>(&mut self, s: S) -> Result<Constant>
    where
        S: Into<String>,
    {
        Ok(Constant::String(self.environment_mut().new_constant(s)?))
    }

    pub fn make_term_variable<S>(&mut self, s: S) -> Result<Term>
    where
        S: Into<String>,
    {
        Ok(Term::Variable(self.environment_mut().new_variable(s)?))
    }

    pub fn make_predicate_relation<S>(&mut self, s: S) -> Result<Predicate>
    where
        S: Into<String>,
    {
        Ok(Predicate::Relation(self.environment_mut().new_relation(s)?))
    }

    pub fn make_predicate_string<S>(&mut self, s: S) -> Result<Predicate>
    where
        S: Into<String>,
    {
        Ok(Predicate::String(self.environment_mut().new_constant(s)?))
    }

    // --------------------------------------------------------------------------------------------

    pub fn push<E>(&mut self, element: E) -> Result<()>
    where
        E: Into<ProgramElement>,
    {
        let element = element.into();
        if let ProgramElement::Rule(rule) = &element {
            rule.check_well_formed(&self.features, &self.environment())?;
        }
        self.elements.push(element);
        Ok(())
    }

    pub fn extend<I, E>(&mut self, iter: I) -> Result<()>
    where
        E: Into<ProgramElement>,
        I: IntoIterator<Item = E>,
    {
        for element in iter.into_iter() {
            self.push(element)?;
        }
        Ok(())
    }

    pub fn remove<E>(&mut self, element: E) -> &mut Self
    where
        E: Into<ProgramElement>,
    {
        let element = element.into();
        self.elements.retain(|e| e != &element);
        self
    }

    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    pub fn elements(&self) -> impl Iterator<Item = &ProgramElement> {
        self.elements.iter()
    }

    pub fn elements_mut(&mut self) -> impl Iterator<Item = &mut ProgramElement> {
        self.elements.iter_mut()
    }

    // --------------------------------------------------------------------------------------------

    /// extensional predicate symbols
    pub fn facts(&self) -> impl Iterator<Item = &Fact> {
        self.elements().filter_map(|e| match e {
            ProgramElement::Fact(e) => Some(e),
            _ => None,
        })
    }

    /// intensional predicate symbols
    pub fn rules(&self) -> impl Iterator<Item = &Rule> {
        self.elements().filter_map(|e| match e {
            ProgramElement::Rule(e) => Some(e),
            _ => None,
        })
    }

    pub fn queries(&self) -> impl Iterator<Item = &Query> {
        self.elements().filter_map(|e| match e {
            ProgramElement::Query(e) => Some(e),
            _ => None,
        })
    }

    // --------------------------------------------------------------------------------------------

    pub fn is_well_formed(&self) -> bool {
        self.rules().all(|r| {
            r.check_well_formed(&self.features, &self.environment())
                .is_ok()
        })
    }

    // --------------------------------------------------------------------------------------------

    pub fn inference(&self, evaluator: &NaiveEvaluator) -> Result<()> {
        evaluator.init(self)?;

        let mut inferred: Vec<ProgramElement> = Default::default();
        loop {
            let mut additions = evaluator.infer_once(self, &inferred)?;
            if additions.is_empty() {
                break;
            } else {
                inferred.append(&mut additions)
            }
        }
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------

impl From<Fact> for ProgramElement {
    fn from(e: Fact) -> Self {
        Self::Fact(e)
    }
}

impl From<Rule> for ProgramElement {
    fn from(e: Rule) -> Self {
        Self::Rule(e)
    }
}

impl From<Query> for ProgramElement {
    fn from(e: Query) -> Self {
        Self::Query(e)
    }
}

impl From<Pragma> for ProgramElement {
    fn from(e: Pragma) -> Self {
        Self::Pragma(e)
    }
}

impl DisplayExt for ProgramElement {
    fn to_extern_string(&self, environment: &Environment) -> String {
        match self {
            Self::Fact(e) => e.to_extern_string(environment),
            Self::Rule(e) => e.to_extern_string(environment),
            Self::Pragma(e) => e.to_extern_string(environment),
            Self::Query(e) => e.to_extern_string(environment),
        }
    }
}

impl ProgramElement {
    pub fn is_fact(&self) -> bool {
        matches!(self, Self::Fact(_))
    }
    pub fn is_rule(&self) -> bool {
        matches!(self, Self::Rule(_))
    }
    pub fn is_pragma(&self) -> bool {
        matches!(self, Self::Pragma(_))
    }
    pub fn is_query(&self) -> bool {
        matches!(self, Self::Query(_))
    }
}

// ------------------------------------------------------------------------------------------------

impl DisplayExt for Fact {
    fn to_extern_string(&self, environment: &Environment) -> String {
        format!(
            "{}{}{}",
            self.predicate.to_extern_string(environment),
            if self.arguments.is_empty() {
                String::new()
            } else {
                format!(
                    "{}{}{}",
                    CHAR_LEFT_PAREN,
                    self.arguments
                        .iter()
                        .map(|v| v.to_extern_string(environment))
                        .collect::<Vec<String>>()
                        .join(&format!("{} ", CONJUNCTION_ASCII)),
                    CHAR_RIGHT_PAREN,
                )
            },
            CHAR_PERIOD
        )
    }
}

impl Fact {
    pub fn new(predicate: Predicate) -> Self {
        Self {
            predicate,
            arguments: Default::default(),
        }
    }

    pub fn new_with_arguments<C: Into<Vec<Constant>>>(predicate: Predicate, arguments: C) -> Self {
        Self {
            predicate,
            arguments: arguments.into(),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn predicate(&self) -> &Predicate {
        &self.predicate
    }

    // --------------------------------------------------------------------------------------------

    pub fn has_arguments(&self) -> bool {
        !self.arguments.is_empty()
    }

    pub fn add_argument<V: Into<Constant>>(&mut self, argument: V) -> &mut Self {
        self.arguments.push(argument.into());
        self
    }

    pub fn extend_arguments<C: Into<Vec<Constant>>>(&mut self, arguments: C) -> &mut Self {
        self.arguments.append(&mut arguments.into());
        self
    }

    pub fn remove_argument<V: Into<Constant>>(&mut self, argument: V) -> &mut Self {
        let argument: Constant = argument.into();
        self.arguments.retain(move |p| p != &argument);
        self
    }

    pub fn arguments(&self) -> impl Iterator<Item = &Constant> {
        self.arguments.iter()
    }
}

// ------------------------------------------------------------------------------------------------

impl DisplayExt for Rule {
    fn to_extern_string(&self, environment: &Environment) -> String {
        format!(
            "{}{}{}",
            self.head.to_extern_string(environment),
            if self.body.is_empty() {
                unreachable!()
            } else {
                format!(
                    " {} {}",
                    IMPLICATION_UNICODE_ARROW,
                    self.body
                        .iter()
                        .map(|atom| atom.to_extern_string(environment))
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
            let lit_variables: HashSet<&Interned> = HashSet::from_iter(lit.variables().into_iter());
            lit_variables == all_variables
        })
    }

    fn is_frontier_guarded(&self) -> bool {
        let frontier_variables: HashSet<&Interned> = self
            .head_variables()
            .intersection(&self.variables())
            .copied()
            .collect();
        self.literals().any(|lit| {
            let lit_variables: HashSet<&Interned> = HashSet::from_iter(lit.variables().into_iter());
            lit_variables == frontier_variables
        })
    }

    fn is_non_recursive(&self) -> bool {
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

impl Rule {
    pub fn new<V: Into<Vec<Atom>>>(head: Atom) -> Self {
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

    pub fn head_terms(&self) -> HashSet<&Term> {
        self.head().terms().collect()
    }

    pub fn terms(&self) -> HashSet<&Term> {
        self.body.iter().map(|lit| lit.terms()).flatten().collect()
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

    pub fn head_variables(&self) -> HashSet<&Interned> {
        self.head().variables().collect()
    }

    pub fn variables(&self) -> HashSet<&Interned> {
        self.body
            .iter()
            .map(|lit| lit.variables())
            .flatten()
            .collect()
    }

    pub fn positive_variables(&self) -> HashSet<&Interned> {
        self.body
            .iter()
            .filter(|lit| lit.is_positive())
            .map(|lit| lit.variables())
            .flatten()
            .collect()
    }

    pub fn negative_variables(&self) -> HashSet<&Interned> {
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
    fn check_well_formed(&self, _features: &FeatureSet, environment: &Environment) -> Result<()> {
        if !self.body.is_empty() {
            let body_positive_terms = self.positive_terms();

            let missing: Vec<&Term> = self
                .head_terms()
                .into_iter()
                .filter(|term| !body_positive_terms.contains(term))
                .collect();
            if !missing.is_empty() {
                return Err(Error::HeadVariablesMissingInBody(
                    self.head().predicate().to_extern_string(environment),
                    self.head().source_location().cloned(),
                    missing
                        .iter()
                        .map(|t| t.to_extern_string(environment))
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
                    self.head().predicate().to_extern_string(environment),
                    self.head().source_location().cloned(),
                    missing
                        .iter()
                        .map(|t| t.to_extern_string(environment))
                        .collect::<Vec<String>>(),
                ));
            }
        }
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------

impl DisplayExt for Atom {
    fn to_extern_string(&self, environment: &Environment) -> String {
        format!(
            "{}{}{}{}",
            self.predicate.to_extern_string(environment),
            CHAR_LEFT_PAREN,
            if self.arguments.is_empty() {
                unreachable!()
            } else {
                self.arguments
                    .iter()
                    .map(|v| v.to_extern_string(environment))
                    .collect::<Vec<String>>()
                    .join(", ")
            },
            CHAR_RIGHT_PAREN,
        )
    }
}

impl Atom {
    pub fn new<V: Into<Vec<Term>>>(predicate: Predicate, arguments: V) -> Self {
        let arguments = arguments.into();
        assert!(!arguments.is_empty());
        Self {
            predicate,
            arguments,
            src_loc: None,
        }
    }

    pub fn new_at_location<V: Into<Vec<Term>>>(
        predicate: Predicate,
        arguments: V,
        location: SourceLocation,
    ) -> Self {
        let arguments = arguments.into();
        assert!(!arguments.is_empty());
        Self {
            predicate,
            arguments,
            src_loc: Some(location),
        }
    }

    pub fn predicate(&self) -> &Predicate {
        &self.predicate
    }

    pub fn push<V: Into<Term>>(&mut self, argument: V) -> &mut Self {
        self.arguments.push(argument.into());
        self
    }

    pub fn extend<T: Into<Vec<Term>>>(&mut self, arguments: T) -> &mut Self {
        self.arguments.append(&mut arguments.into());
        self
    }

    pub fn remove<V: Into<Term>>(&mut self, argument: V) -> &mut Self {
        assert!(self.arguments.len() > 1);
        let argument: Term = argument.into();
        self.arguments.retain(move |p| p != &argument);
        self
    }

    pub fn terms(&self) -> impl Iterator<Item = &Term> {
        self.arguments.iter()
    }

    pub fn variables(&self) -> impl Iterator<Item = &Interned> {
        self.arguments.iter().filter_map(|t| {
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

impl DisplayExt for Literal {
    fn to_extern_string(&self, environment: &Environment) -> String {
        format!(
            "{}{}",
            if self.negative {
                NOT_UNICODE_SYMBOL
            } else {
                EMPTY_STR
            },
            match &self.inner {
                LiteralInner::Atom(atom) => atom.to_extern_string(environment),
                LiteralInner::Expression(expr) => expr.to_extern_string(environment),
            }
        )
    }
}

impl From<Atom> for Literal {
    fn from(v: Atom) -> Self {
        Self::atom(v)
    }
}

impl From<LiteralExpression> for Literal {
    fn from(v: LiteralExpression) -> Self {
        Self::expression(v)
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

    pub fn expression(expression: LiteralExpression) -> Self {
        Self {
            negative: false,
            inner: expression.into(),
        }
    }

    pub fn negative_expression(expression: LiteralExpression) -> Self {
        Self {
            negative: true,
            inner: expression.into(),
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
            LiteralInner::Expression(v) => match &v.rest {
                None => vec![&v.left],
                Some((_, right)) => vec![&v.left, right],
            },
        }
    }

    pub fn variables(&self) -> Vec<&Interned> {
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

    pub fn is_expression(&self) -> bool {
        self.inner.is_expression()
    }
}

// ------------------------------------------------------------------------------------------------

impl From<Atom> for LiteralInner {
    fn from(v: Atom) -> Self {
        Self::Atom(v)
    }
}

impl From<LiteralExpression> for LiteralInner {
    fn from(v: LiteralExpression) -> Self {
        Self::Expression(v)
    }
}

impl LiteralInner {
    pub fn is_atom(&self) -> bool {
        matches!(self, LiteralInner::Atom(_))
    }

    pub fn is_expression(&self) -> bool {
        matches!(self, LiteralInner::Expression(_))
    }
}

// ------------------------------------------------------------------------------------------------

impl DisplayExt for LiteralExpression {
    fn to_extern_string(&self, environment: &Environment) -> String {
        format!(
            "{}{}",
            self.left.to_extern_string(environment),
            if let Some((op, right)) = &self.rest {
                format!(" {} {}", op, right.to_extern_string(environment))
            } else {
                String::new()
            }
        )
    }
}

impl LiteralExpression {
    pub fn term(left: Term) -> Self {
        Self { left, rest: None }
    }

    pub fn new(left: Term, op: ComparisonOperator, right: Term) -> Self {
        Self {
            left,
            rest: Some((op, right)),
        }
    }

    pub fn eq(left: Term, right: Term) -> Self {
        Self::new(left, ComparisonOperator::Equal, right)
    }

    pub fn ne(left: Term, right: Term) -> Self {
        Self::new(left, ComparisonOperator::NotEqual, right)
    }

    pub fn lt(left: Term, right: Term) -> Self {
        Self::new(left, ComparisonOperator::LessThan, right)
    }

    pub fn lte(left: Term, right: Term) -> Self {
        Self::new(left, ComparisonOperator::LessThanOrEqual, right)
    }

    pub fn gt(left: Term, right: Term) -> Self {
        Self::new(left, ComparisonOperator::GreaterThan, right)
    }

    pub fn gte(left: Term, right: Term) -> Self {
        Self::new(left, ComparisonOperator::GreaterThanOrEqual, right)
    }

    pub fn terms(&self) -> Vec<&Term> {
        let left = &self.left;
        match &self.rest {
            None => vec![left],
            Some((_, right)) => vec![left, right],
        }
    }

    pub fn variables(&self) -> Vec<&Interned> {
        let mut result = match &self.left {
            Term::Variable(v) => vec![v],
            _ => vec![],
        };
        if let Some((_, Term::Variable(v))) = &self.rest {
            result.push(v)
        }
        result
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

impl DisplayExt for ComparisonOperator {
    fn to_extern_string(&self, _: &Environment) -> String {
        (match self {
            Self::Equal => OPERATOR_ASCII_EQUAL,
            Self::NotEqual => OPERATOR_UNICODE_NOT_EQUAL,
            Self::LessThan => OPERATOR_ASCII_LESS_THAN,
            Self::LessThanOrEqual => OPERATOR_UNICODE_LESS_THAN_OR_EQUAL,
            Self::GreaterThan => OPERATOR_ASCII_GREATER_THAN,
            Self::GreaterThanOrEqual => OPERATOR_UNICODE_GREATER_THAN_OR_EQUAL,
        })
        .to_string()
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

impl DisplayExt for Query {
    fn to_extern_string(&self, environment: &Environment) -> String {
        format!(
            "{} {}{}",
            QUERY_PREFIX_ASCII,
            self.0.to_extern_string(environment),
            CHAR_PERIOD
        )
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

// ------------------------------------------------------------------------------------------------

impl From<i64> for Constant {
    fn from(v: i64) -> Self {
        Self::Integer(v)
    }
}

// impl From<f64> for Constant {
//     fn from(v: f64) -> Self {
//         Self::Float(v)
//     }
// }

impl From<bool> for Constant {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}

impl DisplayExt for Constant {
    fn to_extern_string(&self, environment: &Environment) -> String {
        match self {
            Self::Identifier(v) => environment.identifier_string(v).unwrap().clone(),
            Self::String(v) => format!("{:?}", environment.constant_string(v).unwrap()),
            Self::Integer(v) => v.to_string(),
            //            Self::Float(v) => v.to_string(),
            Self::Boolean(v) => {
                if *v {
                    format!("{}{}", RESERVED_PREFIX, RESERVED_BOOLEAN_TRUE)
                } else {
                    format!("{}{}", RESERVED_PREFIX, RESERVED_BOOLEAN_FALSE)
                }
            }
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl DisplayExt for Term {
    fn to_extern_string(&self, environment: &Environment) -> String {
        match self {
            Term::Variable(v) => environment.variable_string(v).unwrap().clone(),
            Term::Constant(v) => v.to_extern_string(environment),
        }
    }
}

impl From<Constant> for Term {
    fn from(v: Constant) -> Self {
        Self::Constant(v)
    }
}

// ------------------------------------------------------------------------------------------------

impl DisplayExt for Predicate {
    fn to_extern_string(&self, environment: &Environment) -> String {
        match self {
            Self::Relation(v) => environment.relation_string(v).unwrap().clone(),
            Self::String(v) => format!("{:?}", environment.constant_string(v).unwrap().clone()),
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl DisplayExt for Pragma {
    fn to_extern_string(&self, _: &Environment) -> String {
        match self {
            Self::Include(file_path) => format!(
                "{}{}{}{:?}{}{}",
                RESERVED_PREFIX,
                RESERVED_PRAGMA_INCLUDE,
                CHAR_LEFT_PAREN,
                file_path,
                CHAR_RIGHT_PAREN,
                CHAR_PERIOD,
            ),
        }
    }
}

impl Pragma {
    pub fn try_from<C: Into<Vec<Constant>>>(
        identifier: &str,
        arguments: C,
        env: &Environment,
    ) -> Result<Self> {
        let arguments = arguments.into();
        if identifier != RESERVED_PRAGMA_INCLUDE {
            Error::InvalidPragmaName(identifier.to_string()).into()
        } else if arguments.len() != 1 {
            Error::InvalidPragmaArgumentCount(1, arguments.len()).into()
        } else if let Constant::String(s) = arguments.get(0).unwrap() {
            Ok(Self::Include(PathBuf::from(
                env.constant_string(s).unwrap(),
            )))
        } else {
            Error::InvalidPragmaArgumentType(TYPE_NAME_CONST_STRING.to_string()).into()
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl From<(usize, usize)> for SourceLocation {
    fn from(v: (usize, usize)) -> Self {
        Self {
            line: v.0,
            column: v.1,
        }
    }
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}, column {}]", self.line, self.column)
    }
}

impl SourceLocation {
    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

pub mod environment;

pub mod error;

pub mod eval;

pub mod features;

#[cfg(feature = "parser")]
pub mod parse;

mod syntax;
