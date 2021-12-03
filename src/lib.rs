/*!
One-line description.

More detailed description, with

# Source Syntax

```prolog
parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ⋀ parent(Z, Y).

?- ancestor(xerces, X).
```

## Facts

```prolog
parent("Xerces", brooke).
```

### Constants

* Identifier
* String
* Integer
* Boolean

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
use std::cell::{Ref, RefMut};
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    environment: EnvironmentRef,
    elements: Vec<ProgramElement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProgramElement {
    Fact(Fact),
    Rule(Rule),
    Pragma(Pragma),
    Query(Query),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fact {
    predicate: Predicate,
    parameters: Vec<Constant>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rule {
    head: Atom,
    body: Vec<Atom>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Query(Atom);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pragma {
    Include(PathBuf),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Atom {
    predicate: Predicate,
    parameters: Vec<Term>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Constant {
    Identifier(Interned),
    String(Interned),
    Integer(i64),
    Boolean(bool),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Variable(Interned),
    Constant(Constant),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Predicate {
    Relation(Interned),
    String(Interned),
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

impl Program {
    pub fn new<E>(elements: E) -> Self
    where
        E: Into<Vec<ProgramElement>>,
    {
        Self::new_with_environment(Default::default(), elements)
    }

    pub fn new_with_environment<E>(environment: EnvironmentRef, elements: E) -> Self
    where
        E: Into<Vec<ProgramElement>>,
    {
        Self {
            environment,
            elements: elements.into(),
        }
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

    pub fn push<E>(&mut self, element: E)
    where
        E: Into<ProgramElement>,
    {
        self.elements.push(element.into());
    }

    pub fn extend<I, E>(&mut self, iter: I)
    where
        E: Into<ProgramElement>,
        I: IntoIterator<Item = E>,
    {
        self.elements.extend(iter.into_iter().map(|e| e.into()));
    }

    pub fn remove<E>(&mut self, element: E) -> &mut Self
    where
        E: Into<ProgramElement>,
    {
        let element = element.into();
        self.elements.retain(|e| e != &element);
        self
    }

    pub fn iter(&self) -> impl Iterator<Item = &ProgramElement> {
        self.elements.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut ProgramElement> {
        self.elements.iter_mut()
    }

    // --------------------------------------------------------------------------------------------

    pub fn facts(&self) -> impl Iterator<Item = &Fact> {
        self.iter().filter_map(|e| match e {
            ProgramElement::Fact(e) => Some(e),
            _ => None,
        })
    }

    pub fn rules(&self) -> impl Iterator<Item = &Rule> {
        self.iter().filter_map(|e| match e {
            ProgramElement::Rule(e) => Some(e),
            _ => None,
        })
    }

    pub fn queries(&self) -> impl Iterator<Item = &Query> {
        self.iter().filter_map(|e| match e {
            ProgramElement::Query(e) => Some(e),
            _ => None,
        })
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
            "{}{}.",
            self.predicate.to_extern_string(environment),
            if self.parameters.is_empty() {
                String::new()
            } else {
                format!(
                    "({})",
                    self.parameters
                        .iter()
                        .map(|v| v.to_extern_string(environment))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        )
    }
}

impl Fact {
    pub fn new(predicate: Predicate) -> Self {
        Self {
            predicate,
            parameters: Default::default(),
        }
    }

    pub fn new_with_parameters<C: Into<Vec<Constant>>>(
        predicate: Predicate,
        parameters: C,
    ) -> Self {
        Self {
            predicate,
            parameters: parameters.into(),
        }
    }

    pub fn add_parameter<V: Into<Constant>>(&mut self, parameter: V) -> &mut Self {
        self.parameters.push(parameter.into());
        self
    }

    pub fn extend_parameters<C: Into<Vec<Constant>>>(&mut self, parameters: C) -> &mut Self {
        self.parameters.append(&mut parameters.into());
        self
    }

    pub fn remove_parameter<V: Into<Constant>>(&mut self, parameter: V) -> &mut Self {
        let parameter: Constant = parameter.into();
        self.parameters.retain(move |p| p != &parameter);
        self
    }

    pub fn parameters(&self) -> impl Iterator<Item = &Constant> {
        self.parameters.iter()
    }
}

// ------------------------------------------------------------------------------------------------

impl DisplayExt for Rule {
    fn to_extern_string(&self, environment: &Environment) -> String {
        format!(
            "{} ⟵ {}.",
            self.head.to_extern_string(environment),
            if self.body.is_empty() {
                unreachable!()
            } else {
                self.body
                    .iter()
                    .map(|atom| atom.to_extern_string(environment))
                    .collect::<Vec<String>>()
                    .join(" ⋀ ")
            }
        )
    }
}

impl Rule {
    pub fn new<A: Into<Vec<Atom>>>(head: Atom, body: A) -> Self {
        Self {
            head,
            body: body.into(),
        }
    }

    pub fn add_to_body(&mut self, atom: Atom) -> &mut Self {
        self.body.push(atom);
        self
    }

    pub fn extend_body<A: Into<Vec<Atom>>>(&mut self, atoms: A) -> &mut Self {
        self.body.append(&mut atoms.into());
        self
    }

    pub fn remove_body(&mut self, atom: Atom) -> &mut Self {
        self.body.retain(move |p| p != &atom);
        self
    }

    pub fn body(&self) -> impl Iterator<Item = &Atom> {
        self.body.iter()
    }
}

// ------------------------------------------------------------------------------------------------

impl DisplayExt for Atom {
    fn to_extern_string(&self, environment: &Environment) -> String {
        format!(
            "{}{}",
            self.predicate.to_extern_string(environment),
            if self.parameters.is_empty() {
                String::new()
            } else {
                format!(
                    "({})",
                    self.parameters
                        .iter()
                        .map(|v| v.to_extern_string(environment))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        )
    }
}

impl Atom {
    pub fn new(predicate: Predicate) -> Self {
        Self {
            predicate,
            parameters: Default::default(),
        }
    }
    pub fn new_with_parameters<C: Into<Vec<Term>>>(predicate: Predicate, parameters: C) -> Self {
        Self {
            predicate,
            parameters: parameters.into(),
        }
    }

    pub fn add_parameter<V: Into<Term>>(&mut self, parameter: V) -> &mut Self {
        self.parameters.push(parameter.into());
        self
    }

    pub fn extend_parameters<C: Into<Vec<Term>>>(&mut self, parameters: C) -> &mut Self {
        self.parameters.append(&mut parameters.into());
        self
    }

    pub fn remove_parameter<V: Into<Term>>(&mut self, parameter: V) -> &mut Self {
        let parameter: Term = parameter.into();
        self.parameters.retain(move |p| p != &parameter);
        self
    }

    pub fn parameters(&self) -> impl Iterator<Item = &Term> {
        self.parameters.iter()
    }
}

// ------------------------------------------------------------------------------------------------

impl DisplayExt for Query {
    fn to_extern_string(&self, environment: &Environment) -> String {
        format!("?- {}.", self.0.to_extern_string(environment))
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

impl DisplayExt for Constant {
    fn to_extern_string(&self, environment: &Environment) -> String {
        match self {
            Constant::Identifier(v) => environment.identifier_string(v).unwrap().clone(),
            Constant::String(v) => format!("{:?}", environment.constant_string(v).unwrap()),
            Constant::Integer(v) => v.to_string(),
            Constant::Boolean(v) => v.to_string(),
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
            Self::Include(file_path) => format!("@include({:?}).", file_path),
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
        if identifier != "include" {
            Error::InvalidPragmaName(identifier.to_string()).into()
        } else if arguments.len() != 1 {
            Error::InvalidPragmaArgumentCount(1, arguments.len()).into()
        } else if let Constant::String(s) = arguments.get(0).unwrap() {
            Ok(Self::Include(PathBuf::from(
                env.constant_string(s).unwrap(),
            )))
        } else {
            Error::InvalidPragmaArgumentType("String".to_string()).into()
        }
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

#[cfg(feature = "parser")]
pub mod parse;
