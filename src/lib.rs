/*!
Another Simplistic Datalog Implementation (ASDI), in Rust.

This package provides a data model to represent Datalog programs in memory, a parser for common
syntactic expression, and some evaluation implementations. The text representation parser is a separate
feature, so if you only need to construct and evaluate programs using the API you do not pull
in the [Pest](https://pest.rs) parser and support.

Tools that operate on the model to _validate_, _derive_, and _evaluate_ the program are
implementations of traits defined in the [`eval`](eval/index.html) module. This makes it easier to
integrate new more sophisticated implementations over time.

# Datalog Defined

* **Program**
  * _Facts_ and _Rules_
* **Atom** (Atomic Formula)
  * **Predicate**
  Variable names **must** begin with a lowercase letter followed by any number of Unicode letters,
  numbers, or the underscore `_` character.
    Predicates are sometimes refereed to as _Relations_.
  * **Fact**
    Fact arguments may only be _Constants_.
    Facts are sometimes refereed to as _Axioms_.
  * **Literal**
    Literal arguments may be _Constants_ or _Variables_.
  * **Expression**
* **Rule**
  * **head**
  * **body**
* **Query**
* **Constant**
  * **Identifier**
  * **String**
  * **Integer**
  * **Boolean**
* **Variable**
  Variable names **must** begin with an uppercase letter followed by any number of Unicode letters,
  numbers, or the underscore `_` character.

```abnf
program         = *[ element ]
element         = statement / query
statement       = ( fact / rule ) "."
query           = ( "?-" query "." ) / ( query "?" )
fact            = predicate [ constant_list ] "."
rule            = atom ":-" [ literal_list ] "."
constant_list   = "(" [ constant *[ "," constant ] ] ")"
constant        = identifier / string / integer / boolean
literal_list    = literal *[ conjunction literal ]
literal         = [ negation ] atom / expression
atom            = predicate "(" term *[ "," term ] ")"
expression      = term [ operator term ]
term_list       = "(" [ term *[ "," term ] ] ")"
term            = variable / constant
operator        = "=" / "!=" / "<" / "<=" / ">" / ">="
predicate       = LC_ALPHA *[ ALPHA / DIGIT / "_" ]
variable        = UC_ALPHA *[ ALPHA / DIGIT / "_" ]
identifier      = ALPHA *[ ALPHA / DIGIT / "_" ]
string          = DQUOTE ... DQUOTE
conjunction     = "," / "AND"
negation        = "!" / "NOT"
```

[Augmented BNF for Syntax Specifications: ABNF](https://datatracker.ietf.org/doc/html/rfc5234)

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

use crate::edb::{Attribute, AttributeKind, Database, DbValidation, Predicate, Relation};
use crate::error::{Error, Result};
use crate::eval::Table;
use crate::features::FeatureSet;
use crate::idb::{Atom, Literal, Rule, Term, Variable};
use crate::query::Query;
use crate::syntax::*;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    features: FeatureSet,
    edb: Database,
    idb: HashSet<Rule>,
    queries: HashSet<Query>,
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

impl Default for Program {
    fn default() -> Self {
        Self {
            features: Default::default(),
            edb: Default::default(),
            idb: Default::default(),
            queries: Default::default(),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.features.is_default() {
            writeln!(f, "{}", self.features)?;
            writeln!(f)?;
        }

        write!(f, "{}", self.edb)?;

        for rule in self.idb.iter() {
            writeln!(f, "{}", rule)?;
        }
        writeln!(f)?;

        for query in self.queries() {
            writeln!(f, "{}", query)?;
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
    pub fn new_with_features(features: FeatureSet) -> Self {
        Self {
            features,
            edb: Default::default(),
            idb: Default::default(),
            queries: Default::default(),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn features(&self) -> &FeatureSet {
        &self.features
    }

    pub(crate) fn features_mut(&mut self) -> &mut FeatureSet {
        &mut self.features
    }

    // --------------------------------------------------------------------------------------------

    // pub fn environment(&self) -> &Environment {
    //     &self.environment
    // }
    //
    // pub fn environment_mut(&mut self) -> &mut Environment {
    //     &mut self.environment
    // }

    // --------------------------------------------------------------------------------------------

    pub fn database(&self) -> &Database {
        &self.edb
    }

    pub fn database_mut(&mut self) -> &mut Database {
        &mut self.edb
    }

    pub fn add_relation(&mut self, relation: Relation) {
        self.database_mut().add(relation)
    }

    pub fn make_new_relation<V: Into<Vec<AttributeKind>>>(
        &mut self,
        predicate: Predicate,
        schema: V,
    ) -> Result<Relation> {
        self.edb.make_new_relation(
            predicate,
            schema
                .into()
                .into_iter()
                .map(|a| a.into())
                .collect::<Vec<Attribute>>(),
        )
    }

    pub fn add_new_relation<V: Into<Vec<AttributeKind>>>(
        &mut self,
        predicate: Predicate,
        schema: V,
    ) -> Result<()> {
        let relation = self.make_new_relation(predicate, schema)?;
        self.database_mut().add(relation);
        Ok(())
    }

    // --------------------------------------------------------------------------------------------

    /// intensional predicate symbols
    pub fn rules(&self) -> impl Iterator<Item = &Rule> {
        self.idb.iter()
    }

    pub fn make_new_rule<H: Into<Vec<Term>>, B: Into<Vec<Literal>>>(
        &self,
        head_predicate: Predicate,
        head_terms: H,
        body: B,
    ) -> Result<Rule> {
        Ok(Rule::new_with_body(
            Atom::new(head_predicate, head_terms),
            body,
        ))
    }

    pub fn add_new_rule<H: Into<Vec<Term>>, B: Into<Vec<Literal>>>(
        &mut self,
        head_predicate: Predicate,
        head_terms: H,
        body: B,
    ) -> Result<bool> {
        let rule = self.make_new_rule(head_predicate, head_terms, body)?;
        self.add_rule(rule)
    }

    pub fn add_rule(&mut self, rule: Rule) -> Result<bool> {
        rule.check_well_formed(self.features())?;

        self.add_rule_relations(rule.head(), &rule);
        rule.literals()
            .filter_map(Literal::as_atom)
            .for_each(|a| self.add_rule_relations(a, &rule));

        rule.validate(self.database_mut())?;
        Ok(self.idb.insert(rule))
    }

    fn add_rule_relations(&mut self, atom: &Atom, rule: &Rule) {
        if !self.edb.contains(atom.predicate()) {
            let mut schema = Vec::with_capacity(atom.arity());
            for term in atom.terms() {
                match term {
                    Term::Variable(v) => schema.push(self.infer_attribute(v, rule)),
                    Term::Constant(c) => schema.push(Attribute::from(c.kind())),
                }
            }
            // TODO: propagate errors
            let relation = self
                .edb
                .make_new_relation(atom.predicate().clone(), schema)
                .unwrap();
            self.edb.add(relation);
        }
    }

    fn infer_attribute(&self, variable: &Variable, rule: &Rule) -> Attribute {
        let candidates: Vec<(&Predicate, usize)> = rule
            .literals()
            .filter_map(Literal::as_atom)
            .filter_map(|a| {
                a.terms()
                    .enumerate()
                    .filter_map(|(i, term)| term.as_variable().map(|var| (i, var)))
                    .find(|(_, var)| var == &variable)
                    .map(|(i, _)| (a.predicate(), i))
            })
            .collect();
        for (predicate, i) in candidates {
            if let Some(relation) = self.database().relation(predicate) {
                return relation.schema().get(i).unwrap().clone();
            }
        }
        Attribute::unknown()
    }

    // --------------------------------------------------------------------------------------------

    pub fn queries(&self) -> impl Iterator<Item = &Query> {
        self.queries.iter()
    }

    pub fn make_new_query<T: Into<Vec<Term>>>(
        &self,
        predicate: Predicate,
        terms: T,
    ) -> Result<Query> {
        Ok(Query::new(predicate, terms))
    }

    pub fn add_new_query<T: Into<Vec<Term>>>(
        &mut self,
        predicate: Predicate,
        terms: T,
    ) -> Result<bool> {
        let query = self.make_new_query(predicate, terms)?;
        self.add_query(query)
    }

    pub fn add_query(&mut self, query: Query) -> Result<bool> {
        query.validate(self.database_mut())?;
        Ok(self.queries.insert(query))
    }

    pub fn eval_query(&self, query: &Query) -> Result<Table> {
        Ok(self.database().matches(query.as_ref()))
    }

    pub fn eval_queries(&self) -> Vec<(&Query, Result<Table>)> {
        self.queries().map(|q| (q, self.eval_query(q))).collect()
    }

    // --------------------------------------------------------------------------------------------

    pub fn check_well_formed(&self, _features: &FeatureSet) -> Result<()> {
        let result: Result<()> = self
            .rules()
            .try_for_each(|r| r.check_well_formed(&self.features));
        result
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

//TODO: add back: pub mod environment;

pub mod error;

pub mod eval;

pub mod features;

#[cfg(feature = "parser")]
pub mod parse;

pub mod edb;

pub mod idb;

pub mod query;

mod syntax;

#[cfg(feature = "typeset")]
pub mod typeset;
