# Crate asdi

Another Simplistic Datalog Implementation (in Rust).


![MIT License](https://img.shields.io/badge/license-mit-118811.svg)
![Minimum Rust Version](https://img.shields.io/badge/Min%20Rust-1.53-green.svg)
[![crates.io](https://img.shields.io/crates/v/asdi.svg)](https://crates.io/crates/asdi)
[![docs.rs](https://docs.rs/asdi/badge.svg)](https://docs.rs/asdi)
[![Book](https://github.com/johnstonskj/rust-asdi/actions/workflows/gh-pages.yml/badge.svg)](https://github.com/johnstonskj/rust-asdi/actions/workflows/gh-pages.yml)
[![Build](https://github.com/johnstonskj/rust-asdi/workflows/Rust/badge.svg)](https://github.com/johnstonskj/rust-asdi/actions/workflows/rust.yml)
[![Audit](https://github.com/johnstonskj/rust-asdi/workflows/Security%20audit/badge.svg)]((https://github.com/johnstonskj/rust-asdi/actions/workflows/cargo-audit.yml))

This package provides a data model to represent [Datalog](https://en.wikipedia.org/wiki/Datalog)
programs in memory, a parser for the textual representation, and some evaluation implementations. For more
information see the [ASDI book](https://simonkjohnston.life/rust-asdi/).

The text representation parser is a separate feature, so if you only need to construct and evaluate
programs using the API you may opt out of the [Pest](https://pest.rs) parser and support.

# Status

1. Library **API** mostly stable, the next effort will be to make the construction API more ergonomic.
2. Library **Documentation** good top-level documentation but very little else right now.
4. **Parser** full support for the core language as well as pragmas, require more unit tests.
5. **I/O** relations are now connected to the file input/output pragmas, the `io` module includes traits for 
   reading/writing relations as well as basic JSON and CSV support.
6. **Evaluation** currently have a naïve and a stratified semi-naïve implementation.

## Example

```datalog
parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ⋀ parent(Z, Y).

?- ancestor(xerces, X).
```

```rust
use asdi::edb::{Attribute, Predicate};
use asdi::idb::{Atom, Query, Term, Variable};
use asdi::Program;
use std::str::FromStr;

fn ancestor_example() {
    // See https://en.wikipedia.org/wiki/Datalog

    let mut ancestors = Program::default();

    let parent_predicate = Predicate::from_str("parent").unwrap();
    {
        let parent = ancestors
            .add_new_relation(
                parent_predicate.clone(),
                vec![Attribute::string(), Attribute::string()],
            )
            .unwrap();
        parent
            .add_as_fact(["xerces".into(), "brooke".into()])
            .unwrap();
        parent
            .add_as_fact(["brooke".into(), "damocles".into()])
            .unwrap();
    };

    let ancestor_predicate = Predicate::from_str("ancestor").unwrap();

    let var_x: Term = Variable::from_str("X").unwrap().into();
    let var_y: Term = Variable::from_str("Y").unwrap().into();
    let var_z: Term = Variable::from_str("Z").unwrap().into();

    ancestors
        .add_new_rule(
            ancestor_predicate.clone(),
            [var_x.clone(), var_y.clone()],
            [Atom::new(parent_predicate.clone(), [var_x.clone(), var_y.clone()]).into()],
        )
        .unwrap();
    ancestors
        .add_new_rule(
            ancestor_predicate.clone(),
            [var_x.clone(), var_y.clone()],
            [
                Atom::new(parent_predicate, [var_x.clone(), var_z.clone()]).into(),
                Atom::new(ancestor_predicate.clone(), [var_z, var_y]).into(),
            ],
        )
        .unwrap();

    ancestors
        .add_new_query(ancestor_predicate, ["xerces".into(), var_x])
        .unwrap();

    println!(">{}<", ancestors);
}
```

## Crate features

| Feature    | Default      | Enables                                                       |
|------------|--------------|---------------------------------------------------------------|
| `graphviz` | Yes          | Graph representation for dependency graphs and stratification |
| `parser`   | Yes          | Parsing of textual representation                             |
| `tabular`  | Yes          | Tabular output for views                                      |
| `io`       | Yes          | collects all the common I/O formats                           |
| `io_csv`   | _Indirectly_ | Delimited line format support                                 |
| `io_json`  | _Indirectly_ | JSON format support                                           |
| `io_text`  | _Indirectly_ | Native text format (write only) support                       |

## Changes

**Version 0.2.5**

* The focus is largely on documentation, both rustdoc and mdbook.
* Book now contains much better concrete documentation and UML for most major model areas.
* Some API changes as the documentation uncovered inconsistencies.
* Significant changes in the visitor and writer areas.

**Version 0.2.4**

* Completed work to support stratification, including precedence graphs.
* Added new `StratifiedEvaluator` implementation that performs stratification as well as using the semi-naive evaluation
  scheme.
* Added [Criterion](https://bheisler.github.io/criterion.rs/book/index.html)-based benchmarks for evaluation implementations.
* Separated query trait from relational operations.
* Minor changes in API for consistency.

**Version 0.2.3**

* Added new "*=" operator for regular expressions
* Renamed the variants of `LiteralInner`/`Literal` to be `Relational` and `Arithmetic` to convey meaning, not just type.
* Renamed `Relations` to `RelationSet` and `Rules` to `RuleSet`.
* Renamed `PredicateSet` to `NameReferenceSet` and used for variables also; this also added
  `AttributeNameRef` type for clarity.
* Updated the highlight.js datalog language definition for comment and operator changes.
* Replaced more uses of the term "comparison (operator)?" with arithmetic literal
* Fixed more Clippy warnings.

**Version 0.2.2**

* Made changes to syntax to align more closely with traditional Datalog, as well as [Soufflé](https://souffle-lang.github.io/):
  * using `%` instead of `#` for line comments,
  * added `/*` and `*/` for block comments,
  * removed the need for a `@` prefix on the boolean constants `true` and `false`,
  * using `.` instead of `@` for pragmas.
  * ASDI still uses separate `assert` and `infer` pragmas rather than Soufflé's single `decl`. 
  * Also, to keep pragmas regular with other statements they end in a `.` whereas in Soufflé they do not.
* Fixed parsing errors in CSV and JSON files, mainly parsing strings (include/exclude quotes).

**Version 0.2.1**

* Added traits for consistency across types.
* Added a predicate cache to reduce duplication.
* Finished the file input and output for relations.
* Cleaned up the error module and added functions for error construction.

**Version 0.2.0**

* New internal structures and library layout.
* Less cloning, but still too much.
* Finished top-level documentation, more to come. 

**Version 0.1.0**

* Not published, used for own verification only.

**Version 0.1.0-dev**

* Initial release, mostly to verify CI infrastructure through to crates.io
