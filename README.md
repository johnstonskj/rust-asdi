# Crate asdi

Another Simplistic Datalog Implementation (in Rust).


![MIT License](https://img.shields.io/badge/license-mit-118811.svg)
![Minimum Rust Version](https://img.shields.io/badge/Min%20Rust-1.53-green.svg)
[![crates.io](https://img.shields.io/crates/v/asdi.svg)](https://crates.io/crates/asdi)
[![docs.rs](https://docs.rs/asdi/badge.svg)](https://docs.rs/asdi)
![Build](https://github.com/johnstonskj/rust-asdi/workflows/Rust/badge.svg)
![Audit](https://github.com/johnstonskj/rust-asdi/workflows/Security%20audit/badge.svg)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/rust-asdi.svg)](https://github.com/johnstonskj/rust-asdi/stargazers)

This package provides a data model to represent [Datalog](https://en.wikipedia.org/wiki/Datalog)
programs in memory, a parser for the textual representation, and some evaluation implementations.

The text representation parser is a separate feature, so if you only need to construct and evaluate
programs using the API you may opt out of the [Pest](https://pest.rs) parser and support.

# Status

1. Library **API** mostly stable, but minimal, will try and make the construction API more ergonomic.
2. Library **Documentation** good top-level documentation but very little else right now.
3. Library **validation checking** basic checking, but some is done in the parser and needs to be in the library.
4. **Parser** full support for the core language as well as pragmas, require more unit tests.
5. **I/O** relations are now connected to the file input/output pragmas, the `io` module includes traits for 
   reading/writing relations as well as basic JSON and CSV support.
6. **Evaluation** current evaluator is basically top-down brute force and does not support any additional language features.

# Example

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

## Changes

**Version 0.2.2**

Made changes to syntax to align more closely with traditional Datalog, as well as [Soufflé](https://souffle-lang.github.io/):

* using `%` instead of `#` for line comments,
* added `/*` and `*/` for block comments,
* removed the need for a `@` prefix on the boolean constants `true` and `false`,
* using `.` instead of `@` for pragmas.

ASDI still uses separate `assert` and `infer` pragmas rather than Soufflé's single `decl`. Also, to keep pragmas regular
with other statements they end in a `.` whereas in Soufflé they do not.

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
