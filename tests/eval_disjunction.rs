//! End-to-end tests for the disjunction language feature.
//!
//! Semantics (this implementation):
//!   `H1 ; H2 ; ... ; Hn :- body.`
//! is evaluated as the union of `H1 :- body. H2 :- body. ...` — i.e.
//! every head atom is derived from every binding of the body.
//!
//! This is the only sound bottom-up reading of inclusive disjunction:
//! the resulting model contains every `Hi` and therefore satisfies the
//! disjunction `H1 ∨ … ∨ Hn`. (Minimal-model / stable-model semantics
//! would let you pick a smaller subset, but those are NP-hard and
//! deliberately out of scope here.)

use asdi::edb::Predicate;
use asdi::features::{FeatureSet, FEATURE_DISJUNCTION};
use asdi::idb::eval::{Evaluator, StratifiedEvaluator};
use asdi::parse::parse_str_with_features;
use asdi::{Collection};
use std::str::FromStr;

fn evaluate(src: &str) -> asdi::edb::RelationSet {
    let features = FeatureSet::default().add_support_for(&FEATURE_DISJUNCTION);
    let program = parse_str_with_features(src, features)
        .unwrap_or_else(|e| panic!("parse failed: {}", e))
        .into_parsed();
    StratifiedEvaluator::default()
        .inference(&program)
        .expect("inference should succeed")
}

fn count(derived: &asdi::edb::RelationSet, predicate: &str) -> usize {
    let p = Predicate::from_str(predicate).unwrap();
    derived.get(&p).map(|r| r.len()).unwrap_or(0)
}

/// Single binding, two heads → 2 derived facts (one per head atom).
#[test]
fn two_heads_one_binding() {
    const SRC: &str = r#".feature(disjunction).
.assert seed(string).

seed("x").

a(X) ; b(X) :- seed(X).
"#;
    let derived = evaluate(SRC);
    assert_eq!(count(&derived, "a"), 1, "a should contain (\"x\")");
    assert_eq!(count(&derived, "b"), 1, "b should contain (\"x\")");
}

/// Multiple bindings × multiple heads → each head gets every binding.
#[test]
fn three_heads_multiple_bindings() {
    const SRC: &str = r#".feature(disjunction).
.assert seed(string).

seed("x"). seed("y"). seed("z").

a(X) ; b(X) ; c(X) :- seed(X).
"#;
    let derived = evaluate(SRC);
    assert_eq!(count(&derived, "a"), 3);
    assert_eq!(count(&derived, "b"), 3);
    assert_eq!(count(&derived, "c"), 3);
}

/// Disjunctive rule across two extensional relations — exercises that
/// disjunctive heads are derived from the *natural join* binding, not
/// from one source side only.
#[test]
fn disjunction_with_join_body() {
    const SRC: &str = r#".feature(disjunction).
.assert person(string).
.assert likes(string, string).

person("ann"). person("bob").
likes("ann", "cats"). likes("bob", "dogs").

owner(X, Y) ; fan(X, Y) :- person(X), likes(X, Y).
"#;
    let derived = evaluate(SRC);
    assert_eq!(count(&derived, "owner"), 2);
    assert_eq!(count(&derived, "fan"), 2);
}

/// A recursive disjunctive rule — exercise that disjunctive heads play
/// nicely with the semi-naive delta loop.
///
/// `successor(X) ; orphan(X) :- node(X).` is non-recursive in this
/// stratum, but the multi-head derivation should still drop two facts
/// per node across two relations.
#[test]
fn disjunction_in_multi_stratum() {
    const SRC: &str = r#".feature(disjunction).
.assert edge(integer, integer).

edge(1, 2). edge(2, 3).

node(X) :- edge(X, _).
node(Y) :- edge(_, Y).

leaf(X) ; root(X) :- node(X).
"#;
    let derived = evaluate(SRC);
    assert_eq!(count(&derived, "node"), 3, "nodes: 1, 2, 3");
    assert_eq!(count(&derived, "leaf"), 3);
    assert_eq!(count(&derived, "root"), 3);
}
