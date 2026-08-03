//! End-to-end tests for the constraints language feature.
//!
//! Semantics: a constraint `:- body.` is an *integrity constraint* on
//! the final model. After all forward inference has settled, every
//! constraint's body is evaluated against the full `EDB ∪ IDB`. If any
//! binding satisfies the body, the program is **inconsistent** and the
//! evaluator returns `Err(Error::ConstraintViolated { .. })`. A
//! constraint with no satisfying binding is silently OK.

use asdi::error::Error;
use asdi::features::{FeatureSet, FEATURE_CONSTRAINTS};
use asdi::idb::eval::{Evaluator, StratifiedEvaluator};
use asdi::parse::parse_str_with_features;

fn run(src: &str) -> Result<(), Error> {
    let features = FeatureSet::default().add_support_for(&FEATURE_CONSTRAINTS);
    let program = parse_str_with_features(src, features)
        .unwrap_or_else(|e| panic!("parse failed: {}", e))
        .into_parsed();
    StratifiedEvaluator::default().inference(&program).map(|_| ())
}

/// A constraint whose body is never satisfied → inference succeeds.
#[test]
fn unsatisfied_constraint_is_ok() {
    const SRC: &str = r#".feature(constraints).
.assert person(string).

person("ann"). person("bob").

% No one is named "ghost", so this constraint can't fire.
:- person("ghost").
"#;
    let result = run(SRC);
    assert!(result.is_ok(), "constraint should not fire on this EDB, got: {:?}", result.err());
}

/// A constraint whose body *is* satisfied → `ConstraintViolated` error.
#[test]
fn satisfied_constraint_produces_violation_error() {
    const SRC: &str = r#".feature(constraints).
.assert person(string).

person("ann"). person("bob"). person("ghost").

% "ghost" IS in the EDB, so the constraint fires.
:- person("ghost").
"#;
    let err = run(SRC).expect_err("expected ConstraintViolated, got Ok");
    match err {
        Error::ConstraintViolated { constraint } => {
            assert!(
                constraint.contains("person"),
                "rendered constraint should mention the predicate, got {:?}",
                constraint
            );
        }
        other => panic!("expected ConstraintViolated, got {:?}", other),
    }
}

/// Constraint references an IDB (derived) predicate — must run AFTER
/// inference, not before.
#[test]
fn constraint_on_derived_predicate() {
    const SRC: &str = r#".feature(constraints).
.assert edge(string, string).

edge("a", "b"). edge("b", "c").

reachable(X, Y) :- edge(X, Y).
reachable(X, Z) :- reachable(X, Y), edge(Y, Z).

% Should fail because `reachable("a", "c")` IS derived.
:- reachable("a", "c").
"#;
    let err = run(SRC).expect_err("constraint on derived fact should fail");
    matches!(err, Error::ConstraintViolated { .. });
}

/// Constraint with variables (universally-quantified body).
/// `:- p(X), q(X).` means "no X can be in both p and q".
#[test]
fn constraint_with_join() {
    const SRC: &str = r#".feature(constraints).
.assert p(string).
.assert q(string).

p("a"). p("b").
q("c"). q("d").

% p and q are disjoint here → constraint OK.
:- p(X), q(X).
"#;
    assert!(run(SRC).is_ok());

    // Now make them overlap → violation.
    const SRC_BAD: &str = r#".feature(constraints).
.assert p(string).
.assert q(string).

p("a"). p("b").
q("b"). q("c").

:- p(X), q(X).
"#;
    let err = run(SRC_BAD).expect_err("overlapping p/q should violate");
    assert!(matches!(err, Error::ConstraintViolated { .. }));
}

/// Multiple constraints: the first one that fires wins (the evaluator
/// short-circuits — no point checking the rest once a violation is
/// found).
#[test]
fn multiple_constraints_short_circuit() {
    const SRC: &str = r#".feature(constraints).
.assert thing(string).

thing("ok"). thing("bad").

:- thing("nope").
:- thing("bad").
"#;
    let err = run(SRC).expect_err("second constraint should fire");
    assert!(matches!(err, Error::ConstraintViolated { .. }));
}
