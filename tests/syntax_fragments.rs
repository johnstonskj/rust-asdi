use asdi::features::{FeatureSet, FEATURE_NEGATION};
use asdi::{MaybePositive, SyntaxFragments};

mod common;
use crate::common::{quick_program_check, quick_program_check_with_options};

#[test]
fn is_positive() {
    quick_program_check_with_options(
        "foo(X, Y) :- bar(X, Z), foo(Z, Y).",
        FeatureSet::default().add_support_for(&FEATURE_NEGATION),
        |p| {
            assert!(p.is_positive());
        },
    )
}

#[test]
fn is_non_positive() {
    quick_program_check_with_options(
        "foo(X, Y) :- bar(X, Y), NOT foo(X, Z), baz(Z).",
        FeatureSet::default().add_support_for(&FEATURE_NEGATION),
        |p| {
            assert!(!p.is_positive());
        },
    )
}

// ------------------------------------------------------------------------------------------------

#[test]
fn is_linear() {
    quick_program_check("foo(X, Y) :- bar(X, Y).", |p| {
        assert!(p.is_linear());
    })
}

#[test]
fn is_non_linear() {
    quick_program_check("foo(X, Y) :- bar(X, Y), foo(Z, Y).", |p| {
        assert!(!p.is_linear());
    })
}

// ------------------------------------------------------------------------------------------------

#[test]
fn is_guarded() {
    quick_program_check("foo(X, Y) :- bar(X, Y), foo(Z, Y), guard(X, Y, Z).", |p| {
        assert!(p.is_guarded());
    })
}

#[test]
fn is_non_guarded() {
    quick_program_check("foo(X, Y) :- bar(X, Y), foo(Z, Y).", |p| {
        assert!(!p.is_guarded());
    })
}

// ------------------------------------------------------------------------------------------------

#[test]
fn is_frontier_guarded() {
    quick_program_check("foo(X, Y) :- bar(X, Z), foo(Y, Z), guard(X, Y).", |p| {
        assert!(p.is_frontier_guarded());
    })
}

#[test]
fn is_non_frontier_guarded() {
    quick_program_check("foo(X, Y) :- bar(X, Z), foo(Y, Z).", |p| {
        assert!(!p.is_frontier_guarded());
    })
}

// ------------------------------------------------------------------------------------------------

#[test]
fn is_non_recursive() {
    quick_program_check("foo(X, Y) :- bar(X, Y).", |p| {
        assert!(!p.is_recursive());
    })
}

#[test]
fn is_recursive() {
    quick_program_check("foo(X, Y) :- bar(X, Z), foo(Z, Y).", |p| {
        assert!(p.is_recursive());
    })
}
