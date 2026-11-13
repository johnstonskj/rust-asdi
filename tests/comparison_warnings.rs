//! Regression tests for the §B9 / §1.9 fix: tautological / contradictory
//! arithmetic comparisons are *warnings*, not hard errors, per the spec's
//! `WARN_ARITHMETIC_LITERAL_IS_ALWAYS_TRUE/FALSE`.

use asdi::idb::{Comparison, ComparisonOperator, Term, Variable};
use std::rc::Rc;
use std::str::FromStr;

/// `X = X` used to be `Err(comparison_is_always_true(...))`. It's a warning
/// now, so `Comparison::new` returns `Ok`.
#[test]
fn tautology_on_variables_is_ok() {
    let x: Term = Rc::new(Variable::from_str("X").unwrap()).into();
    let result = Comparison::new(x.clone(), ComparisonOperator::Equal, x);
    assert!(
        result.is_ok(),
        "X = X should be a WARN, not an Err: {:?}",
        result.err()
    );
}

#[test]
fn contradiction_on_variables_is_ok() {
    let x: Term = Rc::new(Variable::from_str("X").unwrap()).into();
    let result = Comparison::new(x.clone(), ComparisonOperator::NotEqual, x);
    assert!(
        result.is_ok(),
        "X != X should be a WARN, not an Err: {:?}",
        result.err()
    );
}

#[test]
fn tautology_on_equal_constants_is_ok() {
    // 7 = 7 is a WARN, not an Err.
    let result = Comparison::new(7_i64, ComparisonOperator::Equal, 7_i64);
    assert!(result.is_ok());
}

#[test]
fn contradiction_on_unequal_constants_is_ok() {
    // 7 = 8 is a WARN, not an Err (always false).
    let result = Comparison::new(7_i64, ComparisonOperator::Equal, 8_i64);
    assert!(result.is_ok());
}

#[test]
fn incompatible_constant_types_still_err() {
    // 7 = "seven" is a hard error — different kinds. Must still fail.
    let result = Comparison::new(7_i64, ComparisonOperator::Equal, "seven");
    assert!(
        result.is_err(),
        "integer = string is a real type error, must remain Err"
    );
}
