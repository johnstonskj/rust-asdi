//! Numeric-literal coverage for the spec § Numbers conformance push.
//!
//! Covers:
//! - The latent bug: plain decimals `3.14` not parsing (the old `float`
//!   rule required scientific notation).
//! - IEEE-special literals `+inf.0` / `-inf.0` per R7RS.
//! - The intentionally-not-yet-supported `+nan.0` (gated on the
//!   `OrderedFloat` migration tracked in IMPROVEMENTS.md §1.4 item 5).
//!
//! Round 6 / Round 7 boundary: Round 6 lands plain decimals + inf
//! literals; `+nan.0` and the full `decimal` type are Round 7.

use asdi::edb::{Constant, Predicate};
use asdi::parse::parse_str;
use asdi::{Collection, ProgramCore};
use std::str::FromStr;

fn single_value(src: &str) -> Option<Constant> {
    let program = parse_str(src).ok()?.into_parsed();
    let pred = Predicate::from_str("x").ok()?;
    let rel = program.extensional().get(&pred)?;
    // Bind every intermediate so the boxed iterators outlive each
    // step — chaining `.next()?.iter().next()` walks into NLL territory
    // because each `Box<dyn Iterator>` is a temporary.
    let mut facts = rel.iter();
    let fact = facts.next()?;
    let mut consts = fact.iter();
    let value = consts.next()?.clone();
    Some(value)
}

/// The Round-6 latent-bug fix: `3.14` MUST now parse as a float
/// constant. (Before the fix the grammar only matched `1.5e3`-style.)
#[test]
fn plain_decimal_literal_parses() {
    let src = "x(3.14).\n";
    let v = single_value(src).expect("plain decimal should parse");
    match v {
        Constant::Number(n) => assert!(n.is_float(), "expected float, got {:?}", n),
        other => panic!("expected Number, got {:?}", other),
    }
}

#[test]
fn scientific_notation_still_parses() {
    let src = "x(1.5e3).\n";
    let v = single_value(src).expect("scientific notation should parse");
    assert!(matches!(v, Constant::Number(n) if n.is_float()));
}

#[test]
fn negative_decimal_parses() {
    let src = "x(-2.5).\n";
    let v = single_value(src).expect("negative decimal should parse");
    match v {
        Constant::Number(n) => {
            assert!(n.is_float());
            assert_eq!(*n.as_float().unwrap(), -2.5);
        }
        other => panic!("expected Number, got {:?}", other),
    }
}

#[test]
fn positive_infinity_literal() {
    let src = "x(+inf.0).\n";
    let v = single_value(src).expect("+inf.0 should parse");
    match v {
        Constant::Number(n) => {
            assert!(n.is_float());
            assert_eq!(*n.as_float().unwrap(), f64::INFINITY);
        }
        other => panic!("expected Number, got {:?}", other),
    }
}

#[test]
fn negative_infinity_literal() {
    let src = "x(-inf.0).\n";
    let v = single_value(src).expect("-inf.0 should parse");
    match v {
        Constant::Number(n) => {
            assert!(n.is_float());
            assert_eq!(*n.as_float().unwrap(), f64::NEG_INFINITY);
        }
        other => panic!("expected Number, got {:?}", other),
    }
}

/// `+nan.0` is deliberately NOT supported in Round 6 — the current
/// `Constant::Number` wraps `NotNan<f64>` which rejects NaN. This
/// test pins the current behaviour so a future Round-7 migration to
/// `OrderedFloat` can flip it intentionally.
#[test]
fn nan_literal_not_yet_supported() {
    let src = "x(+nan.0).\n";
    let result = parse_str(src);
    assert!(
        result.is_err(),
        "+nan.0 should still fail to parse in Round 6 (see §1.4 item 5)"
    );
}

#[test]
fn plain_integer_still_parses() {
    let src = "x(42).\n";
    let v = single_value(src).expect("integer should parse");
    match v {
        Constant::Number(n) => {
            assert!(n.is_integer());
            assert_eq!(*n.as_integer().unwrap(), 42);
        }
        other => panic!("expected Number, got {:?}", other),
    }
}
