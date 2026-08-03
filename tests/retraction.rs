//! Spec § Relations & Facts — Retraction.
//!
//! ```text
//! fact       ::= assertion | retraction ;
//! assertion  ::= fact-body "." ;
//! retraction ::= fact-body "~" ;
//! ```
//!
//! Asserting and then retracting a fact within the same source should
//! leave the EDB without that fact. Retracting a fact that was never
//! asserted is a no-op (not an error). Mixing terminators inside one
//! relation is legal.

use asdi::edb::Predicate;
use asdi::parse::parse_str;
use asdi::{Collection, ProgramCore};
use std::str::FromStr;

fn fact_count(program: &asdi::Program, name: &str) -> usize {
    let pred = Predicate::from_str(name).unwrap();
    program
        .extensional()
        .get(&pred)
        .map(|r| r.iter().count())
        .unwrap_or(0)
}

/// Assert then retract the same fact — nothing should remain.
#[test]
fn retract_after_assert_removes_fact() {
    const SRC: &str = "
        person(\"alice\").
        person(\"alice\")~
    ";
    let program = parse_str(SRC).expect("parse").into_parsed();
    assert_eq!(
        fact_count(&program, "person"),
        0,
        "asserted-then-retracted fact should be gone"
    );
}

/// Asserted but never retracted — still there.
#[test]
fn assert_alone_keeps_fact() {
    const SRC: &str = "person(\"alice\").\n";
    let program = parse_str(SRC).expect("parse").into_parsed();
    assert_eq!(fact_count(&program, "person"), 1);
}

/// Retracting a non-existent fact is a no-op, not an error.
#[test]
fn retract_without_assert_is_noop() {
    const SRC: &str = "person(\"ghost\")~\n";
    let result = parse_str(SRC);
    assert!(
        result.is_ok(),
        "retracting a non-existent fact should not be an error, got: {:?}",
        result.err()
    );
    // The relation may or may not exist depending on how the parser
    // handles dangling retractions; either way it should be empty.
    assert_eq!(fact_count(&result.unwrap().into_parsed(), "person"), 0);
}

/// Mixed sequence — assert two, retract one, assert another.
#[test]
fn mixed_assert_retract_sequence() {
    const SRC: &str = "
        animal(\"cat\").
        animal(\"dog\").
        animal(\"cat\")~
        animal(\"hamster\").
    ";
    let program = parse_str(SRC).expect("parse").into_parsed();
    assert_eq!(
        fact_count(&program, "animal"),
        2,
        "expected `dog` and `hamster` to survive"
    );

    // Verify the *right* facts survived, not just the count.
    let pred = Predicate::from_str("animal").unwrap();
    let rel = program.extensional().get(&pred).expect("relation exists");
    let values: Vec<String> = rel
        .iter()
        .flat_map(|f| f.iter().map(|c| c.to_string()))
        .collect();
    assert!(values.iter().any(|v| v.contains("dog")));
    assert!(values.iter().any(|v| v.contains("hamster")));
    assert!(
        !values.iter().any(|v| v.contains("cat")),
        "`cat` was retracted but is still present: {:?}",
        values
    );
}

/// Retraction integrates with the public Relation API too — not just
/// the parser. (This exercises the same code path through the
/// programmatic surface.)
#[test]
fn relation_retract_api_round_trip() {
    use asdi::edb::Constant;
    let mut program = asdi::Program::default();
    let pred = program.predicates().fetch("color").unwrap();
    let red: Constant = "red".to_string().into();

    let rel = program
        .extensional_mut()
        .add_new_relation_from(pred.clone(), &[red.clone()])
        .unwrap();
    assert!(rel.add_as_fact([red.clone()]).unwrap());
    assert_eq!(rel.iter().count(), 1);

    // First retract: hit — returns true.
    assert!(rel.retract_as_fact([red.clone()]).unwrap());
    assert_eq!(rel.iter().count(), 0);

    // Second retract: miss — returns false, not an error.
    assert!(!rel.retract_as_fact([red]).unwrap());
}
