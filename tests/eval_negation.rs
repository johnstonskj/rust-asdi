//! End-to-end tests for the `negation` language feature, evaluated via the
//! stratified evaluator. These exercise the anti-join code path added in the
//! Round 2 work.

use asdi::edb::Predicate;
use asdi::features::{FeatureSet, FEATURE_NEGATION};
use asdi::idb::eval::{Evaluator, StratifiedEvaluator};
use asdi::idb::query::{Queryable, Row};
use asdi::parse::parse_str_with_features;
use asdi::Collection;
use std::str::FromStr;

mod common;

/// The canonical text-book program for stratified negation: the transitive
/// *complement*. Compute the transitive closure `t` of an edge relation `r`,
/// then compute `tc(X, Y)` for every pair of vertices `(X, Y)` such that no
/// path exists from `X` to `Y`.
///
/// Edges:
///     a → b, b → c, d → e
///
/// Vertices: {a, b, c, d, e}
/// Paths (t): {(a,b), (b,c), (a,c), (d,e)}
/// Non-paths (tc): every (X, Y) ∈ V × V not in t  ⇒ 25 − 4 = 21 rows.
#[test]
fn transitive_complement() {
    const SRC: &str = r#".feature(negation).
.assert r(string, string).

r("a", "b").
r("b", "c").
r("d", "e").

v(X) :- r(X, _).
v(Y) :- r(_, Y).

t(X, Y) :- r(X, Y).
t(X, Y) :- t(X, Z), r(Z, Y).

tc(X, Y) :- v(X), v(Y), NOT t(X, Y).
"#;

    let features = FeatureSet::default().add_support_for(&FEATURE_NEGATION);
    let program = parse_str_with_features(SRC, features)
        .expect("program should parse")
        .into_parsed();

    let evaluator = StratifiedEvaluator::default();
    let derived = evaluator
        .inference(&program)
        .expect("inference should succeed with negation enabled");

    let p_v = Predicate::from_str("v").unwrap();
    let p_t = Predicate::from_str("t").unwrap();
    let p_tc = Predicate::from_str("tc").unwrap();

    assert_eq!(derived.get(&p_v).unwrap().len(), 5, "5 vertices");
    assert_eq!(
        derived.get(&p_t).unwrap().len(),
        4,
        "transitive closure has 4 edges"
    );
    assert_eq!(
        derived.get(&p_tc).unwrap().len(),
        21,
        "transitive complement = 5*5 - 4 = 21 pairs"
    );

    // Spot-check: (a, b) is reachable, so should NOT be in tc.
    // (c, a) is unreachable, so it SHOULD be in tc.
    let tc = derived.get(&p_tc).unwrap();
    let tc_rows: std::collections::HashSet<Vec<String>> = tc
        .iter()
        .map(|fact| fact.iter().map(|c| c.to_string()).collect())
        .collect();
    assert!(
        !tc_rows.contains(&vec!["a".into(), "b".into()]),
        "(a, b) IS reachable; should not be in tc"
    );
    assert!(
        tc_rows.contains(&vec!["c".into(), "a".into()]),
        "(c, a) is NOT reachable; should be in tc. Got rows: {:?}",
        tc_rows
    );
    assert!(
        tc_rows.contains(&vec!["a".into(), "d".into()]),
        "(a, d) is in a disconnected component; should be in tc"
    );
}

/// Simpler smoke test: a single negative literal eliminating known facts.
/// `single(X) :- candidate(X), NOT excluded(X).`
#[test]
fn anti_join_removes_excluded() {
    const SRC: &str = r#".feature(negation).
.assert candidate(string).
.assert excluded(string).

candidate("a").
candidate("b").
candidate("c").
excluded("b").

keep(X) :- candidate(X), NOT excluded(X).
"#;

    let features = FeatureSet::default().add_support_for(&FEATURE_NEGATION);
    let program = parse_str_with_features(SRC, features).unwrap().into_parsed();

    let derived = StratifiedEvaluator::default()
        .inference(&program)
        .expect("inference should succeed");

    let p_keep = Predicate::from_str("keep").unwrap();
    let keep = derived.get(&p_keep).unwrap();
    assert_eq!(keep.len(), 2, "should keep two candidates");

    let rows: std::collections::HashSet<String> = keep
        .iter()
        .map(|fact| fact.iter().next().unwrap().to_string())
        .collect();
    assert!(rows.contains("a"));
    assert!(rows.contains("c"));
    assert!(!rows.contains("b"));
}

/// Negation with a constant in the negated atom. `quiet(X) :- person(X), NOT
/// likes(X, "noise").` — should anti-join after projecting `likes` to its
/// first column.
#[test]
fn negation_with_constant_in_negated_atom() {
    const SRC: &str = r#".feature(negation).
.assert person(string).
.assert likes(string, string).

person("ann").
person("bob").
person("cal").

likes("ann", "noise").
likes("bob", "quiet").
likes("cal", "noise").

quiet(X) :- person(X), NOT likes(X, "noise").
"#;

    let features = FeatureSet::default().add_support_for(&FEATURE_NEGATION);
    let program = parse_str_with_features(SRC, features).unwrap().into_parsed();
    let derived = StratifiedEvaluator::default().inference(&program).unwrap();

    let p_quiet = Predicate::from_str("quiet").unwrap();
    let quiet = derived.get(&p_quiet).unwrap();
    assert_eq!(quiet.len(), 1, "only bob doesn't like noise");

    let names: Vec<String> = quiet
        .iter()
        .map(|fact| fact.iter().next().unwrap().to_string())
        .collect();
    assert_eq!(names, vec!["bob".to_string()]);
}

/// Re-query the derived `keep` relation through `query_atom`, to make sure
/// the result of stratified evaluation plays nicely with the Queryable API.
#[test]
fn negation_results_are_queryable() {
    const SRC: &str = r#".feature(negation).
.assert candidate(string).
.assert excluded(string).

candidate("a").
candidate("b").
candidate("c").
excluded("a").

keep(X) :- candidate(X), NOT excluded(X).

?- keep(X).
"#;

    let features = FeatureSet::default().add_support_for(&FEATURE_NEGATION);
    let program = parse_str_with_features(SRC, features).unwrap().into_parsed();
    let derived = StratifiedEvaluator::default().inference(&program).unwrap();

    let query = program.queries().iter().next().unwrap();
    let view = derived
        .query(query)
        .expect("query should not error")
        .expect("query should return some view");

    assert_eq!(view.len(), 2);
    let values: std::collections::HashSet<Vec<String>> = view
        .iter()
        .map(|row: &Row| row.iter().map(|c| c.to_string()).collect())
        .collect();
    assert!(values.contains(&vec!["b".into()]));
    assert!(values.contains(&vec!["c".into()]));
}
