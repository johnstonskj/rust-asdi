//! Correctness checks for the semi-naive stratified evaluator.
//!
//! Semi-naive is an *optimisation* — it must produce the exact same set of
//! derived facts as a naive evaluator. These tests pick programs where the
//! delta-substitution mechanic actually does something non-trivial
//! (recursion across multiple body atoms, multi-stratum negation), and
//! assert the derived counts match what you'd compute by hand.

use asdi::edb::Predicate;
use asdi::features::{FeatureSet, FEATURE_NEGATION};
use asdi::idb::eval::{Evaluator, StratifiedEvaluator};
use asdi::parse::{parse_str, parse_str_with_features};
use asdi::{Collection};
use std::str::FromStr;

/// Classic transitive closure on a 5-node path. The body has *two*
/// recursive atoms in the rule `path(X,Z) :- path(X,Y), edge(Y,Z)` (well,
/// one recursive + one extensional), but the stratum identification
/// machinery has to thread `path` through every pass without
/// double-counting.
///
/// Edges: 1 → 2 → 3 → 4 → 5.
/// Paths: 4 (length 1) + 3 + 2 + 1 = 10 reachable pairs.
#[test]
fn transitive_closure_linear_chain() {
    const SRC: &str = r#".assert edge(integer, integer).

edge(1, 2). edge(2, 3). edge(3, 4). edge(4, 5).

path(X, Y) :- edge(X, Y).
path(X, Z) :- path(X, Y), edge(Y, Z).
"#;

    let program = parse_str(SRC).unwrap().into_parsed();
    let derived = StratifiedEvaluator::default().inference(&program).unwrap();

    let p_path = Predicate::from_str("path").unwrap();
    let path = derived.get(&p_path).unwrap();
    assert_eq!(path.len(), 10, "expected 10 reachable pairs in a 5-chain");
}

/// Self-recursive rule with two recursive body atoms (binary TC). This
/// exercises the part of the semi-naive loop that iterates over multiple
/// recursive positions: `path(X,Z) :- path(X,Y), path(Y,Z)`.
#[test]
fn transitive_closure_two_recursive_atoms() {
    const SRC: &str = r#".assert edge(integer, integer).

edge(1, 2). edge(2, 3). edge(3, 4).

path(X, Y) :- edge(X, Y).
path(X, Z) :- path(X, Y), path(Y, Z).
"#;

    let program = parse_str(SRC).unwrap().into_parsed();
    let derived = StratifiedEvaluator::default().inference(&program).unwrap();

    let p_path = Predicate::from_str("path").unwrap();
    let path = derived.get(&p_path).unwrap();
    // 1→2, 2→3, 3→4, 1→3, 2→4, 1→4 = 6 pairs.
    assert_eq!(path.len(), 6, "expected 6 reachable pairs in a 4-chain");
}

/// Cycle: a → b → c → a. Reachability should give all 9 pairs (every
/// node reaches every node, including itself). This is a stress test
/// for *not* terminating prematurely — a buggy "no new delta"
/// short-circuit could miss reflexive pairs introduced late.
#[test]
fn transitive_closure_cycle() {
    const SRC: &str = r#".assert edge(string, string).

edge("a", "b"). edge("b", "c"). edge("c", "a").

path(X, Y) :- edge(X, Y).
path(X, Z) :- path(X, Y), edge(Y, Z).
"#;

    let program = parse_str(SRC).unwrap().into_parsed();
    let derived = StratifiedEvaluator::default().inference(&program).unwrap();

    let p_path = Predicate::from_str("path").unwrap();
    let path = derived.get(&p_path).unwrap();
    assert_eq!(path.len(), 9, "3-cycle should yield all 9 pairs");
}

/// Multi-stratum: lower stratum is the transitive closure, upper is
/// negation. Validates that semi-naive in each stratum still
/// communicates correctly with the cross-stratum visibility logic.
#[test]
fn semi_naive_interacts_with_negation() {
    const SRC: &str = r#".feature(negation).
.assert edge(string, string).

edge("a", "b"). edge("b", "c"). edge("d", "e").

v(X) :- edge(X, _).
v(Y) :- edge(_, Y).

t(X, Y) :- edge(X, Y).
t(X, Z) :- t(X, Y), edge(Y, Z).

tc(X, Y) :- v(X), v(Y), NOT t(X, Y).
"#;

    let features = FeatureSet::default().add_support_for(&FEATURE_NEGATION);
    let program = parse_str_with_features(SRC, features).unwrap().into_parsed();
    let derived = StratifiedEvaluator::default().inference(&program).unwrap();

    let p_t = Predicate::from_str("t").unwrap();
    let p_tc = Predicate::from_str("tc").unwrap();
    assert_eq!(derived.get(&p_t).unwrap().len(), 4);
    assert_eq!(derived.get(&p_tc).unwrap().len(), 21);
}

/// A non-recursive rule should reach its fixpoint in pass 0 of the
/// semi-naive loop. Smoke-check that the result is correct (the
/// "skip non-recursive rules on subsequent passes" optimisation
/// doesn't drop facts).
#[test]
fn non_recursive_stratum() {
    const SRC: &str = r#".assert parent(string, string).

parent("alice", "bob"). parent("bob", "carol"). parent("carol", "dan").

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
"#;
    let program = parse_str(SRC).unwrap().into_parsed();
    let derived = StratifiedEvaluator::default().inference(&program).unwrap();

    let p_gp = Predicate::from_str("grandparent").unwrap();
    let gp = derived.get(&p_gp).unwrap();
    assert_eq!(gp.len(), 2, "(alice, carol) and (bob, dan)");
}
