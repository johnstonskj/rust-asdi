//! Spec-aligned `.pragma <name>[=<value>].` syntax tests.
//!
//! Per the spec (§ Processing Instructions, § Pragmas):
//! ```text
//! pi-pragma ::= "pragma" predicate ( "=" constant )? ;
//! ```
//!
//! Known names that are feature flags (`negation`, `comparisons`,
//! `constraints`, `disjunction`, `functional_dependencies`,
//! `extended_numerics`) enable the corresponding feature.
//!
//! Known names that are program-level settings (`strict`, `base`,
//! `results`) set the corresponding field on `Program`.
//!
//! Unknown names emit a `tracing::warn!` (lax mode) — once strict mode
//! is enabled, they would error. We don't assert on tracing output here
//! (no subscriber wired up in tests), but we *do* assert the legacy
//! `.feature(...)` form keeps working in parallel.
//!
//! NB: `.pragma functional_dependencies` is *technically* a feature
//! pragma but the FD grammar production (`.fd` PI) is independent of
//! the feature flag — they're separately gated — so we don't exercise
//! it in this file.

use asdi::features::{
    FEATURE_COMPARISONS, FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION, FEATURE_EXTENDED_NUMERICS,
    FEATURE_NEGATION,
};
use asdi::parse::parse_str;
use asdi::ProgramCore;

/// `.pragma negation.` enables FEATURE_NEGATION on the program.
#[test]
fn pragma_negation_enables_feature() {
    const SRC: &str = ".pragma negation.\n";
    let program = parse_str(SRC).expect("parse").into_parsed();
    assert!(program.features().supports(&FEATURE_NEGATION));
}

/// All five feature-flag pragmas wire through correctly.
#[test]
fn all_feature_pragmas() {
    const SRC: &str = "
        .pragma negation.
        .pragma comparisons.
        .pragma constraints.
        .pragma disjunction.
        .pragma extended_numerics.
    ";
    let program = parse_str(SRC).expect("parse").into_parsed();
    let f = program.features();
    assert!(f.supports(&FEATURE_NEGATION));
    assert!(f.supports(&FEATURE_COMPARISONS));
    assert!(f.supports(&FEATURE_CONSTRAINTS));
    assert!(f.supports(&FEATURE_DISJUNCTION));
    assert!(f.supports(&FEATURE_EXTENDED_NUMERICS));
}

/// `.pragma strict.` sets the flag-style program-level setting.
#[test]
fn pragma_strict_sets_strict_mode() {
    const SRC: &str = ".pragma strict.\n";
    let program = parse_str(SRC).expect("parse").into_parsed();
    assert!(
        program.is_strict(),
        "expected is_strict() to be true after `.pragma strict.`"
    );
}

/// `.pragma base = "<uri>".` records the URI for later use by the
/// I/O resolver. We assert only the storage round-trip.
#[test]
fn pragma_base_uri_round_trip() {
    const SRC: &str = ".pragma base = \"https://example.org/datalog/\".\n";
    let program = parse_str(SRC).expect("parse").into_parsed();
    assert_eq!(
        program.base_uri(),
        Some("https://example.org/datalog/"),
        "base URI should round-trip through the parser"
    );
}

/// `.pragma results = tabular.` records the format identifier.
#[test]
fn pragma_results_format() {
    const SRC: &str = ".pragma results = \"tabular\".\n";
    let program = parse_str(SRC).expect("parse").into_parsed();
    assert_eq!(program.results_format(), Some("tabular"));
}

/// The legacy `.feature(...)` form still works.
#[test]
fn legacy_feature_pragma_still_works() {
    const SRC: &str = ".feature(negation, constraints).\n";
    let program = parse_str(SRC).expect("parse").into_parsed();
    assert!(program.features().supports(&FEATURE_NEGATION));
    assert!(program.features().supports(&FEATURE_CONSTRAINTS));
}

/// Unknown pragmas in lax mode are silently ignored (warn-only).
/// Parse should succeed.
#[test]
fn unknown_pragma_lax_is_tolerated() {
    const SRC: &str = ".pragma my_custom_thing.\n";
    let result = parse_str(SRC);
    assert!(
        result.is_ok(),
        "unknown pragma should not error in lax mode, got: {:?}",
        result.err()
    );
}

/// Unknown pragmas in *strict* mode (set earlier in the same source)
/// become an error.
#[test]
fn unknown_pragma_in_strict_mode_errors() {
    const SRC: &str = "
        .pragma strict.
        .pragma my_custom_thing.
    ";
    let result = parse_str(SRC);
    assert!(
        result.is_err(),
        "unknown pragma should error after `.pragma strict.`"
    );
}

/// Round 6 §1.1-follow-up: strict mode also forbids facts for
/// undeclared relations. Without `.assert`, the parser used to silently
/// infer the schema — friendly to one-liners, but exactly the kind of
/// thing the spec's strict mode is designed to catch (typos in
/// predicate names becoming brand-new relations).
#[test]
fn strict_mode_rejects_facts_for_undeclared_relations() {
    const SRC: &str = "
        .pragma strict.
        person(\"alice\").
    ";
    let result = parse_str(SRC);
    assert!(
        result.is_err(),
        "strict mode should reject `person(\"alice\").` with no preceding .assert"
    );
}

/// In strict mode an *explicit* `.assert` declaration permits the
/// fact (round-trip sanity).
#[test]
fn strict_mode_accepts_facts_after_assert() {
    const SRC: &str = "
        .pragma strict.
        .assert person(name: string).
        person(\"alice\").
    ";
    let result = parse_str(SRC);
    assert!(
        result.is_ok(),
        "strict mode should accept facts for declared relations, got: {:?}",
        result.err()
    );
}

/// Without strict mode the old auto-inference path is preserved.
#[test]
fn lax_mode_still_auto_infers_relations() {
    const SRC: &str = "person(\"alice\").\n";
    let result = parse_str(SRC);
    assert!(
        result.is_ok(),
        "lax mode (default) should still auto-infer relation schemas"
    );
}

/// Mixing universal-pragma and legacy-feature forms in the same source
/// (sanity check — they should compose).
#[test]
fn mixed_legacy_and_spec_forms() {
    const SRC: &str = "
        .pragma negation.
        .feature(disjunction).
        .pragma base = \"file:///tmp/\".
    ";
    let program = parse_str(SRC).expect("parse").into_parsed();
    assert!(program.features().supports(&FEATURE_NEGATION));
    assert!(program.features().supports(&FEATURE_DISJUNCTION));
    assert_eq!(program.base_uri(), Some("file:///tmp/"));
}
