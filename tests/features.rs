use crate::common::quick_parser_check;
use asdi::features::{FeatureSet, FEATURE_COMPARISONS, FEATURE_DISJUNCTION, FEATURE_NEGATION};

mod common;

#[test]
fn parse_feature() {
    quick_parser_check("@feature(constraints).", None)
}

#[test]
fn parse_more_feature() {
    quick_parser_check("@feature(constraints, disjunction).", None)
}

#[test]
#[should_panic]
fn parse_unknown_feature() {
    quick_parser_check("@feature(no_empty_promises).", None)
}

#[test]
fn test_to_label() {
    assert_eq!(FeatureSet::default().language(), "Datalog");
    assert_eq!(
        FeatureSet::default()
            .add_support_for(&FEATURE_NEGATION)
            .language(),
        "(￢)Datalog"
    );
    assert_eq!(
        FeatureSet::default()
            .add_support_for(&FEATURE_NEGATION)
            .add_support_for(&FEATURE_DISJUNCTION)
            .add_support_for(&FEATURE_COMPARISONS)
            .language(),
        "(￢,θ,∨)Datalog"
    );
}

#[test]
fn test_to_string() {
    assert_eq!(FeatureSet::default().to_string(), "");
    assert_eq!(
        FeatureSet::default()
            .add_support_for(&FEATURE_NEGATION)
            .to_string(),
        "@feature(negation)."
    );
    assert_eq!(
        FeatureSet::default()
            .add_support_for(&FEATURE_NEGATION)
            .add_support_for(&FEATURE_DISJUNCTION)
            .add_support_for(&FEATURE_COMPARISONS)
            .to_string(),
        "@feature(negation, comparisons, disjunction)."
    );
}
