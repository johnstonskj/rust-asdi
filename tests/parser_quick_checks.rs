use asdi::features::{
    FeatureSet, FEATURE_COMPARISONS, FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION, FEATURE_NEGATION,
};
use asdi::Program;

mod common;
use common::{quick_parser_check, quick_parser_check_with_options};

// ------------------------------------------------------------------------------------------------
// Success cases
// ------------------------------------------------------------------------------------------------

#[test]
fn test_parse_nothing() {
    quick_parser_check("", Program::default());
}

#[test]
fn test_parse_string_fact() {
    quick_parser_check("human(\"Socrates\").", None);
}

#[test]
fn test_parse_number_fact() {
    quick_parser_check("age(socrates, 42).", None);
}

#[test]
fn test_parse_boolean_fact() {
    quick_parser_check("deceased(socrates, @true).", None);
}

#[test]
fn test_parse_identifier_fact() {
    quick_parser_check("edge(a, b).", None);
}

#[test]
fn test_parse_one_rule_turnstile() {
    quick_parser_check("mortal(X) :- human(X).", None);
}

#[test]
#[should_panic]
fn test_parse_rule_expression_fail() {
    quick_parser_check("old(X) :- human(X), X > 50.", None);
}

#[test]
fn test_parse_one_rule_unicode_arrow() {
    quick_parser_check("mortal(X) ⟵ human(X).", None);
}

#[test]
fn test_parse_one_rule_ascii_arrow() {
    quick_parser_check("mortal(X) <- human(X).", None);
}

#[test]
fn test_parse_one_rule_multiples_unicode_and() {
    quick_parser_check("path(X, Y) ⟵ edge(X, Z) ∧ path(Z, Y).", None);
}

#[test]
fn test_parse_one_rule_multiples_comma() {
    quick_parser_check("path(X, Y) ⟵ edge(X, Z), path(Z, Y).", None);
}

#[test]
fn test_parse_one_rule_multiples_ampersand() {
    quick_parser_check("path(X, Y) ⟵ edge(X, Z) & path(Z, Y).", None);
}

#[test]
fn test_parse_one_rule_multiples_and() {
    quick_parser_check("path(X, Y) ⟵ edge(X, Z) AND path(Z, Y).", None);
}

#[test]
fn test_parse_query_prefixed() {
    quick_parser_check("@infer path(string, string). ?- path(X, Y).", None);
}

#[test]
fn test_parse_query_suffixed() {
    quick_parser_check("@infer path(string, string). path(X, Y)?", None);
}

#[test]
fn test_parse_pragma_assert() {
    quick_parser_check(r#"@assert human(string)."#, None);
}

#[test]
fn test_parse_pragma_assert_named() {
    quick_parser_check(r#"@assert human(name: string)."#, None);
}

// ------------------------------------------------------------------------------------------------
// Feature-gated success cases
// ------------------------------------------------------------------------------------------------

#[test]
fn test_parse_rule_expression_negated_term() {
    quick_parser_check_with_options(
        "alien(X) :- living(X), ! human(X).",
        FeatureSet::default().add_support_for(&FEATURE_NEGATION),
        None,
    );
}

#[test]
fn test_parse_rule_with_comparisons() {
    quick_parser_check_with_options(
        "old(X) :- human(X), X > 50.",
        FeatureSet::default().add_support_for(&FEATURE_COMPARISONS),
        None,
    );
}

#[test]
fn test_parse_constraint_rule() {
    quick_parser_check_with_options(
        "⊥ :- dead(X) AND alive(X).",
        FeatureSet::default().add_support_for(&FEATURE_CONSTRAINTS),
        None,
    );
    quick_parser_check_with_options(
        ":- dead(X) AND alive(X).",
        FeatureSet::default().add_support_for(&FEATURE_CONSTRAINTS),
        None,
    );
}

#[test]
fn test_parse_rule_with_disjunction() {
    quick_parser_check_with_options(
        "mother(X); father(X) :- parent(X).",
        FeatureSet::default().add_support_for(&FEATURE_DISJUNCTION),
        None,
    );
    quick_parser_check_with_options(
        "mother(X) | father(X) :- parent(X).",
        FeatureSet::default().add_support_for(&FEATURE_DISJUNCTION),
        None,
    );
    quick_parser_check_with_options(
        "mother(X) OR father(X) :- parent(X).",
        FeatureSet::default().add_support_for(&FEATURE_DISJUNCTION),
        None,
    );
    quick_parser_check_with_options(
        "mother(X) ∨ father(X) :- parent(X).",
        FeatureSet::default().add_support_for(&FEATURE_DISJUNCTION),
        None,
    );
}

// ------------------------------------------------------------------------------------------------
// Should panic cases
// ------------------------------------------------------------------------------------------------

#[test]
#[should_panic]
fn test_fail_disabled_negated_term() {
    quick_parser_check("alien(X) :- ! human(X).", None);
}

#[test]
#[should_panic]
fn test_fail_not_well_formed_1() {
    quick_parser_check("mortal(X, Y) :- human(X).", None);
}

#[test]
#[should_panic]
fn test_fail_not_well_formed_2() {
    quick_parser_check_with_options(
        "alien(X) :- ! human(X).",
        FeatureSet::default().add_support_for(&FEATURE_NEGATION),
        None,
    );
}

#[cfg(test_old)]
mod tests {
    use super::*;
    use crate::parse::parse_str;

    #[test]
    fn test_well_formed() {
        let program = parse_str(r#"reachable(n1,n3) :- link(n1,n2), reachable(n2,n3)."#);
        assert!(program.is_ok());
        let well_formed = WellFormedValidator::default().validate(&program.unwrap().into_parsed());
        assert!(well_formed.is_ok())
    }

    #[test]
    fn test_not_well_formed() {
        let program = parse_str(r#"reachable(n1,n3) :- link(n1,n2), reachable(n1,n2)."#);
        assert!(program.is_ok());
        let well_formed = WellFormedValidator::default().validate(&program.unwrap().into_parsed());
        assert_eq!(
            well_formed.err().unwrap().to_string(),
            String::from("Atom 'reachable', [line 1, column 1]: the variables 'n3' in the head do not appear in any positive form in the body.")
        );
    }

    #[test]
    #[ignore]
    fn test_well_formed_negative() {
        let program = parse_str(r#"moreThanOneHop(n1,n2) :‐ reachable(n1,n2), !link(n1,n2)."#);
        assert!(program.is_ok());
        let well_formed = WellFormedValidator::default().validate(&program.unwrap().into_parsed());
        assert!(well_formed.is_ok())
    }

    #[test]
    #[ignore]
    fn test_not_well_formed_negative() {
        let program = parse_str(r#"moreThanOneHop(n1,n2) :‐ !link(n1,n2)."#);
        assert!(program.is_ok());
        let well_formed = WellFormedValidator::default().validate(&program.unwrap().into_parsed());
        assert!(well_formed.is_ok())
    }
}
