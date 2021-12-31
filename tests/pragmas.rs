mod common;

use common::quick_parser_check;

// ------------------------------------------------------------------------------------------------
// Success cases
// ------------------------------------------------------------------------------------------------

#[test]
fn test_declare_assert_success() {
    quick_parser_check("@assert human (string).", None);
}

#[test]
fn test_declare_assert_multiple_success() {
    quick_parser_check("@assert human (string, integer, boolean).", None);
}

#[test]
fn test_declare_assert_named_success() {
    quick_parser_check("@assert human (name: string, age: integer, boolean).", None);
}

#[test]
#[should_panic]
fn test_declare_assert_type_fail() {
    quick_parser_check("@assert human (name).", None);
}

#[test]
fn test_declare_infer_success() {
    quick_parser_check("@infer human (string).", None);
}

#[test]
#[should_panic]
fn test_declare_assert_no_type_fail() {
    quick_parser_check("@assert human (name:).", None);
}

#[test]
fn test_feature_success() {
    quick_parser_check("@feature(negation, comparisons).", None);
}

#[test]
fn test_include_success() {
    quick_parser_check(r#"@include("file-name")."#, None);
}

#[test]
fn test_input_success() {
    quick_parser_check(r#"@input(human, "file-name")."#, None);
}

#[test]
fn test_output_success() {
    quick_parser_check(r#"@output(mortal, "file-name")."#, None);
}

// ------------------------------------------------------------------------------------------------
// Include pragma failure cases
// ------------------------------------------------------------------------------------------------

#[test]
#[should_panic]
fn test_include_no_args() {
    quick_parser_check("@include().", None);
}

#[test]
#[should_panic]
fn test_include_too_many_args() {
    quick_parser_check("@include(\"dir-name\", \"file-name\").", None);
}

#[test]
#[should_panic]
fn test_include_invalid_args_type() {
    quick_parser_check("@include(42).", None);
}

// ------------------------------------------------------------------------------------------------
// General failures cases
// ------------------------------------------------------------------------------------------------

#[test]
#[should_panic]
fn test_unknown_pragma() {
    quick_parser_check("@include_file(\"file-name\").", None);
}
