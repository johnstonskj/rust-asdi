mod common;

use common::quick_parser_check;

// ------------------------------------------------------------------------------------------------
// Success cases
// ------------------------------------------------------------------------------------------------

#[test]
fn test_declare_success() {
    quick_parser_check("@declare human (string).", None);
}

#[test]
fn test_declare_multiple_success() {
    quick_parser_check("@declare human (string, integer, boolean).", None);
}

#[test]
fn test_declare_named_success() {
    quick_parser_check(
        "@declare human (name: string, age: integer, boolean).",
        None,
    );
}

#[test]
fn test_feature_success() {
    quick_parser_check("@feature(negation, comparisons).", None);
}

#[test]
fn test_include_success() {
    quick_parser_check("@include \"file-name\".", None);
}

#[test]
fn test_input_success() {
    quick_parser_check("@input human \"file-name\".", None);
}

#[test]
fn test_output_success() {
    quick_parser_check("@output mortal \"file-name\".", None);
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
