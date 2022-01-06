mod common;

use common::quick_parser_check;

// ------------------------------------------------------------------------------------------------
// Success cases
// ------------------------------------------------------------------------------------------------

#[test]
fn test_declare_assert_success() {
    quick_parser_check(".assert human (string).", None);
}

#[test]
fn test_declare_assert_multiple_success() {
    quick_parser_check(".assert human (string, integer, boolean).", None);
}

#[test]
fn test_declare_assert_named_success() {
    quick_parser_check(".assert human (name: string, age: integer, boolean).", None);
}

#[test]
fn test_declare_infer_success() {
    quick_parser_check(".infer human (string).", None);
}

#[test]
fn test_declare_infer_from_success() {
    quick_parser_check(".assert human(string). .infer mortal from human.", None);
}

#[test]
fn test_feature_success() {
    quick_parser_check(".feature(negation, comparisons).", None);
}

#[test]
fn test_input_success() {
    quick_parser_check(
        r#".assert human(string). .input(human, "file-name")."#,
        None,
    );
}

#[test]
fn test_output_success() {
    quick_parser_check(
        r#".infer mortal(string). .output(mortal, "file-name")."#,
        None,
    );
}

// ------------------------------------------------------------------------------------------------
// Include pragma failure cases
// ------------------------------------------------------------------------------------------------

#[test]
#[should_panic]
fn test_declare_assert_no_name_fail() {
    quick_parser_check(".assert (string, integer).", None);
}

#[test]
#[should_panic]
fn test_declare_assert_no_columns_fail() {
    quick_parser_check(".assert human ().", None);
}

#[test]
#[should_panic]
fn test_declare_assert_no_type_fail() {
    quick_parser_check(".assert human (name:).", None);
}

#[test]
#[should_panic]
fn test_declare_assert_type_fail() {
    quick_parser_check(".assert human (name).", None);
}

// ------------------------------------------------------------------------------------------------
// General failures cases
// ------------------------------------------------------------------------------------------------

#[test]
#[should_panic]
fn test_unknown_pragma() {
    quick_parser_check(".include (\"file-name\").", None);
}
