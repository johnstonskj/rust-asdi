mod common;

use asdi::{Pragma, Program};
use common::quick_parser_check;
use std::path::PathBuf;

// ------------------------------------------------------------------------------------------------
// Success cases
// ------------------------------------------------------------------------------------------------

#[test]
fn test_include_success() {
    quick_parser_check("@include(\"file-name\").", None);
}

#[test]
fn test_to_string_success() {
    let mut program = Program::default();
    program
        .push(Pragma::Include(PathBuf::from("file-name")))
        .unwrap();
    assert_eq!(
        program.to_string(),
        String::from("@include(\"file-name\").\n")
    )
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
