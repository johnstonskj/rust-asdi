mod common;
use crate::common::quick_parser_check;

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
