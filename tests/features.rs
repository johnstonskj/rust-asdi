mod common;
use crate::common::quick_parser_check;

#[test]
#[ignore]
#[should_panic]
fn parse_unknown_feature() {
    quick_parser_check("@feature(no_empty_promises).", None)
}
