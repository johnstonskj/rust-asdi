//! Regression tests for §1.7 — string escape sequence parsing.
//!
//! Spec § Strings:
//!   string-escape ::= "\\" ( DQUOTE | [tnr] )
//!                  | "\u{" HEXDIGIT{4} ( HEXDIGIT{4} )? "}" ;
//!
//! Prior to the fix:
//!   - `\n` and `\r` did not parse.
//!   - `\u{XX}` (2 hex) parsed (it shouldn't — spec requires exactly 4 or 8).
//!   - **All** escapes were stored verbatim with the backslash, never decoded.

use asdi::edb::{Constant, Predicate};
use asdi::parse::parse_str;
use asdi::{Collection, ProgramCore};
use std::str::FromStr;

/// Parse a one-fact program `p("<body>")` and return the decoded string.
fn parse_string_fact(body: &str) -> String {
    let source = format!(".assert p(string).\np(\"{}\").\n", body);
    let program = parse_str(&source)
        .unwrap_or_else(|e| panic!("parse failed for input {:?}: {}", body, e))
        .into_parsed();
    let p = Predicate::from_str("p").unwrap();
    let relation = program.extensional().get(&p).unwrap();
    let fact = relation.iter().next().expect("one fact");
    let result = match fact.iter().next().unwrap() {
        Constant::String(s) => s.clone(),
        other => panic!("expected string constant, got {:?}", other),
    };
    result
}

fn parse_fails(body: &str) {
    let source = format!(".assert p(string).\np(\"{}\").\n", body);
    assert!(
        parse_str(&source).is_err(),
        "expected parse to fail for input {:?}",
        body
    );
}

#[test]
fn plain_string_round_trips() {
    assert_eq!(parse_string_fact("hello"), "hello");
}

#[test]
fn escape_tab() {
    assert_eq!(parse_string_fact("a\\tb"), "a\tb");
}

#[test]
fn escape_newline() {
    // \n used to be a hard parse error. Now it's a real LF.
    assert_eq!(parse_string_fact("a\\nb"), "a\nb");
}

#[test]
fn escape_carriage_return() {
    assert_eq!(parse_string_fact("a\\rb"), "a\rb");
}

#[test]
fn escape_quote() {
    assert_eq!(parse_string_fact("a\\\"b"), "a\"b");
}

#[test]
fn escape_backslash() {
    assert_eq!(parse_string_fact("a\\\\b"), "a\\b");
}

#[test]
fn unicode_escape_4_hex() {
    // \u{00E9} = 'é'
    assert_eq!(parse_string_fact("caf\\u{00E9}"), "café");
}

#[test]
fn unicode_escape_8_hex() {
    // \u{0001F436} = 🐶
    assert_eq!(parse_string_fact("\\u{0001F436}"), "🐶");
}

#[test]
fn unicode_escape_2_hex_is_rejected() {
    // Spec says exactly {4} or {8} — 2-hex should be a parse error now.
    parse_fails("\\u{41}");
}

#[test]
fn unicode_escape_6_hex_is_rejected() {
    // 6 is neither 4 nor 8. Reject.
    parse_fails("\\u{0001F4}");
}
