//! §1.6 — TSV (`text/tab-separated-values`) is a MUST-implement format per
//! the datalog spec. These tests prove the format short-name resolves
//! correctly, the writer emits tabs, and a write→read round-trip works.

use asdi::edb::io::csv::{Options as CsvOptions, PRAGMA_ID_TSV};
use asdi::edb::io::{read_relation, write_relation, FilePragma, Format};
use asdi::edb::Predicate;
use asdi::{Collection, ProgramCore};
use std::path::PathBuf;
use std::str::FromStr;

pub mod common;
use common::make_and_evaluate_ancestors;

const TMP_DIR: &str = env!("CARGO_TARGET_TMPDIR");

/// `Format::label` should map a tab-delimited config back to `"tsv"`.
#[test]
fn label_distinguishes_tsv_from_csv() {
    let fmt_tsv = Format::DelimitedLines(CsvOptions::tab_separated());
    let fmt_csv = Format::DelimitedLines(CsvOptions::comma_separated());
    assert_eq!(fmt_tsv.label(), PRAGMA_ID_TSV);
    assert_eq!(fmt_tsv.label(), "tsv");
    assert_eq!(fmt_csv.label(), "csv");
}

/// Write a relation as TSV, then read it back; row count must match and
/// the on-disk file must actually use tab separators.
#[test]
fn tsv_write_then_read_round_trips() {
    let program = make_and_evaluate_ancestors();

    let p_ancestor = Predicate::from_str("ancestor").unwrap();
    let original = program
        .intensional()
        .get(&p_ancestor)
        .expect("ancestor relation derived");
    assert!(!original.is_empty(), "setup should derive some facts");

    // -- write -------------------------------------------------------
    let out_file = PathBuf::from(TMP_DIR).join("tsv-roundtrip.tsv");
    let pragma = FilePragma::new(
        out_file.clone(),
        Format::DelimitedLines(CsvOptions::tab_separated()),
    );
    write_relation(original, &pragma).unwrap();

    // -- verify the file actually uses tabs --------------------------
    let body = std::fs::read_to_string(&out_file).unwrap();
    assert!(
        body.contains('\t'),
        "TSV output should contain tab characters, got:\n{}",
        body
    );
    assert!(
        !body.lines().next().unwrap().contains(','),
        "first line of TSV output should not contain commas"
    );

    // -- read back -------------------------------------------------
    // `read_relation` needs a *schema* template; the easiest way is to
    // clone the original with no rows.
    let schema_only = original.clone_with_schema_only();
    let loaded = read_relation(&schema_only, &pragma).expect("read should succeed");
    assert_eq!(
        loaded.len(),
        original.len(),
        "round-trip should preserve fact count"
    );
}
