use asdi::io::Format;
use asdi::parse::parse_str;
use std::path::PathBuf;

#[test]
fn test_input_success_default() {
    let program = parse_str(
        r#"@assert human(string).
@input(human, "file-name")."#,
    )
    .unwrap()
    .into_parsed();

    let relation = program
        .extensional()
        .get(&program.predicates().fetch("human").unwrap())
        .unwrap();
    assert!(relation.has_file_pragma());
    let pragma = relation.file_pragma().unwrap();
    assert_eq!(pragma.file_name(), &PathBuf::from("file-name"));
    assert_eq!(
        pragma.file_format(),
        &Format::DelimitedLines(Default::default())
    )
}

#[test]
fn test_input_success_json() {
    let program = parse_str(
        r#"@assert human(string).
@input(human, "file-name", "json")."#,
    )
    .unwrap()
    .into_parsed();

    let relation = program
        .extensional()
        .get(&program.predicates().fetch("human").unwrap())
        .unwrap();
    assert!(relation.has_file_pragma());
    let pragma = relation.file_pragma().unwrap();
    assert_eq!(pragma.file_name(), &PathBuf::from("file-name"));
    assert_eq!(pragma.file_format(), &Format::Json(Default::default()))
}

#[test]
fn test_output_success_default() {
    let program = parse_str(
        r#"@infer mortal(string).
@output(mortal, "file-name")."#,
    )
    .unwrap()
    .into_parsed();

    let relation = program
        .intensional()
        .get(&program.predicates().fetch("mortal").unwrap())
        .unwrap();
    assert!(relation.has_file_pragma());
    let pragma = relation.file_pragma().unwrap();
    assert_eq!(pragma.file_name(), &PathBuf::from("file-name"));
    assert_eq!(
        pragma.file_format(),
        &Format::DelimitedLines(Default::default())
    )
}

#[test]
fn test_output_success_json() {
    let program = parse_str(
        r#"@infer mortal(string).
@output(mortal, "file-name", "json")."#,
    )
    .unwrap()
    .into_parsed();

    let relation = program
        .intensional()
        .get(&program.predicates().fetch("mortal").unwrap())
        .unwrap();
    assert!(relation.has_file_pragma());
    let pragma = relation.file_pragma().unwrap();
    assert_eq!(pragma.file_name(), &PathBuf::from("file-name"));
    assert_eq!(pragma.file_format(), &Format::Json(Default::default()))
}

#[test]
fn test_invalid_format() {
    let program = parse_str(
        r#"@assert human(string).
@input(human, "file-name", "arrow")."#,
    );

    assert!(program.is_err());

    println!("{:?}", program);
}
