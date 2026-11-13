use crate::edb::io::{string_to_format, FilePragma, Format};
use crate::edb::{Attribute, AttributeIndex, AttributeKind, Constant, Predicate};
use crate::error::{
    attribute_does_not_exist, attribute_index_invalid, invalid_value, language_feature_disabled,
    Error, Result, SourceLocation,
};
use crate::features::{
    Feature, FeatureSet, FEATURE_COMPARISONS, FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION,
    FEATURE_EXTENDED_NUMERICS, FEATURE_FUNCTIONAL_DEPENDENCIES, FEATURE_NEGATION,
};
use crate::idb::{
    query::Query, Atom, Comparison, ComparisonOperator, Literal, Rule as DlRule, Term,
};
use crate::parse::Parsed;
use crate::syntax::{
    TYPE_NAME_CONST_BOOLEAN, TYPE_NAME_CONST_FLOAT, TYPE_NAME_CONST_INTEGER,
    TYPE_NAME_CONST_STRING,
};
use crate::{error, relation_does_not_exist, Collection, IndexedCollection, Program, ProgramCore};
use pest::iterators::{Pair, Pairs};
use pest::{Parser, Span};
use pest_derive::Parser;
use std::fmt::Debug;
use std::path::PathBuf;
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Parser)]
#[grammar = "parse/datalog.pest"]
struct Datalog;

// ------------------------------------------------------------------------------------------------
// Private Macros
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

pub(crate) fn program(
    source: &str,
    features: FeatureSet,
    from_file: Option<PathBuf>,
) -> Result<Parsed> {
    let mut parsed =
        Datalog::parse(Rule::program, source).map_err(|e| Error::ParserError(Box::new(e)))?;
    let matched_str = parsed.as_str();
    let pair = parsed.next().unwrap();

    let mut program: Program = Program::new_with_features(features);

    if let Some(file_path) = from_file {
        program.set_source_file_path(file_path);
    }

    Ok(make_parsed(
        parse_into_program(pair, program)?,
        source,
        matched_str,
    ))
}

// ------------------------------------------------------------------------------------------------
// Parser Macros
// ------------------------------------------------------------------------------------------------

macro_rules! match_then_any {
    ($input_pair:expr => $program:expr ; $outer_rule:ident ; $($rule:ident => $call:expr),+ $(; $EOI:ident)?) => {
        match $input_pair.as_rule() {
            Rule::$outer_rule =>
                match_any!(
                    $input_pair.into_inner() => $program ;
                    $(
                        $rule => $call
                    ),+
                    $(
                        ; $EOI
                    )?
                ),
            _ => unexpected_rule!($input_pair),
        }
    };
}

macro_rules! match_any {
    ($input_pairs:expr => $program:expr ; $($rule:ident => $call:expr),+ $(; $EOI:ident)?) => {
        for inner_pair in $input_pairs {
            match_one!(
                inner_pair => $program ;
                $(
                    $rule => $call
                ),+
                $(
                    ; $EOI
                )?
            );
        }
    };
}

macro_rules! match_one {
    ($input_pair:expr => $program:expr ; $($rule:ident => $call:expr),+ $(; $EOI:ident)?) => {
        match $input_pair.as_rule() {
            $(
                Rule::$rule => $call($input_pair.into_inner(), $program)?,
            )+
            $(
                Rule::$EOI => {}
            )?
            _ => unexpected_rule!($input_pair),
        }
    };
}

macro_rules! if_match {
    ($pairs:expr, $rule:ident => ($call:expr, $program:expr)) => {{
        let next = $pairs.next().unwrap();
        if_match_inner!(next, $rule => ($call, $program))
    }};
}

macro_rules! if_match_inner {
    ($pair:expr, $rule:ident => ($call:expr, $program:expr)) => {{
        if $pair.as_rule() == Rule::$rule {
            $call($pair.into_inner(), $program)?
        } else {
            unexpected_rule!($pair)
        }
    }};
}

macro_rules! if_match_str {
    ($pairs:expr, $rule:ident => $call:expr) => {{
        let next = $pairs.next().unwrap();
        if_match_str_inner!(next, $rule => $call)
    }};
}

macro_rules! if_match_str_inner {
    ($pair:expr, $rule:ident => $call:expr) => {{
        if $pair.as_rule() == Rule::$rule {
            $call($pair.as_str())?
        } else {
            unexpected_rule!($pair)
        }
    }};
}

macro_rules! if_match_string {
    ($pairs:expr, $rule:ident) => {{
        let next = $pairs.next().unwrap();
        if next.as_rule() == Rule::$rule {
            next.as_str().to_string()
        } else {
            unexpected_rule!(next)
        }
    }};
}

macro_rules! unexpected_rule {
    ($pair:expr) => {
        unreachable!(
            "Unexpected rule {:?} at '{}'.",
            $pair.as_rule(),
            $pair.as_str()
        )
    };
}

macro_rules! not_expecting_more {
    ($pairs:expr) => {
        if let Some(unexpected) = $pairs.next() {
            unexpected_rule!(unexpected)
        }
    };
}

macro_rules! pest_error {
    ($span:expr, $message:expr) => {{
        let e: pest::error::Error<Rule> = pest::error::Error::new_from_span(
            pest::error::ErrorVariant::CustomError {
                message: String::from($message),
            },
            $span,
        );
        let e = Error::ParserError(Box::new(e));
        e
    }};
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

fn make_parsed(parsed: Program, original_str: &str, matched_str: &str) -> Parsed {
    let original_len = original_str.len();
    let matched_len = matched_str.len();
    if matched_len < original_len {
        Parsed::new_with_more(parsed, original_str[matched_len..original_len].to_string())
    } else {
        Parsed::new(parsed)
    }
}

// ------------------------------------------------------------------------------------------------

fn parse_into_program(input_pair: Pair<'_, Rule>, mut program: Program) -> Result<Program> {
    match_then_any! {
        input_pair => &mut program ;
        program ;
        pragma => parse_pragma,
        fact => parse_fact,
        rule => parse_rule,
        query => parse_query ;
        EOI
    }

    Ok(program)
}

// ------------------------------------------------------------------------------------------------

fn parse_fact(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    let predicate = if_match_str!(input_pairs, predicate => |s:&str|program.predicates().fetch(s));

    // Walk remaining pairs. They're a mix of `constant`s (the fact's
    // arguments) and finally a `fact_terminator` (either `.` or `~`).
    // Per spec § Relations & Facts: `.` asserts, `~` retracts.
    let mut attributes = Vec::new();
    let mut is_retraction = false;
    for inner_pair in input_pairs {
        match inner_pair.as_rule() {
            Rule::constant => {
                attributes.push(parse_constant(inner_pair.into_inner(), program)?);
            }
            Rule::fact_terminator => {
                // `.as_str()` returns the literal `.` or `~` the user typed.
                is_retraction = inner_pair.as_str().trim() == "~";
            }
            _ => unexpected_rule!(inner_pair),
        }
    }

    // Strict-mode (spec § Strict Mode): in strict mode every relation
    // must be declared with `.assert` before any fact references it.
    // Lax mode (the default) auto-infers a schema from the first
    // fact's value kinds — convenient for one-liners, hostile to
    // typos.
    let strict = program.is_strict();
    let edb = program.extensional_mut();
    if is_retraction {
        // Per spec: retraction of a fact whose relation doesn't exist
        // is a no-op (not an error). Same for non-existent facts.
        if let Some(relation) = edb.get_mut(&predicate) {
            relation.retract_as_fact(attributes)?;
        }
        return Ok(());
    }

    let relation = if !edb.contains(&predicate) {
        if strict {
            return Err(relation_does_not_exist(predicate));
        }
        edb.add_new_relation_from(predicate.clone(), &attributes)?
    } else {
        edb.get_mut(&predicate).unwrap()
    };

    relation.add_as_fact(attributes)?;
    Ok(())
}

fn parse_rule(input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    let mut head: Vec<Atom> = Default::default();
    for inner_pair in input_pairs {
        let span = inner_pair.as_span();
        match inner_pair.as_rule() {
            Rule::rule_head => head = parse_rule_head(inner_pair.into_inner(), program)?,
            Rule::rule_body => {
                let body = parse_rule_body(inner_pair.into_inner(), program)?;
                let rule = DlRule::new(head, body);
                if let Err(e) = rule.safety_check(program.features()) {
                    return Err(pest_error!(span, e.to_string()));
                } else {
                    program.add_rule(rule)?;
                    break;
                }
            }
            _ => unexpected_rule!(inner_pair),
        }
    }
    Ok(())
}

fn parse_rule_head(input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Vec<Atom>> {
    let mut body: Vec<Atom> = Default::default();
    for inner_pair in input_pairs {
        let atom = if_match_inner!(inner_pair, atom => (parse_atom, program));
        body.push(atom);
    }
    if body.is_empty() && !program.features().supports(&FEATURE_CONSTRAINTS) {
        Err(language_feature_disabled(FEATURE_CONSTRAINTS))
    } else if body.len() > 1 && !program.features().supports(&FEATURE_DISJUNCTION) {
        Err(language_feature_disabled(FEATURE_DISJUNCTION))
    } else {
        Ok(body)
    }
}

fn parse_rule_body(input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Vec<Literal>> {
    let mut body: Vec<Literal> = Default::default();
    for inner_pair in input_pairs {
        let literal = if_match_inner!(inner_pair, literal => (parse_literal, program));
        body.push(literal);
    }
    Ok(body)
}

fn parse_pragma(input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    let inner_pair = input_pairs.into_iter().next().unwrap();

    match_one! {
        inner_pair => program ;
        pragma_assert => parse_decl_asserted_relation,
        pragma_infer => parse_decl_inferred_relation,
        pragma_fd => parse_decl_pragma_fd,
        pragma_feature => parse_decl_feature,
        pragma_input => parse_decl_input,
        pragma_output => parse_decl_output,
        pragma_pragma => parse_decl_pragma_pragma
    }

    Ok(())
}

fn parse_decl_asserted_relation(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
) -> Result<()> {
    let predicate = if_match_str!(input_pairs, predicate => |s:&str|program.predicates().fetch(s));

    let mut attributes: Vec<Attribute<Predicate>> = Default::default();
    for inner_pair in input_pairs {
        let attribute =
            if_match_inner!(inner_pair, attribute_declaration => (parse_attribute, program));
        attributes.push(attribute);
    }

    let _ = program
        .extensional_mut()
        .add_new_relation(predicate, attributes)?;

    Ok(())
}

fn parse_decl_inferred_relation(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
) -> Result<()> {
    let relation_label =
        if_match_str!(input_pairs, predicate => |s:&str|program.predicates().fetch(s));

    let mut attributes: Vec<Attribute<Predicate>> = Default::default();
    for inner_pair in input_pairs {
        match inner_pair.as_rule() {
            Rule::attribute_declaration => {
                attributes.push(parse_attribute(inner_pair.into_inner(), program)?)
            }
            Rule::predicate => {
                // .infer <foo> from <bar>.
                let other_relation_label = program.predicates().fetch(inner_pair.as_str())?;
                if let Some(from) = program.extensional().get(&other_relation_label) {
                    attributes.extend(from.schema().iter().cloned());
                } else {
                    return Err(relation_does_not_exist(other_relation_label));
                }
            }
            _ => unexpected_rule!(inner_pair),
        }
    }

    let _ = program
        .intensional_mut()
        .add_new_relation(relation_label, attributes)?;

    Ok(())
}

fn parse_decl_pragma_fd(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    if !program
        .features()
        .supports(&FEATURE_FUNCTIONAL_DEPENDENCIES)
    {
        return Err(pest_error!(
            input_pairs.next().unwrap().as_span(),
            language_feature_disabled(FEATURE_FUNCTIONAL_DEPENDENCIES).to_string()
        ));
    }

    let relation_label =
        if_match_str!(input_pairs, predicate => |s:&str|program.predicates().fetch(s));

    if program.extensional().contains(&relation_label) {
        let mut determinant = Vec::default();
        let mut dependent = Vec::default();
        let mut attributes = &mut determinant;
        {
            let schema = program.extensional().get(&relation_label).unwrap().schema();
            for inner_pair in input_pairs {
                match inner_pair.as_rule() {
                    Rule::subscript => {
                        let index = usize::from_str(inner_pair.as_str()).map_err(|e| {
                            invalid_value(
                                TYPE_NAME_CONST_INTEGER.to_string(),
                                format!("{} ({})", inner_pair.as_str(), e),
                            )
                        })?;
                        let attribute_index: AttributeIndex<Predicate> =
                            AttributeIndex::Index(index - 1);
                        if schema.contains_index(&attribute_index) {
                            attributes.push(attribute_index);
                        } else {
                            return Err(attribute_index_invalid(index));
                        }
                    }
                    Rule::predicate => {
                        let label = program.predicates().fetch(inner_pair.as_str())?;
                        let attribute_index: AttributeIndex<Predicate> =
                            AttributeIndex::Label(label);
                        if schema.contains_index(&attribute_index) {
                            attributes.push(attribute_index);
                        } else {
                            return Err(attribute_does_not_exist(inner_pair.as_str()));
                        }
                    }
                    Rule::depends_on => {
                        attributes = &mut dependent;
                    }
                    _ => unexpected_rule!(inner_pair),
                }
            }
        }
        let schema = program
            .extensional_mut()
            .get_mut(&relation_label)
            .unwrap()
            .schema_mut();
        schema.add_functional_dependency(determinant, dependent)?;
        Ok(())
    } else {
        Err(relation_does_not_exist(relation_label))
    }
}

#[allow(unused_assignments)]
fn parse_attribute(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
) -> Result<Attribute<Predicate>> {
    let mut label = None;
    for inner_pair in input_pairs {
        match inner_pair.as_rule() {
            Rule::predicate => label = Some(program.predicates().fetch(inner_pair.as_str())?),
            Rule::type_id_string => {
                return Ok(Attribute::new_inner(label, AttributeKind::String));
            }
            Rule::type_id_integer => {
                return Ok(Attribute::new_inner(label, AttributeKind::Integer));
            }
            Rule::type_id_boolean => {
                return Ok(Attribute::new_inner(label, AttributeKind::Boolean));
            }
            _ => unexpected_rule!(inner_pair),
        }
    }
    unreachable!("Expecting an attribute type.")
}

fn parse_decl_feature(input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    for inner_pair in input_pairs {
        match inner_pair.as_rule() {
            Rule::feature_id_negation => program.features_mut().add_support_for(&FEATURE_NEGATION),
            Rule::feature_id_comparisons => {
                program.features_mut().add_support_for(&FEATURE_COMPARISONS)
            }
            Rule::feature_id_disjunction => {
                program.features_mut().add_support_for(&FEATURE_DISJUNCTION)
            }
            Rule::feature_id_constraints => {
                program.features_mut().add_support_for(&FEATURE_CONSTRAINTS)
            }
            Rule::feature_id_functional_dependencies => program
                .features_mut()
                .add_support_for(&FEATURE_FUNCTIONAL_DEPENDENCIES),
            _ => unexpected_rule!(inner_pair),
        };
    }
    Ok(())
}

/// Lookup table for spec-defined pragma names that map onto a feature
/// flag. Centralising this keeps `parse_decl_pragma_pragma` (and any
/// future tooling that needs to know "is this name a feature?") DRY.
const PRAGMA_NAME_TO_FEATURE: &[(&str, &Feature)] = &[
    ("negation", &FEATURE_NEGATION),
    ("comparisons", &FEATURE_COMPARISONS),
    ("constraints", &FEATURE_CONSTRAINTS),
    ("disjunction", &FEATURE_DISJUNCTION),
    ("functional_dependencies", &FEATURE_FUNCTIONAL_DEPENDENCIES),
    ("extended_numerics", &FEATURE_EXTENDED_NUMERICS),
];

fn parse_decl_pragma_pragma(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
) -> Result<()> {
    // Grammar: `keyword_pragma ~ predicate ~ ( EQUALS ~ constant )?`.
    // The `keyword_pragma` is silent, so the first inner pair is the
    // `predicate` carrying the pragma name; an optional `constant`
    // follows for the `name=value` form.
    let name = if_match_str!(input_pairs, predicate => |s: &str| Ok::<_, Error>(s.to_owned()));

    let value: Option<Constant> = match input_pairs.next() {
        Some(inner) if inner.as_rule() == Rule::constant => {
            Some(parse_constant(inner.into_inner(), program)?)
        }
        Some(other) => unexpected_rule!(other),
        None => None,
    };

    // 1. Feature-enabling pragmas (`negation`, `comparisons`, …).
    if let Some((_, feature)) = PRAGMA_NAME_TO_FEATURE.iter().find(|(n, _)| *n == name) {
        if value.is_some() {
            tracing::warn!(
                "`.pragma {} = ...` ignores the value — feature-enabling pragmas take no argument.",
                name
            );
        }
        program.features_mut().add_support_for(feature);
        return Ok(());
    }

    // 2. Program-level setting pragmas.
    match name.as_str() {
        // `.pragma strict.` — no value form per spec.
        "strict" => {
            if value.is_some() {
                tracing::warn!("`.pragma strict = ...` ignores the value — strict is a flag.");
            }
            program.set_strict_mode(true);
        }
        // `.pragma base = "https://example.org/".`
        "base" => match value {
            Some(Constant::String(uri)) => program.set_base_uri(uri),
            Some(other) => {
                return Err(invalid_value(
                    "string",
                    &format!("{} (expected a quoted URI string)", other),
                ))
            }
            None => {
                return Err(invalid_value(
                    "string",
                    "<missing> (.pragma base requires a `= \"<uri>\"` value)",
                ))
            }
        },
        // `.pragma results = tabular.` (or `native`, …)
        "results" => match value {
            Some(Constant::String(fmt)) => program.set_results_format(fmt),
            // Per spec the value is a `constant`, but bare-word
            // identifiers also round-trip through `Constant::String`
            // via the unquoted-string production, so this is just
            // belt-and-braces.
            Some(other) => {
                return Err(invalid_value(
                    "identifier-string",
                    &format!("{}", other),
                ))
            }
            None => {
                return Err(invalid_value(
                    "identifier-string",
                    "<missing> (.pragma results requires a value)",
                ))
            }
        },
        // 3. Unknown name. In strict mode this would be an error; for
        // now we follow the spec's lax-mode behaviour and emit a
        // warning.
        other => {
            if program.is_strict() {
                return Err(invalid_value(
                    "known pragma name",
                    other,
                ));
            }
            tracing::warn!(
                "Unknown pragma `.pragma {}{}` — ignoring (enable strict mode to make this an error).",
                other,
                value
                    .as_ref()
                    .map(|v| format!(" = {}", v))
                    .unwrap_or_default()
            );
        }
    }
    Ok(())
}

fn parse_decl_input(input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    parse_decl_file_io(input_pairs, program, true)
}

fn parse_decl_output(input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    parse_decl_file_io(input_pairs, program, false)
}

/// Known parameter names for the spec-conformant `.input(...)` /
/// `.output(...)` parameter-assignment form. Centralised so adding a
/// new parameter is a one-line change.
mod io_param {
    pub(super) const URI: &str = "uri";
    pub(super) const TYPE: &str = "type";
    pub(super) const HEADER: &str = "header";
}

fn parse_decl_file_io(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    input: bool,
) -> Result<()> {
    let relation_label =
        if_match_str!(input_pairs, predicate => |s:&str|program.predicates().fetch(s));

    // Capture the source-file path and base URI *before* taking the
    // mutable borrow of the relation set, otherwise we can't read them
    // back inside the borrow scope.
    let base_path = program.source_file_path().cloned();
    let base_uri = program.base_uri().map(str::to_owned);
    let strict = program.is_strict();

    // The next pair is either an `io_parameter_list` (new spec form)
    // or an `io_positional` (legacy form). Resolve to a `FilePragma`
    // before we grab a mutable borrow of the relation.
    let details_pair = input_pairs
        .next()
        .expect("grammar guarantees io_parameter_list or io_positional");
    let raw_pragma = match details_pair.as_rule() {
        Rule::io_parameter_list => {
            parse_io_parameter_list(details_pair.into_inner(), program, strict)?
        }
        Rule::io_positional => parse_io_positional(details_pair.into_inner())?,
        _ => unexpected_rule!(details_pair),
    };

    // Resolve the URI against `base_uri` if present (spec § URI
    // References), then fall back to the source-file directory for
    // bare paths (legacy behaviour). Hardening that path to handle
    // *real* URI schemes (file://, https://, …) is tracked in §1.6;
    // for now "resolve relative to base" is plain `PathBuf::join`.
    let cwd = std::env::current_dir()?;
    let resolved_path = resolve_io_path(&raw_pragma.uri, base_uri.as_deref(), base_path.as_ref(), &cwd);

    let relations = if input {
        program.extensional_mut()
    } else {
        program.intensional_mut()
    };

    if let Some(relation) = relations.get_mut(&relation_label) {
        let format = raw_pragma.into_format()?;
        relation.set_file_pragma(FilePragma::new(resolved_path, format));
        Ok(())
    } else {
        Err(relation_does_not_exist(relation_label))
    }
}

/// In-progress I/O pragma, materialised before we know whether the
/// relation exists. Models exactly the spec's parameter-assignment
/// set we currently understand.
struct RawIoPragma {
    uri: String,
    /// Media-type string (e.g. `"text/csv"`, `"csv"`, `"json"`).
    /// `None` means "use the default format" (delimited lines).
    media_type: Option<String>,
    /// CSV header flag (per spec: `header=present|absent`). `None`
    /// means "format-default".
    header: Option<bool>,
}

impl RawIoPragma {
    fn into_format(self) -> Result<Format> {
        let mut format = match self.media_type.as_deref() {
            Some(s) => string_to_format(s)?,
            None => Format::DelimitedLines(Default::default()),
        };
        // Layer the `header=` parameter on top of the format if it's
        // a CSV/TSV (the only format where it makes sense today).
        if let Some(header) = self.header {
            #[cfg(feature = "io_csv")]
            if let Format::DelimitedLines(opts) = &mut format {
                opts.has_headers = header;
            }
        }
        Ok(format)
    }
}

fn parse_io_parameter_list(
    pairs: Pairs<'_, Rule>,
    program: &mut Program,
    strict: bool,
) -> Result<RawIoPragma> {
    let mut uri: Option<String> = None;
    let mut media_type: Option<String> = None;
    let mut header: Option<bool> = None;

    for assignment in pairs {
        if assignment.as_rule() != Rule::parameter_assignment {
            unexpected_rule!(assignment);
        }
        let mut inner = assignment.into_inner();
        let name = inner
            .next()
            .map(|p| p.as_str().to_owned())
            .expect("grammar guarantees parameter_assignment has a predicate");
        let value_pair = inner
            .next()
            .expect("grammar guarantees parameter_assignment has a constant");
        let value = parse_constant(value_pair.into_inner(), program)?;

        match name.as_str() {
            io_param::URI => uri = Some(constant_to_string(value)?),
            io_param::TYPE => media_type = Some(constant_to_string(value)?),
            io_param::HEADER => header = Some(parse_header_value(value)?),
            other => {
                // Unknown parameters: same lax/strict policy as
                // unknown pragmas — warn by default, error in strict.
                if strict {
                    return Err(invalid_value(
                        "known I/O parameter (uri, type, header)".to_string(),
                        other.to_string(),
                    ));
                }
                tracing::warn!(
                    "Unknown I/O parameter `{}` — ignoring (enable strict mode to make this an error).",
                    other
                );
            }
        }
    }

    let uri = uri.ok_or_else(|| {
        invalid_value(
            "required `uri=\"...\"` parameter".to_string(),
            "<missing> (spec § io-details requires uri)".to_string(),
        )
    })?;
    Ok(RawIoPragma {
        uri,
        media_type,
        header,
    })
}

fn parse_io_positional(mut pairs: Pairs<'_, Rule>) -> Result<RawIoPragma> {
    // Legacy form: first string is the URI/path, optional second
    // string is the media-type. No header control was possible.
    let uri = if_match_string!(pairs, string);
    let media_type = pairs.next().map(|p| p.as_str().to_owned());
    Ok(RawIoPragma {
        uri,
        media_type,
        header: None,
    })
}

/// Extract a plain string from a `Constant`. Spec § Pragmas says the
/// value side of a parameter-assignment is a `constant`; for URIs and
/// media-types we want the raw text whether the user wrote it as a
/// quoted string or a bareword (the parser flattens both into
/// `Constant::String` today — see §9.6 for the planned
/// `Constant::Identifier` split).
fn constant_to_string(c: Constant) -> Result<String> {
    match c {
        Constant::String(s) => Ok(s),
        other => Err(invalid_value(
            "string-valued I/O parameter".to_string(),
            format!("{} (expected a string or bareword)", other),
        )),
    }
}

/// `header=present|absent` is the spec spelling; we also accept the
/// Rust-ish `true|false` for convenience.
fn parse_header_value(c: Constant) -> Result<bool> {
    let expected = "`header=present` or `header=absent`".to_string();
    match c {
        Constant::Boolean(b) => Ok(b),
        Constant::String(s) => match s.as_str() {
            "present" | "true" | "yes" => Ok(true),
            "absent" | "false" | "no" => Ok(false),
            _ => Err(invalid_value(expected, s)),
        },
        other => Err(invalid_value(expected, other.to_string())),
    }
}

/// Resolve an I/O URI into a `PathBuf`. Priority:
///   1. If `base_uri` is set, join `uri` onto it (spec § URI References).
///   2. Otherwise, if the program was loaded from a source file,
///      resolve relative to that file's directory.
///   3. Otherwise, resolve relative to the current working directory.
///
/// Note: this is the lightweight "treat URIs as paths" implementation.
/// Full `file://` / `https://` scheme support is tracked in §1.6.
fn resolve_io_path(
    uri: &str,
    base_uri: Option<&str>,
    source_file: Option<&PathBuf>,
    cwd: &std::path::Path,
) -> PathBuf {
    if let Some(base) = base_uri {
        // Strip a leading `file://` so the result is a usable path
        // rather than a URI-shaped one.
        let base = base.strip_prefix("file://").unwrap_or(base);
        return PathBuf::from(base).join(uri);
    }
    if let Some(src) = source_file {
        return src.parent().unwrap_or(cwd).join(uri);
    }
    PathBuf::from(uri)
}

fn parse_query(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    let atom = if_match!(input_pairs, atom => (parse_atom, program));

    not_expecting_more!(input_pairs);

    program.add_query(Query::from(atom)).map(|_| ())
}

// ------------------------------------------------------------------------------------------------

fn parse_literal(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Literal> {
    let mut next = input_pairs.next().unwrap();
    let negative = next.as_rule() == Rule::negation;
    if negative && !program.features().supports(&FEATURE_NEGATION) {
        return Err(pest_error!(
            next.as_span(),
            language_feature_disabled(FEATURE_NEGATION).to_string()
        ));
    } else if negative {
        next = input_pairs.next().unwrap();
    }

    Ok(match next.as_rule() {
        Rule::atom => {
            let atom = parse_atom(next.into_inner(), program)?;
            if negative {
                Literal::negative_relational(atom)
            } else {
                Literal::relational(atom)
            }
        }
        Rule::comparison => {
            if !program.features().supports(&FEATURE_COMPARISONS) {
                return Err(pest_error!(
                    next.as_span(),
                    language_feature_disabled(FEATURE_COMPARISONS).to_string()
                ));
            }
            let comparison = parse_comparison(next.into_inner(), program)?;
            if negative {
                Literal::negative_arithmetic(comparison)
            } else {
                Literal::arithmetic(comparison)
            }
        }
        _ => unexpected_rule!(next),
    })
}

fn parse_atom(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Atom> {
    let first = input_pairs.next().unwrap();
    let location = pair_to_src_location(&first);
    let atom_label = if_match_str_inner!(first, predicate => |s:&str|program.predicates().fetch(s));

    let mut terms: Vec<Term> = Default::default();
    for inner_pair in input_pairs {
        let term = if_match_inner!(inner_pair, term => (parse_term, program));
        terms.push(term);
    }

    Ok(Atom::new_at_location(atom_label, &terms, location))
}

fn parse_comparison(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Comparison> {
    if !program.features().supports(&FEATURE_COMPARISONS) {
        Err(pest_error!(
            input_pairs.next().unwrap().as_span(),
            language_feature_disabled(FEATURE_COMPARISONS).to_string()
        ))
    } else {
        let left = if_match!(input_pairs, named_term => (parse_named_term, program));

        let op = if_match_str!(input_pairs, comparison_operator => |s:&str| ComparisonOperator::from_str(s));

        let right = if_match!(input_pairs, named_term => (parse_named_term, program));

        Ok(Comparison::new(left, op, right)?)
    }
}

fn parse_constant(mut input_pairs: Pairs<'_, Rule>, _: &mut Program) -> Result<Constant> {
    let first = input_pairs.next().unwrap();
    let value = match first.as_rule() {
        // Qnames / identifier-strings have no escape sequences per spec
        // § Strings; they round-trip verbatim. (See §1.5 in the design
        // notes — the qname structure is currently flattened into
        // `Constant::String`.)
        Rule::unquoted_string => Constant::String(first.as_str().to_string()),
        Rule::string => Constant::String(unescape_string(first.as_str())?),
        Rule::integer => i64::from_str(first.as_str())
            .map_err(|e| {
                invalid_value(
                    TYPE_NAME_CONST_INTEGER.to_string(),
                    format!("{} ({})", first.as_str(), e),
                )
            })?
            .into(),
        Rule::float => f64::from_str(first.as_str())
            .map_err(|e| {
                invalid_value(
                    TYPE_NAME_CONST_FLOAT.to_string(),
                    format!("{} ({})", first.as_str(), e),
                )
            })?
            .try_into()?,
        // IEEE-special literals: `+inf.0`, `-inf.0` (R7RS-style, spec §
        // Numbers). We deliberately don't take the `f64::from_str` path
        // because `"+inf.0"` is not a valid Rust f64 literal — Rust
        // accepts only `"inf"`/`"infinity"` (no sign suffix). Encode the
        // sign manually.
        Rule::float_special => {
            let raw = first.as_str();
            let value = if raw.starts_with('-') {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            };
            value.try_into()?
        }
        Rule::boolean => bool::from_str(first.as_str())
            .map_err(|_| error::invalid_value(TYPE_NAME_CONST_BOOLEAN, first.as_str()))?
            .into(),
        _ => unexpected_rule!(first),
    };

    not_expecting_more!(input_pairs);

    Ok(value)
}

fn parse_term(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Term> {
    let first = input_pairs.next().unwrap();
    let value = match first.as_rule() {
        Rule::anonymous_variable => Term::Anonymous,
        Rule::named_variable => program.variables().fetch(first.as_str())?.into(),
        Rule::constant => Term::Constant(parse_constant(first.into_inner(), program)?),
        _ => unexpected_rule!(first),
    };

    not_expecting_more!(input_pairs);

    Ok(value)
}

fn parse_named_term(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Term> {
    let first = input_pairs.next().unwrap();
    let value = match first.as_rule() {
        Rule::named_variable => program.variables().fetch(first.as_str())?.into(),
        Rule::constant => Term::Constant(parse_constant(first.into_inner(), program)?),
        _ => unexpected_rule!(first),
    };

    not_expecting_more!(input_pairs);

    Ok(value)
}

// ------------------------------------------------------------------------------------------------

fn pair_to_src_location(p: &Pair<'_, Rule>) -> SourceLocation {
    span_to_src_location(&p.as_span())
}

fn span_to_src_location(s: &Span<'_>) -> SourceLocation {
    s.start_pos().line_col().into()
}

///
/// Decode datalog-spec string escapes inside a `Rule::string` capture.
///
/// Per § Strings: one-character escapes are `\"`, `\\`, `\t`, `\n`,
/// `\r`; Unicode escapes are `\u{XXXX}` (4 hex) or `\u{XXXXXXXX}`
/// (8 hex). The pest grammar already enforces these shapes, so this
/// function's `unreachable!` branches only fire on a grammar bug.
///
fn unescape_string(raw: &str) -> Result<String> {
    let mut out = String::with_capacity(raw.len());
    let mut chars = raw.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('"') => out.push('"'),
            Some('\\') => out.push('\\'),
            Some('t') => out.push('\t'),
            Some('n') => out.push('\n'),
            Some('r') => out.push('\r'),
            Some('u') => {
                // Grammar guarantees `{` HEX{4|8} `}`.
                debug_assert_eq!(chars.next(), Some('{'));
                let mut hex = String::with_capacity(8);
                for ch in chars.by_ref() {
                    if ch == '}' {
                        break;
                    }
                    hex.push(ch);
                }
                let code = u32::from_str_radix(&hex, 16).map_err(|_| {
                    invalid_value(TYPE_NAME_CONST_STRING, format!("\\u{{{}}}", hex).as_str())
                })?;
                let ch = char::from_u32(code).ok_or_else(|| {
                    invalid_value(
                        TYPE_NAME_CONST_STRING,
                        format!("\\u{{{}}} is not a valid Unicode scalar", hex).as_str(),
                    )
                })?;
                out.push(ch);
            }
            other => unreachable!("grammar admitted unsupported escape: \\{:?}", other),
        }
    }
    Ok(out)
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
