/*!
This module provides the type [`Parsed`] that contains a program parsed from the text representation
as well as the [`parse_file`], [`parse_str`], and [`parse_str_with_features`] functions.

TBD

# Example

TBD

*/

use crate::edb::{Attribute, AttributeKind, Constant, Predicate};
use crate::error::{invalid_value, language_feature_disabled, Error, Result, SourceLocation};
use crate::features::{
    FeatureSet, FEATURE_COMPARISONS, FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION, FEATURE_NEGATION,
};
use crate::idb::{Atom, Comparison, ComparisonOperator, Literal, Query, Rule as DlRule, Term};
use crate::io::{string_to_format, FilePragma, Format};
use crate::syntax::{TYPE_NAME_CONST_BOOLEAN, TYPE_NAME_CONST_INTEGER};
use crate::{error, relation_does_not_exist, Collection, Program};
use pest::iterators::{Pair, Pairs};
use pest::{Parser, Span};
use pest_derive::Parser;
use std::fmt::Debug;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// This contains the parsed content from a source, along with any remaining, un-parsed, content.
/// This allows for parser rules that match and return but have not matched everything.
///
#[derive(Clone, Debug)]
pub struct Parsed {
    parsed: Program,
    rest: Option<String>,
}

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

///
/// Parse the file at the provided path.
///
pub fn parse_file<P: AsRef<Path>>(file_path: P) -> Result<Parsed> {
    parse_str(&read_to_string(file_path.as_ref())?)
}

///
/// Parse the file at the provided path, but enabled the set of language features first.
///
pub fn parse_file_with_features<P: AsRef<Path>>(
    file_path: P,
    features: FeatureSet,
) -> Result<Parsed> {
    parse_str_with_features(&read_to_string(file_path.as_ref())?, features)
}

///
/// Parse the string content provided.
///
pub fn parse_str(source: &str) -> Result<Parsed> {
    parse_str_with_features(source, FeatureSet::default())
}

///
/// Parse the string content provided.
///
pub fn parse_str_into(source: &str, program: Program) -> Result<Parsed> {
    let mut parsed =
        Datalog::parse(Rule::program, source).map_err(|e| Error::ParserError(Box::new(e)))?;
    let matched_str = parsed.as_str();
    let pair = parsed.next().unwrap();

    Ok(make_parsed(
        parse_program(pair, program.features)?,
        source,
        matched_str,
    ))
}

///
/// Parse the string content provided, but enabled the set of language features first.
///
pub fn parse_str_with_features(source: &str, features: FeatureSet) -> Result<Parsed> {
    let mut parsed =
        Datalog::parse(Rule::program, source).map_err(|e| Error::ParserError(Box::new(e)))?;
    let matched_str = parsed.as_str();
    let pair = parsed.next().unwrap();

    Ok(make_parsed(
        parse_program(pair, features)?,
        source,
        matched_str,
    ))
}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Parsed {
    fn new(parsed: Program) -> Self {
        Self { parsed, rest: None }
    }

    fn new_with_more(parsed: Program, rest: String) -> Self {
        if !rest.is_empty() {
            Self {
                parsed,
                rest: Some(rest),
            }
        } else {
            Self::new(parsed)
        }
    }

    pub fn parsed(&self) -> &Program {
        &self.parsed
    }

    pub fn into_parsed(self) -> Program {
        self.parsed
    }

    pub fn rest_of_input(&self) -> Option<&String> {
        self.rest.as_ref()
    }

    pub fn has_more_input(&self) -> bool {
        self.rest
            .as_ref()
            .map(|s| !s.is_empty())
            .unwrap_or_default()
    }
}

// ------------------------------------------------------------------------------------------------
// Parser Macros
// ------------------------------------------------------------------------------------------------

macro_rules! match_then_any {
    (( $input_pair:expr, $features:expr ) => $program:expr ; $outer_rule:ident ; $($rule:ident => $call:expr),+ $(; $EOI:ident)?) => {
        match $input_pair.as_rule() {
            Rule::$outer_rule =>
                match_any!(
                    ( $input_pair.into_inner(), $features ) => $program ;
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
    (( $input_pairs:expr, $features:expr ) => $program:expr ; $($rule:ident => $call:expr),+ $(; $EOI:ident)?) => {
        for inner_pair in $input_pairs {
            match_one!(
                ( inner_pair, $features ) => $program ;
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
    (( $input_pair:expr, $features:expr ) => $program:expr ; $($rule:ident => $call:expr),+ $(; $EOI:ident)?) => {
        match $input_pair.as_rule() {
            $(
                Rule::$rule => $call($input_pair.into_inner(), $program, $features)?,
            )+
            $(
                Rule::$EOI => {}
            )?
            _ => unexpected_rule!($input_pair),
        }
    };
}

macro_rules! if_match {
    ($pairs:expr, $rule:ident => ($call:expr, $program:expr, $features:expr)) => {{
        let next = $pairs.next().unwrap();
        if_match_inner!(next, $rule => ($call, $program, $features))
    }};
}

macro_rules! if_match_inner {
    ($pair:expr, $rule:ident => ($call:expr, $program:expr, $features:expr)) => {{
        if $pair.as_rule() == Rule::$rule {
            $call($pair.into_inner(), $program, $features)?
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

fn parse_program(input_pair: Pair<'_, Rule>, features: FeatureSet) -> Result<Program> {
    let program: Program = Program::new_with_features(features);

    parse_into_program(input_pair, program, features)
}

fn parse_into_program(
    input_pair: Pair<'_, Rule>,
    mut program: Program,
    features: FeatureSet,
) -> Result<Program> {
    match_then_any! {
        (input_pair, features) => &mut program ;
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

fn parse_fact(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    let predicate = if_match_str!(input_pairs, predicate => |s:&str|program.predicates().fetch(s));

    let mut attributes = Vec::new();
    for inner_pair in input_pairs {
        let constant = if_match_inner!(inner_pair, constant => (parse_constant, program, features));
        attributes.push(constant);
    }

    let edb = program.extensional_mut();
    let relation = if !edb.contains(&predicate) {
        edb.add_new_relation_from(predicate.clone(), &attributes)?
    } else {
        edb.get_mut(&predicate).unwrap()
    };

    relation.add_as_fact(attributes)
}

fn parse_rule(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    let mut head: Vec<Atom> = Default::default();
    for inner_pair in input_pairs {
        let span = inner_pair.as_span();
        match inner_pair.as_rule() {
            Rule::rule_head => head = parse_rule_head(inner_pair.into_inner(), program, features)?,
            Rule::rule_body => {
                let body = parse_rule_body(inner_pair.into_inner(), program, features)?;
                let rule = DlRule::new(head, body);
                if let Err(e) = rule.well_formed_check(&features) {
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

fn parse_rule_head(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<Vec<Atom>> {
    let mut body: Vec<Atom> = Default::default();
    for inner_pair in input_pairs {
        let atom = if_match_inner!(inner_pair, atom => (parse_atom, program, features));
        body.push(atom);
    }
    if body.is_empty() && !features.supports(&FEATURE_CONSTRAINTS) {
        Err(language_feature_disabled(FEATURE_CONSTRAINTS))
    } else if body.len() > 1 && !features.supports(&FEATURE_DISJUNCTION) {
        Err(language_feature_disabled(FEATURE_DISJUNCTION))
    } else {
        Ok(body)
    }
}

fn parse_rule_body(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<Vec<Literal>> {
    let mut body: Vec<Literal> = Default::default();
    for inner_pair in input_pairs {
        let literal = if_match_inner!(inner_pair, literal => (parse_literal, program, features));
        body.push(literal);
    }
    Ok(body)
}

fn parse_pragma(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    let inner_pair = input_pairs.into_iter().next().unwrap();

    match_one! {
        (inner_pair, features) => program ;
        pragma_assert => parse_decl_asserted_relation,
        pragma_infer => parse_decl_inferred_relation,
        pragma_feature => parse_decl_feature,
        pragma_input => parse_decl_input,
        pragma_output => parse_decl_output
    }

    Ok(())
}

fn parse_decl_asserted_relation(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    let predicate = if_match_str!(input_pairs, predicate => |s:&str|program.predicates().fetch(s));

    let mut attributes: Vec<Attribute<Predicate>> = Default::default();
    for inner_pair in input_pairs {
        let attribute = if_match_inner!(inner_pair, attribute_declaration => (parse_attribute, program, features));
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
    features: FeatureSet,
) -> Result<()> {
    let relation_label =
        if_match_str!(input_pairs, predicate => |s:&str|program.predicates().fetch(s));

    let mut attributes: Vec<Attribute<Predicate>> = Default::default();
    for inner_pair in input_pairs {
        match inner_pair.as_rule() {
            Rule::attribute_declaration => {
                attributes.push(parse_attribute(inner_pair.into_inner(), program, features)?)
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

#[allow(unused_assignments)]
fn parse_attribute(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    _: FeatureSet,
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

fn parse_decl_feature(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    _features: FeatureSet,
) -> Result<()> {
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
            _ => unexpected_rule!(inner_pair),
        };
    }

    Ok(())
}

fn parse_decl_input(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    parse_decl_file_io(input_pairs, program, features, true)
}

fn parse_decl_output(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    parse_decl_file_io(input_pairs, program, features, false)
}

fn parse_decl_file_io(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    _: FeatureSet,
    input: bool,
) -> Result<()> {
    let relation_label =
        if_match_str!(input_pairs, predicate => |s:&str|program.predicates().fetch(s));

    let relations = if input {
        program.extensional_mut()
    } else {
        program.intensional_mut()
    };

    if let Some(relation) = relations.get_mut(&relation_label) {
        let file_name = if_match_string!(input_pairs, string);

        let pragma = if let Some(format) = input_pairs.next() {
            FilePragma::new(
                PathBuf::from(&file_name),
                string_to_format(format.as_str())?,
            )
        } else {
            FilePragma::new(
                PathBuf::from(file_name),
                Format::DelimitedLines(Default::default()),
            )
        };
        relation.set_file_pragma(pragma);
        Ok(())
    } else {
        Err(relation_does_not_exist(relation_label))
    }
}

fn parse_query(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    let atom = if_match!(input_pairs, atom => (parse_atom, program, features));

    not_expecting_more!(input_pairs);

    program.add_query(Query::from(atom)).map(|_| ())
}

// ------------------------------------------------------------------------------------------------

fn parse_literal(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<Literal> {
    let mut next = input_pairs.next().unwrap();
    let negative = next.as_rule() == Rule::negation;
    if negative && !features.supports(&FEATURE_NEGATION) {
        return Err(pest_error!(
            next.as_span(),
            language_feature_disabled(FEATURE_NEGATION).to_string()
        ));
    } else if negative {
        next = input_pairs.next().unwrap();
    }

    Ok(match next.as_rule() {
        Rule::atom => {
            let atom = parse_atom(next.into_inner(), program, features)?;
            if negative {
                Literal::negative_relational(atom)
            } else {
                Literal::relational(atom)
            }
        }
        Rule::comparison => {
            let comparison = parse_comparison(next.into_inner(), program, features)?;
            if negative {
                Literal::negative_arithmetic(comparison)
            } else {
                Literal::arithmetic(comparison)
            }
        }
        _ => unexpected_rule!(next),
    })
}

fn parse_atom(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<Atom> {
    let first = input_pairs.next().unwrap();
    let location = pair_to_src_location(&first);
    let atom_label = if_match_str_inner!(first, predicate => |s:&str|program.predicates().fetch(s));

    let mut terms: Vec<Term> = Default::default();
    for inner_pair in input_pairs {
        let term = if_match_inner!(inner_pair, term => (parse_term, program, features));
        terms.push(term);
    }

    Ok(Atom::new_at_location(atom_label.into(), &terms, location))
}

fn parse_comparison(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    _features: FeatureSet,
) -> Result<Comparison> {
    if !_features.supports(&FEATURE_COMPARISONS) {
        Err(pest_error!(
            input_pairs.into_iter().next().unwrap().as_span(),
            language_feature_disabled(FEATURE_COMPARISONS).to_string()
        ))
    } else {
        let left = if_match!(input_pairs, term => (parse_term, program, _features));

        let op = if_match_str!(input_pairs, comparison_operator => |s:&str| ComparisonOperator::from_str(s));

        let right = if_match!(input_pairs, term => (parse_term, program, _features));

        Ok(Comparison::new(left, op, right))
    }
}

fn parse_constant(
    mut input_pairs: Pairs<'_, Rule>,
    _program: &mut Program,
    _features: FeatureSet,
) -> Result<Constant> {
    let first = input_pairs.next().unwrap();
    let value = match first.as_rule() {
        Rule::unquoted_string => Constant::String(first.as_str().to_string()),
        Rule::string => Constant::String(first.as_str().to_owned()),
        Rule::number => i64::from_str(first.as_str())
            .map_err(|e| {
                invalid_value(
                    TYPE_NAME_CONST_INTEGER.to_string(),
                    format!("{} ({})", first.as_str(), e),
                )
            })?
            .into(),
        Rule::boolean => bool::from_str(first.as_str())
            .map_err(|_| error::invalid_value(TYPE_NAME_CONST_BOOLEAN, first.as_str()))?
            .into(),
        _ => unexpected_rule!(first),
    };

    not_expecting_more!(input_pairs);

    Ok(value)
}

fn parse_term(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<Term> {
    let first = input_pairs.next().unwrap();
    let value = match first.as_rule() {
        Rule::anonymous_variable => Term::Anonymous,
        Rule::named_variable => program.variables().fetch(first.as_str())?.into(),
        Rule::constant => Term::Constant(parse_constant(first.into_inner(), program, features)?),
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

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
