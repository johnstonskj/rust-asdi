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
use crate::idb::{
    Atom, Comparison, ComparisonOperator, Literal, Query, Rule as DlRule, Term, Variable,
};
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
            _ => unreachable!($input_pair.as_str()),
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
            _ => unreachable!($input_pair.as_str()),
        }
    };
}

#[allow(unused_macros)]
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
    let first = input_pairs.next().unwrap();
    let predicate = match first.as_rule() {
        Rule::predicate => program.predicates().fetch(first.as_str())?,
        _ => unreachable!(first.as_str()),
    };

    let mut attributes = Vec::new();
    for inner_pair in input_pairs {
        let constant = match inner_pair.as_rule() {
            Rule::constant => parse_constant(inner_pair.into_inner(), program, features)?,
            _ => unreachable!(inner_pair.as_str()),
        };
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
            _ => unreachable!(inner_pair.as_str()),
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
        let one = match inner_pair.as_rule() {
            Rule::atom => parse_atom(inner_pair.into_inner(), program, features)?,
            _ => unreachable!(inner_pair.as_str()),
        };
        body.push(one);
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
        let one = match inner_pair.as_rule() {
            Rule::literal => parse_literal(inner_pair.into_inner(), program, features)?,
            _ => unreachable!(inner_pair.as_str()),
        };
        body.push(one);
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
    let first = input_pairs.next().unwrap();
    let predicate = match first.as_rule() {
        Rule::predicate => program.predicates().fetch(first.as_str())?,
        _ => unreachable!(first.as_str()),
    };

    let mut attributes: Vec<Attribute<Predicate>> = Default::default();
    for inner_pair in input_pairs {
        match inner_pair.as_rule() {
            Rule::attribute_declaration => {
                attributes.push(parse_attribute(inner_pair.into_inner(), program, features)?)
            }
            _ => unreachable!("{:?}: {}", inner_pair.as_rule(), inner_pair.as_str()),
        }
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
    let first = input_pairs.next().unwrap();
    let predicate = match first.as_rule() {
        Rule::predicate => program.predicates().fetch(first.as_str())?,
        _ => unreachable!(first.as_str()),
    };

    let mut attributes: Vec<Attribute<Predicate>> = Default::default();
    for inner_pair in input_pairs {
        match inner_pair.as_rule() {
            Rule::attribute_declaration => {
                attributes.push(parse_attribute(inner_pair.into_inner(), program, features)?)
            }
            Rule::predicate => {
                let name = program.predicates().fetch(inner_pair.as_str())?;
                if let Some(from) = program.extensional().get(&name) {
                    attributes.extend(from.schema().iter().cloned());
                } else {
                    return Err(relation_does_not_exist(name));
                }
            }
            _ => unreachable!("{:?}: {}", inner_pair.as_rule(), inner_pair.as_str()),
        }
    }

    let _ = program
        .intensional_mut()
        .add_new_relation(predicate, attributes)?;

    Ok(())
}

#[allow(unused_assignments)]
fn parse_attribute(
    input_pairs: Pairs<'_, Rule>,
    _program: &mut Program,
    _: FeatureSet,
) -> Result<Attribute<Predicate>> {
    let mut name = None;
    for inner_pair in input_pairs {
        match inner_pair.as_rule() {
            Rule::predicate => name = Some(Predicate::from_str_unchecked(inner_pair.as_str())),
            Rule::type_id_string => {
                return Ok(Attribute::new_inner(name, AttributeKind::String));
            }
            Rule::type_id_integer => {
                return Ok(Attribute::new_inner(name, AttributeKind::Integer));
            }
            Rule::type_id_boolean => {
                return Ok(Attribute::new_inner(name, AttributeKind::Boolean));
            }
            _ => unreachable!("{:?}: {}", inner_pair.as_rule(), inner_pair.as_str()),
        }
    }
    unreachable!()
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
            _ => unreachable!(inner_pair.as_str()),
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
    let first = input_pairs.next().unwrap();
    let predicate = match first.as_rule() {
        Rule::predicate => program.predicates().fetch(first.as_str())?,
        _ => unreachable!(first.as_str()),
    };

    let relations = if input {
        program.extensional_mut()
    } else {
        program.intensional_mut()
    };

    if let Some(relation) = relations.get_mut(&predicate) {
        let next = input_pairs.next().unwrap();
        let file_name = match next.as_rule() {
            Rule::string => next.as_str().to_string(),
            _ => unreachable!(next.as_str()),
        };
        let file_name = &file_name[1..file_name.len() - 1];

        let pragma = if let Some(format) = input_pairs.next() {
            let format = format.as_str();
            let format = &format[1..format.len() - 1];
            FilePragma::new(PathBuf::from(file_name), string_to_format(format)?)
        } else {
            FilePragma::new(
                PathBuf::from(file_name),
                Format::DelimitedLines(Default::default()),
            )
        };
        relation.set_file_pragma(pragma);
        Ok(())
    } else {
        Err(relation_does_not_exist(predicate))
    }
}

fn parse_query(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let atom = match first.as_rule() {
        Rule::atom => parse_atom(first.into_inner(), program, features)?,
        _ => unreachable!(first.as_str()),
    };

    if input_pairs.next().is_some() {
        unreachable!(input_pairs.as_str());
    } else {
        program.add_query(Query::from(atom))?;
    }

    Ok(())
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
                Literal::negative_comparison(comparison)
            } else {
                Literal::arithmetic(comparison)
            }
        }
        _ => unreachable!(next.as_str()),
    })
}

fn parse_atom(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<Atom> {
    let first = input_pairs.next().unwrap();
    let location = pair_to_src_location(&first);
    let predicate = match first.as_rule() {
        Rule::predicate => Predicate::from_str_unchecked(first.as_str()),
        _ => unreachable!(first.as_str()),
    };

    let mut terms: Vec<Term> = Default::default();
    for inner_pair in input_pairs {
        let term = match inner_pair.as_rule() {
            Rule::term => parse_term(inner_pair.into_inner(), program, features)?,
            _ => unreachable!(inner_pair.as_str()),
        };
        terms.push(term);
    }

    Ok(Atom::new_at_location(predicate.into(), &terms, location))
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
        let next = input_pairs.next().unwrap();
        let term = match next.as_rule() {
            Rule::term => parse_term(next.into_inner(), program, _features)?,
            _ => unreachable!(next.as_str()),
        };

        let next = input_pairs.next().unwrap();
        let op = match next.as_rule() {
            Rule::comparison_operator => ComparisonOperator::from_str(next.as_str())?,
            _ => unreachable!(next.as_str()),
        };

        let next = input_pairs.next().unwrap();
        let right: Term = match next.as_rule() {
            Rule::term => parse_term(next.into_inner(), program, _features)?,
            _ => unreachable!(next.as_str()),
        };

        Ok(Comparison::new(term, op, right))
    }
}
//
// fn parse_predicate(
//     mut input_pairs: Pairs<'_, Rule>,
//     _program: &mut Program,
//     _features: FeatureSet,
// ) -> Result<Predicate> {
//     let first = input_pairs.next().unwrap();
//     let value = match first.as_rule() {
//         Rule::predicate => Predicate::from_str(first.as_str())?,
//         _ => unreachable!(first.as_str()),
//     };
//
//     if input_pairs.next().is_some() {
//         unreachable!(input_pairs.as_str())
//     }
//
//     Ok(value)
// }

fn parse_constant(
    mut input_pairs: Pairs<'_, Rule>,
    _program: &mut Program,
    _features: FeatureSet,
) -> Result<Constant> {
    let first = input_pairs.next().unwrap();
    let value = match first.as_rule() {
        Rule::identifier => Constant::String(first.as_str().to_string()),
        Rule::string => {
            let string = first.as_str();
            Constant::String(string[1..string.len() - 1].to_string())
        }
        Rule::number => {
            // if first.as_str().contains('.') {
            //     f64::from_str(first.as_str())
            //         .map_err(|e| {
            //             Error::InvalidValue(
            //                 TYPE_NAME_CONST_FLOAT.to_string(),
            //                 format!("{} ({})", first.as_str(), e),
            //             )
            //         })?
            //         .into()
            // } else {
            i64::from_str(first.as_str())
                .map_err(|e| {
                    invalid_value(
                        TYPE_NAME_CONST_INTEGER.to_string(),
                        format!("{} ({})", first.as_str(), e),
                    )
                })?
                .into()
            // }
        }
        Rule::boolean => bool::from_str(first.as_str())
            .map_err(|_| error::invalid_value(TYPE_NAME_CONST_BOOLEAN, first.as_str()))?
            .into(),
        _ => unreachable!(first.as_str()),
    };

    if input_pairs.next().is_some() {
        unreachable!(input_pairs.as_str())
    }

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
        Rule::named_variable => Variable::from_str_unchecked(first.as_str()).into(),
        Rule::constant => Term::Constant(parse_constant(first.into_inner(), program, features)?),
        _ => unreachable!(first.as_str()),
    };

    if input_pairs.next().is_some() {
        unreachable!(input_pairs.as_str())
    }

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
