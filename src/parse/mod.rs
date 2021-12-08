/*!
One-line description.

More detailed description, with

# Example

*/

use crate::error::{Error, Result};
use crate::features::{FeatureSet, FEATURE_NEGATION};
use crate::{
    Atom, ComparisonOperator, Constant, Fact, Literal, LiteralExpression, Pragma, Predicate,
    Program, Query, Rule as DlRule, SourceLocation, Term, RESERVED_BOOLEAN_TRUE, RESERVED_PREFIX,
    TYPE_NAME_CONST_INTEGER,
};
use pest::iterators::{Pair, Pairs};
use pest::{Parser, Span};
use pest_derive::Parser;
use std::fmt::Debug;
use std::fs::read_to_string;
use std::path::Path;
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Parser)]
#[grammar = "parse/datalog.pest"]
struct Datalog;

#[derive(Clone, Debug)]
pub struct Parsed<T>
where
    T: Clone + Debug,
{
    parsed: T,
    rest: Option<String>,
}

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Macros
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

pub fn parse_file<P: AsRef<Path>>(file_path: P) -> Result<Parsed<Program>> {
    parse_str(&read_to_string(file_path.as_ref())?)
}

pub fn parse_str(source: &str) -> Result<Parsed<Program>> {
    parse_str_with_features(source, FeatureSet::default())
}

pub fn parse_str_with_features(source: &str, features: FeatureSet) -> Result<Parsed<Program>> {
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

fn make_parsed<T>(parsed: T, original_str: &str, matched_str: &str) -> Parsed<T>
where
    T: Clone + Debug,
{
    let original_len = original_str.len();
    let matched_len = matched_str.len();
    if matched_len < original_len {
        Parsed::new_with_more(parsed, original_str[matched_len..original_len].to_string())
    } else {
        Parsed::new(parsed)
    }
}

impl<T> Parsed<T>
where
    T: Clone + Debug,
{
    fn new(parsed: T) -> Self {
        Self { parsed, rest: None }
    }

    fn new_with_more(parsed: T, rest: String) -> Self {
        if !rest.is_empty() {
            Self {
                parsed,
                rest: Some(rest),
            }
        } else {
            Self::new(parsed)
        }
    }

    pub fn parsed(&self) -> &T {
        &self.parsed
    }

    pub fn into_parsed(self) -> T {
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
            match inner_pair.as_rule() {
                $(
                    Rule::$rule => $call(inner_pair.into_inner(), $program, $features)?,
                )+
                $(
                    Rule::$EOI => {}
                )?
                _ => unreachable!(inner_pair.as_str()),
            }
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

fn parse_program(input_pair: Pair<'_, Rule>, features: FeatureSet) -> Result<Program> {
    let mut program: Program = features.into();

    match_then_any! {
        (input_pair, features) => &mut program ;
        program ;
        statement => parse_statement,
        query => parse_query ;
        EOI
    }

    Ok(program)
}

// ------------------------------------------------------------------------------------------------

fn parse_statement(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    match_any! {
        (input_pairs, features) => program ;
        fact => parse_fact,
        rule => parse_rule,
        pragma => parse_pragma
    }
    Ok(())
}

fn parse_fact(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let predicate = match first.as_rule() {
        Rule::predicate => parse_predicate(first.into_inner(), program, features)?,
        _ => unreachable!(first.as_str()),
    };

    let mut fact = Fact::new(predicate);

    for inner_pair in input_pairs {
        let constant = match inner_pair.as_rule() {
            Rule::constant => parse_constant(inner_pair.into_inner(), program, features)?,
            _ => unreachable!(inner_pair.as_str()),
        };
        fact.add_argument(constant);
    }

    program.push(fact)?;

    Ok(())
}

fn parse_rule(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let head = match first.as_rule() {
        Rule::atom => parse_atom(first.into_inner(), program, features)?,
        _ => unreachable!(first.as_str()),
    };

    let mut body: Vec<Literal> = Default::default();
    for inner_pair in input_pairs {
        let one = match inner_pair.as_rule() {
            Rule::literal => parse_literal(inner_pair.into_inner(), program, features)?,
            _ => unreachable!(inner_pair.as_str()),
        };
        body.push(one);
    }

    program.push(DlRule::new_with_body(head, body))?;

    Ok(())
}

fn parse_pragma(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let identifier = match first.as_rule() {
        Rule::identifier => first.as_str(),
        _ => unreachable!(first.as_str()),
    };

    let mut arguments: Vec<Constant> = Default::default();
    for inner_pair in input_pairs {
        let constant = match inner_pair.as_rule() {
            Rule::constant => parse_constant(inner_pair.into_inner(), program, features)?,
            _ => unreachable!(inner_pair.as_str()),
        };
        arguments.push(constant);
    }

    let pragma = Pragma::try_from(identifier, arguments, &program.environment()).unwrap();
    program.push(pragma)?;

    Ok(())
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
        program.push(Query::from(atom))?;
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
            "The Language feature 'negation' is disabled"
        ));
    } else if negative {
        next = input_pairs.next().unwrap();
    }

    Ok(match next.as_rule() {
        Rule::atom => {
            let atom = parse_atom(next.into_inner(), program, features)?;
            if negative {
                Literal::negative_atom(atom)
            } else {
                Literal::atom(atom)
            }
        }
        Rule::expression => {
            let expression = parse_expression(next.into_inner(), program, features)?;
            if negative {
                Literal::negative_expression(expression)
            } else {
                Literal::expression(expression)
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
        Rule::predicate => parse_predicate(first.into_inner(), program, features)?,
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

    Ok(Atom::new_at_location(predicate, terms, location))
}

fn parse_expression(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    _features: FeatureSet,
) -> Result<LiteralExpression> {
    let next = input_pairs.next().unwrap();
    let term = match next.as_rule() {
        Rule::term => parse_term(next.into_inner(), program, _features)?,
        _ => unreachable!(next.as_str()),
    };

    let next = input_pairs.next();

    if let Some(next) = next {
        let op = match next.as_rule() {
            Rule::operator => ComparisonOperator::from_str(next.as_str())?,
            _ => unreachable!(next.as_str()),
        };

        let next = input_pairs.next().unwrap();
        let right: Term = match next.as_rule() {
            Rule::term => parse_term(next.into_inner(), program, _features)?,
            _ => unreachable!(next.as_str()),
        };

        Ok(LiteralExpression::new(term, op, right))
    } else {
        Ok(LiteralExpression::term(term))
    }
}

fn parse_predicate(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    _features: FeatureSet,
) -> Result<Predicate> {
    let first = input_pairs.next().unwrap();
    let value = match first.as_rule() {
        Rule::relation => program.make_predicate_relation(first.as_str())?,
        Rule::string => program.make_predicate_string(first.as_str())?,
        _ => unreachable!(first.as_str()),
    };

    if input_pairs.next().is_some() {
        unreachable!(input_pairs.as_str())
    }

    Ok(value)
}

fn parse_constant(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    _features: FeatureSet,
) -> Result<Constant> {
    let first = input_pairs.next().unwrap();
    let value = match first.as_rule() {
        Rule::identifier => program.make_constant_identifier(first.as_str())?,
        Rule::string => program.make_constant_string(first.as_str())?,
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
                    Error::InvalidValue(
                        TYPE_NAME_CONST_INTEGER.to_string(),
                        format!("{} ({})", first.as_str(), e),
                    )
                })?
                .into()
            // }
        }
        Rule::boolean => (first.as_str()
            == format!("{}{}", RESERVED_PREFIX, RESERVED_BOOLEAN_TRUE).as_str())
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
        Rule::variable => program.make_term_variable(first.as_str())?,
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
