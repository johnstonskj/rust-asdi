/*!
One-line description.

More detailed description, with

# Example

*/

use crate::edb::{Constant, Predicate};
use crate::error::{Error, Result};
use crate::features::{FeatureSet, FEATURE_COMPARISONS, FEATURE_DISJUNCTION, FEATURE_NEGATION};
use crate::idb::{Atom, Comparison, ComparisonOperator, Literal, Term, Variable};
use crate::syntax::{RESERVED_BOOLEAN_TRUE, RESERVED_PREFIX, TYPE_NAME_CONST_INTEGER};
use crate::{Attribute, AttributeKind, Program, Query, Rule as DlRule};
use pest::iterators::{Pair, Pairs};
use pest::{Parser, Span};
use pest_derive::Parser;
use std::fmt::{Debug, Display, Formatter};
use std::fs::read_to_string;
use std::path::Path;
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Parsed<T>
where
    T: Clone + Debug,
{
    parsed: T,
    rest: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceLocation {
    line: usize,
    column: usize,
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

impl From<(usize, usize)> for SourceLocation {
    fn from(v: (usize, usize)) -> Self {
        Self {
            line: v.0,
            column: v.1,
        }
    }
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}, column {}]", self.line, self.column)
    }
}

impl SourceLocation {
    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
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

// ------------------------------------------------------------------------------------------------

fn parse_program(input_pair: Pair<'_, Rule>, features: FeatureSet) -> Result<Program> {
    let mut program: Program = Program::new_with_features(features);

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
        Rule::predicate => Predicate::from_str_unchecked(first.as_str()),
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

    let edb = program.database_mut();
    if !edb.contains(&predicate) {
        let relation = { edb.make_new_relation_from(predicate.clone(), &attributes)? };
        edb.add(relation);
    }

    if let Some(relation) = edb.relation_mut(&predicate) {
        relation.add(attributes)
    } else {
        unreachable!()
    }

    Ok(())
}

fn parse_rule(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    // TODO: disjunction requires a more complex head!
    let first = input_pairs.next().unwrap();
    let just_in_case = first.as_span().clone();
    let head = match first.as_rule() {
        Rule::atom => parse_atom(first.into_inner(), program, features)?,
        Rule::disjunction => {
            return Err(pest_error!(
                first.as_span(),
                Error::LanguageFeatureDisabled(FEATURE_DISJUNCTION).to_string()
            ))
        }
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

    let rule = DlRule::new_with_body(head, body);
    if let Err(e) = rule.check_well_formed(&features) {
        Err(pest_error!(just_in_case, e.to_string()))
    } else {
        program.add_rule(rule)?;
        Ok(())
    }
}

fn parse_pragma(
    input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    features: FeatureSet,
) -> Result<()> {
    let inner_pair = input_pairs.into_iter().next().unwrap();

    match_one! {
        (inner_pair, features) => program ;
        decl_relation => parse_decl_relation,
        decl_feature => parse_decl_feature,
        decl_include => parse_decl_include,
        decl_input => parse_decl_input,
        decl_output => parse_decl_output
    }

    Ok(())
}

fn parse_decl_relation(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    _features: FeatureSet,
) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let predicate = match first.as_rule() {
        Rule::predicate => Predicate::from_str_unchecked(first.as_str()),
        _ => unreachable!(first.as_str()),
    };

    let mut attributes: Vec<Attribute> = Default::default();
    for inner_pair in input_pairs {
        match inner_pair.as_rule() {
            Rule::attribute => attributes.push(parse_attribute(
                inner_pair.into_inner(),
                program,
                _features,
            )?),
            _ => unreachable!("{:?}: {}", inner_pair.as_rule(), inner_pair.as_str()),
        }
    }

    let edb = program.database_mut();
    let relation = { edb.make_new_relation(predicate, attributes)? };
    edb.add(relation);

    Ok(())
}

#[allow(unused_assignments)]
fn parse_attribute(
    input_pairs: Pairs<'_, Rule>,
    _program: &mut Program,
    _: FeatureSet,
) -> Result<Attribute> {
    let mut name = None;
    for inner_pair in input_pairs {
        match inner_pair.as_rule() {
            Rule::predicate => name = Some(Predicate::from_str_unchecked(inner_pair.as_str())),
            Rule::tid_string => {
                return Ok(Attribute::new_inner(name, AttributeKind::String));
            }
            Rule::tid_integer => {
                return Ok(Attribute::new_inner(name, AttributeKind::Integer));
            }
            Rule::tid_boolean => {
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
            Rule::fid_negation => program.features_mut().add_support_for(&FEATURE_NEGATION),
            Rule::fid_comparisons => program.features_mut().add_support_for(&FEATURE_COMPARISONS),
            Rule::fid_disjunction => program.features_mut().add_support_for(&FEATURE_DISJUNCTION),
            _ => unreachable!(inner_pair.as_str()),
        };
    }

    Ok(())
}

fn parse_decl_include(
    mut input_pairs: Pairs<'_, Rule>,
    _: &mut Program,
    _: FeatureSet,
) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let file_name = match first.as_rule() {
        Rule::string => first.as_str().to_string(),
        _ => unreachable!(first.as_str()),
    };

    if input_pairs.next().is_some() {
        unreachable!(input_pairs.as_str());
    } else {
        println!("Unimplemented: include and parse `{}`", file_name);
        // TODO: include and parse `file_name`
    }

    Ok(())
}

fn parse_decl_input(
    mut input_pairs: Pairs<'_, Rule>,
    _: &mut Program,
    _: FeatureSet,
) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let predicate = match first.as_rule() {
        Rule::predicate => Predicate::from_str_unchecked(first.as_str()),
        _ => unreachable!(first.as_str()),
    };

    let next = input_pairs.next().unwrap();
    let file_name = match next.as_rule() {
        Rule::string => next.as_str().to_string(),
        _ => unreachable!(next.as_str()),
    };

    if input_pairs.next().is_some() {
        unreachable!(input_pairs.as_str());
    } else {
        println!(
            "Unimplemented: input data for `{}` relation from `{}`",
            predicate, file_name
        );
        // TODO: input data for `predicate` relation from `file_name`
    }

    Ok(())
}

fn parse_decl_output(
    mut input_pairs: Pairs<'_, Rule>,
    _: &mut Program,
    _: FeatureSet,
) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let predicate = match first.as_rule() {
        Rule::predicate => Predicate::from_str_unchecked(first.as_str()),
        _ => unreachable!(first.as_str()),
    };

    let next = input_pairs.next().unwrap();
    let file_name = match next.as_rule() {
        Rule::string => next.as_str().to_string(),
        _ => unreachable!(next.as_str()),
    };

    if input_pairs.next().is_some() {
        unreachable!(input_pairs.as_str());
    } else {
        println!(
            "Unimplemented: output data for `{}` relation into `{}`",
            predicate, file_name
        );
        // TODO: output data for `predicate` relation into `file_name`
    }

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
            Error::LanguageFeatureDisabled(FEATURE_NEGATION).to_string()
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
        Rule::comparison => {
            let comparison = parse_comparison(next.into_inner(), program, features)?;
            if negative {
                Literal::negative_comparison(comparison)
            } else {
                Literal::comparison(comparison)
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

    Ok(Atom::new_at_location(predicate, &terms, location))
}

fn parse_comparison(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    _features: FeatureSet,
) -> Result<Comparison> {
    if !_features.supports(&FEATURE_COMPARISONS) {
        Err(pest_error!(
            input_pairs.into_iter().next().unwrap().as_span(),
            Error::LanguageFeatureDisabled(FEATURE_COMPARISONS).to_string()
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
        Rule::variable => Variable::from_str(first.as_str())?.into(),
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
