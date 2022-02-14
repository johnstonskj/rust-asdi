use crate::edb::io::{string_to_format, FilePragma, Format};
use crate::edb::{Attribute, AttributeIndex, AttributeKind, Constant, Predicate};
use crate::error::{
    attribute_does_not_exist, attribute_index_invalid, invalid_value, language_feature_disabled,
    Error, Result, SourceLocation,
};
use crate::features::{
    FeatureSet, FEATURE_COMPARISONS, FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION,
    FEATURE_FUNCTIONAL_DEPENDENCIES, FEATURE_NEGATION,
};
use crate::idb::{
    query::Query, Atom, Comparison, ComparisonOperator, Literal, Rule as DlRule, Term,
};
use crate::parse::Parsed;
use crate::syntax::{TYPE_NAME_CONST_BOOLEAN, TYPE_NAME_CONST_INTEGER};
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

    let mut attributes = Vec::new();
    for inner_pair in input_pairs {
        let constant = if_match_inner!(inner_pair, constant => (parse_constant, program));
        attributes.push(constant);
    }

    let edb = program.extensional_mut();
    let relation = if !edb.contains(&predicate) {
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
        pragma_output => parse_decl_output
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
            input_pairs.into_iter().next().unwrap().as_span(),
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

fn parse_decl_input(input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    parse_decl_file_io(input_pairs, program, true)
}

fn parse_decl_output(input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    parse_decl_file_io(input_pairs, program, false)
}

fn parse_decl_file_io(
    mut input_pairs: Pairs<'_, Rule>,
    program: &mut Program,
    input: bool,
) -> Result<()> {
    let relation_label =
        if_match_str!(input_pairs, predicate => |s:&str|program.predicates().fetch(s));

    let base_path = program.source_file_path().cloned();

    let relations = if input {
        program.extensional_mut()
    } else {
        program.intensional_mut()
    };

    if let Some(relation) = relations.get_mut(&relation_label) {
        //
        // Resolve input/output files from source if present, or current.
        //
        let root = std::env::current_dir()?;
        let file_path = if_match_string!(input_pairs, string);
        let file_path = if let Some(base_path) = base_path {
            base_path.parent().unwrap_or(&root).join(file_path)
        } else {
            PathBuf::from(file_path)
        };

        let pragma = if let Some(format) = input_pairs.next() {
            FilePragma::new(file_path, string_to_format(format.as_str())?)
        } else {
            FilePragma::new(file_path, Format::DelimitedLines(Default::default()))
        };
        relation.set_file_pragma(pragma);
        Ok(())
    } else {
        Err(relation_does_not_exist(relation_label))
    }
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

    Ok(Atom::new_at_location(atom_label.into(), &terms, location))
}

fn parse_comparison(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Comparison> {
    if !program.features().supports(&FEATURE_COMPARISONS) {
        Err(pest_error!(
            input_pairs.into_iter().next().unwrap().as_span(),
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

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
