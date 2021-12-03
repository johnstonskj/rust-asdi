/*!
One-line description.

More detailed description, with

# Example

*/

use crate::error::{Error, Result};
use crate::{Atom, Constant, Fact, Pragma, Predicate, Program, Query, Rule as DlRule, Term};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use std::fmt::Debug;
use std::fs::read_to_string;
use std::path::Path;

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
    let mut parsed =
        Datalog::parse(Rule::program, source).map_err(|e| Error::ParserError(Box::new(e)))?;
    let matched_str = parsed.as_str();
    let pair = parsed.next().unwrap();

    Ok(make_parsed(parse_program(pair)?, source, matched_str))
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
            _ => unreachable!($input_pair.as_str()),
        }
    };
}

macro_rules! match_any {
    ($input_pairs:expr => $program:expr ; $($rule:ident => $call:expr),+ $(; $EOI:ident)?) => {
        for inner_pair in $input_pairs {
            match inner_pair.as_rule() {
                $(
                    Rule::$rule => $call(inner_pair.into_inner(), $program)?,
                )+
                $(
                    Rule::$EOI => {}
                )?
                _ => unreachable!(inner_pair.as_str()),
            }
        }
    };
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

fn parse_program(input_pair: Pair<'_, Rule>) -> Result<Program> {
    let mut program: Program = Default::default();

    match_then_any! {
        input_pair => &mut program ;
        program ;
        statement => parse_statement,
        query => parse_query ;
        EOI
    }

    Ok(program)
}

// ------------------------------------------------------------------------------------------------

fn parse_statement(input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    match_any! {
        input_pairs => program ;
        fact => parse_fact,
        rule => parse_rule,
        pragma => parse_pragma
    }
    Ok(())
}

fn parse_fact(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let predicate = match first.as_rule() {
        Rule::predicate => parse_predicate(first.into_inner(), program)?,
        _ => unreachable!(first.as_str()),
    };

    let mut fact = Fact::new(predicate);

    for inner_pair in input_pairs {
        let constant = match inner_pair.as_rule() {
            Rule::constant => parse_constant(inner_pair.into_inner(), program)?,
            _ => unreachable!(inner_pair.as_str()),
        };
        fact.add_parameter(constant);
    }

    program.push(fact);

    Ok(())
}

fn parse_rule(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let head = match first.as_rule() {
        Rule::atom => parse_atom(first.into_inner(), program)?,
        _ => unreachable!(first.as_str()),
    };

    let mut body: Vec<Atom> = Default::default();
    for inner_pair in input_pairs {
        let one = match inner_pair.as_rule() {
            Rule::atom => parse_atom(inner_pair.into_inner(), program)?,
            _ => unreachable!(inner_pair.as_str()),
        };
        body.push(one);
    }

    program.push(DlRule::new(head, body));

    Ok(())
}

fn parse_pragma(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let identifier = match first.as_rule() {
        Rule::identifier => first.as_str(),
        _ => unreachable!(first.as_str()),
    };

    let mut arguments: Vec<Constant> = Default::default();
    for inner_pair in input_pairs {
        let constant = match inner_pair.as_rule() {
            Rule::constant => parse_constant(inner_pair.into_inner(), program)?,
            _ => unreachable!(inner_pair.as_str()),
        };
        arguments.push(constant);
    }

    let pragma = Pragma::try_from(identifier, arguments, &program.environment()).unwrap();
    program.push(pragma);

    Ok(())
}

fn parse_query(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<()> {
    let first = input_pairs.next().unwrap();
    let atom = match first.as_rule() {
        Rule::atom => parse_atom(first.into_inner(), program)?,
        _ => unreachable!(first.as_str()),
    };

    if input_pairs.next().is_some() {
        unreachable!(input_pairs.as_str())
    } else {
        program.push(Query::from(atom))
    }

    Ok(())
}

// ------------------------------------------------------------------------------------------------

fn parse_atom(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Atom> {
    let first = input_pairs.next().unwrap();
    let predicate = match first.as_rule() {
        Rule::predicate => parse_predicate(first.into_inner(), program)?,
        _ => unreachable!(first.as_str()),
    };

    let mut terms: Vec<Term> = Default::default();
    for inner_pair in input_pairs {
        let term = match inner_pair.as_rule() {
            Rule::term => parse_term(inner_pair.into_inner(), program)?,
            _ => unreachable!(inner_pair.as_str()),
        };
        terms.push(term);
    }

    Ok(Atom::new_with_parameters(predicate, terms))
}

fn parse_predicate(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Predicate> {
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

fn parse_constant(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Constant> {
    let first = input_pairs.next().unwrap();
    let value = match first.as_rule() {
        Rule::identifier => program.make_constant_identifier(first.as_str())?,
        Rule::string => program.make_constant_string(first.as_str())?,
        _ => unreachable!(first.as_str()),
    };

    if input_pairs.next().is_some() {
        unreachable!(input_pairs.as_str())
    }

    Ok(value)
}

fn parse_term(mut input_pairs: Pairs<'_, Rule>, program: &mut Program) -> Result<Term> {
    let first = input_pairs.next().unwrap();
    let value = match first.as_rule() {
        Rule::variable => program.make_term_variable(first.as_str())?,
        Rule::constant => Term::Constant(parse_constant(first.into_inner(), program)?),
        _ => unreachable!(first.as_str()),
    };

    if input_pairs.next().is_some() {
        unreachable!(input_pairs.as_str())
    }

    Ok(value)
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_nothing() {
        let result = parse_str("");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_string_fact() {
        let result = parse_str("human(\"Socrates\").");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_identifier_fact() {
        let result = parse_str("edge(a, b).");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_string_identifier_fact() {
        let result = parse_str("\"edge\"(a, b).");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_one_rule_turnstile() {
        let result = parse_str("mortal(X) :- human(X).");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_one_rule_unicode_arrow() {
        let result = parse_str("mortal(X) ⟵ human(X).");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_one_rule_ascii_arrow() {
        let result = parse_str("mortal(X) <- human(X).");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_one_rule_multiples_unicode_and() {
        let result = parse_str("path(X, Y) ⟵ edge(X, Z) ⋀ path(Z, Y).");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_one_rule_multiples_comma() {
        let result = parse_str("path(X, Y) ⟵ edge(X, Z), path(Z, Y).");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_one_rule_multiples_ampersand() {
        let result = parse_str("path(X, Y) ⟵ edge(X, Z) & path(Z, Y).");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_one_rule_multiples_and() {
        let result = parse_str("path(X, Y) ⟵ edge(X, Z) AND path(Z, Y).");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_query_prefixed() {
        let result = parse_str("?- path(X, Y).");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_query_suffixed() {
        let result = parse_str("path(X, Y)?");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }

    #[test]
    fn test_parse_pragma_include() {
        let result = parse_str("@include(\"./file\").");
        assert!(result.is_ok());
        println!("{:#?}", result.unwrap());
    }
}
