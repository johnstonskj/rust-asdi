/*!
One-line description.

More detailed description, with

# Example

*/

use crate::edb::{AttributeName, Constant, Fact};
use crate::error::Result;
use crate::idb::{Atom, ComparisonOperator, Literal, LiteralInner, Rule, Term};
use crate::program::Program;
use crate::query::Query;
use crate::syntax::{CHAR_COMMA, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, CHAR_UNDERSCORE};
use std::fmt::Write;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

pub trait Typesetter {
    fn program(&self, value: &Program) -> Result<String>;
    fn fact<T>(&self, value: &Fact<'_, T>, inline: bool) -> Result<String>
    where
        T: AttributeName;
    fn rule(&self, value: &Rule, inline: bool) -> Result<String>;
    fn query(&self, value: &Query, inline: bool) -> Result<String>;
}

///
/// # Preamble
///
/// ```latex
/// \usepackage{listings}
/// \usepackage{xcolor}
///
/// \lstset{
///     frameround=fttt,
///     breaklines=true,
///     numberstyle=\color{gray}\footnotesize,
///     numbers=left,
/// }
/// \lstdefinelanguage{Datalog}{%
///     language=Prolog,
///     basicstyle=\color{black}\ttfamily,
///     commentstyle=\color{teal}\sffamily\slshape,
///     morecomment=[l]{\#},
///     keywordstyle=\color{violet}\bfseries,
///     otherkeywords={AND, OR, NOT},
///     morekeywords=[1]{AND, OR, NOT},
///     mathescape
/// }
/// \lstMakeShortInline[language=Datalog]|
/// /// ```
///
#[derive(Debug, Default)]
pub(crate) struct LatexTypesetter {}

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Macros
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

const LATEX_INLINE_LISTING: &str = "|";

impl Typesetter for LatexTypesetter {
    fn program(&self, value: &Program) -> Result<String> {
        let mut result = String::new();
        writeln!(result, r"\begin{{lstlisting}}[language=Prolog,mathescape]")?;

        for relation in value.database().iter() {
            for fact in relation.facts() {
                writeln!(result, "{}", self.fact(&fact, false)?)?;
            }
        }

        for rule in value.rules() {
            writeln!(result, "{}", self.rule(rule, false)?)?;
        }

        for query in value.queries() {
            writeln!(result, "{}", self.query(query, false)?)?;
        }

        write!(result, "{}", r"\end{lstlisting}")?;
        Ok(result)
    }

    fn fact<T>(&self, value: &Fact<'_, T>, inline: bool) -> Result<String>
    where
        T: AttributeName,
    {
        let mut result = String::new();
        if inline {
            write!(result, "{}", LATEX_INLINE_LISTING)?;
        }
        write!(
            result,
            "{}{}${}${}{}",
            match &value.name() {
                None => CHAR_UNDERSCORE.to_string(),
                Some(p) => p.to_string(),
            },
            CHAR_LEFT_PAREN,
            value
                .iter()
                .map(Constant::to_string)
                .collect::<Vec<String>>()
                .join(&format!("{} ", CHAR_COMMA)),
            CHAR_RIGHT_PAREN,
            CHAR_PERIOD
        )?;
        if inline {
            write!(result, "{}", LATEX_INLINE_LISTING)?;
        }
        Ok(result)
    }

    fn rule(&self, value: &Rule, inline: bool) -> Result<String> {
        let mut result = String::new();
        if inline {
            write!(result, "{}", LATEX_INLINE_LISTING)?;
        }

        write!(
            result,
            "{} $\\leftarrow$ {}{}",
            self.atom(value.head())?,
            value
                .literals()
                .map(|l| self.literal(l))
                .collect::<Result<Vec<String>>>()?
                .join(" $\\land$ "),
            CHAR_PERIOD,
        )?;

        if inline {
            write!(result, "{}", LATEX_INLINE_LISTING)?;
        }
        Ok(result)
    }

    fn query(&self, value: &Query, inline: bool) -> Result<String> {
        let mut result = String::new();
        if inline {
            write!(result, "{}", LATEX_INLINE_LISTING)?;
        }
        write!(result, "{}?", self.atom(value.as_ref())?)?;
        if inline {
            write!(result, "{}", LATEX_INLINE_LISTING)?;
        }
        Ok(result)
    }
}

impl LatexTypesetter {
    fn atom(&self, value: &Atom) -> Result<String> {
        Ok(format!(
            "{}{}${}${}",
            value.predicate(),
            CHAR_LEFT_PAREN,
            value
                .terms()
                .map(Term::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            CHAR_RIGHT_PAREN,
        ))
    }

    fn literal(&self, value: &Literal) -> Result<String> {
        Ok(format!(
            "{}{}",
            if !value.is_positive() {
                "$\\lnot$".to_string()
            } else {
                String::new()
            },
            match value.inner() {
                LiteralInner::Atom(a) => self.atom(a)?,
                LiteralInner::Comparison(c) => format!(
                    "${} {} {}$",
                    c.lhs(),
                    match c.operator() {
                        ComparisonOperator::Equal => "=",
                        ComparisonOperator::NotEqual => "\\neq",
                        ComparisonOperator::LessThan => "<",
                        ComparisonOperator::LessThanOrEqual => "\\leq",
                        ComparisonOperator::GreaterThan => ">",
                        ComparisonOperator::GreaterThanOrEqual => "\\geq",
                    },
                    c.rhs()
                ),
            }
        ))
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(feature = "parser")]
#[cfg(test)]
mod tests {
    use crate::edb::Predicate;
    use crate::parse::parse_str;
    use crate::typeset::{LatexTypesetter, Typesetter};

    #[test]
    fn test_wikipedia_example() {
        let program = parse_str(
            r#"parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ⋀ parent(Z, Y).

?- ancestor(xerces, X).
"#,
        );
        let program = program.unwrap().into_parsed();

        let typesetter = LatexTypesetter::default();

        println!("{}", typesetter.program(&program).unwrap());

        {
            // Test fact
            let relation = program
                .database()
                .relation(&Predicate::from_str_unchecked("parent"))
                .unwrap();
            let fact = relation.facts().next().unwrap();
            match fact.to_string().as_str() {
                "parent(xerces, brooke)." => assert_eq!(
                    typesetter.fact(&fact, true).unwrap(),
                    String::from(r"|parent($xerces, brooke$).|")
                ),
                "parent(brooke, damocles)." => assert_eq!(
                    typesetter.fact(&fact, true).unwrap(),
                    String::from(r"|parent($brooke, damocles$).|")
                ),
                s => panic!("not expecting fact: {}", s),
            }
        }

        {
            // Test rule
            let rule = program.rules().next().unwrap();
            match rule.to_string().as_str() {
                "ancestor(X, Y) ⟵ parent(X, Y)." => assert_eq!(
                    typesetter.rule(rule, true).unwrap(),
                    String::from(r"|ancestor($X, Y$) $\leftarrow$ parent($X, Y$).|")
                ),
                "ancestor(X, Y) ⟵ parent(X, Z) ⋀ parent(Z, Y)." => assert_eq!(
                    typesetter.rule(&rule, true).unwrap(),
                    String::from(
                        r"|ancestor($X, Y$) $\leftarrow$ parent($X, Z$) $\land$ parent($Z, Y$).|"
                    )
                ),
                s => panic!("not expecting rule: {}", s),
            }
        }

        {
            // Test query
            let query = program.queries().next().unwrap();
            assert_eq!(
                typesetter.query(query, true).unwrap(),
                String::from(r"|ancestor($xerces, X$)?|")
            )
        }
    }
}
