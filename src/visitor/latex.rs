/*!
This module provides the trait [Typesetter](trait.Typesetter.html), and implementation
[LatexTypesetter](struct.LatexTypesetter.html) to allow more detailed output of a program's
source according to different documentation tools.

TBD

# Example

TBD

*/

use crate::edb::{Constant, Fact};
use crate::error::Result;
use crate::idb::{query::Query, Atom, ComparisonOperator, Literal, LiteralInner, Rule, Term};
use crate::syntax::{
    CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, COMMA_SEPARATOR, DISJUNCTION_UNICODE_SYMBOL,
};
use crate::visitor::{ProgramVisitor, ProgramWriter, QueryVisitor, RelationVisitor, RuleVisitor};
use crate::{Collection, Labeled, MaybePositive, Program};
use std::cell::RefCell;

use std::io::Write;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// An implementation of the trait [Typesetter](trait.Typesetter.html) that outputs LaTeX
/// formatted program source.
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
/// ```
///
#[derive(Debug)]
pub struct LatexFormatter<W: Write> {
    writer: RefCell<W>,
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

pub fn make_latex_writer<W: Write>(writer: W) -> LatexFormatter<W> {
    LatexFormatter {
        writer: RefCell::new(writer),
    }
}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

const LATEX_INLINE_LISTING: &str = "|";

#[allow(single_use_lifetimes)]
impl<W: Write> ProgramWriter for LatexFormatter<W> {}

#[allow(single_use_lifetimes)]
impl<W: Write> ProgramVisitor for LatexFormatter<W> {
    fn start_program(&self, _: &Program) -> Result<()> {
        writeln!(
            self.writer.borrow_mut(),
            "\\begin{{lstlisting}}[language=Prolog,mathescape]",
        )?;
        Ok(())
    }

    fn end_program(&self, _: &Program) -> Result<()> {
        writeln!(self.writer.borrow_mut(), "\\end{{lstlisting}}")?;
        Ok(())
    }

    fn relation_visitor(&self) -> Option<&dyn RelationVisitor> {
        Some(self)
    }

    fn rule_visitor(&self) -> Option<&dyn RuleVisitor> {
        Some(self)
    }

    fn query_visitor(&self) -> Option<&dyn QueryVisitor> {
        Some(self)
    }
}

#[allow(single_use_lifetimes)]
impl<W: Write> RelationVisitor for LatexFormatter<W> {
    fn fact(&self, fact: &Fact) -> Result<()> {
        self.write_fact(fact, false)?;
        Ok(())
    }
}

#[allow(single_use_lifetimes)]
impl<W: Write> RuleVisitor for LatexFormatter<W> {
    fn rule(&self, rule: &Rule) -> Result<()> {
        self.write_rule(rule, false)?;
        Ok(())
    }
}

#[allow(single_use_lifetimes)]
impl<W: Write> QueryVisitor for LatexFormatter<W> {
    fn query(&self, query: &Query) -> Result<()> {
        self.write_query(query, false)?;
        writeln!(self.writer.borrow_mut())?;
        Ok(())
    }
}

#[allow(single_use_lifetimes)]
impl<W: Write> LatexFormatter<W> {
    fn write_fact(&self, value: &Fact, inline: bool) -> Result<()> {
        if inline {
            write!(self.writer.borrow_mut(), "{}", LATEX_INLINE_LISTING)?;
        }
        write!(
            self.writer.borrow_mut(),
            "{}{}${}${}{}",
            value.label(),
            CHAR_LEFT_PAREN,
            value
                .iter()
                .map(Constant::to_string)
                .collect::<Vec<String>>()
                .join(COMMA_SEPARATOR),
            CHAR_RIGHT_PAREN,
            CHAR_PERIOD
        )?;
        if inline {
            write!(self.writer.borrow_mut(), "{}", LATEX_INLINE_LISTING)?;
        } else {
            writeln!(self.writer.borrow_mut())?;
        }
        Ok(())
    }

    fn write_rule(&self, value: &Rule, inline: bool) -> Result<()> {
        if inline {
            write!(self.writer.borrow_mut(), "{}", LATEX_INLINE_LISTING)?;
        }

        let head: Vec<&Atom> = value.head().collect();
        write!(
            self.writer.borrow_mut(),
            "{} $\\leftarrow$ {}{}",
            if head.is_empty() {
                "$\\bot$".to_string()
            } else if head.len() == 1 {
                self.format_atom(head.get(0).unwrap())?
            } else {
                head.iter()
                    .map(|atom| atom.to_string())
                    .collect::<Vec<String>>()
                    .join(&format!(" {} ", DISJUNCTION_UNICODE_SYMBOL))
            },
            value
                .literals()
                .map(|l| self.format_literal(l))
                .collect::<Result<Vec<String>>>()?
                .join(" $\\land$ "),
            CHAR_PERIOD,
        )?;

        if inline {
            write!(self.writer.borrow_mut(), "{}", LATEX_INLINE_LISTING)?;
        } else {
            writeln!(self.writer.borrow_mut())?;
        }
        Ok(())
    }

    fn write_query(&self, value: &Query, inline: bool) -> Result<()> {
        if inline {
            write!(self.writer.borrow_mut(), "{}", LATEX_INLINE_LISTING)?;
        }
        write!(
            self.writer.borrow_mut(),
            "{}?",
            self.format_atom(value.as_ref())?
        )?;
        if inline {
            write!(self.writer.borrow_mut(), "{}", LATEX_INLINE_LISTING)?;
        }
        Ok(())
    }

    fn format_atom(&self, value: &Atom) -> Result<String> {
        Ok(format!(
            "{}{}${}${}",
            value.label(),
            CHAR_LEFT_PAREN,
            value
                .iter()
                .map(Term::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            CHAR_RIGHT_PAREN,
        ))
    }

    fn format_literal(&self, value: &Literal) -> Result<String> {
        Ok(format!(
            "{}{}",
            if !value.is_positive() {
                "$\\lnot$".to_string()
            } else {
                String::new()
            },
            match value.as_ref() {
                LiteralInner::Relational(a) => self.format_atom(a)?,
                LiteralInner::Arithmetic(c) => format!(
                    "${} {} {}$",
                    c.lhs(),
                    match c.operator() {
                        ComparisonOperator::Equal => "=",
                        ComparisonOperator::NotEqual => "\\neq",
                        ComparisonOperator::LessThan => "<",
                        ComparisonOperator::LessThanOrEqual => "\\leq",
                        ComparisonOperator::GreaterThan => ">",
                        ComparisonOperator::GreaterThanOrEqual => "\\geq",
                        ComparisonOperator::StringMatch => r"\overset{\star}{=}",
                    },
                    c.rhs()
                ),
            }
        ))
    }
}

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(feature = "parser")]
#[cfg(test)]
mod tests {
    use crate::parse::parse_str;
    use crate::visitor::{make_latex_writer, write_program};

    #[test]
    fn test_wikipedia_example() {
        let program = parse_str(
            r#"parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ∧ parent(Z, Y).

?- ancestor(xerces, X).
"#,
        );
        let program = program.unwrap().into_parsed();

        use std::io::BufWriter;
        let mut buffer = BufWriter::new(Vec::new());
        let visitor = make_latex_writer(&mut buffer);
        let result = write_program(&program, &visitor);
        assert!(result.is_ok());
        let string = String::from_utf8(buffer.into_inner().unwrap()).unwrap();
        println!("{}", string);
    }
}
