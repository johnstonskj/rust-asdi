/*!
One-line description.

More detailed description, with

# Example

*/

use crate::edb::{Fact, Relation};
use crate::idb::{Rule, RuleSet};
use crate::syntax::{
    CHAR_COLON, CHAR_LEFT_PAREN, CHAR_PERIOD, CHAR_RIGHT_PAREN, COMMA_SEPARATOR, PRAGMA_ID_FD,
    PRAGMA_ID_INPUT, PRAGMA_ID_OUTPUT,
};
use crate::visitor::{ProgramVisitor, ProgramWriter, QueryVisitor, RelationVisitor, RuleVisitor};
use crate::{Collection, Labeled, Program, ProgramCore, Query};
use std::cell::RefCell;
use std::io::Write;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct NativeFormatter<W: Write> {
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

pub fn make_native_writer<W: Write>(writer: W) -> NativeFormatter<W> {
    NativeFormatter {
        writer: RefCell::new(writer),
    }
}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl<W: Write> ProgramWriter for NativeFormatter<W> {}

impl<W: Write> ProgramVisitor for NativeFormatter<W> {
    fn start_program(&self, program: &Program) -> crate::Result<()> {
        let writer = &mut self.writer.borrow_mut();

        write!(writer, "{}", program.features())?;

        for edb in [true, false] {
            let relations = if edb {
                program.extensional()
            } else {
                program.intensional()
            };
            if !relations.is_empty() {
                for relation in relations.iter() {
                    writeln!(writer, "{}", relation.to_schema_decl(edb, false))?;
                    if let Some(file_pragma) = relation.file_pragma() {
                        let pragma_id = if edb {
                            PRAGMA_ID_INPUT
                        } else {
                            PRAGMA_ID_OUTPUT
                        };
                        write!(
                            writer,
                            "{}{}{}{}{}{:?}{}{}{}{}",
                            CHAR_PERIOD,
                            pragma_id,
                            CHAR_LEFT_PAREN,
                            relation.label(),
                            COMMA_SEPARATOR,
                            file_pragma.file_name(),
                            COMMA_SEPARATOR,
                            file_pragma.file_format().label(),
                            CHAR_RIGHT_PAREN,
                            CHAR_PERIOD,
                        )?;
                    }
                    for dependency in relation.schema().functional_dependencies() {
                        writeln!(
                            writer,
                            "{}{} {}{} {}{}",
                            CHAR_PERIOD,
                            PRAGMA_ID_FD,
                            relation.label(),
                            CHAR_COLON,
                            dependency,
                            CHAR_PERIOD,
                        )?;
                    }
                }
                writeln!(writer)?;
            }
        }

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

impl<W: Write> RelationVisitor for NativeFormatter<W> {
    fn fact(&self, fact: &Fact) -> crate::Result<()> {
        write!(self.writer.borrow_mut(), "{}", fact)?;
        Ok(())
    }

    fn end_relation(&self, relation: &Relation, _: bool) -> crate::Result<()> {
        if !relation.is_empty() {
            writeln!(self.writer.borrow_mut())?;
        }
        Ok(())
    }
}

impl<W: Write> RuleVisitor for NativeFormatter<W> {
    fn rule(&self, rule: &Rule) -> crate::Result<()> {
        write!(self.writer.borrow_mut(), "{}", rule)?;
        Ok(())
    }

    fn end_rules(&self, _: &RuleSet) -> crate::Result<()> {
        writeln!(self.writer.borrow_mut())?;
        Ok(())
    }
}

impl<W: Write> QueryVisitor for NativeFormatter<W> {
    fn query(&self, query: &Query) -> crate::Result<()> {
        write!(self.writer.borrow_mut(), "{}", query)?;
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
