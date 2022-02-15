/*!
This module provides the trait [Typesetter](trait.Typesetter.html), and implementation

![module UML](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/book/src/model/visitor.svg)

TBD

# Example

TBD

*/

use crate::edb::Fact;
use crate::error::Result;
use crate::idb::{query::Query, Rule};
use crate::{Collection, Program, ProgramCore, QuerySet, Relation, RuleSet};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

pub trait ProgramVisitor {
    fn start_program(&self, _program: &Program) -> Result<()> {
        Ok(())
    }
    fn end_program(&self, _program: &Program) -> Result<()> {
        Ok(())
    }

    fn relation_visitor(&self) -> Option<&dyn RelationVisitor> {
        None
    }

    fn rule_visitor(&self) -> Option<&dyn RuleVisitor> {
        None
    }

    fn query_visitor(&self) -> Option<&dyn QueryVisitor> {
        None
    }
}

pub trait ProgramWriter: ProgramVisitor {}

pub trait RelationVisitor {
    fn start_relation(&self, _relation: &Relation, _extensional: bool) -> Result<()> {
        Ok(())
    }
    fn fact(&self, _fact: &Fact) -> Result<()> {
        Ok(())
    }
    fn end_relation(&self, _relation: &Relation, _extensional: bool) -> Result<()> {
        Ok(())
    }
}

pub trait RuleVisitor {
    fn start_rules(&self, _rules: &RuleSet) -> Result<()> {
        Ok(())
    }
    fn rule(&self, _rule: &Rule) -> Result<()> {
        Ok(())
    }
    fn end_rules(&self, _rules: &RuleSet) -> Result<()> {
        Ok(())
    }
}

pub trait QueryVisitor {
    fn start_queries(&self, _queries: &QuerySet) -> Result<()> {
        Ok(())
    }
    fn query(&self, _query: &Query) -> Result<()> {
        Ok(())
    }
    fn end_queries(&self, _queries: &QuerySet) -> Result<()> {
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

pub fn write_program(program: &Program, visitor: &impl ProgramWriter) -> Result<()> {
    visit_program(program, visitor)
}

pub fn visit_program(program: &Program, visitor: &impl ProgramVisitor) -> Result<()> {
    visitor.start_program(program)?;

    if let Some(relation_visitor) = visitor.relation_visitor() {
        for edb in [true, false] {
            let relations = if edb {
                program.extensional()
            } else {
                program.intensional()
            };
            if !relations.is_empty() {
                for relation in relations.iter() {
                    relation_visitor.start_relation(relation, edb)?;
                    for fact in relation.iter() {
                        relation_visitor.fact(fact)?;
                    }
                    relation_visitor.end_relation(relation, edb)?;
                }
            }
        }
    }

    let rules = program.rules();
    if !rules.is_empty() {
        if let Some(rule_visitor) = visitor.rule_visitor() {
            rule_visitor.start_rules(&rules)?;
            for rule in rules.iter() {
                rule_visitor.rule(rule)?;
            }
            rule_visitor.end_rules(&rules)?;
        }
    }

    let queries = program.queries();
    if !queries.is_empty() {
        if let Some(query_visitor) = visitor.query_visitor() {
            query_visitor.start_queries(queries)?;
            for query in queries.iter() {
                query_visitor.query(query)?;
            }
            query_visitor.end_queries(queries)?;
        }
    }

    visitor.end_program(program)
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

mod latex;
pub use latex::{make_latex_writer, LatexFormatter};

mod native;
pub use native::{make_native_writer, NativeFormatter};
