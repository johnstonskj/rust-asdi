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
use crate::{Collection, Program, ProgramCore, QuerySet, Relation, RelationSet, RuleSet};
use std::fmt::Display;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

pub trait ProgramVisitor<T> {
    fn start_program(&self, program: &Program) -> Result<T>;
    fn end_program(&self, program: &Program) -> Result<T>;

    fn start_relations(
        &self,
        relations: &RelationSet,
        extensional: bool,
    ) -> Result<Option<Box<dyn RelationVisitor<T>>>>;
    fn end_relations(&self, visitor: Box<dyn RelationVisitor<T>>) -> Result<T>;

    fn start_rules(&self, relations: &RuleSet) -> Result<Option<Box<dyn RuleVisitor<T>>>>;
    fn end_rules(&self, visitor: Box<dyn RuleVisitor<T>>) -> Result<T>;

    fn start_queries(&self, queries: &QuerySet) -> Result<Option<Box<dyn QueryVisitor<T>>>>;
    fn end_queries(&self, visitor: Box<dyn QueryVisitor<T>>) -> Result<T>;
}

pub trait RelationVisitor<T> {
    fn start_relation(&self, relation: &Relation) -> Result<T>;
    fn fact(&self, fact: &Fact) -> Result<T>;
    fn end_relation(&self, relation: &Relation) -> Result<T>;
}

pub trait RuleVisitor<T> {
    fn start_rule(&self, rule: &Rule) -> Result<T>;
    fn end_rule(&self, rule: &Rule) -> Result<T>;
}

pub trait QueryVisitor<T> {
    fn start_query(&self, query: &Query) -> Result<T>;
    fn end_query(&self, query: &Query) -> Result<T>;
}

///
/// A convenience for tools that are formatting and displaying a program. This requires the
/// underlying visitor to deal with strings and also to support `Display` so that a natural
/// call to `to_string` will return the calculated representation.
///
pub trait Formatter: Display + ProgramVisitor<String> {}

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

pub fn format_program(program: &Program, visitor: &impl Formatter) -> Result<String> {
    visit_program(program, visitor)
}

pub fn visit_program<T>(program: &Program, visitor: &impl ProgramVisitor<T>) -> Result<T> {
    visitor.start_program(program)?;

    for edb in [true, false] {
        let relations = if edb {
            program.extensional()
        } else {
            program.intensional()
        };
        if !relations.is_empty() {
            if let Some(relation_visitor) = visitor.start_relations(&relations, edb)? {
                for relation in relations.iter() {
                    relation_visitor.start_relation(relation)?;
                    for fact in relation.iter() {
                        relation_visitor.fact(fact)?;
                    }
                    relation_visitor.end_relation(relation)?;
                }
                visitor.end_relations(relation_visitor)?;
            }
        }
    }

    let rules = program.rules();
    if !rules.is_empty() {
        if let Some(rule_visitor) = visitor.start_rules(rules)? {
            for rule in rules.iter() {
                rule_visitor.start_rule(rule)?;
                rule_visitor.end_rule(rule)?;
            }
            visitor.end_rules(rule_visitor)?;
        }
    }

    let queries = program.queries();
    if !queries.is_empty() {
        if let Some(query_visitor) = visitor.start_queries(queries)? {
            for query in queries.iter() {
                query_visitor.start_query(query)?;
                query_visitor.end_query(query)?;
            }
            visitor.end_queries(query_visitor)?;
        }
    }

    visitor.end_program(program)
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

mod latex;
pub use latex::LatexTypesetter;
