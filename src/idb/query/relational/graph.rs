/*!
TBD
 */

use super::{Row, View};
use crate::edb::{Attribute, Constant, Schema};
use crate::error::{
    attribute_index_invalid, incompatible_types, nullary_facts_not_allowed, Error, Result,
};
use crate::idb::{Atom, Comparison, ComparisonOperator, Rule, Term, Variable};
use crate::ProgramCore;
use crate::{Collection, Labeled, MaybeAnonymous, MaybePositive, PredicateRef};
use paste::paste;
use regex::Regex;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use tracing::warn;

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

// TODO: (ISSUES/rust-asdi/13) Change current form from cluster-by-rule to cluster-by-strata
pub fn program_to_graphviz(program: &impl ProgramCore) -> String {
    format!(
        "digraph G {{\n{}\n}}\n",
        program
            .rules()
            .iter()
            .enumerate()
            .map(|(index, rule)| {
                let expr = RelationalOp::compile_rule(rule).unwrap();
                expr.to_graphviz_graph((index + 1) as u32, Some(rule.to_string()))
            })
            .collect::<Vec<String>>()
            .join("\n")
    )
}

pub fn relational_to_graphviz(v: &RelationalOp) -> Result<String> {
    Ok(self.to_graphviz_graph(1, None))
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

fn to_graphviz_graph(v: &RelationalOp, graph_index: u32, label: Option<String>) -> String {
    let (_, nodes, edges) = self.graphviz_one(1 + (graph_index * 100));
    format!(
            "{}{}\n{}\n}}",
            if graph_index == 0 {
                "digraph G {{\n".to_string()
            } else {
                format!(
                    "subgraph cluster_{} {{\n  color=gray;\n  label=\"{}\";\n\n",
                    graph_index,
                    match label {
                        None => format!("Rule {}", graph_index),
                        Some(label) => label,
                    }
                )
            },
            nodes
                .into_iter()
    3M            .map(|(_, (_, string))| string)
                .collect::<Vec<String>>()
                .join("\n"),
            edges
                .into_iter()
                .map(|(lhs, rhs)| format!("  node{} -> node{};", lhs, rhs))
                .collect::<Vec<String>>()
                .join("\n"),
        )
}

#[allow(clippy::type_complexity)]
fn graphviz_one(
    v: &RelationalOp,
    index: u32,
) -> (u32, HashMap<&Self, (u32, String)>, Vec<(u32, u32)>) {
    let mut node_map: HashMap<&Self, (u32, String)> = Default::default();
    let mut edge_vec: Vec<(u32, u32)> = Default::default();

    let next_index = match self {
        RelationalOp::Relation(op) => {
            if !node_map.contains_key(self) {
                node_map.insert(
                    self,
                    (
                        index,
                        if op.is_extensional.unwrap_or(false) {
                            format!(" node{}  [style=filled; label=\"{}\"];\n", index, op.source)
                        } else {
                            format!("  node{} [label=\"{}\"];", index, op.source)
                        },
                    ),
                );
            }
            index + 1
        }
        RelationalOp::Selection(op) => {
            let (next_index, nodes, edges) = op.source.graphviz_one(index + 1);
            node_map.extend(nodes.into_iter());
            edge_vec.extend(edges.into_iter());
            if let Some((source_index, _)) = node_map.get(op.source.as_ref()) {
                edge_vec.push((index, *source_index));
            } else {
                unreachable!()
            }

            node_map.insert(
                self,
                (
                    index,
                    format!(
                        "  node{} [label=\"σ\\n[{}]\"];",
                        index,
                        op.criteria
                            .iter()
                            .map(|c| c.to_string())
                            .collect::<Vec<String>>()
                            .join(", "),
                    ),
                ),
            );
            next_index + 1
        }
        RelationalOp::Projection(op) => {
            let (next_index, nodes, edges) = op.source.graphviz_one(index + 1);
            node_map.extend(nodes.into_iter());
            edge_vec.extend(edges.into_iter());
            if let Some((source_index, _)) = node_map.get(op.source.as_ref()) {
                edge_vec.push((index, *source_index));
            } else {
                unreachable!()
            }

            node_map.insert(
                self,
                (
                    index,
                    format!(
                        "  node{} [label=\"Π\\n[{}]\"];",
                        index,
                        op.attributes
                            .iter()
                            .map(|v| if let Some(label) = v.label() {
                                label.to_string()
                            } else if let Some(index) = v.index() {
                                index.to_string()
                            } else {
                                unreachable!()
                            })
                            .collect::<Vec<String>>()
                            .join(", "),
                    ),
                ),
            );
            next_index + 1
        }
        RelationalOp::Join(op) => {
            let (first_next_index, nodes, edges) = op.lhs.graphviz_one(index + 1);
            node_map.extend(nodes.into_iter());
            edge_vec.extend(edges.into_iter());
            if let Some((source_index, _)) = node_map.get(op.lhs.as_ref()) {
                edge_vec.push((index, *source_index));
            } else {
                unreachable!()
            }

            let (next_index, nodes, edges) = op.rhs.graphviz_one(first_next_index + 1);
            node_map.extend(nodes.into_iter());
            edge_vec.extend(edges.into_iter());
            if let Some((source_index, _)) = node_map.get(op.rhs.as_ref()) {
                edge_vec.push((index, *source_index));
            } else {
                unreachable!()
            }

            if op.is_natural() {
                node_map.insert(self, (index, format!("  node{} [label=\"⨝\"];", index,)));
            } else {
                node_map.insert(
                    self,
                    (
                        index,
                        format!(
                            "  node{} [label=\"⨝𝞱\\n[{}]\"];",
                            index,
                            op.criteria
                                .iter()
                                .map(|c| c.to_string())
                                .collect::<Vec<String>>()
                                .join(", "),
                        ),
                    ),
                );
            }
            next_index + 1
        }
        RelationalOp::SetOperation(op) => {
            let (first_next_index, nodes, edges) = op.lhs.graphviz_one(index + 1);
            node_map.extend(nodes.into_iter());
            edge_vec.extend(edges.into_iter());
            if let Some((source_index, _)) = node_map.get(op.lhs.as_ref()) {
                edge_vec.push((index, *source_index));
            } else {
                unreachable!()
            }

            let (next_index, nodes, edges) = op.rhs.graphviz_one(first_next_index + 1);
            node_map.extend(nodes.into_iter());
            edge_vec.extend(edges.into_iter());
            if let Some((source_index, _)) = node_map.get(op.rhs.as_ref()) {
                edge_vec.push((index, *source_index));
            } else {
                unreachable!()
            }

            node_map.insert(
                self,
                (index, format!("  node{} [label=\"{}\"];", index, op.op)),
            );

            next_index + 1
        }
        RelationalOp::Sink(op) => {
            let (next_index, nodes, edges) = op.source.graphviz_one(index + 1);
            node_map.extend(nodes.into_iter());
            edge_vec.extend(edges.into_iter());
            if let Some((source_index, _)) = node_map.get(op.source.as_ref()) {
                edge_vec.push((index, *source_index));
            } else {
                unreachable!()
            }

            node_map.insert(
                self,
                (index, format!("  node{} [label=\"{}\"];", index, op.sink)),
            );
            next_index + 1
        }
    };
    (next_index, node_map, edge_vec)
}
