/*!
One-line description.

![module UML](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/book/src/model/idb_eval_strata.svg)

More detailed description, with

# Example

*/

use crate::edb::{Predicate, RelationSet};
use crate::error::{program_not_stratifiable, Result};
use crate::features::{FeatureSet, FEATURE_DISJUNCTION};
use crate::idb::{Literal, Rule, RuleSet};
use crate::{Collection, Labeled, MaybePositive, Program, ProgramCore};
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::time::Instant;
use tracing::trace;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// The result of stratification, this representation of an IDB [RuleSet](../struct.RuleSet.html) is an optimization in
/// evaluation.
///
/// # Details
///
/// A stratification of a $\small\text{Datalog}^{\lnot}$, strictly termed $\small\text{Datalog}^{\lnot{s}}$,
/// program $P$ is a partition of $P$ into Datalog sub-programs $P_1, \ldots, P_m$ such that:
///
/// * All the rules defining the same IDB relation, $r$, are in the same partition.
/// * If an IDB $r$ appears positive in the body of a rule with head $r^{\prime}$, $r$ should be
///   defined in a partition before or where $r^{\prime}$ is defined.
/// * If an IDB $r$ appears negated in the body of a rule with head $r^{\prime}$, $r$ should be
///   defined in partition strictly before where $r^{\prime}$ is defined.
///
/// ```datalog
/// [P1]   v(X) :- r(X, Y).
/// [P1]   v(Y) :- r(X, Y).
/// [P2]   t(X, Y) :- r(X, Y).
/// [P2]   t(X, Y) :- t(X, Z), r(Z, Y).
/// [P3]   tc(X, Y):- v(X), v(Y), not t(X, Y).
/// ```
///
/// Notice that (P2) defines the IDB $t$, and (P3) defines $tc$; $tc$ has a negated IDB in the body of
/// a rule that defines it, but this IDB appears in the previous stratum (P2).
///
#[derive(Debug)]
pub struct StratifiedProgram<'a> {
    strata: Vec<SubProgram<'a>>,
}

#[derive(Debug, PartialEq)]
#[allow(single_use_lifetimes)]
pub struct SubProgram<'a> {
    program: &'a Program,
    strata: RuleSet,
}

///
///
/// # Example
///
/// ```datalog
/// .feature(negation).
/// .assert r(string, string).
///
/// v(X) :- r(X, Y).
/// v(Y) :- r(X, Y).
/// t(X, Y) :- r(X, Y).
/// t(X, Y) :- t(X, Z), r(Z, Y).
/// tc(X, Y):- v(X), v(Y), not t(X, Y).
/// ```
///
/// ```text
/// digraph G {
///   t -> r;
///   t -> t;
///   tc -> t [arrowhead=empty];
///   tc -> v;
///   v -> r;
///
///   r [style=filled; color=lightgrey];
/// }
/// ```
///
#[derive(Debug, Default)]
#[allow(single_use_lifetimes)]
pub struct PrecedenceGraph<'a>(HashSet<PrecedenceNode<'a>>);

#[derive(Debug, PartialEq, Eq, Hash)]
#[allow(single_use_lifetimes)]
pub struct PrecedenceNode<'a> {
    negative: bool,
    extensional: bool,
    source: &'a Predicate,
    target: &'a Predicate,
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

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Display for StratifiedProgram<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, strata) in self.strata.iter().enumerate() {
            writeln!(f, "[{}", i)?;
            for (j, rule) in strata.rules().iter().enumerate() {
                writeln!(f, "  [{}] {:?}", j, rule)?;
            }
            writeln!(f, "]")?;
        }
        Ok(())
    }
}

impl<'a> Collection<SubProgram<'a>> for StratifiedProgram<'a> {
    fn is_empty(&self) -> bool {
        self.strata.is_empty()
    }

    fn len(&self) -> usize {
        self.strata.len()
    }

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ SubProgram<'a>> + '_> {
        Box::new(self.strata.iter())
    }

    fn contains(&self, value: &SubProgram<'_>) -> bool {
        self.strata.contains(value)
    }
}

impl<'a> StratifiedProgram<'a> {
    pub fn from(program: &'a Program) -> Result<Self> {
        // TODO: (ISSUE/rust-asdi/7) Design for disjunction
        assert!(!program.features().supports(&FEATURE_DISJUNCTION));

        let start = Instant::now();

        let rules = program.rules();
        let graph = PrecedenceGraph::from(program);
        let mut strata: Vec<SubProgram<'_>> = Vec::with_capacity(graph.sources().len());

        if graph.is_stratifiable() {
            for rule in rules.iter() {
                if strata.is_empty() {
                    strata.push(SubProgram::from_with_rule(program, rule));
                } else {
                    let mut leveled: bool = false;
                    let head_label = rule.head().map(|atom| atom.label()).next().unwrap();

                    // 1. All the rules defining the same IDB relation are in the same partition.
                    for stratum in strata.iter_mut() {
                        if stratum
                            .rules()
                            .iter()
                            .next()
                            .unwrap()
                            .head()
                            .map(|atom| atom.label())
                            .next()
                            .unwrap()
                            == head_label
                        {
                            stratum.add_rule(rule);
                            leveled = true;
                            break;
                        }
                    }

                    // 2. If an IDB R appears positive in the body of a rule with head R0, R should
                    //    be defined in a partition before or where R0 is defined.
                    //
                    // r0(X) :- r(X). => [[r...][r0...]] | [[r..., r0...]]
                    //
                    // 3. If an IDB R appears negated in the body of a rule with head R0, R should
                    //    be defined in partition strictly before where R0 is defined.
                    //
                    // r0(X) :- r(X). => [[r...][r0...]]
                    //
                    if !leveled {
                        for (i, stratum) in strata.iter().enumerate() {
                            if stratum.rules().iter().any(|r2| {
                                r2.literals()
                                    .filter_map(Literal::as_relational)
                                    .any(|atom| atom.label() == head_label)
                            }) {
                                strata.insert(i, SubProgram::from_with_rule(program, rule));
                                leveled = true;
                                break;
                            }
                        }
                    }

                    if !leveled {
                        strata.push(SubProgram::from_with_rule(program, rule));
                    }
                }
            }
            let delta = start.elapsed();
            trace!(
                "Stratified {} rules into {} strata, in {}s",
                program.rules().len(),
                strata.len(),
                delta.as_secs_f64()
            );
            Ok(Self { strata })
        } else {
            Err(program_not_stratifiable())
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl ProgramCore for SubProgram<'_> {
    fn features(&self) -> &FeatureSet {
        self.program.features()
    }

    fn extensional(&self) -> &RelationSet {
        self.program.extensional()
    }

    fn intensional(&self) -> &RelationSet {
        self.program.intensional()
    }

    fn rules(&self) -> &RuleSet {
        &self.strata
    }
}

impl<'a> SubProgram<'a> {
    fn from_with_rule(program: &'a Program, rule: &Rule) -> Self {
        let mut new = Self {
            program,
            strata: Default::default(),
        };
        new.add_rule(rule);
        new
    }

    fn add_rule(&mut self, rule: &Rule) {
        self.strata.add(rule.clone());
    }
}

// ------------------------------------------------------------------------------------------------

impl<'a> PrecedenceGraph<'a> {
    pub fn from(program: &'a Program) -> Self {
        let mut graph = PrecedenceGraph::default();
        for rule in program.rules().iter() {
            for from_label in head_predicates(rule) {
                for (negative, to_label) in body_predicates(rule) {
                    graph.0.insert(PrecedenceNode {
                        negative,
                        extensional: program.extensional().contains(to_label),
                        source: from_label,
                        target: to_label,
                    });
                }
            }
        }
        graph
    }

    pub fn edges(&self) -> impl Iterator<Item = &'_ PrecedenceNode<'_>> {
        self.0.iter()
    }

    pub fn sources(&self) -> HashSet<&'_ Predicate> {
        self.edges().map(|e| e.source()).collect()
    }

    pub fn targets(&self) -> HashSet<&'_ Predicate> {
        self.edges().map(|e| e.target()).collect()
    }

    pub fn is_level_zero(&self, source: &Predicate) -> bool {
        self.directly_reachable_nodes_from(source)
            .into_iter()
            .all(|node| node.is_extensional_target())
    }

    pub fn is_recursive(&self) -> bool {
        self.sources()
            .into_iter()
            .any(|source| self.reachable_from(source).contains(source))
    }

    pub fn is_positive(&self) -> bool {
        self.edges().all(|edge| !edge.is_negative_target())
    }

    pub fn is_semi_positive(&self) -> bool {
        let all_sources = self.sources();
        all_sources.iter().all(|source| {
            self.directly_reachable_nodes_from(source)
                .into_iter()
                .filter(|node| node.is_negative_target())
                .all(|node| node.is_extensional_target())
        })
    }

    pub fn is_stratifiable(&self) -> bool {
        let all_sources = self.sources();
        all_sources.iter().all(|source| {
            self.directly_reachable_nodes_from(source)
                .into_iter()
                .filter(|node| node.is_negative_target())
                .all(|node| !self.reachable_from(node.target()).contains(source))
        })
    }

    pub fn directly_reachable_from(&self, source: &Predicate) -> HashSet<&'_ Predicate> {
        self.edges()
            .filter(|e| e.source() == source)
            .map(PrecedenceNode::target)
            .collect()
    }

    pub fn reachable_from(&self, source: &Predicate) -> HashSet<&'_ Predicate> {
        let mut initial: HashSet<&Predicate> = self.directly_reachable_from(source);
        if initial.contains(source) {
            initial
        } else {
            let next: HashSet<&Predicate> = initial
                .iter()
                .map(|s| self.directly_reachable_from(s))
                .flatten()
                .collect();
            initial.extend(next);
            initial
        }
    }

    fn directly_reachable_nodes_from(&self, source: &Predicate) -> HashSet<&'_ PrecedenceNode<'_>> {
        self.edges().filter(|e| e.source() == source).collect()
    }

    pub fn to_graphviz_string(&self) -> Result<String> {
        let mut edges: Vec<String> = self
            .edges()
            .map(|edge| {
                format!(
                    "  {} -> {}{};\n",
                    edge.source(),
                    edge.target(),
                    if edge.is_negative_target() {
                        " [arrowhead=empty]"
                    } else {
                        ""
                    },
                )
            })
            .collect();
        edges.sort();

        let mut edb: Vec<String> = self
            .edges()
            .filter_map(|r| {
                if r.is_extensional_target() {
                    Some(format!(
                        "  {} [style=filled; color=lightgrey];\n",
                        r.target()
                    ))
                } else {
                    None
                }
            })
            .collect();
        edb.sort();
        edb.dedup();

        Ok(format!(
            "digraph G {{\n{}\n{}}}",
            edges.join(""),
            edb.join("")
        ))
    }
}

// ------------------------------------------------------------------------------------------------

impl<'a> PrecedenceNode<'a> {
    #[inline]
    pub fn is_negative_target(&self) -> bool {
        self.negative
    }

    #[inline]
    pub fn is_extensional_target(&self) -> bool {
        self.extensional
    }

    #[inline]
    pub fn source(&self) -> &'a Predicate {
        self.source
    }

    #[inline]
    pub fn target(&self) -> &'a Predicate {
        self.target
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

#[inline]
fn head_predicates(rule: &Rule) -> HashSet<&Predicate> {
    rule.head().map(|a| a.label()).collect()
}

#[inline]
fn body_predicates(rule: &Rule) -> HashSet<(bool, &Predicate)> {
    rule.literals()
        .filter_map(|l| l.as_relational().map(|a| (l.is_negative(), a.label())))
        .collect()
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Unit Tests
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::idb::eval::strata::{PrecedenceGraph, StratifiedProgram};
    use crate::parse::parse_str;
    use crate::Predicate;
    use std::str::FromStr;

    #[test]
    fn test_stratification() {
        let program = parse_str(
            r#".feature(negation).
.assert r(string, string).

v(X) :- r(X, Y).
v(Y) :- r(X, Y).
t(X, Y) :- r(X, Y).
t(X, Y) :- t(X, Z), r(Z, Y).
tc(X, Y):- v(X), v(Y), NOT t(X, Y).
"#,
        )
        .unwrap()
        .into_parsed();

        let stratified = StratifiedProgram::from(&program);
        assert!(stratified.is_ok());
        println!("{}", stratified.unwrap());
    }

    #[test]
    fn test_precedence_graph() {
        let program = parse_str(
            r#".feature(negation).
.assert r(string, string).

v(X) :- r(X, Y).
v(Y) :- r(X, Y).
t(X, Y) :- r(X, Y).
t(X, Y) :- t(X, Z), r(Z, Y).
tc(X, Y):- v(X), v(Y), NOT t(X, Y).
"#,
        )
        .unwrap()
        .into_parsed();

        assert!(program.is_recursive());

        let graph = PrecedenceGraph::from(&program);
        println!("{}", graph.to_graphviz_string().unwrap());

        assert!(graph.is_recursive());
        assert!(!graph.is_semi_positive());
        assert!(graph.is_stratifiable());

        let pred_t = Predicate::from_str("t").unwrap();
        let pred_v = Predicate::from_str("v").unwrap();
        let pred_tc = Predicate::from_str("tc").unwrap();

        assert!(graph.is_level_zero(&pred_v));
        assert!(!graph.is_level_zero(&pred_t));
        assert!(!graph.is_level_zero(&pred_tc));

        println!(
            "t -> {:?}",
            graph.reachable_from(&Predicate::from_str("t").unwrap())
        );
        println!(
            "tc -> {:?}",
            graph.reachable_from(&Predicate::from_str("tc").unwrap())
        );
    }

    #[test]
    fn test_precedence_graph_rdfs() {
        let program = parse_str(
            r#".feature(comparisons).
.assert triple(string, string, string).

% Section 2.2: Class
class(C) :- triple(_, rdf:type, C).

% Section 2.4: Datatype
subClass(R, rdfs:Literal) :- instanceOf(R, rdfs:Datatype).

% Section 2.8: Property
property(P) :- triple(_, P, _).
property(P) :- triple(P, rdf:type, rdfs:Property).

% Section 3.1: range
range(P, C) :- triple(P, rdfs:range, C).
instanceOf(R, C) :- triple(_, P, R), range(P, C).
range(P2, R) :- range(P, R), subProperty(P2, P).

% Section 3.2: domain
domain(P, C) :- triple(P, rdfs:domain, C).
instanceOf(R, C) :- triple(R, P, _), domain(P, C).
domain(P2, R) :- domain(P, R), subProperty(P2, P).

% Section 3.3: rdf:type
instanceOf(R, C) :- triple(R, rdf:type, C).
instanceOf(R, C) :- triple(R, P, _), triple(P, rdfs:domain, C).
instanceOf(R, C) :- triple(_, P, R), triple(P, rdfs:range, C).

% Section 3.4: subClassOf
subClass(C, P) :- triple(C, rdfs:subClassOf, P).
class(C) :- subClass(C, _).
class(C) :- subClass(_, C).
instanceOf(C, C2) :- instanceOf(C, C1), subClass(C1, C2).
subClass(C, rdfs:Class) :- class(C) AND C /= rdfs:Class.

% Section 3.5: subPropertyOf
subProperty(C, P) :- triple(C, rdfs:subPropertyOf, P).
property(P) :- subProperty(P, _).
property(P) :- subProperty(_, P).
instanceOf(P, P2) :- instanceOf(P, P1), subProperty(P1, P2).
subProperty(P, rdfs:Property) :- property(P) AND P /= rdfs:Property.

% Section 5.1.5: ContainerMembershipProperty
subProperty(P, rdfs:member) :- instanceOf(P, rdfs:ContainerMembershipProperty).
"#,
        )
        .unwrap()
        .into_parsed();

        let graph = PrecedenceGraph::from(&program);
        println!("{}", graph.to_graphviz_string().unwrap());

        assert!(graph.is_recursive());
        assert!(graph.is_semi_positive());
        assert!(graph.is_stratifiable());

        println!(
            "property -> {:?}",
            graph.reachable_from(&Predicate::from_str("property").unwrap())
        );

        println!(
            "instanceOf -> {:?}",
            graph.reachable_from(&Predicate::from_str("instanceOf").unwrap())
        );
    }

    #[test]
    fn test_is_semi_positive() {
        let program = parse_str(
            r#".feature(negation).
.assert link(string, string).

reachable(X, Y) :- link(X, Y).
reachable(X, Y) :- link(X, Z), reachable(Z, Y).
indirect(X, Y) :- reachable(X, Y), NOT link(X, Y).
"#,
        )
        .unwrap()
        .into_parsed();

        let graph = PrecedenceGraph::from(&program);
        println!("{}", graph.to_graphviz_string().unwrap());

        assert!(graph.is_recursive());
        assert!(graph.is_semi_positive());
        assert!(graph.is_stratifiable());
    }

    #[test]
    fn test_is_stratifiable() {
        let program = parse_str(
            r#".feature(negation).
.assert link(string, string).

reachable(X, Y) :- link(X, Y).
reachable(X, Y) :- link(X, Z), reachable(Z, Y).
anode(X) :- link(X, _).
anode(Y) :- link(_, Y).
unreachable(X, Y) :- reachable(X, Y), anode(X), anode(Y), NOT reachable(X, Y).
"#,
        )
        .unwrap()
        .into_parsed();

        let graph = PrecedenceGraph::from(&program);
        println!("{}", graph.to_graphviz_string().unwrap());

        assert!(graph.is_recursive());
        assert!(!graph.is_semi_positive());
        assert!(graph.is_stratifiable());
    }

    #[test]
    fn test_is_not_stratifiable() {
        let program = parse_str(
            r#".feature(negation).
.assert link(string, string).

reachable(X, Y) :- link(X, Y).
reachable(X, Y) :- link(X, Z), reachable(Z, Y).
absurdity(X, Y) :- unreachable(X, Y).
unreachable(X, Y) :- reachable(X, Y), NOT absurdity(X, Y).
"#,
        )
        .unwrap()
        .into_parsed();

        let graph = PrecedenceGraph::from(&program);
        println!("{}", graph.to_graphviz_string().unwrap());

        assert!(graph.is_recursive());
        assert!(!graph.is_semi_positive());
        assert!(!graph.is_stratifiable());

        let stratified = StratifiedProgram::from(&program);
        assert!(stratified.is_err());
    }
}
