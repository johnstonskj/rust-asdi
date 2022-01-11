/*!
One-line description.

More detailed description, with

# Example

*/

use crate::error::Result;
use crate::idb::{Atom, Literal, Rule};
use crate::{error, FEATURE_DISJUNCTION};
use crate::{Collection, Labeled, MaybePositive, Predicate, Program};
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

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
pub struct StratifiedRules<'a> {
    strata: Vec<Vec<&'a Rule>>,
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

impl Display for StratifiedRules<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, strata) in self.strata.iter().enumerate() {
            writeln!(f, "[{}", i)?;
            for (j, rule) in strata.iter().enumerate() {
                writeln!(f, "  [{}] {}", j, rule)?;
            }
            writeln!(f, "]")?;
        }
        Ok(())
    }
}

impl<'a> From<StratifiedRules<'a>> for Vec<&'a Rule> {
    fn from(rules: StratifiedRules<'a>) -> Self {
        rules.strata.into_iter().flatten().collect()
    }
}

impl<'a> StratifiedRules<'a> {
    pub fn from(program: &'a Program) -> Result<Self> {
        // TODO: lift this restriction
        assert!(!program.features().supports(&FEATURE_DISJUNCTION));

        let rules = program.rules();
        let graph = PrecedenceGraph::from(program);
        let mut strata: Vec<Vec<&'a Rule>> = Vec::with_capacity(graph.sources().len());

        if graph.is_stratifiable() {
            for rule in rules.iter() {
                if strata.is_empty() {
                    strata.push(vec![rule]);
                } else {
                    let mut leveled: bool = false;
                    let head_label = rule.head().map(|atom| atom.label()).next().unwrap();

                    // 1. All the rules defining the same IDB relation are in the same partition.
                    for stratum in strata.iter_mut() {
                        if stratum
                            .get(0)
                            .unwrap()
                            .head()
                            .map(|atom| atom.label())
                            .next()
                            .unwrap()
                            == head_label
                        {
                            stratum.push(rule);
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
                            if stratum.iter().any(|r2| {
                                r2.literals()
                                    .filter_map(Literal::as_relational)
                                    .any(|atom| atom.label() == head_label)
                            }) {
                                strata.insert(i, vec![rule]);
                                leveled = true;
                                break;
                            }
                        }
                    }

                    if !leveled {
                        strata.push(vec![rule]);
                    }
                }
            }
            Ok(Self { strata })
        } else {
            error::program_not_stratifiable().into()
        }
    }

    pub fn is_empty(&self) -> bool {
        self.strata.is_empty()
    }

    pub fn len(&self) -> usize {
        self.strata.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Vec<&'a Rule>> {
        self.strata.iter()
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

    pub fn to_graphviz_string(&self) -> String {
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

        format!("digraph G {{\n{}\n{}}}", edges.join(""), edb.join(""))
    }
}

// ------------------------------------------------------------------------------------------------

impl<'a> PrecedenceNode<'a> {
    pub fn is_negative_target(&self) -> bool {
        self.negative
    }

    pub fn is_extensional_target(&self) -> bool {
        self.extensional
    }

    pub fn source(&self) -> &'a Predicate {
        self.source
    }

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
    use crate::idb::eval::strata::PrecedenceGraph;
    use crate::idb::eval::StratifiedRules;
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

        let stratified = StratifiedRules::from(&program);
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
        println!("{}", graph.to_graphviz_string());

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
        println!("{}", graph.to_graphviz_string());

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
        println!("{}", graph.to_graphviz_string());

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
        println!("{}", graph.to_graphviz_string());

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
        println!("{}", graph.to_graphviz_string());

        assert!(graph.is_recursive());
        assert!(!graph.is_semi_positive());
        assert!(!graph.is_stratifiable());

        let stratified = StratifiedRules::from(&program);
        assert!(stratified.is_err());
    }
}
