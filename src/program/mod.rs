/*!
One-line description.

More detailed description, with

# Example

 */

use crate::edb::{Attribute, Database, DbValidation, Predicate, Relation, Schema};
use crate::error::Result;
use crate::features::FeatureSet;
use crate::idb::{Atom, Literal, Rule, Term, Variable};
use crate::query::{Query, View};
use crate::SyntacticFragments;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// A program is a complete world from which we can derive new facts, or query. A program consists
/// therefore an [_environment_](environment/struct.Environment.html) that captures identifiers
/// and string constants, and a set of program elements.
///
/// While some literature requires that a program's text representation list all [_facts_](struct.Fact.html), then
/// all [_rules_](struct.Rule.html), and then any [_queries_](struct.Query.html), this model
/// makes no such distinction.
///
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Program {
    features: FeatureSet,
    edb: Database,
    idb: HashSet<Rule>,
    queries: HashSet<Query>,
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

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.features.is_default() {
            writeln!(f, "{}", self.features)?;
            writeln!(f)?;
        }

        write!(f, "{}", self.edb)?;

        for rule in self.idb.iter() {
            writeln!(f, "{}", rule)?;
        }
        writeln!(f)?;

        for query in self.queries() {
            writeln!(f, "{}", query)?;
        }

        Ok(())
    }
}

impl SyntacticFragments for Program {
    fn is_positive(&self) -> bool {
        self.rules().all(|rule| rule.is_positive())
    }

    fn is_linear(&self) -> bool {
        self.rules().all(|rule| rule.is_linear())
    }

    fn is_guarded(&self) -> bool {
        self.rules().all(|rule| rule.is_guarded())
    }

    fn is_frontier_guarded(&self) -> bool {
        self.rules().all(|rule| rule.is_frontier_guarded())
    }

    fn is_non_recursive(&self) -> bool {
        self.rules().all(|rule| rule.is_non_recursive())
    }
}

impl Program {
    pub fn new_with_features(features: FeatureSet) -> Self {
        Self {
            features,
            edb: Default::default(),
            idb: Default::default(),
            queries: Default::default(),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn features(&self) -> &FeatureSet {
        &self.features
    }

    pub(crate) fn features_mut(&mut self) -> &mut FeatureSet {
        &mut self.features
    }

    // --------------------------------------------------------------------------------------------

    pub fn database(&self) -> &Database {
        &self.edb
    }

    pub fn database_mut(&mut self) -> &mut Database {
        &mut self.edb
    }

    pub fn add_relation(&mut self, relation: Relation) {
        self.database_mut().add(relation)
    }

    pub fn add_new_relation<V: Into<Schema<Predicate>>>(
        &mut self,
        predicate: Predicate,
        schema: V,
    ) -> Result<&mut Relation> {
        self.database_mut()
            .add_new_relation(predicate, schema.into())
    }

    // --------------------------------------------------------------------------------------------

    /// intensional predicate symbols
    pub fn rules(&self) -> impl Iterator<Item = &Rule> {
        self.idb.iter()
    }

    pub fn make_new_rule<H: Into<Vec<Term>>, B: Into<Vec<Literal>>>(
        &self,
        head_predicate: Predicate,
        head_terms: H,
        body: B,
    ) -> Result<Rule> {
        Ok(Rule::new(Atom::new(head_predicate, head_terms), body))
    }

    pub fn add_new_rule<H: Into<Vec<Term>>, B: Into<Vec<Literal>>>(
        &mut self,
        head_predicate: Predicate,
        head_terms: H,
        body: B,
    ) -> Result<bool> {
        let rule = self.make_new_rule(head_predicate, head_terms, body)?;
        self.add_rule(rule)
    }

    pub fn add_rule(&mut self, rule: Rule) -> Result<bool> {
        rule.check_well_formed(self.features())?;

        //TODO: fix this: self.add_rule_relations(rule.head(), &rule);
        rule.literals()
            .filter_map(Literal::as_atom)
            .for_each(|a| self.add_rule_relations(a, &rule));

        rule.validate(self.database_mut())?;
        Ok(self.idb.insert(rule))
    }

    fn add_rule_relations(&mut self, atom: &Atom, rule: &Rule) {
        if !self.edb.contains(atom.predicate()) {
            let mut schema = Vec::with_capacity(atom.arity());
            for term in atom.terms() {
                match term {
                    Term::Variable(v) => schema.push(self.infer_attribute(v, rule)),
                    Term::Constant(c) => schema.push(Attribute::from(c.kind())),
                }
            }
            // TODO: propagate errors
            let _ = self
                .database_mut()
                .add_new_relation(atom.predicate().clone(), schema);
        }
    }

    fn infer_attribute(&self, variable: &Variable, rule: &Rule) -> Attribute<Predicate> {
        let candidates: Vec<(&Predicate, usize)> = rule
            .literals()
            .filter_map(Literal::as_atom)
            .filter_map(|a| {
                a.terms()
                    .enumerate()
                    .filter_map(|(i, term)| term.as_variable().map(|var| (i, var)))
                    .find(|(_, var)| var == &variable)
                    .map(|(i, _)| (a.predicate(), i))
            })
            .collect();
        for (predicate, i) in candidates {
            if let Some(relation) = self.database().relation(predicate) {
                return relation.schema().get(i).unwrap().clone();
            }
        }
        Attribute::anonymous()
    }

    // --------------------------------------------------------------------------------------------

    pub fn queries(&self) -> impl Iterator<Item = &Query> {
        self.queries.iter()
    }

    pub fn make_new_query<T: Into<Vec<Term>>>(
        &self,
        predicate: Predicate,
        terms: T,
    ) -> Result<Query> {
        Ok(Query::new(predicate, terms))
    }

    pub fn add_new_query<T: Into<Vec<Term>>>(
        &mut self,
        predicate: Predicate,
        terms: T,
    ) -> Result<bool> {
        let query = self.make_new_query(predicate, terms)?;
        self.add_query(query)
    }

    pub fn add_query(&mut self, query: Query) -> Result<bool> {
        query.validate(self.database_mut())?;
        Ok(self.queries.insert(query))
    }

    pub fn eval_query(&self, query: &Query) -> Result<View> {
        Ok(self.database().query(query))
    }

    pub fn eval_queries(&self) -> Vec<(&Query, Result<View>)> {
        self.queries().map(|q| (q, self.eval_query(q))).collect()
    }

    // --------------------------------------------------------------------------------------------

    pub fn check_well_formed(&self, _features: &FeatureSet) -> Result<()> {
        let result: Result<()> = self
            .rules()
            .try_for_each(|r| r.check_well_formed(&self.features));
        result
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
