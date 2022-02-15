/*!
Another Simplistic [Datalog](https://en.wikipedia.org/wiki/Datalog) Implementation (ASDI), in Rust.

This package provides a data model to represent [Datalog](https://en.wikipedia.org/wiki/Datalog)
programs in memory, a parser for the textual representation, and some evaluation implementations.

![module UML](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/book/src/model/lib.svg)

The text representation parser is a separate feature, so if you only need to construct and evaluate
programs using the API you may opt out of the [Pest](https://pest.rs) parser and support.

# Example

The following program is the classical syllogism example, in the text representation.

```datalog
human("Socrates").

mortal(X) <- human(X).

?- mortal("Socrates").
```

Note in this example we allow the parser to identify the schema for the relations `human` and
`mortal` rather than using the pragmas `assert` and `infer`.

The following is the same example constructed via the ASDI library.

```rust
use asdi::{NameReferenceSet, Program};
use asdi::edb::{Attribute, Predicate};
use asdi::idb::{Atom, Term, Variable};
use std::str::FromStr;

let mut syllogism = Program::default();

let predicates = syllogism.predicates();
let p_human = predicates.fetch("human").unwrap();
let p_mortal = predicates.fetch("mortal").unwrap();

let human = syllogism
    .add_new_extensional_relation(p_human.clone(), vec![Attribute::string()])
    .unwrap();
human.add_as_fact(["Socrates".into()]).unwrap();

let variables = syllogism.variables();
let var_x: Term = variables.fetch("X").unwrap().into();

syllogism
    .add_new_pure_rule(
        p_mortal.clone(),
        [var_x.clone()],
        [Atom::new(p_human, [var_x]).into()],
    )
    .unwrap();

syllogism
    .add_new_query(p_mortal, ["Socrates".into()])
    .unwrap();
```

The execution of this program will start with the goal query "_is Socrates mortal?_" and in
doing so will evaluate the necessary rule and derive the relation _mortal_. The result is a
boolean value denoting whether the goal is satisfied.

```text
+------------+
| _: boolean |
+============+
| true      |
+------------+
```

However, if we were to change the final query to replace the constant with a variable, as follows.

```datalog
?- mortal(X).
```

The program will select all matching (in this case all) facts from the _mortal_ relation.

```text
+------------+
| X: string  |
+============+
| "Socrates" |
+------------+
```

*/

#![warn(
    unknown_lints,
    // ---------- Stylistic
    absolute_paths_not_starting_with_crate,
    elided_lifetimes_in_paths,
    explicit_outlives_requirements,
    macro_use_extern_crate,
    nonstandard_style, /* group */
    noop_method_call,
    rust_2018_idioms,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    // ---------- Future
    future_incompatible, /* group */
    rust_2021_compatibility, /* group */
    // ---------- Public
    missing_debug_implementations,
    // missing_docs,
    unreachable_pub,
    // ---------- Unsafe
    unsafe_code,
    unsafe_op_in_unsafe_fn,
    // ---------- Unused
    unused, /* group */
)]
#![deny(
    // ---------- Public
    exported_private_dependencies,
    private_in_public,
    // ---------- Deprecated
    anonymous_parameters,
    bare_trait_objects,
    ellipsis_inclusive_range_patterns,
    // ---------- Unsafe
    deref_nullptr,
    drop_bounds,
    dyn_drop,
)]

use crate::edb::{Attribute, Predicate, PredicateRef, Relation, RelationSet, Schema};
use crate::error::{
    extensional_predicate_in_rule_head, language_feature_unsupported, relation_does_not_exist,
    Result,
};
use crate::features::{FeatureSet, FEATURE_CONSTRAINTS, FEATURE_DISJUNCTION, FEATURE_NEGATION};
use crate::idb::eval::{strata::PrecedenceGraph, Evaluator};
use crate::idb::query::{Query, QuerySet, Queryable, View};
use crate::idb::{Atom, Literal, Rule, RuleForm, RuleSet, Term, Variable, VariableRef};
use crate::visitor::{make_native_writer, write_program};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::path::PathBuf;
use std::rc::Rc;
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

pub const MIME_TYPE_BASE: &str = "text/vnd.datalog";

///
/// Core, readable, properties of a Datalog program.
pub trait ProgramCore {
    ///
    /// Returns the set of features currently supported by this program.
    ///
    fn features(&self) -> &FeatureSet;

    ///
    /// Returns the current set of extensional relations.
    ///
    fn extensional(&self) -> &RelationSet;

    ///
    /// Returns the current set of intensional relations.
    ///
    fn intensional(&self) -> &RelationSet;

    ///
    /// Return an iterator over the rules in the intensional database.
    ///
    fn rules(&self) -> &RuleSet;
}

///
/// A program consists of a set of extensional [`RelationSet`], a set of intensional
/// [`RelationSet`], a set of [`RuleSet`], and a set of [queries](Query).
///  
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Program {
    from_file: Option<PathBuf>,
    features: FeatureSet,
    predicate_cache: NameReferenceSet<Predicate>,
    variable_cache: NameReferenceSet<Variable>,
    extensional: RelationSet,
    intensional: RelationSet,
    rules: RuleSet,
    queries: QuerySet,
}

///
/// The predicate set $\small\mathcal{P}$ determines the labels of relations, atoms, and facts. This
/// type keeps a mapping of strings to [PredicateRef]s to reduce memory duplication.
///
/// ```rust
/// use asdi::Program;
///
/// let mut program = Program::default();
///
/// let p_human = program.predicates().fetch("human").unwrap();
/// let p_mortal = program.predicates().fetch("mortal").unwrap();
///
/// assert!(program.predicates().fetch("Not A Predicate").is_err());
/// ```
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NameReferenceSet<T>(RefCell<BTreeMap<String, AttributeNameRef<T>>>)
where
    T: AttributeName;

///
/// Attributes, the members of [Schema] are named using different types in [Relation]s and [View]s.
/// This trait identifies the minimum set of implementations required for an attribute name.
///
pub trait AttributeName:
    AsRef<str> + Clone + Debug + Display + FromStr + PartialEq + Eq + PartialOrd + Ord + Hash
{
    ///
    /// Return `true` if the string `s` is a valid value for the implementing type, else `false`.
    ///
    fn is_valid(s: &str) -> bool;

    ///
    /// Return the type identifier for the implementation, used in format errors.
    ///
    fn type_name() -> &'static str;
}

///
/// A reference type for attribute names.
///
#[allow(type_alias_bounds)]
pub type AttributeNameRef<T: AttributeName> = Rc<T>;

///
/// All collections of things in the library implement these basic methods.
///
pub trait Collection<T> {
    fn is_empty(&self) -> bool;

    fn len(&self) -> usize;

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ T> + '_>;

    fn contains(&self, value: &T) -> bool;
}

///
/// All indexed collections of things in the library implement these basic methods.
///
pub trait IndexedCollection<K, V>: Collection<V> {
    fn get(&self, index: &K) -> Option<&V>;

    fn contains_index(&self, index: &K) -> bool;
}

///
/// Implemented by types that have a label.
///
pub trait Labeled {
    ///
    /// Return the label associated with this value.
    ///
    fn label(&self) -> &Predicate;

    fn label_ref(&self) -> PredicateRef;
}

///
/// Implemented by types that have the notion of an anonymous value.
///
pub trait MaybeAnonymous {
    ///
    /// Construct a new anonymous instance.
    ///
    fn anonymous() -> Self
    where
        Self: Sized;

    ///
    /// Return `true` if this value is anonymous, else `false`.
    ///
    fn is_anonymous(&self) -> bool;
}

///
/// Implemented by types that need to distinguished between values that may contain negative literals.
///
pub trait MaybePositive {
    ///
    /// Returns `true` if this value is positive, else `false`.
    ///
    fn is_positive(&self) -> bool;

    ///
    /// Returns `true` if this value is negative, else `false`.
    ///
    fn is_negative(&self) -> bool {
        !self.is_positive()
    }
}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use std::io::BufWriter;
        let mut buffer = BufWriter::new(Vec::new());
        let visitor = make_native_writer(&mut buffer);
        // TODO: propagate errors
        write_program(self, &visitor).unwrap();
        let s = String::from_utf8(buffer.into_inner().unwrap()).unwrap();
        write!(f, "{}", s)
    }
}

impl MaybePositive for Program {
    fn is_positive(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_positive())
    }
}

impl ProgramCore for Program {
    fn features(&self) -> &FeatureSet {
        &self.features
    }

    fn extensional(&self) -> &RelationSet {
        &self.extensional
    }

    fn intensional(&self) -> &RelationSet {
        &self.intensional
    }

    fn rules(&self) -> &RuleSet {
        &self.rules
    }
}

impl Program {
    pub fn new_with_features(features: FeatureSet) -> Self {
        Self {
            from_file: None,
            features,
            predicate_cache: Default::default(),
            variable_cache: Default::default(),
            extensional: Default::default(),
            intensional: Default::default(),
            queries: Default::default(),
            rules: Default::default(),
        }
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// If this program were read from a source file, this will return the path to the file that
    /// was read, else `None`.
    ///
    pub fn source_file_path(&self) -> Option<&PathBuf> {
        self.from_file.as_ref()
    }

    pub(crate) fn set_source_file_path(&mut self, file_name: PathBuf) {
        self.from_file = Some(file_name);
    }

    // --------------------------------------------------------------------------------------------

    pub(crate) fn features_mut(&mut self) -> &mut FeatureSet {
        &mut self.features
    }

    // --------------------------------------------------------------------------------------------

    pub fn predicates(&self) -> &NameReferenceSet<Predicate> {
        &self.predicate_cache
    }

    pub fn variables(&self) -> &NameReferenceSet<Variable> {
        &self.variable_cache
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// Returns the current set of extensional relations in a mutable state.
    ///
    pub fn extensional_mut(&mut self) -> &mut RelationSet {
        &mut self.extensional
    }

    ///
    /// Add a new relation to the extensional database with the given `label` and `schema`.
    ///
    pub fn add_new_extensional_relation<V: Into<Schema<Predicate>>>(
        &mut self,
        label: PredicateRef,
        schema: V,
    ) -> Result<&mut Relation> {
        let label = self.predicate_cache.canonical(label);
        self.extensional_mut()
            .add_new_relation(label, schema.into())
    }

    ///
    /// Add the provided `relation` to the extensional database.
    ///
    pub fn add_extensional_relation(&mut self, relation: Relation) {
        self.extensional_mut().add(relation)
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// Returns the current set of intensional relations in a mutable state.
    ///
    pub fn intensional_mut(&mut self) -> &mut RelationSet {
        &mut self.intensional
    }

    ///
    /// Add a new relation to the intensional database with the given `label` and `schema`.
    ///
    pub fn add_new_intensional_relation<V: Into<Schema<Predicate>>>(
        &mut self,
        label: PredicateRef,
        schema: V,
    ) -> Result<&mut Relation> {
        let label = self.predicate_cache.canonical(label);
        self.intensional_mut()
            .add_new_relation(label, schema.into())
    }

    ///
    /// Add the provided `relation` to the intensional database.
    ///
    pub fn add_intensional_relation(&mut self, relation: Relation) {
        self.intensional_mut().add(relation)
    }

    // Note: there is no `rules_mut` as we cannot allow clients to add rules without going through
    // the program so that we can ensure schema updates.

    ///
    /// Add a new _pure_ rule to the intensional database with the given head label and terms as
    /// well as the list of body literals.
    ///
    pub fn add_new_pure_rule<H: Into<Vec<Term>>, B: Into<Vec<Literal>>>(
        &mut self,
        head_label: PredicateRef,
        head_terms: H,
        body: B,
    ) -> Result<bool> {
        let head_label = self.predicate_cache.canonical(head_label);
        let rule = Rule::new_pure(Atom::new(head_label, head_terms), body);
        self.add_rule(rule)
    }

    ///
    /// Add a new _constraint_ rule to the intensional database with the given list of body literals.
    ///
    pub fn add_new_constraint_rule<B: Into<Vec<Literal>>>(&mut self, body: B) -> Result<bool> {
        let rule = Rule::new_constraint(body);
        self.add_rule(rule)
    }

    ///
    /// Add a new _disjunctive_ rule to the intensional database with the given list of head atoms, as
    /// well as the list of body literals.
    ///
    pub fn add_new_disjunctive_rule<A: Into<Vec<Atom>>, B: Into<Vec<Literal>>>(
        &mut self,
        head: A,
        body: B,
    ) -> Result<bool> {
        let rule = Rule::new_disjunctive(head, body);
        self.add_rule(rule)
    }

    ///
    /// Add the provided `rule` to the intensional database.
    ///
    pub fn add_rule(&mut self, rule: Rule) -> Result<bool> {
        rule.safety_check(self.features())?;

        if rule.form() == RuleForm::Constraint && !self.features().supports(&FEATURE_CONSTRAINTS) {
            return Err(language_feature_unsupported(FEATURE_CONSTRAINTS));
        }

        if rule.form() == RuleForm::Disjunctive && !self.features().supports(&FEATURE_DISJUNCTION) {
            return Err(language_feature_unsupported(FEATURE_DISJUNCTION));
        }

        for atom in rule.head() {
            //
            // Update the database schema based on atoms found in the rule's head.
            //
            if self.extensional.contains(atom.label()) {
                return Err(extensional_predicate_in_rule_head(
                    atom.label_ref(),
                    atom.source_location().cloned(),
                ));
            } else if !self.intensional.contains(atom.label()) {
                let mut schema = Vec::with_capacity(atom.len());
                for term in atom.iter() {
                    match term {
                        Term::Anonymous => {}
                        Term::Variable(v) => schema.push(self.infer_attribute(v, &rule)),
                        Term::Constant(c) => schema.push(Attribute::from(c.kind())),
                    }
                }
                self.intensional_mut()
                    .add_new_relation(atom.label_ref(), schema)?;
            }
        }

        Ok(self.rules.add(rule))
    }

    fn infer_attribute(&self, variable: &VariableRef, rule: &Rule) -> Attribute<Predicate> {
        let candidates: Vec<(&Predicate, usize)> = rule
            .literals()
            .filter_map(Literal::as_relational)
            .filter_map(|a| {
                a.iter()
                    .enumerate()
                    .filter_map(|(i, term)| term.as_variable().map(|var| (i, var)))
                    .find(|(_, var)| var == &variable)
                    .map(|(i, _)| (a.label(), i))
            })
            .collect();
        for (predicate, i) in candidates {
            if let Some(relation) = self.extensional().get(predicate) {
                return relation.schema().get(&(i.into())).unwrap().clone();
            }
        }
        Attribute::anonymous()
    }

    ///
    /// A recursive $\small\text{Datalog}$ program has rules that directly, or indirectly recurse.
    ///
    pub fn is_recursive(&self) -> bool {
        PrecedenceGraph::from(self).is_recursive()
    }

    ///
    /// In $\small\text{Datalog}^{\lnot}$ a program is _semi-positive_ **iff** the only literals that
    /// are negated are EDB relations.
    ///
    pub fn is_semi_positive(&self) -> bool {
        if self.features().supports(&FEATURE_NEGATION) {
            PrecedenceGraph::from(self).is_semi_positive()
        } else {
            false
        }
    }

    ///
    /// Linear $\small\text{Datalog}$ is defined where rule bodies have **at most** one IDB relation.
    ///
    /// $$\tag{i}\small linear\(r\) \coloneqq \ldots$$
    ///
    pub fn is_linear(&self) -> bool {
        self.rules().iter().all(|rule| {
            rule.literals()
                .filter_map(|literal| literal.as_relational())
                .filter(|atom| self.intensional().contains(atom.label()))
                .count()
                <= 1
        })
    }

    ///
    /// Guarded $\small\text{Datalog}$ is defined where for every rule, all the variables that occur
    /// in the rule bodies must occur together in at least one atom, called a guard atom.
    ///
    pub fn is_guarded(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_guarded())
    }

    ///
    /// Frontier-Guarded $\small\text{Datalog}$ is defined where for every rule, all the variables
    /// that are shared between the rule body and the rule head (called the frontier variables) must
    /// all occur together in a guard atom.
    ///
    pub fn is_frontier_guarded(&self) -> bool {
        self.rules().iter().all(|rule| rule.is_frontier_guarded())
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// Return an iterator over the queries in the program.
    ///
    pub fn queries(&self) -> &QuerySet {
        &self.queries
    }

    ///
    /// Add a new query to the program with the given `label` and `schema`.
    ///
    pub fn add_new_query<T: Into<Vec<Term>>>(
        &mut self,
        label: PredicateRef,
        terms: T,
    ) -> Result<bool> {
        let label = self.predicate_cache.canonical(label);
        let query = Query::new(label, terms);
        self.add_query(query)
    }

    ///
    /// Add the provided `query` to the program.
    ///
    pub fn add_query(&mut self, query: Query) -> Result<bool> {
        let predicate = query.as_ref().label_ref();
        if !self.extensional().contains(&predicate) && !self.intensional().contains(&predicate) {
            Err(relation_does_not_exist(predicate))
        } else {
            Ok(self.queries.add(query))
        }
    }

    // --------------------------------------------------------------------------------------------

    ///
    /// Load any data required from external files. For each relation any attached a [FilePragma](io/struct.FilePragma.html)
    /// is used to load data into that relation.
    ///
    /// All files will be loaded either relative to the current working directory, or if the program
    /// was loaded from a source file relative to that same source file.
    ///
    pub fn load_extensional_data(&mut self) -> Result<()> {
        for relation in self.extensional_mut().iter_mut() {
            relation.load_from_file()?;
        }
        Ok(())
    }

    ///
    /// Store any data required to external files. For each relation any attached a [FilePragma](io/struct.FilePragma.html)
    /// is used to store the relation's facts into a file.
    ///
    /// All files will be stored either relative to the current working directory, or if the program
    /// was loaded from a source file relative to that same source file.
    ///
    pub fn store_intensional_data(&mut self) -> Result<()> {
        for relation in self.intensional_mut().iter_mut() {
            relation.store_to_file()?;
        }
        Ok(())
    }

    // --------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------------------------------

    ///
    /// Running a program performs the following steps:
    ///
    /// 1. load external files into the extensional database (if required),
    /// 2. call the `inference` method on the provided [Evaluator], resulting in a set of new
    ///    intensional relations,
    /// 3. merge these new relations into the existing intensional database,
    /// 4. store intensional database to external files,
    /// 5. for each query in the program, evaluate it against the new intensional database and
    ///    existing extensional database and display any results.
    ///
    pub fn run(&mut self, evaluator: impl Evaluator, load_extensional: bool) -> Result<()> {
        if load_extensional {
            self.load_extensional_data()?;
        }
        let new_idb = evaluator.inference(self)?;
        println!("{:?}", new_idb);
        self.intensional_mut().merge_from(new_idb)?;
        self.store_intensional_data()?;
        let results = self.eval_queries()?;
        for (query, view) in results {
            println!("{:?}", query);
            if let Some(view) = view {
                println!("{}", view);
            }
        }
        Ok(())
    }

    ///
    /// Evaluate a query against the program's current extensional and intensional databases.
    ///
    pub fn eval_query(&self, query: &Query) -> Result<Option<View>> {
        self.inner_eval_query(query, self.intensional())
    }

    ///
    /// Evaluate a query against the program's current extensional and intensional databases.
    ///
    pub fn eval_query_with(
        &self,
        query: &Query,
        _evaluator: impl Evaluator,
    ) -> Result<Option<View>> {
        let new_idb = _evaluator.inference(self)?;
        self.inner_eval_query(query, &new_idb)
    }

    ///
    /// Evaluate all the queries in the program against the program's current extensional and
    /// intensional databases.
    ///
    pub fn eval_queries(&self) -> Result<Vec<(&Query, Option<View>)>> {
        let results: Result<Vec<Option<View>>> = self
            .queries()
            .iter()
            .map(|q| self.inner_eval_query(q, self.intensional()))
            .collect();
        match results {
            Ok(results) => Ok(self.queries.iter().zip(results.into_iter()).collect()),
            Err(e) => Err(e),
        }
    }

    ///
    /// Evaluate all the queries in the program against the program's current extensional and
    /// intensional databases.
    ///
    pub fn eval_queries_with(
        &self,
        _evaluator: impl Evaluator,
    ) -> Result<Vec<(&Query, Option<View>)>> {
        let new_idb = _evaluator.inference(self)?;
        let results: Result<Vec<Option<View>>> = self
            .queries()
            .iter()
            .map(|q| self.inner_eval_query(q, &new_idb))
            .collect();
        match results {
            Ok(results) => Ok(self.queries.iter().zip(results.into_iter()).collect()),
            Err(e) => Err(e),
        }
    }

    fn inner_eval_query(&self, query: &Query, intensional: &RelationSet) -> Result<Option<View>> {
        let label = query.as_ref().label_ref();
        if intensional.contains(&label) {
            intensional.query(query)
        } else if self.extensional().contains(&label) {
            self.extensional().query(query)
        } else {
            Err(relation_does_not_exist(label))
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl<T> Default for NameReferenceSet<T>
where
    T: AttributeName,
{
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> NameReferenceSet<T>
where
    T: AttributeName,
{
    ///
    /// Add the value `s` as a predicate in this set.
    ///
    /// This will fail if the value provided in `s` does not the check [Predicate::is_valid].
    ///
    pub fn add<S: AsRef<str>>(&self, s: S) -> Result<()> {
        self.fetch(s.as_ref()).map(|_| ())
    }

    ///
    /// Add all the values in `all` as predicates in this set.
    ///
    /// This will fail if any value provided in `all` does not the check [Predicate::is_valid].
    ///
    pub fn add_all<S: AsRef<str>>(&self, all: impl Iterator<Item = S>) -> Result<()> {
        for s in all {
            self.fetch(s.as_ref()).map(|_| ())?;
        }
        Ok(())
    }

    ///
    /// Returns `true` if there is a predicate in the set with the string representation `s`, else
    /// `false`.
    ///
    pub fn contains<S: AsRef<str>>(&self, s: S) -> bool {
        self.0.borrow().contains_key(s.as_ref())
    }

    ///
    /// Fetch will return a predicate from the set if one exists, else it will create a new
    /// predicate from `s` and add to the set and finally return this new value.
    ///
    /// This will fail if the value provided in `s` does not the check [Predicate::is_valid].
    ///
    pub fn fetch<S: Into<String>>(&self, s: S) -> Result<AttributeNameRef<T>> {
        let s = s.into();
        let found = { self.0.borrow().get(&s).cloned() };
        match found {
            None => {
                let predicate: AttributeNameRef<T> = T::from_str(&s)
                    .map_err(|_| error::invalid_value(T::type_name(), &s))?
                    .into();
                let _ = self.0.borrow_mut().insert(s, predicate.clone());
                Ok(predicate)
            }
            Some(p) => Ok(p),
        }
    }

    ///
    /// This will return the canonical predicate reference where canonical implies the first
    /// instance added to the set.
    ///
    /// Thus, if the predicate is in the set the existing one is returned, else the provided `p`
    /// is added to the set and returned.
    ///
    #[inline]
    pub fn canonical(&self, p: AttributeNameRef<T>) -> AttributeNameRef<T> {
        let s: &str = p.as_ref().as_ref();
        let found = { self.0.borrow().get(s).cloned() };
        match found {
            None => {
                let _ = self.0.borrow_mut().insert(p.to_string(), p.clone());
                p
            }
            Some(p) => p,
        }
    }
}

// ------------------------------------------------------------------------------------------------
// Private Modules
// ------------------------------------------------------------------------------------------------

#[macro_use]
mod macros;

mod syntax;

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

pub mod error;

pub mod features;

pub mod edb;

pub mod idb;

// ------------------------------------------------------------------------------------------------
// Feature-gated Modules
// ------------------------------------------------------------------------------------------------

#[cfg(feature = "parser")]
pub mod parse;

pub mod visitor;
