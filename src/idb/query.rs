/*!
This module provides the [Query] type that represents a query goal as well as the [View] and [Row].
types used to return query results.
*/

use crate::edb::{Attribute, AttributeIndex, AttributeKind, Constant, Fact, Schema};
use crate::error::Result;
use crate::idb::Term;
use crate::idb::{Atom, Variable};
use crate::syntax::{CHAR_PERIOD, QUERY_PREFIX_ASCII};
use crate::{Collection, MaybeLabeled, PredicateRef};
use paste::paste;
use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};
use tracing::{error, trace};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// A query, or goal, is an atom to be matched against [relations](../edb/struct.Relation.html)
/// known to the program.
///
/// # Examples
///
/// ```datalog
/// .assert car(make: string, model: string, year: integer).
///
/// ## Is there a car model Fiesta, from Ford, with a model year 2010?
/// car(ford, focus, 2010)?
///
/// ## Return all the models Ford made with a model year 2010.
/// car(ford, X, 2010)?
///
/// ## Return all the model years for Ford Fiesta.
/// car(ford, focus, X)?
/// ```
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Query(Atom);

///
/// A view is an intermediate structure, a selection/projection of a [relation](../edb/struct.Relation.html),
/// or the join of two or more views.
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct View {
    schema: Schema<Variable>,
    facts: HashSet<Row>,
}

///
/// A row within a view corresponds to a [`Fact`] within a [`Relation`](../edb/struct.Relation.html),
/// except that it is not labeled.
///
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Row(Vec<Constant>);

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Display for Query {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}{}", QUERY_PREFIX_ASCII, self.0, CHAR_PERIOD)
    }
}

impl From<Atom> for Query {
    fn from(v: Atom) -> Self {
        Self(v)
    }
}

impl AsRef<Atom> for Query {
    fn as_ref(&self) -> &Atom {
        &self.0
    }
}

impl Query {
    pub fn new<T: Into<Vec<Term>>>(predicate: PredicateRef, terms: T) -> Self {
        Self(Atom::new(predicate, terms))
    }
}

// ------------------------------------------------------------------------------------------------

#[cfg(feature = "tabular")]
impl Display for View {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use prettytable::format::Alignment;
        use prettytable::Table;
        use prettytable::{Attr, Cell};

        let mut table = Table::new();

        table.set_titles(
            self.schema
                .iter()
                .map(|c| {
                    Cell::new_align(&c.to_column_decl(false), Alignment::CENTER)
                        .with_style(Attr::Bold)
                })
                .collect(),
        );

        for row in self.iter() {
            table.add_row(row.iter().map(|c| Cell::new(&c.to_string())).collect());
        }

        write!(f, "{}", table)
    }
}

#[cfg(not(feature = "tabular"))]
impl Display for View {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "| {} |",
            self.schema
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<String>>()
                .join(" | ")
        )?;

        for row in self.iter() {
            writeln!(
                f,
                "| {} |",
                row.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(" | ")
            )?;
        }
        Ok(())
    }
}

impl View {
    pub fn empty() -> Self {
        Self {
            schema: Schema::empty(),
            facts: Default::default(),
        }
    }

    pub fn new<V: Into<Schema<Variable>>>(schema: V) -> Self {
        Self {
            schema: schema.into(),
            facts: Default::default(),
        }
    }

    pub fn new_with_facts<V: Into<Schema<Variable>>, C: Into<Vec<Row>>>(
        schema: V,
        facts: C,
    ) -> Self {
        Self {
            schema: schema.into(),
            facts: HashSet::from_iter(facts.into()),
        }
    }

    pub fn new_true() -> Self {
        Self::new_with_facts(
            vec![Attribute::from(AttributeKind::Boolean)],
            vec![Row::from(Constant::new_true())],
        )
    }

    pub fn new_false() -> Self {
        Self::new_with_facts(
            vec![Attribute::from(AttributeKind::Boolean)],
            vec![Row::from(Constant::new_false())],
        )
    }

    // --------------------------------------------------------------------------------------------

    pub fn schema(&self) -> &Schema<Variable> {
        &self.schema
    }

    pub fn attribute_index(&self, index: AttributeIndex<Variable>) -> Option<usize> {
        let index = match &index {
            AttributeIndex::Label(n) => self.schema.label_to_index(n),
            AttributeIndex::Index(i) => Some(*i),
        };
        index.map(|index| {
            assert!(index < self.schema.len());
            index
        })
    }

    // --------------------------------------------------------------------------------------------

    delegate!(is_empty, facts -> bool);

    delegate!(len, facts -> usize);

    delegate!(iter, facts -> impl Iterator<Item = &Row>);

    pub fn add(&mut self, row: Row) -> Result<()> {
        self.facts.insert(row);
        Ok(())
    }

    pub fn extend(&mut self, other: Self) -> Result<()> {
        trace!("extend > schema {:?} == {:?}", self.schema, other.schema);
        assert_eq!(self.schema, other.schema);
        for fact in other.facts.into_iter() {
            self.add(fact)?;
        }
        Ok(())
    }

    // --------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------------------------------

    pub fn join_all<V: Into<Vec<View>>>(views: V) -> Result<Self> {
        let mut views = views.into();
        assert!(!views.is_empty());
        if views.len() == 1 {
            Ok(views.remove(0))
        } else {
            let mut views = views.into_iter();
            let mut result = views.next().unwrap();
            for next in views {
                result = result.join(&next)?;
            }
            Ok(result)
        }
    }

    pub(crate) fn join(&self, other: &Self) -> Result<Self> {
        let mut new_table: Self = Self::new(
            self.schema()
                .label_union(other.schema())
                .into_iter()
                .map(Attribute::from)
                .collect::<Vec<Attribute<Variable>>>(),
        );
        let common_variables = self.schema().label_intersection(other.schema());

        // TODO: infer attribute types for new results!
        for left_row in self.iter() {
            for right_row in other.filter(
                common_variables
                    .iter()
                    .map(|(_, left_i, right_i)| (left_row.get(*left_i).unwrap().clone(), *right_i))
                    .collect(),
            ) {
                let mut new_row: Vec<Constant> = Vec::with_capacity(new_table.schema().len());
                for (i, column) in new_table.schema().iter().enumerate() {
                    if let Some(index) = self.schema().label_to_index(column.label().unwrap()) {
                        new_row.insert(i, left_row.get(index).unwrap().clone())
                    } else if let Some(index) =
                        other.schema().label_to_index(column.label().unwrap())
                    {
                        new_row.insert(i, right_row.get(index).unwrap().clone())
                    } else {
                        error!(
                            "The column {:?} ({}) was found in neither table.",
                            column, i
                        );
                        unreachable!()
                    }
                }
                new_table.add(new_row.into())?;
            }
        }
        Ok(new_table)
    }

    fn filter(&self, values: Vec<(Constant, usize)>) -> impl Iterator<Item = &Row> {
        self.iter()
            .filter(move |row| values.iter().all(|(v, i)| row.get(*i).unwrap() == v))
    }

    // pub(crate) fn contains(&self, fact: &[Constant]) -> bool {
    //     self.facts.contains(fact)
    // }
    //
    // --------------------------------------------------------------------------------------------
    // Query
    // --------------------------------------------------------------------------------------------
    //
    // pub(crate) fn matches(&self, atom: &Atom) -> View {
    //     let terms: Vec<&Term> = atom.terms().collect();
    //     self.match_terms(terms)
    // }
    //
    // #[allow(single_use_lifetimes)]
    // fn match_terms<'a, V: Into<Vec<&'a Term>>>(&self, terms: V) -> View {
    //     let terms = terms.into();
    //
    //     View::new_with_facts(
    //         terms
    //             .iter()
    //             .enumerate()
    //             .map(|(i, term)| {
    //                 let mut term = match term {
    //                     Term::Variable(v) => Attribute::from(v.clone()),
    //                     Term::Constant(v) => Attribute::typed(v.kind()),
    //                 };
    //                 if term.kind().is_none() {
    //                     if let Some(kind) = self.schema().get(i).unwrap().kind() {
    //                         term.override_kind(kind);
    //                     }
    //                 }
    //                 term
    //             })
    //             .collect::<Vec<Attribute<Variable>>>(),
    //         self.facts
    //             .iter()
    //             .filter_map(|fact| self.match_terms_inner(&terms, fact.values()))
    //             .collect::<Vec<Row>>(),
    //     )
    // }
    //
    // fn match_terms_inner(&self, terms: &[&Term], fact: &[Constant]) -> Option<Row> {
    //     if terms
    //         .iter()
    //         .enumerate()
    //         .filter(|(_, term)| term.is_constant())
    //         .all(|(i, term)| term.as_constant().unwrap() == fact.get(i).unwrap())
    //     {
    //         Some(Row::from(fact.to_vec()))
    //     } else {
    //         None
    //     }
    // }
    //
    // --------------------------------------------------------------------------------------------
    //
    // pub(crate) fn select(&self, _comparisons: &[Comparison]) -> Self {
    //     unimplemented!()
    // }
    //
    // pub(crate) fn select_attributes(&self, attributes: HashSet<AttributeIndex<T>>) -> Self {
    //     let selection: Vec<usize> = attributes
    //         .iter()
    //         .map(|a| {
    //             assert!(self.schema().contains(a.clone()));
    //             match a {
    //                 AttributeIndex::Name(n) => self.schema().name_to_index(n).unwrap(),
    //                 AttributeIndex::Index(i) => *i,
    //             }
    //         })
    //         .collect();
    //     self.select_attributes_by_index(&selection)
    // }
    //
    // pub(crate) fn select_attributes_by_index(&self, attributes: &[usize]) -> Self {
    //     let schema = Schema::new(
    //         self.schema
    //             .iter()
    //             .enumerate()
    //             .filter_map(|(i, attr)| {
    //                 if attributes.contains(&i) {
    //                     Some(attr)
    //                 } else {
    //                     None
    //                 }
    //             })
    //             .cloned()
    //             .collect::<Vec<Attribute<T>>>(),
    //     );
    //
    //     let facts: Vec<Vec<Constant>> = self
    //         .iter()
    //         .map(|row| {
    //             row.iter()
    //                 .enumerate()
    //                 .filter_map(|(i, attr)| {
    //                     if attributes.contains(&i) {
    //                         Some(attr)
    //                     } else {
    //                         None
    //                     }
    //                 })
    //                 .cloned()
    //                 .collect::<Vec<Constant>>()
    //         })
    //         .collect();
    //
    //     Self::new_with_facts(schema, facts)
    // }
    //
    // pub(crate) fn select_only_named(&self) -> Self {
    //     let selection: Vec<usize> = self
    //         .schema
    //         .iter()
    //         .enumerate()
    //         .filter_map(|(i, c)| if !c.is_anonymous() { Some(i) } else { None })
    //         .collect();
    //     self.select_attributes_by_index(&selection)
    // }
    //
    // pub(crate) fn project<A: Into<AttributeIndex<T>>>(&self, attributes: Vec<A>) -> Self {
    //     let attributes: HashSet<usize> = HashSet::from_iter(attributes.into_iter().map(|a| {
    //         let a: AttributeIndex<T> = a.into();
    //         match a {
    //             AttributeIndex::Name(n) => self.schema().name_to_index(&n).unwrap(),
    //             AttributeIndex::Index(i) => i,
    //         }
    //     }));
    //     Self {
    //         name: None,
    //         schema: Schema::new(
    //             self.schema()
    //                 .iter()
    //                 .enumerate()
    //                 .filter_map(|(i, a)| {
    //                     if attributes.contains(&i) {
    //                         Some(a)
    //                     } else {
    //                         None
    //                     }
    //                 })
    //                 .cloned()
    //                 .collect::<Vec<Attribute<T>>>(),
    //         ),
    //         facts: self
    //             .iter()
    //             .map(|f| {
    //                 f.iter()
    //                     .enumerate()
    //                     .filter_map(|(i, c)| {
    //                         if attributes.contains(&i) {
    //                             Some(c)
    //                         } else {
    //                             None
    //                         }
    //                     })
    //                     .cloned()
    //                     .collect()
    //             })
    //             .collect(),
    //     }
    // }
    //
    // fn remove_anonymous_attributes(&mut self) {
    //     let mut remove: Vec<usize> = self
    //         .schema
    //         .iter()
    //         .enumerate()
    //         .filter_map(|(i, c)| if c.is_anonymous() { Some(i) } else { None })
    //         .collect();
    //     trace!("reduce > remove (before)\n{:?}", remove);
    //     remove.sort_by(|a, b| b.cmp(a));
    //     trace!("reduce > remove (after)\n{:?}", remove);
    //
    //     let mut result: HashSet<Vec<Constant>> = HashSet::with_capacity(self.facts.len());
    //     for mut row in self.facts.drain() {
    //         mutator(&mut row)?;
    //         result.insert(row);
    //     }
    //     self.facts = result;
    //
    //
    //     self.mutate(|row| {
    //         for col in &remove {
    //             row.remove(*col);
    //         }
    //         Ok(())
    //     })
    //     .unwrap();
    //     self.schema = Schema::new(
    //         self.schema
    //             .iter()
    //             .filter(|col| !col.is_anonymous())
    //             .cloned()
    //             .collect::<Vec<Attribute<T>>>(),
    //     );
    // }
}

// ------------------------------------------------------------------------------------------------

impl From<Vec<Constant>> for Row {
    fn from(v: Vec<Constant>) -> Self {
        Self(v)
    }
}

impl From<Constant> for Row {
    fn from(v: Constant) -> Self {
        Self(vec![v])
    }
}

impl From<Row> for Vec<Constant> {
    fn from(v: Row) -> Self {
        v.0
    }
}

impl From<Fact> for Row {
    fn from(v: Fact) -> Self {
        Self(v.into())
    }
}

impl Row {
    delegate!(is_empty -> bool);

    delegate!(len -> usize);

    delegate!(iter -> impl Iterator<Item = &Constant>);

    pub fn get(&self, index: usize) -> Option<&Constant> {
        self.0.get(index)
    }
}
