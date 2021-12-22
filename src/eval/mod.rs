/*!
One-line description.

More detailed description, with

# Example

*/

use crate::edb::Constant;
use crate::error::Result;
use crate::idb::Variable;
use crate::{AttributeKind, Database, Program};
use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use tracing::{error, trace};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

pub trait Evaluator {
    fn inference(&self, program: &Program, database: &Database) -> Result<Database>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Table {
    schema: Schema,
    rows: Vec<Vec<Constant>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Schema {
    columns: Vec<Column>,
    variables: BTreeMap<Variable, usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Column {
    name: Option<Variable>,
    kind: Option<AttributeKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ColumnIndex {
    Name(Variable),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq)]
#[allow(single_use_lifetimes)] // <-- this appears to be a false positive
pub struct Row<'a> {
    table: &'a Table,
    data: &'a Vec<Constant>,
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

#[cfg(feature = "tabular")]
impl Display for Table {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use prettytable::format::Alignment;
        use prettytable::Table;
        use prettytable::{Attr, Cell};

        let mut table = Table::new();

        table.set_titles(
            self.schema
                .columns()
                .map(|c| Cell::new_align(&c.to_string(), Alignment::CENTER).with_style(Attr::Bold))
                .collect(),
        );

        for row in self.iter() {
            table.add_row(row.iter().map(|c| Cell::new(&c.to_string())).collect());
        }

        write!(f, "{}", table)
    }
}

#[cfg(not(feature = "tabular"))]
impl Display for Table {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "| {} |", self.schema.columns().join(" | "))?;

        for row in self.iter() {
            writeln!(f, "| {} |", row.join(" | "))?;
        }
    }
}

impl Table {
    pub fn new<S: Into<Schema>>(schema: S) -> Self {
        Self {
            schema: schema.into(),
            rows: Default::default(),
        }
    }

    pub fn new_with_rows<S: Into<Schema>, R: Into<Vec<Vec<Constant>>>>(schema: S, rows: R) -> Self {
        Self {
            schema: schema.into(),
            rows: rows.into(),
        }
    }

    pub fn new_true() -> Self {
        Self {
            schema: vec![Column::from(AttributeKind::Boolean)].into(),
            rows: vec![vec![true.into()]],
        }
    }

    pub fn new_false() -> Self {
        Self {
            schema: vec![Column::from(AttributeKind::Boolean)].into(),
            rows: vec![vec![true.into()]],
        }
    }

    pub fn new_count(v: usize) -> Self {
        Self {
            schema: vec![Column::from(AttributeKind::Integer)].into(),
            rows: vec![vec![(v as i64).into()]],
        }
    }

    pub fn empty() -> Self {
        Self {
            schema: Schema::empty(),
            rows: vec![],
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn schema(&self) -> &Schema {
        &self.schema
    }

    pub fn contains_column<I: Into<ColumnIndex>>(&self, index: I) -> bool {
        self.schema().contains(index)
    }

    // --------------------------------------------------------------------------------------------

    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    pub fn len(&self) -> usize {
        self.rows.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Vec<Constant>> {
        self.rows.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Vec<Constant>> {
        self.rows.iter_mut()
    }

    pub fn rows(&self) -> impl Iterator<Item = Row<'_>> {
        self.rows.iter().map(|row| Row::new(self, row))
    }

    pub fn push<R: Into<Vec<Constant>>>(&mut self, row: R) -> Result<()> {
        let row = row.into();
        if self.schema.conforms(&row) {
            self.rows.push(row);
            Ok(())
        } else {
            error!("row {:?} does not conform to schema {:?}", row, self.schema);
            panic!();
        }
    }

    pub fn extend(&mut self, other: Table) {
        assert_eq!(self.schema, other.schema);
        self.rows.extend(other.rows);
    }

    // --------------------------------------------------------------------------------------------

    pub fn join_all<V: Into<Vec<Self>>>(tables: V) -> Self {
        let mut tables = tables.into();
        assert!(!tables.is_empty());
        if tables.len() == 1 {
            let mut result = tables.remove(0);
            result.reduce(true);
            result
        } else {
            let mut tables = tables.into_iter();
            let mut result = tables.next().unwrap();
            for next in tables {
                result = result.join(&next);
            }
            result
        }
    }

    pub fn join(&self, other: &Self) -> Self {
        let mut new_table: Table = Table::new(self.schema().variable_union(other.schema()));
        let common_variables = self.schema().variable_intersection(other.schema());

        for left_row in self.rows() {
            for right_row in other.filter(
                common_variables
                    .iter()
                    .map(|(_, left_i, right_i)| (left_row.get(*left_i).unwrap().clone(), *right_i))
                    .collect(),
            ) {
                let mut new_row: Vec<Constant> =
                    Vec::with_capacity(new_table.schema().column_count());
                for (i, column) in new_table.schema().columns().enumerate() {
                    if let Some(index) = self.schema().name_to_index(column.name().unwrap()) {
                        new_row.insert(i, left_row.get(index).unwrap().clone())
                    } else if let Some(index) = other.schema().name_to_index(column.name().unwrap())
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
                // TODO: propagate errors
                new_table.push(new_row).unwrap();
            }
        }
        new_table
    }

    fn reduce(&mut self, dedup: bool) {
        let mut remove: Vec<usize> = self
            .schema
            .columns()
            .enumerate()
            .filter_map(|(i, c)| if c.is_anonymous() { Some(i) } else { None })
            .collect();
        trace!("reduce > remove (before)\n{:?}", remove);
        remove.sort_by(|a, b| b.cmp(a));
        trace!("reduce > remove (after)\n{:?}", remove);
        for row in &mut self.rows {
            for col in &remove {
                row.remove(*col);
            }
        }
        self.schema = Schema::new(
            self.schema
                .columns()
                .into_iter()
                .filter(|col| col.is_named())
                .cloned()
                .collect::<Vec<Column>>(),
        );
        if dedup {
            self.rows.sort();
            self.rows.dedup();
        }
    }

    fn filter(&self, values: Vec<(Constant, usize)>) -> impl Iterator<Item = &Vec<Constant>> {
        self.rows
            .iter()
            .filter(move |row| values.iter().all(|(v, i)| row.get(*i).unwrap() == v))
    }
}

// ------------------------------------------------------------------------------------------------

impl From<Column> for Schema {
    fn from(c: Column) -> Self {
        Self::new(vec![c])
    }
}

impl From<Vec<Column>> for Schema {
    fn from(cs: Vec<Column>) -> Self {
        Self::new(cs)
    }
}

impl From<Variable> for Schema {
    fn from(v: Variable) -> Self {
        Self::new(vec![Column::from(v)])
    }
}

impl From<Vec<Variable>> for Schema {
    fn from(vs: Vec<Variable>) -> Self {
        Self::new(vs.into_iter().map(Column::from).collect::<Vec<Column>>())
    }
}

impl Schema {
    pub fn new<V: Into<Vec<Column>>>(columns: V) -> Self {
        let columns = columns.into();
        let variables: Vec<(Variable, usize)> = columns
            .iter()
            .enumerate()
            .filter_map(|(i, c)| c.name().map(|var| (var.clone(), i)))
            .collect();

        let variables = BTreeMap::from_iter(variables);

        Self { columns, variables }
    }

    fn empty() -> Self {
        Self {
            columns: Default::default(),
            variables: Default::default(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.columns.is_empty()
    }

    pub fn has_variables(&self) -> bool {
        !self.variables.is_empty()
    }

    pub fn column_count(&self) -> usize {
        self.columns.len()
    }

    pub fn columns(&self) -> impl Iterator<Item = &Column> {
        self.columns.iter()
    }

    pub fn contains<I: Into<ColumnIndex>>(&self, index: I) -> bool {
        match index.into() {
            ColumnIndex::Name(n) => self.variables.contains_key(&n),
            ColumnIndex::Index(i) => i < self.column_count(),
        }
    }

    pub fn variables(&self) -> impl Iterator<Item = &Variable> {
        self.variables.keys()
    }

    pub fn variable_intersection(&self, other: &Self) -> Vec<(&Variable, usize, usize)> {
        self.variables
            .iter()
            .filter_map(|(var, i)| other.variables.get(var).map(|other_i| (var, *i, *other_i)))
            .collect()
    }

    pub fn variable_union(&self, other: &Self) -> Vec<Variable> {
        let mut all: Vec<Variable> = self.variables().cloned().collect();
        for var in other.variables() {
            if !all.contains(var) {
                all.push(var.clone());
            }
        }
        all
    }

    pub fn name_to_index(&self, n: &Variable) -> Option<usize> {
        self.variables.get(n).copied()
    }

    pub fn conforms(&mut self, row: &[Constant]) -> bool {
        row.len() == self.columns.len()
            && row.iter().zip(&self.columns).all(|(v, c)| {
                if let Some(kind) = c.kind {
                    v.kind() == kind
                } else {
                    true
                }
            })
    }
}

// ------------------------------------------------------------------------------------------------

impl Display for Column {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            match &self.name {
                None => String::from("_"),
                Some(v) => v.to_string(),
            },
            match &self.kind {
                None => String::new(),
                Some(v) => format!(": {}", v),
            }
        )
    }
}

impl From<Variable> for Column {
    fn from(v: Variable) -> Self {
        Self {
            name: Some(v),
            kind: None,
        }
    }
}

impl From<AttributeKind> for Column {
    fn from(v: AttributeKind) -> Self {
        Self {
            name: None,
            kind: Some(v),
        }
    }
}

impl Column {
    pub fn new(name: Variable, kind: AttributeKind) -> Self {
        Self {
            name: Some(name),
            kind: Some(kind),
        }
    }

    pub fn unknown() -> Self {
        Self {
            name: None,
            kind: None,
        }
    }

    pub fn name(&self) -> Option<&Variable> {
        self.name.as_ref()
    }

    pub fn kind(&self) -> Option<&AttributeKind> {
        self.kind.as_ref()
    }

    pub fn is_anonymous(&self) -> bool {
        match &self.name {
            None => true,
            Some(n) => n.is_ignored(),
        }
    }

    pub fn is_named(&self) -> bool {
        !self.is_anonymous()
    }
}

// ------------------------------------------------------------------------------------------------

impl From<Variable> for ColumnIndex {
    fn from(v: Variable) -> Self {
        Self::Name(v)
    }
}

impl From<usize> for ColumnIndex {
    fn from(i: usize) -> Self {
        Self::Index(i)
    }
}

impl ColumnIndex {
    pub fn is_named(&self) -> bool {
        matches!(self, Self::Name(_))
    }

    pub fn as_name(&self) -> Option<&Variable> {
        match self {
            ColumnIndex::Name(v) => Some(v),
            _ => None,
        }
    }

    pub fn is_indexed(&self) -> bool {
        matches!(self, Self::Name(_))
    }

    pub fn as_index(&self) -> Option<&usize> {
        match self {
            ColumnIndex::Index(v) => Some(v),
            _ => None,
        }
    }
}

// ------------------------------------------------------------------------------------------------

impl<'a> Row<'a> {
    #[allow(clippy::ptr_arg)] // <-- this makes lifetime management much easier.
    fn new(table: &'a Table, data: &'a Vec<Constant>) -> Self {
        Self { table, data }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Constant> {
        self.data.iter()
    }

    pub fn get<I: Into<ColumnIndex>>(&self, index: I) -> Option<&Constant> {
        match index.into() {
            ColumnIndex::Name(n) => {
                let i = self.table.schema.name_to_index(&n).unwrap();
                self.data.get(i)
            }
            ColumnIndex::Index(i) => self.data.get(i),
        }
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

pub mod naive;
