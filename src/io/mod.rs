/*!
One-line description.

More detailed description, with

# Example

*/

use crate::error::Result;
use crate::Relation;
use std::path::PathBuf;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Direction {
    Input,
    Output,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Format {
    DelimitedLines(csv::Options),
    Json(json::Options),
    Text,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(single_use_lifetimes)]
pub struct FilePragma<'a> {
    direction: Direction,
    relation: &'a Relation,
    file_name: PathBuf,
    file_format: Format,
}

pub trait Reader: Default {
    type Options: Default;

    fn read_from(&self, file_name: PathBuf, as_relation: &Relation) -> Result<Relation> {
        self.read_from_with_options(file_name, as_relation, Self::Options::default())
    }

    fn read_from_with_options(
        &self,
        file_name: PathBuf,
        as_relation: &Relation,
        options: Self::Options,
    ) -> Result<Relation>;
}

pub trait Writer: Default {
    type Options: Default;

    fn write_to(&self, file_name: PathBuf, from_relation: &Relation) -> Result<()> {
        self.write_to_with_options(file_name, from_relation, Self::Options::default())
    }

    fn write_to_with_options(
        &self,
        file_name: PathBuf,
        from_relation: &Relation,
        options: Self::Options,
    ) -> Result<()>;

    fn print(&self, from_relation: &Relation) -> Result<()> {
        self.print_with_options(from_relation, Self::Options::default())
    }

    fn print_with_options(&self, from_relation: &Relation, options: Self::Options) -> Result<()>;
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

pub fn read_relation(mapping: FilePragma<'_>) -> Result<Relation> {
    match mapping.file_format {
        Format::DelimitedLines(options) => {
            let reader = csv::DelimitedLines::default();
            reader.read_from_with_options(mapping.file_name, mapping.relation, options)
        }
        Format::Json(options) => {
            let reader = json::Json::default();
            reader.read_from_with_options(mapping.file_name, mapping.relation, options)
        }
        _ => panic!("Format does not support reading"),
    }
}

pub fn write_relation(relation: &Relation, mapping: FilePragma<'_>) -> Result<()> {
    match mapping.file_format {
        Format::DelimitedLines(options) => {
            let writer = csv::DelimitedLines::default();
            writer.write_to_with_options(mapping.file_name, relation, options)
        }
        Format::Json(options) => {
            let writer = json::Json::default();
            writer.write_to_with_options(mapping.file_name, relation, options)
        }
        Format::Text => {
            let writer = text::TextTables::default();
            writer.write_to(mapping.file_name, relation)
        }
    }
}

pub fn print_relation(relation: &Relation, file_format: Format) -> Result<()> {
    match file_format {
        Format::DelimitedLines(options) => {
            let writer = csv::DelimitedLines::default();
            writer.print_with_options(relation, options)
        }
        Format::Json(options) => {
            let writer = json::Json::default();
            writer.print_with_options(relation, options)
        }
        Format::Text => {
            let writer = text::TextTables::default();
            writer.print(relation)
        }
    }
}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl<'a> FilePragma<'a> {
    pub fn input(relation: &'a Relation, from_file: PathBuf, format: Format) -> Self {
        Self::new(Direction::Input, relation, from_file, format)
    }

    pub fn output(relation: &'a Relation, to_file: PathBuf, format: Format) -> Self {
        Self::new(Direction::Output, relation, to_file, format)
    }
    fn new(
        direction: Direction,
        relation: &'a Relation,
        file_name: PathBuf,
        file_format: Format,
    ) -> Self {
        Self {
            direction,
            file_name,
            file_format,
            relation,
        }
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

#[cfg(feature = "io_csv")]
pub mod csv;

#[cfg(feature = "tabular")]
pub mod text;

#[cfg(feature = "io_json")]
pub mod json;
