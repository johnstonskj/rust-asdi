/*!
One-line description.

More detailed description, with

# Example

*/

use crate::error::{Error, Result};
use crate::Relation;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Format {
    DelimitedLines(csv::Options),
    Json(json::Options),
    Text,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(single_use_lifetimes)]
pub struct FilePragma {
    file_name: PathBuf,
    file_format: Format,
}

pub trait Reader: Default {
    type Options: Default;

    fn read_from(&self, file_name: &Path, as_relation: &Relation) -> Result<Relation> {
        self.read_from_with_options(file_name, as_relation, &Self::Options::default())
    }

    fn read_from_with_options(
        &self,
        file_name: &Path,
        as_relation: &Relation,
        options: &Self::Options,
    ) -> Result<Relation>;
}

pub trait Writer: Default {
    type Options: Default;

    fn write_to(&self, file_name: &Path, from_relation: &Relation) -> Result<()> {
        self.write_to_with_options(file_name, from_relation, &Self::Options::default())
    }

    fn write_to_with_options(
        &self,
        file_name: &Path,
        from_relation: &Relation,
        options: &Self::Options,
    ) -> Result<()>;

    fn print(&self, from_relation: &Relation) -> Result<()> {
        self.print_with_options(from_relation, &Self::Options::default())
    }

    fn print_with_options(&self, from_relation: &Relation, options: &Self::Options) -> Result<()>;
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

pub(crate) fn string_to_format(s: &str) -> Result<Format> {
    match s {
        csv::PRAGMA_ID => Ok(Format::DelimitedLines(Default::default())),
        json::PRAGMA_ID => Ok(Format::Json(Default::default())),
        text::PRAGMA_ID => Ok(Format::Text),
        _ => Err(Error::UnknownSerialization(s.to_string())),
    }
}

pub fn read_relation(relation: &Relation, pragma: &FilePragma) -> Result<Relation> {
    match &pragma.file_format {
        Format::DelimitedLines(options) => {
            let reader = csv::DelimitedLines::default();
            reader.read_from_with_options(&pragma.file_name, relation, options)
        }
        Format::Json(options) => {
            let reader = json::Json::default();
            reader.read_from_with_options(&pragma.file_name, relation, options)
        }
        _ => panic!("Format does not support reading"),
    }
}

pub fn write_relation(relation: &Relation, pragma: &FilePragma) -> Result<()> {
    match &pragma.file_format {
        Format::DelimitedLines(options) => {
            let writer = csv::DelimitedLines::default();
            writer.write_to_with_options(&pragma.file_name, relation, options)
        }
        Format::Json(options) => {
            let writer = json::Json::default();
            writer.write_to_with_options(&pragma.file_name, relation, options)
        }
        Format::Text => {
            let writer = text::TextTables::default();
            writer.write_to(&pragma.file_name, relation)
        }
    }
}

pub fn print_relation(relation: &Relation, file_format: &Format) -> Result<()> {
    match &file_format {
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

impl FilePragma {
    pub fn new_default_format(file_name: PathBuf) -> Self {
        Self {
            file_name,
            file_format: Format::DelimitedLines(csv::Options::default()),
        }
    }

    pub fn new(file_name: PathBuf, file_format: Format) -> Self {
        Self {
            file_name,
            file_format,
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn file_name(&self) -> &PathBuf {
        &self.file_name
    }

    pub fn file_format(&self) -> &Format {
        &self.file_format
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

#[cfg(feature = "io_text")]
pub mod text;

#[cfg(feature = "io_json")]
pub mod json;
