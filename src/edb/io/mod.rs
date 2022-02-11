/*!
This module provides the set of types that are used to implement formatted input/output operations
for [Relation]s.

![module UML](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/book/src/model/edb_io.svg)

*/

use crate::error;
use crate::error::{serialization_operation_unsupported, Result};
use crate::Relation;
use std::path::{Path, PathBuf};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// Identifies the file format to read or write, along with any options the implementation
/// chooses to make available.
///
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Format {
    ///
    /// This represents _Comma Separated Variable_ (CSV) files, although as the _comma_ part is
    /// only one choice the name has been generalized.
    ///
    #[cfg(feature = "io_csv")]
    DelimitedLines(csv::Options),
    #[cfg(feature = "io_json")]
    Json(json::Options),
    #[cfg(feature = "io_text")]
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
        #[cfg(feature = "io_csv")]
        csv::PRAGMA_ID => Ok(Format::DelimitedLines(Default::default())),
        #[cfg(feature = "io_json")]
        json::PRAGMA_ID => Ok(Format::Json(Default::default())),
        #[cfg(feature = "io_text")]
        text::PRAGMA_ID => Ok(Format::Text),
        _ => Err(error::serialization_format_unknown(s)),
    }
}

///
/// Load data from the file identified by `pragma`, using any schema in `relation`, and returning
/// a new [Relation] containing the loaded data.
///
pub fn read_relation(relation: &Relation, pragma: &FilePragma) -> Result<Relation> {
    match &pragma.file_format {
        #[cfg(feature = "io_csv")]
        Format::DelimitedLines(options) => {
            let reader = csv::DelimitedLines::default();
            reader.read_from_with_options(&pragma.file_name, relation, options)
        }
        #[cfg(feature = "io_json")]
        Format::Json(options) => {
            let reader = json::Json::default();
            reader.read_from_with_options(&pragma.file_name, relation, options)
        }
        _ => Err(serialization_operation_unsupported(
            pragma.file_format.label(),
        )),
    }
}

///
/// Store data from `relation` into the file identified by `pragma`.
///
pub fn write_relation(relation: &Relation, pragma: &FilePragma) -> Result<()> {
    match &pragma.file_format {
        #[cfg(feature = "io_csv")]
        Format::DelimitedLines(options) => {
            let writer = csv::DelimitedLines::default();
            writer.write_to_with_options(&pragma.file_name, relation, options)
        }
        #[cfg(feature = "io_json")]
        Format::Json(options) => {
            let writer = json::Json::default();
            writer.write_to_with_options(&pragma.file_name, relation, options)
        }
        #[cfg(feature = "io_text")]
        Format::Text => {
            let writer = text::TextTables::default();
            writer.write_to(&pragma.file_name, relation)
        }
    }
}

///
/// Print data from `relation` to standard output in the provided [Format].
///
pub fn print_relation(relation: &Relation, file_format: &Format) -> Result<()> {
    match &file_format {
        #[cfg(feature = "io_csv")]
        Format::DelimitedLines(options) => {
            let writer = csv::DelimitedLines::default();
            writer.print_with_options(relation, options)
        }
        #[cfg(feature = "io_json")]
        Format::Json(options) => {
            let writer = json::Json::default();
            writer.print_with_options(relation, options)
        }
        #[cfg(feature = "io_text")]
        Format::Text => {
            let writer = text::TextTables::default();
            writer.print(relation)
        }
    }
}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Format {
    pub fn label(&self) -> &'static str {
        match self {
            #[cfg(feature = "io_csv")]
            Format::DelimitedLines(_) => csv::PRAGMA_ID,
            #[cfg(feature = "io_json")]
            Format::Json(_) => json::PRAGMA_ID,
            #[cfg(feature = "io_text")]
            Format::Text => text::PRAGMA_ID,
        }
    }
}
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
