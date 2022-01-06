/*!
This module provides the both a [Reader] and [Writer] implementation for CSV files.
 */

use crate::edb::{AttributeKind, Constant, Fact};
use crate::error::{fact_does_not_correspond_to_schema, Error, Result};
use crate::io::{Reader, Writer};
use crate::syntax::{CHAR_COMMA, COLUMN_NAME_UNKNOWN};
use crate::{Collection, Labeled, MaybeLabeled, Relation};
use csv::{ReaderBuilder, Trim, WriterBuilder};
use std::fs::File;
use std::io::{BufReader, Write};
use std::path::Path;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Options {
    pub delimiter: u8,
    pub has_headers: bool,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct DelimitedLines {}

/// The string used in the `.input` and `.output` pragmas to identify this format.
pub const PRAGMA_ID: &str = "csv";

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

impl Default for Options {
    fn default() -> Self {
        Self {
            delimiter: b',',
            has_headers: false,
        }
    }
}

impl Options {
    pub fn comma_separated() -> Self {
        Self {
            delimiter: b',',
            has_headers: Default::default(),
        }
    }

    pub fn tab_separated() -> Self {
        Self {
            delimiter: b'\t',
            has_headers: Default::default(),
        }
    }

    // --------------------------------------------------------------------------------------------

    pub fn with_delimiter(mut self, delimiter: u8) -> Self {
        self.delimiter = delimiter;
        self
    }

    // --------------------------------------------------------------------------------------------

    pub fn has_headers(mut self) -> Self {
        self.has_headers = true;
        self
    }

    pub fn no_headers(mut self) -> Self {
        self.has_headers = false;
        self
    }
}

// ------------------------------------------------------------------------------------------------

impl Reader for DelimitedLines {
    type Options = Options;

    fn read_from_with_options(
        &self,
        file_name: &Path,
        as_relation: &Relation,
        options: &Self::Options,
    ) -> Result<Relation> {
        let file = File::open(file_name)?;
        let reader = BufReader::new(file);
        let mut reader = ReaderBuilder::new()
            .delimiter(options.delimiter)
            .has_headers(options.has_headers)
            .comment(b'#'.into())
            .double_quote(true)
            .quoting(true)
            .trim(Trim::All)
            .from_reader(reader);

        let mut new_relation = as_relation.clone_with_schema_only();

        let attribute_types: Vec<AttributeKind> = as_relation
            .schema()
            .iter()
            .map(|a| a.kind().unwrap_or(AttributeKind::String))
            .collect();
        let arity = attribute_types.len();

        // MAYBE: could extend this to do name matching, right now it assumes a positional match

        // if reader.has_headers() {
        //     let headers = reader.headers();
        // }

        for result in reader.records() {
            // TODO: propagate error
            let record = result.map_err(|e| Error::Serialization(Box::new(e)))?;
            if record.len() == arity {
                let values: Vec<Constant> = record
                    .iter()
                    .enumerate()
                    .map(|(i, s)| match attribute_types.get(i) {
                        Some(AttributeKind::String) => Constant::String(s.to_string()),
                        Some(AttributeKind::Integer) => Constant::String(s.to_string()),
                        Some(AttributeKind::Boolean) => Constant::String(s.to_string()),
                        _ => unreachable!(),
                    })
                    .collect();
                new_relation.add(Fact::new(new_relation.label_ref(), values))?;
            } else {
                return Err(fact_does_not_correspond_to_schema(
                    new_relation.label_ref(),
                    record
                        .iter()
                        .map(str::to_string)
                        .collect::<Vec<String>>()
                        .join(&format!("{} ", CHAR_COMMA)),
                ));
            }
        }

        Ok(new_relation)
    }
}

impl Writer for DelimitedLines {
    type Options = Options;

    fn write_to_with_options(
        &self,
        file_name: &Path,
        from_relation: &Relation,
        options: &Self::Options,
    ) -> Result<()> {
        let file = File::create(file_name)?;
        let mut writer = WriterBuilder::new()
            .delimiter(options.delimiter)
            .has_headers(options.has_headers)
            .double_quote(true)
            .from_writer(file);
        write(&mut writer, from_relation, options.has_headers)
    }

    fn print_with_options(&self, relation: &Relation, options: &Self::Options) -> Result<()> {
        let mut writer = WriterBuilder::new()
            .delimiter(options.delimiter)
            .has_headers(options.has_headers)
            .from_writer(std::io::stdout());
        write(&mut writer, relation, options.has_headers)
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

fn write<W: Write>(w: &mut csv::Writer<W>, relation: &Relation, has_headers: bool) -> Result<()> {
    if has_headers {
        let headers: Vec<String> = relation
            .schema()
            .iter()
            .map(|a| {
                a.label()
                    .map(|p| p.to_string())
                    .unwrap_or_else(|| COLUMN_NAME_UNKNOWN.to_string())
            })
            .collect();
        w.write_record(&headers)
            .map_err(|e| Error::Serialization(Box::new(e)))?;
    }
    for fact in relation.iter() {
        let record: Vec<String> = fact.iter().map(|c| c.to_string()).collect();
        w.write_record(&record)
            .map_err(|e| Error::Serialization(Box::new(e)))?;
    }
    Ok(())
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
