/*!
This module provides the both a [Writer] implementation for tabular text files.
 */

use crate::error::Result;
use crate::io::Writer;
use crate::{Collection, Relation};
use std::fs::File;
use std::io::Write;
use std::path::Path;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Options {}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct TextTables {}

/// The string used in the `@input` and `@output` pragmas to identify this format.
pub const PRAGMA_ID: &str = "text";

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

impl Writer for TextTables {
    type Options = Options;

    fn write_to_with_options(
        &self,
        file_name: &Path,
        relation: &Relation,
        _: &Self::Options,
    ) -> Result<()> {
        let mut file = File::create(file_name)?;
        write(&mut file, relation)
    }

    fn print_with_options(&self, relation: &Relation, _: &Self::Options) -> Result<()> {
        write(&mut std::io::stdout(), relation)
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

fn write(w: &mut impl Write, relation: &Relation) -> Result<()> {
    use prettytable::format::Alignment;
    use prettytable::Table;
    use prettytable::{Attr, Cell};

    let mut table = Table::new();

    table.set_titles(
        relation
            .schema()
            .iter()
            .map(|attr| {
                Cell::new_align(&attr.to_column_decl(true), Alignment::CENTER)
                    .with_style(Attr::Bold)
            })
            .collect(),
    );

    for row in relation.iter() {
        table.add_row(row.iter().map(|c| Cell::new(&c.to_string())).collect());
    }

    write!(w, "{}", table)?;
    Ok(())
}

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
