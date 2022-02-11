/*!
This module provides the type [`Parsed`] that contains a program parsed from the text representation
as well as the [`parse_file`], [`parse_str`], and [`parse_str_with_features`] functions.

![module UML](https://raw.githubusercontent.com/johnstonskj/rust-asdi/main/book/src/model/parse.svg)

TBD

# Example

```rust
use asdi::parse::parse_file;

match parse_file("examples/rdfs.dl") {
    Err(e) => eprintln!("{}", e),
    Ok(parsed) => {
        let program = parsed.into_parsed();
        println!("{}", program);
    }
}
```

*/

use crate::error::Result;
use crate::features::FeatureSet;
use crate::Program;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

///
/// This contains the parsed content from a source, along with any remaining, un-parsed, content.
/// This allows for parser rules that match and return but have not matched everything.
///
#[derive(Clone, Debug)]
pub struct Parsed {
    parsed: Program,
    rest: Option<String>,
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

///
/// Parse the file at the provided path.
///
pub fn parse_file<P: AsRef<Path>>(file_path: P) -> Result<Parsed> {
    parse_file_with_features(file_path, FeatureSet::default())
}

///
/// Parse the file at the provided path, but enabled the set of language features first.
///
pub fn parse_file_with_features<P: AsRef<Path>>(
    file_path: P,
    features: FeatureSet,
) -> Result<Parsed> {
    parser::program(
        &read_to_string(file_path.as_ref())?,
        features,
        Some(PathBuf::from(file_path.as_ref())),
    )
}

///
/// Parse the string content provided.
///
pub fn parse_str(source: &str) -> Result<Parsed> {
    parse_str_with_features(source, FeatureSet::default())
}

///
/// Parse the string content provided, but enabled the set of language features first.
///
pub fn parse_str_with_features(source: &str, features: FeatureSet) -> Result<Parsed> {
    parser::program(source, features, None)
}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

impl Parsed {
    fn new(parsed: Program) -> Self {
        Self { parsed, rest: None }
    }

    fn new_with_more(parsed: Program, rest: String) -> Self {
        if !rest.is_empty() {
            Self {
                parsed,
                rest: Some(rest),
            }
        } else {
            Self::new(parsed)
        }
    }

    pub fn parsed(&self) -> &Program {
        &self.parsed
    }

    pub fn parsed_mut(&mut self) -> &mut Program {
        &mut self.parsed
    }

    pub fn into_parsed(self) -> Program {
        self.parsed
    }

    pub fn rest_of_input(&self) -> Option<&String> {
        self.rest.as_ref()
    }

    pub fn has_more_input(&self) -> bool {
        self.rest
            .as_ref()
            .map(|s| !s.is_empty())
            .unwrap_or_default()
    }
}

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

mod parser;
