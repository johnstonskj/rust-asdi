/*!
One-line description.

More detailed description, with

# Example

*/

use asdi::features::FeatureSet;
use asdi::parse::parse_str_with_features;
use asdi::Program;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Types & Constants
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Macros
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Public Functions
// ------------------------------------------------------------------------------------------------

#[allow(dead_code)]
pub fn quick_parser_check<P: Into<Option<Program>>>(s: &str, expected: P) {
    quick_parser_check_with_options(s, FeatureSet::default(), expected)
}

#[allow(dead_code)]
pub fn quick_parser_check_with_options<P: Into<Option<Program>>>(
    s: &str,
    features: FeatureSet,
    expected: P,
) {
    match parse_str_with_features(s, features) {
        Ok(program) => {
            if let Some(expected) = expected.into() {
                assert_eq!(program.into_parsed(), expected);
            } else {
                println!("Success:- {:#?}", program);
            }
        }
        Err(e) => {
            println!("{}", e);
            panic!("{}", e.to_string())
        }
    }
}

#[allow(dead_code)]
pub fn quick_program_check(s: &str, and_then: fn(Program) -> ()) {
    quick_program_check_with_options(s, FeatureSet::default(), and_then)
}

#[allow(dead_code)]
pub fn quick_program_check_with_options(
    s: &str,
    features: FeatureSet,
    and_then: fn(Program) -> (),
) {
    match parse_str_with_features(s, features) {
        Ok(program) => and_then(program.into_parsed()),
        Err(e) => {
            println!("{}", e);
            panic!("{}", e.to_string())
        }
    }
}

// ------------------------------------------------------------------------------------------------
// Implementations
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------
