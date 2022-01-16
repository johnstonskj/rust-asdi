#![allow(unused)]
use paste::paste;

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

const_string!(EMPTY_STR => "");

const_string!(DEFAULT_LANGUAGE_NAME => "Datalog");

// ------------------------------------------------------------------------------------------------

const_string_block!(
    TYPE_NAME
    ( COMPARISON_OPERATOR => "ComparisonOperator" ),
    ( PREDICATE => "Predicate" ),
    ( VARIABLE => "Variable" ),
    ( CONSTANT => "Constant" ),
    // ----------------------------------------
    ( CONST_STRING => "String" ),
    ( CONST_INTEGER => "Integer" ),
    ( CONST_FLOAT => "Float" ),
    ( CONST_BOOLEAN => "Boolean" ),
    ( CONST_UNKNOWN => "?" )
);

// ------------------------------------------------------------------------------------------------

pub(crate) const RESERVED_PREFIX: &str = ".";

const_string_block!(
    BOOLEAN_LITERAL
    ( TRUE => "true" ),
    ( FALSE => "false" )
);

const_string_block!(
    PRAGMA_ID
    ( ASSERT => "assert" ),
    ( INFER => "infer" ),
    ( FEATURE => "feature" ),
    ( INCLUDE => "include" ),
    ( INPUT => "input" ),
    ( OUTPUT => "output" )
);

const_string_block!(
    FEATURE
    ( NEGATION_ID => "negation" ),
    ( NEGATION_SYMBOL => "￢" ),
    // ----------------------------------------
    ( COMPARISONS_ID => "comparisons" ),
    ( COMPARISONS_SYMBOL => "θ" ),
    // ----------------------------------------
    ( CONSTRAINTS_ID => "constraints" ),
    ( CONSTRAINTS_SYMBOL => "⇐" ),
    // ----------------------------------------
    ( DISJUNCTION_ID => "disjunction" ),
    ( DISJUNCTION_SYMBOL => "∨" ),
    // ----------------------------------------
    ( EXCLUSIVE_DISJUNCTION_ID => "exclusive-disjunction" ),
    ( EXCLUSIVE_DISJUNCTION_SYMBOL => "⊕" )
);

// ------------------------------------------------------------------------------------------------

const_string_block!(
    ANONYMOUS
    ( COLUMN_NAME => "_" ),
    ( TERM => "_" )
);

// ------------------------------------------------------------------------------------------------

const_string_block!(
    IMPLICATION
    ( ASCII_TURNSTILE => ":-" ),
    ( ASCII_ARROW => "<-" ),
    ( UNICODE_ARROW => "⟵" )
);

const_string_block!(
    CONJUNCTION
    ( ASCII => "," ),
    ( ASCII_SYMBOL => "&" ),
    ( ASCII_WORD => "AND" ),
    ( UNICODE_SYMBOL => "∧" )
);

const_string_block!(
    DISJUNCTION
    ( ASCII => ";" ),
    ( ASCII_SYMBOL => "|" ),
    ( ASCII_WORD => "OR" ),
    ( UNICODE_SYMBOL => "∨" )
);

const_string_block!(
    NEGATION
    ( ASCII_SYMBOL => "!" ),
    ( ASCII_WORD => "NOT" ),
    ( UNICODE_SYMBOL => "￢" )
);

// ------------------------------------------------------------------------------------------------

pub(crate) const TRUE_UNICODE_SYMBOL: &str = "⊤";
pub(crate) const FALSE_UNICODE_SYMBOL: &str = "⊥";

const_string_block!(
    OPERATOR
    ( EQUAL_ASCII => "=" ),
    // ----------------------------------------
    ( NOT_EQUAL_ASCII => "!=" ),
    ( NOT_EQUAL_ASCII_ALT => "/=" ),
    ( NOT_EQUAL_UNICODE => "≠" ),
    // ----------------------------------------
    ( LESS_THAN_ASCII => "<" ),
    // ----------------------------------------
    ( LESS_THAN_OR_EQUAL_ASCII => "<=" ),
    ( LESS_THAN_OR_EQUAL_UNICODE => "," ),
    // ----------------------------------------
    ( GREATER_THAN_ASCII => ">" ),
    // -----------------------------------------
    ( GREATER_THAN_OR_EQUAL_ASCII => ">=" ),
    ( GREATER_THAN_OR_EQUAL_UNICODE => "≥" ),
    // -----------------------------------------
    ( STRING_MATCH_ASCII => "*=" ),
    ( STRING_MATCH_ASCII_WORD => "MATCHES" ),
    ( STRING_MATCH_UNICODE => "≛" )
);

const_string_block!(
    QUERY
    ( ASCII_PREFIX => "?-" ),
    ( ASCII_SUFFIX => "?" )
);

const_char!(CHAR_LEFT_PAREN => '(');
const_char!(CHAR_RIGHT_PAREN => ')');
const_char!(CHAR_PERIOD => '.');
const_char!(CHAR_COLON => ':');
const_char!(CHAR_COMMA => ',');
const_char!(CHAR_UNDERSCORE => '_');
const_char!(CHAR_SEMI_COLON => ';');
