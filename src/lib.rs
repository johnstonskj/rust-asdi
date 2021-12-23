/*!
Another Simplistic Datalog Implementation (ASDI), in Rust.

This package provides a data model to represent Datalog programs in memory, a parser for common
syntactic expression, and some evaluation implementations. The text representation parser is a separate
feature, so if you only need to construct and evaluate programs using the API you do not pull
in the [Pest](https://pest.rs) parser and support.

Tools that operate on the model to _validate_, _derive_, and _evaluate_ the program are
implementations of traits defined in the [`eval`](eval/index.html) module. This makes it easier to
integrate new more sophisticated implementations over time.

# Datalog Defined

* **Program**
  * _Facts_ and _Rules_
* **Atom** (Atomic Formula)
  * **Predicate**
  Variable names **must** begin with a lowercase letter followed by any number of Unicode letters,
  numbers, or the underscore `_` character.
    Predicates are sometimes refereed to as _Relations_.
  * **Fact**
    Fact arguments may only be _Constants_.
    Facts are sometimes refereed to as _Axioms_.
  * **Literal**
    Literal arguments may be _Constants_ or _Variables_.
  * **Expression**
* **Rule**
  * **head**
  * **body**
* **Query**
* **Constant**
  * **Identifier**
  * **String**
  * **Integer**
  * **Boolean**
* **Variable**
  Variable names **must** begin with an uppercase letter followed by any number of Unicode letters,
  numbers, or the underscore `_` character.

```abnf
program         = *[ element ]
element         = statement / query
statement       = ( fact / rule ) "."
query           = ( "?-" query "." ) / ( query "?" )
fact            = predicate [ constant_list ] "."
rule            = atom ":-" [ literal_list ] "."
constant_list   = "(" [ constant *[ "," constant ] ] ")"
constant        = identifier / string / integer / boolean
literal_list    = literal *[ conjunction literal ]
literal         = [ negation ] atom / expression
atom            = predicate "(" term *[ "," term ] ")"
expression      = term [ operator term ]
term_list       = "(" [ term *[ "," term ] ] ")"
term            = variable / constant
operator        = "=" / "!=" / "<" / "<=" / ">" / ">="
predicate       = LC_ALPHA *[ ALPHA / DIGIT / "_" ]
variable        = UC_ALPHA *[ ALPHA / DIGIT / "_" ]
identifier      = ALPHA *[ ALPHA / DIGIT / "_" ]
string          = DQUOTE ... DQUOTE
conjunction     = "," / "AND" / "∧"
negation        = "!" / "NOT" / "￢"
```

[Augmented BNF for Syntax Specifications: ABNF](https://datatracker.ietf.org/doc/html/rfc5234)

# Source Syntax

The text representation that the parser feature accepts is intended to be flexible. While the core program
elements are fixed, there are a number of different operator styles in common usage and where
possible the parser will accept them all.

For example, the following is a simple yet complete program and written using Unicode characters
for an expressive representation.

```datalog
parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ∧ parent(Z, Y).

?- ancestor(xerces, X).
```

## Facts

Facts are introduced in the form _predicate_ followed by a period (end of statement marker) or a
set of _arguments_ within parenthesis. A predicate is either an

```datalog
parent("Xerces", brooke).
```

### Constants

* Identifier
* String
* Integer
* Float
* Boolean `@true` or `@false`

## Rules

* The ASCII string `":-"` (colon `\u{3a}` and hyphen-minus `\u{2d}`).
* The ASCII string `"<-"` (less-than `\u{3c}` and hyphen-minus `\u{2d}`).
* The Unicode character `⟵` (long/full-width leftwards arrow `\u{27f5}`).

```datalog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) <- parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Y).
```

* The ASCII character `,` (comma `\u{2c}`).
* The ASCII character `&` (comma `\u{26}`).
* The Unicode character `∧` (logical and `\u{2227}`).
* The case-sensitive ASCII string `"AND"`.

```datalog
ancestor(X, Y) ⟵ parent(X, Z), ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) & ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) ∧ ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) AND ancestor(Z, Y).
```

$$\forall x\forall y\forall z \left(parent(x, z) ∧ ancestor(z, y) \Rightarrow ancestor(x, y) \right)$$

## Queries

question mark `\u{3f}`

```datalog
?- ancestor(xerces, X).
ancestor(xerces, X)?
```

## Pragmas

commercial at `\u{40}`

```datalog
@include("file").
```

## Comments

```datalog
## Here's a comment
?- ancestor(xerces, X). # and another
```

# Example

TBD

*/

#![warn(
    unknown_lints,
    // ---------- Stylistic
    absolute_paths_not_starting_with_crate,
    elided_lifetimes_in_paths,
    explicit_outlives_requirements,
    macro_use_extern_crate,
    nonstandard_style, /* group */
    noop_method_call,
    rust_2018_idioms,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    // ---------- Future
    future_incompatible, /* group */
    rust_2021_compatibility, /* group */
    // ---------- Public
    missing_debug_implementations,
    // missing_docs,
    unreachable_pub,
    // ---------- Unsafe
    unsafe_code,
    unsafe_op_in_unsafe_fn,
    // ---------- Unused
    unused, /* group */
)]
#![deny(
    // ---------- Public
    exported_private_dependencies,
    private_in_public,
    // ---------- Deprecated
    anonymous_parameters,
    bare_trait_objects,
    ellipsis_inclusive_range_patterns,
    // ---------- Unsafe
    deref_nullptr,
    drop_bounds,
    dyn_drop,
)]

// ------------------------------------------------------------------------------------------------
// Public Types & Constants
// ------------------------------------------------------------------------------------------------

pub trait SyntacticFragments {
    fn is_positive(&self) -> bool;

    /// **linear Datalog**, where rule bodies must consist of a single atom.
    fn is_linear(&self) -> bool;

    /// **guarded Datalog**, where for every rule, all the variables that occur in the rule bodies
    /// must occur together in at least one atom, called a _guard atom_.
    fn is_guarded(&self) -> bool;

    /// **frontier-guarded Datalog**, where for every rule, all the variables that are shared
    /// between the rule body and the rule head (called the _frontier variables_) must all occur
    /// together in a guard atom.
    fn is_frontier_guarded(&self) -> bool;

    /// **non-recursive Datalog** is defined by disallowing recursion in the definition of Datalog
    /// programs.
    fn is_non_recursive(&self) -> bool;
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

// ------------------------------------------------------------------------------------------------
// Private Functions
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// Modules
// ------------------------------------------------------------------------------------------------

pub mod error;

pub mod eval;

pub mod features;

pub mod edb;

pub mod idb;

pub mod program;

pub mod query;

// ------------------------------------------------------------------------------------------------
// Feature-gated Modules
// ------------------------------------------------------------------------------------------------

#[cfg(feature = "parser")]
pub mod parse;

#[cfg(feature = "typeset")]
pub mod typeset;

// ------------------------------------------------------------------------------------------------
// Private Modules
// ------------------------------------------------------------------------------------------------

mod syntax;
