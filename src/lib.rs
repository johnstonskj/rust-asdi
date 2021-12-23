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

**Programs**

```abnf
program         = *[ fact ] *[ rule ] *[ query ]
```

**Facts**

```abnf
fact            = predicate [ constant_list ] "."
predicate       = LC_ALPHA *[ ALPHA / DIGIT / "_" ]
constant_list   = "(" [ constant *[ "," constant ] ] ")"
```

**Constant Values**

```abnf
constant        = string / integer / boolean
string          = short_string / quoted_string
short_string    = predicate [ ":" ALPHA *[ ALPHA / DIGIT / "_" ] ]
quoted_string   = DQUOTE ... DQUOTE
integer         = +DIGIT
boolean         = "@true" / "@false"
```

**Rules**

```abnf
rule            = atom implication [ literal_list ] "."
implication     = ":-" / "<-"
```

**Atoms**

```abnf
atom            = predicate term_list
term_list       = "(" term *[ "," term ] ")"
term            = variable / constant
variable        = named_variable / anon_variable
named_variable  = UC_ALPHA *[ ALPHA / DIGIT / "_" ]
anon_variable   = "_"
```

**Literals**

```abnf
literal_list    = literal *[ conjunction literal ]
literal         = [ negation ] atom / comparison
negation        = "!" / "NOT"
conjunction     = "," / "AND"
```

**Comparisons**

```abnf
comparison      = term [ operator term ]
operator        = "=" / "!=" / "<" / "<=" / ">" / ">="
```

**Queries**

```abnf
query           = ( "?-" atom "." ) / ( atom "?" )
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
