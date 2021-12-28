/*!
Another Simplistic [Datalog](https://en.wikipedia.org/wiki/Datalog) Implementation (ASDI), in Rust.

This package provides a data model to represent [Datalog](https://en.wikipedia.org/wiki/Datalog)
programs in memory, a parser for the textual representation, and some evaluation implementations.

The text representation parser is a separate feature, so if you only need to construct and evaluate
programs using the API you may opt out of the [Pest](https://pest.rs) parser and support.

# Datalog Defined

Datalog is a logic programming language and a subset of the earlier
[Prolog](https://en.wikipedia.org/wiki/Prolog).

The descriptions below use both mathematical definitions as well as ABNF
([Augmented BNF for Syntax Specifications](https://datatracker.ietf.org/doc/html/rfc5234))
descriptions. The ABNF description is somewhat simplified from the grammar used in the ASDI
parser although they do not significantly affect the expressiveness of the language. For example,
the parser accepts a number of representations of common operators or connectives so that
conjunction may be denoted as `","`, `"&"`, `"AND"`, or `"∧"`.


$$\mathcal{P}=\lbrace\mathcal{D},\mathcal{R}\rbrace$$

$\mathcal{D}$ is commonly referred to as the *Extensional Database* or EDB.
$\mathcal{R}$ is commonly referred to as the *Intensional Database* or IDB.


$$\mathcal{D}=\lbrace\mathcal{P},\mathcal{V},\mathcal{C}\rbrace$$


When referring to the specifics of the language we will use the common format $\text{\small{Datalog}}$ with
superscripts that identify specific language extensions; for example, $\text{\small{Datalog}}^{\lnot}$ is
the language extended with negation of literals, and $\text{\small{Datalog}}^{\lnot=}$. is the
language extended with negation of literals and comparison expressions. The order of superscript
symbols is irrelevant.

There exists a relatively simple translation from most Datalog rules to first-order predicate
logic, such that the following simple rule:

```datalog
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

Can be expressed in the following manner:

$$\forall x\forall y\forall z \left(parent(x, z) ∧ ancestor(z, y) \Rightarrow ancestor(x, y) \right)$$


## Programs

```abnf
program         = *[ fact ] *[ rule ] *[ query ]
```

## Facts

Facts are introduced in the form _predicate_ followed by a period (end of statement marker) or a
set of _arguments_ within parenthesis.

```datalog
parent("Xerces", brooke).
```

$p\left(c_{1} \ldots c_{n}\right)$ where predicate $p \in \mathcal{P}$, constant $c \in \mathcal{C}$,
and $n \in \mathbb{P}$. The value $n$ is also termed the arity of the predicate $p$.

```abnf
fact            = predicate [ constant-list ] "."
predicate       = LC_ALPHA *[ ALPHA / DIGIT / "_" ]
constant-list   = "(" [ constant *[ "," constant ] ] ")"
```

## Constant Values

* String
* Integer
* Boolean `@true` or `@false`

```abnf
constant        = string / integer / boolean
string          = short-string / quoted-string
short-string    = predicate [ ":" ALPHA *[ ALPHA / DIGIT / "_" ] ]
quoted-string   = DQUOTE ... DQUOTE
integer         = +DIGIT
boolean         = "@true" / "⊤" / "@false" / "⊥"
```

Boolean values may also be represented using `⊤` (down tack `\u{22a4}`) for true, and `⊥` (up tack
`\u{22a5}`) for false.

## Rules

```abnf
rule            = head implication body "."
head            = [ atom *[ disjunction atom ] | "⊥" ]
disjuntion      = "|" / "OR" / "∨"
implication     = ":-" / "<-" / "⟵"
body            = literal-list
```

Implication may also be written using the Unicode character `⟵` (long leftwards arrow `\u{27f5}`).

```datalog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) <- parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Y).
```

```datalog
father(X) ⋁ mother(X) :- parent(X).
```

**SOME LANGUAGE FEATURE**

```datalog
⊥ :- alive(X), dead(X).
:- alive(X), dead(X).
```

**Facts** are ...

## Atoms

$p\left(t_{1} \ldots t_{n}\right)$ where predicate $p \in \mathcal{P}$ and $n \in \mathbb{P}$.
A term $t$ is either a constant $t \in \mathcal{C}$, a variable $t \in \mathcal{V}$, or the
anonymous variable notation `_`.
The value $n$ is also termed the arity of the predicate $p$.

```abnf
atom            = predicate term-list
term-list       = "(" term *[ "," term ] ")"
term            = variable / constant
variable        = named-variable / anon-variable
named-variable  = UC_ALPHA *[ ALPHA / DIGIT / "_" ]
anon-variable   = "_"
```

## Literals

The addition of the rule `negation` is only present in the $\text{\small{Datalog}}^{\lnot}$ language.
Similarly, the addition of the `comparison` rule is only present in the $\text{\small{Datalog}}^{=}$
language.

```abnf
literal-list    = literal *[ conjunction literal ]
literal         = [ negation ] atom / comparison
negation        = "!" / "NOT" / "￢"
conjunction     = "," / "&" / "AND" / "∧"
```

Negation may also be written using the Unicode character `￢` (full-width not sign `\u{ffe2}`),
and conjunction may be written with the Unicode character `∧` (logical and `\u{2227}`).

```datalog
ancestor(X, Y) ⟵ parent(X, Z), ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) & ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) ∧ ancestor(Z, Y).
ancestor(X, Y) ⟵ parent(X, Z) AND ancestor(Z, Y).
```

## Comparisons

```abnf
comparison      = term operator term
operator        = "=" / "!=" / "/=" / "≠" / "<" / "<=" / "≤" / ">" / ">=" / "≥"
```

The Unicode characters `≠` (not equal to `\u{2260}`), `≤` (less-than or equal to `\u{2264}`), and
`≥` (greater-than or equal to `\u{2265}`) may be substituted for the common comparison operators.

## Queries

```abnf
query           = ( "?-" atom "." ) / ( atom "?" )
```

```datalog
?- ancestor(xerces, X).
ancestor(xerces, X)?
```

## Pragmas

```abnf
pragma          = declare / include / input / output

declare         = "@declare" predicate attribute-list "."
attribute-list  = "(" attribute-decl *[ "," attribute-decl ] ")"
attribute-decl  = [ predicate ":" ] attribute-type
attribute-type  = "boolean" / "integer" / "string"

include         = "@include" quoted-string "."

input           = "@input" "(" predicate "," quoted-string [ "," quoted-string ] ")" "."

output          = "@output" "(" predicate "," quoted-string [ "," quoted-string ] ")" "."
```

```datalog
@include("file").
@declare human(name: string).
@input(human, "file", "csv").
@output(human, "file", "text").
```

## Comments

```datalog
## Here's a comment
?- ancestor(xerces, X). # and another
```


# Example

```datalog
parent(xerces, brooke).
parent(brooke, damocles).

ancestor(X, Y) ⟵ parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Z) ∧ parent(Z, Y).

?- ancestor(xerces, X).
```


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
