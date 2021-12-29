/*!
Another Simplistic [Datalog](https://en.wikipedia.org/wiki/Datalog) Implementation (ASDI), in Rust.

This package provides a data model to represent [Datalog](https://en.wikipedia.org/wiki/Datalog)
programs in memory, a parser for the textual representation, and some evaluation implementations.

The text representation parser is a separate feature, so if you only need to construct and evaluate
programs using the API you may opt out of the [Pest](https://pest.rs) parser and support.

# Datalog Defined

Datalog is a logic programming language and a subset of the earlier
[Prolog](https://en.wikipedia.org/wiki/Prolog).

When referring to the specifics of the language we will use the common format $\text{\small{Datalog}}$ with
superscripts that identify specific language extensions; for example, $\text{\small{Datalog}}^{\lnot}$ is
the language extended with negation of literals, and $\text{\small{Datalog}}^{\lnot=}$. is the
language extended with negation of literals and comparison expressions. The order of superscript
symbols is irrelevant.

## Abstract Syntax

Datalog rules $\small R$ are built from a language $\small \mathcal{L}=\lbrace\mathcal{C},\mathcal{P},\mathcal{V}\rbrace$
that contains the

1. $\small \mathcal{C}$ -- the finite sets of symbols for all constants; e.g. `hello`, `"hi"`
   `123`,
2. $\small \mathcal{P}$ -- the finite set of alphanumeric character strings that begin with a
   lowercase character; e.g. `human`, `size`, `a`,
3. $\small \mathcal{V}$ -- the finite set of alphanumeric character strings that begin with an
   uppercase character; e.g. `X`, `A`, `Var`.

Each rule $\small r \in R$ has the form:

$$\tag{i}\small A_1, \ldots, A_m \leftarrow L_1, \ldots, L_n$$

1. a head $\small head(r)$, is the set of _atom_ values $\small A_1, \ldots, A_m$ where $\small m \in \mathbb{N}$,
2. a body $\small body(r)$, is a set of _literal_ values $\small L_1, \ldots, L_n$ where $\small n \in \mathbb{N}$,
3. a rule is _ground_ if its head and its body are both ground:

$$\tag{ii}\small ground\(r\) \leftarrow \(\forall{a}\in head\(r\)\) \(\forall{l}\in body\(r\)\) \(ground(a) \land ground\(l\)\)$$

The cardinality of the head, the value of $\small m$, defines the type of the rule, as follows.

$$\tag{iii}\small form = \begin{cases} pure, &\text{if } m = 1 \\\\ constraint, &\text{if } m = 0 \land \text{Datalog}^{\lnot} \\\\ disjunct, &\text{if } m > 1  \land \text{Datalog}^{\lor}\end{cases}$$

Atoms represent _relations_; they have

1. a predicate, $\small p \in \mathcal{P}$, that names the relationship,
1. a set of terms $\small t_1, \ldots, t_k$ where $\small t \in \mathcal{T}$ and
   $\small k \in \mathbb{N}^{+}$ that represent the relation attribute values,
1. a fixed arity, that is for all atoms sharing the same predicate the value of $\small k$ will
   match,
1. in a typed $\small\text{Datalog}$ each term $\small t_i$ shall have a type and for each value of
   $\small i$ all atoms sharing the same predicate will have the same type for $\small t_i$,
1. Terms in $\small \mathcal{T}$ may be constant values or variables, such that
   $\small\mathcal{T}=\mathcal{C}\cup\mathcal{V}\cup\bar{t}$ where $\small\bar{t}$ represents an
   unused variable (denoted in the text representation as an underscore `"_"`),
1. an atom is _ground_ if all of it's terms are constants:

$$\tag{iv}\small ground\(a\) \leftarrow \forall{t}\in terms\(a\) \(t \in \mathcal{C}\)$$

Literals represent clauses that select from relations.

1. a literal may be an atom, or in $\text{\small{Datalog}}^{=}$ a conditional expression,
1. in $\text{\small{Datalog}}^{\lnot}$ a literal may be _negated_,
1. a rule is termed _positive_ if all of its literals are positive:

$$\tag{v}\small positive(r) \leftarrow \(\forall{l}\in body\(r\)\) \(\lnot negated\(l\)\)$$

Any ground rule where $\small m=1$ and where $\small n=0$ is termed a _fact_ as it is true by nature of
having body, or alternatively we may consider the body be comprised of the truth value $\small\top$.

$$\tag{vi}\small fact(r) \leftarrow \(|head\(r\)|=1 \land |body\(r\)|=0\)$$

> Note that it is not possible to combine the _constraint_ form of a rule from (iii) and the fact
> form in (vi); $\small head\(r\)=\empty \land body\(r\)=\empty$ is not a valid rule.

An atom may be also used as a _goal clause_, or a _query_ in that it's constant and variable terms may be
used to match facts from the known facts or those that may be inferred from the set of rules
introduced. A ground goal is simply determining that any fact exists that matches all of the constant
values provided and will return true or false. In the case that one or more variables exist a set of
facts will be returned that match the expressed constants and provide the corresponding values for
the variables.

The set of facts known a-priori is termed the _extensional_ database or EDB and as it only contains
relations comprised of constant values (ground facts) it looks a lot like a relational database. The
set of rules, and any inferred facts are termed the _intensional_ database or IDB. A Datalog program
$\small P$ is therefore a set comprising the extensional database $\small D_{E}$, the
intensional database $\small D_{I}$, and a set of queries.

$$\tag{vii}\small P=\lbrace D_E, D_I, Q \rbrace$$

This implies, at least, that the set of predicates accessible to queries in the program is the union
of predicates in the extensional and intensional databases.

$$\tag{viii}\small \mathcal{P}_P = \mathcal{P}_E \cup \mathcal{P}_I$$

It should be obvious that the same exists for constants and variables;
$\small \mathcal{C}_P = \mathcal{C}_E \cup \mathcal{C}_I$ and
$\small \mathcal{V}_P = \mathcal{V}_E \cup \mathcal{V}_I$.

Datalog does not, in general, allow the rules comprising the intensional database to infer new
values for predicates that exist in the extensional database. This may be expressed as follows,
although in the textual representation it is expressed as an error to define a rule with a head
predicate that exists in the extensional database.

$$\tag{ix}\small \mathcal{P}_E \cap \mathcal{P}_I = \empty$$

The same restriction is not required for constants in $\small \mathcal{C}_P$ or variables in
$\small \mathcal{V}_P$.

## Concrete Syntax

The definitions below use ABNF
([Augmented BNF for Syntax Specifications](https://datatracker.ietf.org/doc/html/rfc5234)) and
focus both on the concrete syntax as expressed in the text representation.
The ABNF definition is somewhat simplified from the grammar used in the ASDI parser although any
deviations do not significantly affect the meaning of the language.

### Programs

A program consists of a set of facts that comprise the extensional database, a list of rules that
comprise the intensional database, and possibly a set of queries to interrogate the result of any
reasoning performed over the program.

```abnf
program         = *[ fact ] *[ rule ] *[ query ]
```

### Facts

Facts may only be expressed in the form of stand-alone ground atoms and **must** appear before any
rules.

```abnf
fact            = predicate [ constant-list ] "."
predicate       = LC_ALPHA *[ ALPHA / DIGIT / "_" ]
constant-list   = "(" [ constant *[ "," constant ] ] ")"
```

The following demonstrates a simple fact denoting that the constant `brooke` representing some
individual is the parent of some individual represented by the constant `"Xerces"`.

```datalog
parent("Xerces", brooke).
```

### Constant Values

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
`\u{22a5}`) for false where this may improve readability.

### Rules

As facts are treated as separate from rules in the text representation there is no need for empty
bodies, all rules **must** have at least one literal.

```abnf
rule            = head implication body "."
head            = [ atom *[ disjunction atom ] | "⊥" ]
disjuntion      = ";" / "|" / "OR" / "∨"
implication     = ":-" / "<-" / "⟵"
body            = literal-list
```

Material mplication may also be written using the Unicode character `⟵` (long leftwards arrow
`\u{27f5}`).

```datalog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) <- parent(X, Y).
ancestor(X, Y) ⟵ parent(X, Y).
```

The language feature `disjunction` allows for multiple atoms to appear in the rule's head with the
semantics that these are choices. For example, the following describes the rule that _if X is a
parent then X is **either** a father **or** mother_.

```datalog
@feature(disjunction).

father(X) ⋁ mother(X) :- parent(X).
```

The language feature `constraints` allows the specification of rules with no head, i.e. rules that
may never be true. In this case the material implication symbol is **required**, the falsum
value is optional for readability.

```datalog
@feature(constraints).

⊥ :- alive(X), dead(X).
:- alive(X), dead(X).
```

### Atoms



```abnf
atom            = predicate term-list
term-list       = "(" term *[ "," term ] ")"
term            = variable / constant
variable        = named-variable / anon-variable
named-variable  = UC_ALPHA *[ ALPHA / DIGIT / "_" ]
anon-variable   = "_"
```

### Literals

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

### Comparisons

```abnf
comparison      = term operator term
operator        = "=" / "!=" / "/=" / "≠" / "<" / "<=" / "≤" / ">" / ">=" / "≥"
```

The Unicode characters `≠` (not equal to `\u{2260}`), `≤` (less-than or equal to `\u{2264}`), and
`≥` (greater-than or equal to `\u{2265}`) may be substituted for the common comparison operators.

### Queries

```abnf
query           = ( "?-" atom "." ) / ( atom "?" )
```

```datalog
?- ancestor(xerces, X).
ancestor(xerces, X)?
```

### Pragmas

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
@declare human(name: string).
@input(human, "data/humans.csv", "csv").
@output(mortal, "data/mortals.txt", "text").
```

### Comments

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

```rust
use asdi::program::Program;
use asdi::edb::{Attribute, Predicate};
use asdi::idb::{Atom, Term};
use std::str::FromStr;

let mut syllogism = Program::default();
let p_human = Predicate::from_str("human").unwrap();

let human = syllogism
    .add_new_relation(p_human.clone(), vec![Attribute::string()])
    .unwrap();
human.add(["Socrates".into()]).unwrap();

let var_x: Term = Variable::from_str("X").unwrap().into();

syllogism
    .add_new_rule(
        Predicate::from_str("mortal").unwrap(),
        [var_x.clone()],
        [Atom::new(p_human, [var_x]).into()],
    )
    .unwrap();

syllogism
    .add_new_query(Predicate::from_str("mortal").unwrap(), ["Socrates".into()])
    .unwrap();
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
