# Introduction

Another Simplistic [Datalog](https://en.wikipedia.org/wiki/Datalog) Implementation (ASDI), in Rust.

Datalog is a logic programming language and a subset of the earlier [Prolog](https://en.wikipedia.org/wiki/Prolog)[^1].
The language is interesting as it can be used as a data query language akin to [SQL](https://en.wikipedia.org/wiki/SQL)
with some important additional capabilities such as recursive queries. It is also expressive enough to allow for it's
use as an entailment mechanism for ontology languages such as the 
[Web Ontology Language (OWL)](https://en.wikipedia.org/wiki/https://en.wikipedia.org/wiki/Web_Ontology_Language) and 
the Semantic Web.

This package provides an in-memory model to represent [Datalog](https://en.wikipedia.org/wiki/Datalog) programs, a 
parser for a textual representation, relation input/output implementations and some implementations of common inference
techniques.

**Chapter 1**, provides a brief simple description of Datalog by example. 

**Chapter 2** will provide a more detailed overview of Datalog itself from both an abstract point of view, and a tour of the 
concrete syntax used as the textual representation.

**Chapter 3** will provide a guide to the crate API in a more scenario-based fashion and relies on the 
[rust doc](https://docs.rs/asdi/latest/asdi/) to provide the definitive reference.

**Chapter 4** will cover the major extension points in the crate and identify how to contribute new implementations for
specific extension traits.


## Brief Datalog Example

We will consider the following syllogism from <span class="bibref inline">[Mill1851](../reference/references.md#Mill1851), vol. 1, chap. 2</span>:

> All men are mortal.
> 
> Socrates is a man.
> 
> Therefore, Socrates is mortal.

This can be represented in the following Datalog textual program.

```datalog
human("Socrates").

mortal(X) <- human(X).

?- mortal("Socrates").
```

The execution of this program will start with the goal query "_is Socrates mortal?_" and in
doing so will evaluate the necessary rule and derive the relation _mortal_. The result is a
boolean value denoting whether the goal is satisfied.

```text
+------------+
| _: boolean |
+============+
| true       |
+------------+
```

However, if we were to change the final query to replace the constant with a variable, as follows.

```datalog
?- mortal(X).
```

The program will select all matching (in this case all) facts from the _mortal_ relation.

```text
+------------+
| X: string  |
+============+
| "Socrates" |
+------------+
```

The following is the same example constructed via the ASDI library.

```rust
use asdi::{NameReferenceSet, Program};
use asdi::edb::{Attribute, Predicate};
use asdi::idb::{Atom, Term, Variable};
use std::str::FromStr;

let mut syllogism = Program::default();

let predicates = syllogism.predicates();
let p_human = predicates.fetch("human").unwrap();
let p_mortal = predicates.fetch("mortal").unwrap();

let human = syllogism
    .add_new_extensional_relation(p_human.clone(), vec![Attribute::string()])
    .unwrap();
human.add_as_fact(["Socrates".into()]).unwrap();

let variables = syllogism.variables();
let var_x: Term = variables.fetch("X").unwrap().into();

syllogism
    .add_new_pure_rule(
        p_mortal.clone(),
        [var_x.clone()],
        [Atom::new(p_human, [var_x]).into()],
    )
    .unwrap();

syllogism
    .add_new_query(p_mortal, ["Socrates".into()])
    .unwrap();
```

The textual form of Datalog also has a set of pragmas, or directives, which allow us to specify details of the relations
in our program. For example, in the text below we use the `assert` pragma to identify an EDB relation named "human" with
a single column of type `string`. We then go on to use the `infer` pragma to identify an IDB relation named "mortal",
and we declare that it's schema is the same as the EDB relation "human".

```datalog
.assert human(string).
.infer mortal from human.

human("Socrates").

mortal(X) <- human(X).

?- mortal("Socrates").
```

## License

```text
{{#include ../../../LICENSE}}
```

----------

[^1]: Chapter 1 of <span class="bibref inline">[CeGoTa90](../reference/references.md#CeGoTa90)</span> provides a good overview of the drawbacks of Prolog and the advantages of Datalog for certain tasks.
