# Introduction

Another Simplistic [Datalog](https://en.wikipedia.org/wiki/Datalog) Implementation (ASDI), in Rust.

Datalog is a logic programming language and a subset of the earlier
[Prolog](https://en.wikipedia.org/wiki/Prolog)<span class="fnref">[1](#1)</span>. This package provides a data model to represent 
[Datalog](https://en.wikipedia.org/wiki/Datalog) programs in memory, a parser for the textual representation, 
and some evaluation implementations.

## Brief Example

We will consider the following syllogism from <span class="bibref inline">[Mill1851](../reference/references.md#Mill1851), vol. 1, chap. 2</span>:

> All men are mortal.
> 
> Socrates is a man.
> 
> Therefore, Socrates is mortal.

This can be represented in the following Datalog program.

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
| true      |
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

----------

<div class="footnotes">

1. <a name="1">Chapter</a> 1 of <span class="bibref inline">[CeGoTa90](../reference/references.md#CeGoTa90)</span>
   provides a good overview of the drawbacks of Prolog and the advantages of Datalog for certain tasks.

</div>