# Datalog

Datalog is a logic programming language, but is also an expressive query language for deductive databases. As such it 
can be studied, and described, either as a programming language, as a language for expressing mathematical logic, or
as a query language. 

When considered from a mathematical point of view it is common to start from a basis in
[predicate logic](https://en.wikipedia.org/wiki/First-order_logic) or [Horn clauses](https://en.wikipedia.org/wiki/Horn_clause)
and demonstrate how these relate to the rules in a Datalog program. The following section takes a more direct approach leaving
the relationship with other logic forms for a brief [appendix](../reference/logic.md).

When considered from a database or query point of view it is common to start from a basis in
[relational algebra](https://en.wikipedia.org/wiki/Relational_algebra) and demonstrate the expressive equivalence
between it and Datalog. In considering Datalog as a query language it is noted that every expression in the 
_basic relational algebra_ can be expressed as a Datalog query. A mapping from relational algebra to
Datalog is also included as an [appendix](../reference/relational.md).

## Datalog Languages

When we talk specifically about the Datalog language it is common to represent it in a roman serif face as 
$\small\text{Datalog}$. This allows the distinction of talking in general about Datalog in the abstract vs. the actual
semantics of the language $\small\text{Datalog}$.

This core $\small\text{Datalog}$ language has many documented extensions that provide additional flexibility, conciseness, or
expressiveness. These _languages_, _sub-languages_, _extensions_, or _dialects_ are usually labeled by adding a superscript
symbol that identifies the specific extension. For example, $\small\text{Datalog}^{\lnot}$ is
the language extended with negation of literals, $\small\text{Datalog}^{\Gamma}$ is the language
extended with type checking on attributes, and $\small\text{Datalog}^{\lnot,\theta}$ is the language
extended with negation of literals _and_ arithmetic literals. The order of superscript symbols is
irrelevant.

When referring to the specifics of the language we will use the common format $\small\text{Datalog}$ with
superscripts as necessary to identify specific language extensions being used.  

