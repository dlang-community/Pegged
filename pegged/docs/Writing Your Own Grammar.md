Writing Your Own Grammar
========================

**Pegged** allows you to build your grammar easily, using a short notation that allows you to make a top-down or bottom-up construction.

My first advice would be to structure your grammar in big chunks and see how they play with one another. You can then refine each of these chunks and break it into smaller parts. If a small part seems useful enough, make it its own grammar. That way, it can be used again in other grammars (see [[Grammar Composition]]).

For example, a standard question is 'How is operator precedence defined in grammar like these, there is no operator precedence table anywhere in sight!'. The trick is to begin with the weaker operators and then go down with the stronger binders. For example, let's consider arithmetic expressions that contain +(unary and binary),- (unary and binary),/,*,%, pow (^) and parenthesis. The standard mathematic precedence is (from weaker to stronger): 

* Additive operators: + (binary), - (binary), 

* Multiplicative operators: %, *, /,

* Power: ^, 

* unary operators: -, +,

* Grouping: (, )

* The number themselves (yes, 12 + 3 is parsed as 12 + 3, not 1 (2+3), + does not 'rip' 12 in two).

That means an arithmetic expression is first an addition:

```
Arithmetic <- Add
Add        <- Mul (("+"/"-") Mul)*
```

This process will continue farther down:

```
Mul        <- Pow (("*"/"/"/"%") Pow)*
Pow        <- Unary ("^" Unary)*
Unary      <- ("+"/"-")? Primary
Primary    <- "(" Arithmetic ")"  # Recursion
            / Number              # End case
```

Note how the lowest level ties it into a coherent whole: there is either an end case: `Number` or a branch going up to the higher level, namely `Arithmetic`, to allow nesting expression within expressions. Notice also how `-` can means both a binary minus or an unary one, depending how it's placed in the input. `1-2-3` is parsed as `1 - ( 2 - ( 3 ) )` whereas `1 + - 2` is rightfully recognized as `1 + (-2)`.

The same process can be applied to logical (boolean) expression using || (or), && (and) and ! (not):

```
Boolean <- OrExpr
OrExpr  <- AndExpr ("||" AndExpr)*
AndExpr <- NotExpr ("&&" NotExpr)*
NotExpr <- "!"? Primary
Primary <- '(' Boolean ')' 
         / Atom
Atom <- ...
```

Where `Atom` is the atomic element in a boolean expression: identifiers, entire D expressions, what have you.

And here is an idea: these small boolean grammar could be parametrized on `Atom` and work on any kind of expression (see [[Parametrized Rules]], though there is no way to parametrized an entire grammar in **Pegged** right now).

-----------

Next Lesson: [[Four Levels of Encapsulation]]

-----------

[[Pegged Tutorial]]