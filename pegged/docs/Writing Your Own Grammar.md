Writing Your Own Grammar
========================

**Pegged** allows you to build your grammar easily, using a short notation that alows you to make top-down or bottom-up construction.

My first advice would be to structure your grammar in big chunks and see how they play with one another. You can then refine each of these chunks into smaller parts. If a small part seems useful enough, make it its own grammar. That way, it can be used again in other grammars (see [[Grammar Composition]]).

For example, a standard question is 'How is operator precedence defined in grammar like these, there is no operator precedence table anywhere in sight!'. The trick is to begin with the weaker operators and then go down into the stronger binders. For example, with arithmetic expression that would allow +(unary and binary),- (unary and binary),/,*,%, pow (^) and parenthesis. The standard mathematic precedence is (from weaker to stronger): 

* Additive operators: + (binary), - (binary), 

* Multiplicative operators: %, *, /,

* Power: ^, 

* unary operators: -, +,

* Grouping: (, )

* The number themselves (yes, 12 + 3 is parsed as 12 + 3, not 1 (2+3), + does not 'rip' 12 in two).

That means an arithmetic expression is first a list of additive terms:

```
Arithmetic <- Add (("+"/"-") Add)*
```

Notice how an `Arithmetic` can be a lone `Add`, without any second member. This process will continue farther down:

```
Add <- Mul (("*"/"/"/"%") Mul)*
Mul <- Pow ("^" Pow)*
Pow <- ("+"/"-")? Unary
Unary <- "(" Arithmetic ")"  # Recursion
       / Number
```

Note how the lowest level ties it into a coherent whole: there is either an end case: `Number` or a branch going up to the higher level, namely `Arithmetic`, to allow nesting expression within expressions.


The same process can be applied to logical (boolean) expression using || (or), && (and) and ! (not):

```
Logical <- OrExpr
OrExpr <- AndExpr ("||" AndExpr)*
AndExpr <- NotExpr ("&&" NotExpr)*
NotExpr <- "!" Logical 
         / Atom
Atom <- ...
```

Where `Atom` is the atomic element in a boolean expression: identifiers, entire D expressions, what have you.

And here is an idea: these small boolean grammar could be parametrized on `Atom` and work on any kind of expression (see [[Parametrized Rules]], though there is no way to parametrized an entire grammar in **Pegged** right now).

-----------

Next Lesson: [[Four Levels of Encapsulation]]

-----------

[[Pegged Tutorial]]