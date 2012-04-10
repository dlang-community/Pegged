Generating Code
===============

Once a grammar is defined and a parse tree generated, how can it be used? We already saw how to use the parse tree in [[Using the Parse Tree]], at runtime or compile-time. But since **Pegged** can parse at compile-time, the information can also be used to generate a string containing D code, which can in turn be mixed in standard D code. 

So, what's the difference with what we saw in lesson [[Using the Parse Tree]]? None whatsoever, except be sure to do everything at compile-time, since you cannot mix runtime strings in.

This is *exactly* what **Pegged** itself does with the input you give it. So let's use **Pegged** as an example.

Part of the PEG grammar is :

```
Grammar    <- S Definition+ EOI
Definition <- RuleName Arrow Expression S
RuleName   <- Identifier>(ParamList?) S
Expression <- Sequence (OR Sequence)*
Sequence   <- Element*
Element    <- Prefix (JOIN Prefix)*
Prefix     <- (LOOKAHEAD / NOT / DROP / FUSE)? Suffix
(...)
```

A `Grammar` is a bunch of `Definition`s up to the end of the input (to let nothing unparsed and provoke an error is something is left unparsed. A `Definition` is a `RuleName`, an `Arrow` and an `Expression` and so on. Pretty standard stuff for you now. Let's see how that can be used to create code.

As seen in [[Behind the Curtain: How Pegged Works]], the basic PEG parsers are classes like `Seq` (sequences), `Or` (choices) or `Lit` (literals). A PEG expression like

```
A <- B C D* E?
```

Is implemented in D code as

```d
class A : Seq!(B, C, OneOrMore!(D), Option!(E)) 
{ 
    /* standard parsing stuff here */
}
``` 

So really, to create such a code from a parse tree we have to

1) Get the rule name

2) (Optionally, get the arrow type, not described here)

3) Get the expression PEG parse tree

4) Transform the expression into something like `Seq!(B, C, OneOrMore!(D), Option!(E))`

5) concatenate the parts : name + inheritance + parsing stuff inside the 

6) (Optionally, collect the rule names to get better parse tree deforestation: cutting rules external to the grammar in the final parse tree)

I'll concentrate on steps 1), 3) and 4).

If you look into `pegged.grammar`, you'll find the `PEGtoCode` function.

(*to be continued*)


* * * *

Next lesson: [[User-Defined Parsers]]

* * * *

[[Pegged Tutorial]]