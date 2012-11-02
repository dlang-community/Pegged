Optimizations
=============

Big subject...

Pegged Internal Optimizations
-----------------------------

I won't write here about **Pegged** internal optimizations since, as of this writing, there aren't many of them. **Pegged** simplify sequences and choices when they have one element or are sequences of sequences or choices or choices. It also memoize all parsing results: when a rule is called at a specific index, the grammar first checks whether this particular rule was already called at this position. If yes, then the stored parse tree is returned instead of parsing anew.

The thematic of this page is more 'how can I make my grammar faster?'.

No Recompilation
----------------

First, if you're satisfied with the way a grammar is, you can compile it and drop the resulting code into a file, to make it an independent module. Use `asModule(string moduleName, string grammar)` for that. That'll create a D module that imports `pegged.peg` and contains your grammar. I might add an `asAutonomousModule` function that'd also write the necessary **Pegged** infrastructure in the file, thus making it totally autonomous (you wouldn't need **Pegged** to use it elsewhere).

Once written in a file, the grammar-compiling step can be avoided. It won't make the grammar any faster but does prevent recompiling many times something that is stable.

Inlining
--------

Second, note there is a tension between a very complete grammar (and hence, parse tree) and a fast one: parsing speed depends (at least partially) on the grammar depth, its number of rules. So inlining could maybe be used to accelerate somewhat the parsing:

From:
```d
mixin(grammar("
Arithmetic1:
    Expr     <- Factor AddExpr*
    AddExpr  <- ('+'/'-') Factor
    Factor   <- Primary MulExpr*
    MulExpr  <- ('*'/'/') Primary
    Primary  <- Parens / Number / Variable / '-' Primary

    Parens   <- '(' Expr ')'
    Number   <~ [0-9]+
    Variable <- Identifier
"));
```

to:
```d
mixin(grammar("
Arithmetic2:
    Expr     <- (Primary (('*'/'/') Primary)*) (('+'/'-') Primary (('*'/'/') Primary)*)*
    Primary  <- '(' Expr ')' / [0-9]+ / Identifier / '-' Primary
"));
```

You get the idea. Note the process stopped at `Primary`, since it's directly recursive. In general, any recursion (direct, indirect or mutual) will stop the inlining process.

Notice how the second grammar is far shorter, but also much less readable. Suppressing levels like this will make grammars look like regular expressions, with all that entails. Also, the parse tree will only contain `Expr` and `Primary` nodes, so to calculate the result (for example), it will be a bit more difficult.

However, on my tests, `Arithmetic2` is about *twice* faster than the not-inlined `Arithmetic1`. It's then an user's decision to go for inlining or not.

As a future extension, I plan to add an `inlining` option to force the grammar to be inlined. A first step is getting a better grammar introspection, to get the call graph: first inline terminals, recompute the call graph and stop when every rule is self-recursive. The rules's name should be preserved, so as to get a correct parse tree. Of course, that means the user does not decide which rule is inlined or not. As said before, it's then a choice between a complete and nice parse tree and a speedy parse.

Of course, I'll find ways to make **Pegged** generate better parsers (it already parses about 50 to 100 faster than the first commits speed), thus alleviating the need for inlining.

No BNF
------

Another advice would be to use the PEG operators when possible. Do not do BNF-like sequences like this:

```
A <- Elements
Elements <- Element Elements / Element
Element <- ...
```

Use `+` (or `*`, `?`):

```
A <- Element+
Element <- ...
```

Not only will that make the grammar more readable, it'll also make it more efficient, since `+` and co are directly coded as D `while` loops instead of recursive rules.

----------

* * * *
Next lesson: [[Testing Grammars]]

* * * *

[[Pegged Tutorial]]
