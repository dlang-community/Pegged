Optimizations
=============

Big subject...


I won't write here about **Pegged** internal optimizations since, as of this writing, there aren't many of them. **Pegged** simplify sequences and choices when they have one element or are sequences of sequences or choices or choices. That's about it.

The thematic of this page is more 'how can I make my grammar faster?'.


First, if you're satisfied with the way a grammar is, you can compile it and drop the resulting code into a file, to make it an independent module. Use `asModule(string moduleName, string grammar)` for that. That'll create a D module that imports `pegged.peg` and contains your grammar. I might add an `asAutonomousModule` function that'd also write the necessary **Pegged** infrastructure in the file, thus making it totally autonomous (you wouldn't need **Pegged** to use it elsewhere).

Once written in a file, the grammar-compiling step can be avoided. It won't make the grammar any faster but does prevent recompiling many times something that is stable.

Second, note there is a tension between a very complete grammar (and hence, parse tree) and a fast one: parsing speed depends (at least) on the grammar depth, its number of rules. So inlining could maybe be used to accelerate somewhat the parsing:

From:
```d
mixin(grammar(" 
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
    Expr     <- (Primary (('*'/'/') Primary)*) (('+'/'-') Primary (('*'/'/') Primary)*)*
    Primary  <- '(' Expr ')' / [0-9]+ / Identifier / '-' Primary
"));
```

I most probably botched the manual inlining, but you get the idea. Note the process stopped at `Primary`, since its recursive. In general, any recursion (direct, indirect or mutual) will stop the inlining process.

Notice how the second grammar is far shorter, but also much less readable. Suppressing levels like this will make grammars look like regular expressions, with all that entails. Also, the parse tree will only contain `Expr` and `Primary` nodes, so to calculate the result (for example), it will be a bit more difficult.


Another advice would be to use the PEG operators when possible. Do not do BNF-like sequences like this:

```
A <- Elements
Elements <- Element Elements / Element
Element <- ...
```

Use `+` (or `*`, `?`):

```
A <- Element+
Element <-
```

Not only will that make the grammar more readable, it'll also make it more efficient, since `+` and co are directly coded as D `while` loops instead of recursive rules.



----------

[[Pegged Tutorial]]

[[Home]]