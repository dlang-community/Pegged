Declaring a Grammar
==================

Defining a grammar is quite easy in **Pegged**: just write it in a string, beginning with its name followed by a colon and then the rules (minimum one rule). 

**Note:** previous versions of **Pegged** allowed anonymous grammars. This feature is disabled for now. You *must* give a name to your grammars. I might re-enable this once some bugs concerning parameterized rules and grammars are resolved.

The first rule in a grammar will be the starting rule, the one with which the parsing will begin. For example:

```d
"
Arithmetic:
    Expr     <- Factor AddExpr*
    AddExpr  <- ('+'/'-') Factor
    Factor   <- Primary MulExpr*
    MulExpr  <- ('*'/'/') Primary
    Primary  <- '(' Expr ')' / Number / Variable / '-' Primary

    Number   <- [0-9]+
    Variable <- identifier"
```

This is the grammar seen in the previous lesson, entered in the same way PEGs are defined in Ford's article. To generate the **Pegged** mutually recursive parsers, import `pegged.grammar` and call the `grammar()` function on the string and mix it in your code. By the way, multi-lines rules are OK and you can format your grammar as you wish. **Pegged** will be able to extract the rules from the grammar definition.

```d
import pegged.grammar;

mixin(grammar("
Arithmetic:
    Expr     <- Factor AddExpr*
    AddExpr  <- ('+'/'-') Factor
    Factor   <- Primary MulExpr*
    MulExpr  <- ('*'/'/') Primary
    Primary  <- '(' Expr ')' 
              / Number 
              / Variable 
              / '-' Primary

    Number   <- [0-9]+
    Variable <- identifier
"));
```

As said in the main page, this will create the `Expr`, `AddExpr`, `Factor` parsers and so on. If you know how to write a parsing expression, you know how to write a grammar in **Pegged**. The operators presented previously are there and line comments can be created with the '#' character (once more, this is as was done by Bryan Ford in its article).

```d
import pegged.grammar;

mixin(grammar("
# This grammar recognizes my input file
# Format : 7 columns, the last four being optional

Input:
    line <- col1 col2 col3 colOpt? colOpt? colOpt? colOpt?

# The first three columns contain 
#  - a date in a DD-MM-YYYY format, 
#  - a sample name (an identifier) and 
#  - a batch number (an integer).
# 'identifier' is a pre-defined Pegged parser

    col1  <- date
    col2  <- identifier
    col3  <- digit+
    date  <- digit digit '-' digit digit '-' digit digit digit digit
    digit <- [0-9]

    # the optional 4 columns from 4 to 7 will contain codon sequences for the 4 basic nucleotides (A, C, G, T)
    nucleotide <- 'A' / 'C' / 'G' / 'T'
    colOpt     <- nucleotide+
"));
``` 

Rule Names
----------

You might have seen that in the first example, rules were PascalCased, whereas they are mixed (mainly lowercase) in the `Input` example. There is no particular rule or convention for this. You can use 'lowercase', 'camelCase', 'PascalCase' or 'SHOUTING' as grammar and/or rule names. But, as will be explained in [[Behind the Curtain: How Pegged Works]], **Pegged** grammars are structs and rules are (mostly) methods. So the only rule for naming is: *you shall not use D keywords*.

```d
class <- ...
```

This won't fly and the compiler will shout loudly if you try. FWIW, I use `PascalCase` for my own grammars. [[Predefined Rules]] are all lowercase, as they are functions (following the D convention in this).

* * * *

Next lesson: [[Using a Grammar]]
You can also have a look at the [[Grammar Examples]].

* * * *

[[Pegged Tutorial]]