Declaring a Grammar
==================

Defining a grammar is quite easy in **Pegged**: just write it in a string, beginning with its name followed by a colon and then the rules (minimum one rule). The first rule in a grammar will be the starting rule, the one with which the parsing will begin. For example:

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

And bang, your grammar is there. See [[Using a Grammar]] to continue. You can also have a look at the [[Grammar Examples]].

* * * *

Next lesson: [[Using a Grammar]]

* * * *

[[Pegged Tutorial]]