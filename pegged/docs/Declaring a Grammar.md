Declaring a Grammar
==================

Defining a grammar is quite easy in **Pegged**: just write it in a string:

```d
"Expr    <- Factor AddExpr*
AddExpr  <- ('+'/'-') Factor
Factor   <- Primary MulExpr*
MulExpr  <- ('*'/'/') Primary
Primary  <- '(' Expr ')' / Number / Variable / '-' Primary

Number   <- [0-9]+
Variable <- Identifier"
```

This is the same grammar than seen in the previous lesson. To generate the **Pegged** mutually recursive parsers, import `pegged.grammar` and call the `grammar()` function on the string and mix it in your code. By the way, multi-lines rules are OK:

```d
import pegged.grammar;

mixin(grammar("
Expr     <- Factor AddExpr*
AddExpr  <- ('+'/'-') Factor
Factor   <- Primary MulExpr*
MulExpr  <- ('*'/'/') Primary
Primary  <- '(' Expr ')' 
          / Number 
          / Variable 
          / '-' Primary

Number   <- [0-9]+
Variable <- Identifier
"));
```

As said in the main page, this will create the `Expr`, `AddExpr`, `Factor` parsers and so on. If you know how to write a parsing expression, you know how to write a grammar in **Pegged**. The operators presented previously are there and line comments can be created with the '#' characters (as was done by Bryan Ford in its article).

```d
import pegged.grammar;

mixin(grammar("
 # This grammar recognizes my input file
 # Format : 7 columns, the last four being optional

line <- col1 col2 col3 colOpt? colOpt? colOpt? colOpt?

# The first three columns contain 
#  - a date in a DD-MM-YYYY format, 
#  - a sample name (an identifier) and 
#  - a batch number (an integer).
# 'Identifier' is a pre-defined Pegged parser
col1  <- date
col2  <- Identifier
col3  <- digit+
date  <- digit digit '-' digit digit '-' digit digit digit digit
digit <- [0-9]

# the optional 4 columns from 4 to 7 will contain codon sequences for the 4 basic nucleotides (A, C, G, T)
nucleotide <- 'A' / 'C' / 'G' / 'T'
colOpt <- nucleotide+
"));
``` 

And bang, your grammar is there. See [[Using a Grammar]] to continue. You can also have a look at the [[Grammar Examples]].

Naming Grammars
---------------

In the previous examples, grammars were anonymous, or rather, the only exposed name was the first rule name. You can give a grammar its own name, the syntax is `identifier: (rest of code)` at the beginning:

```d
mixin(grammar(`
Named:
    A <- B*
    B <- 'b'
`));

void main()
{
    Named.parse("bbbbc");
}
```

This will create the `Named` grammar, which is called in a natural way. The rules are internal to the grammar. See [[Four Levels of Encapsulation]] for more on this.

* * * *

Next lesson: [[Using a Grammar]]

* * * *

[[Pegged Tutorial]]