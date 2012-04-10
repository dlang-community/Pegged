Grammars a D Modules
====================

Before being mixed-in, grammars output by `grammar` are just a string. If you do not want to generate your grammar anew each time your module is compiled, save the grammar code as a D module and import it. I found I needed this frequently enough to put the functionality in **Pegged**:

```d
asModule("arithmetic","
    Expr     <- Factor AddExpr*
    AddExpr  <- ('+'/'-') Factor
    Factor   <- Primary MulExpr*
    MulExpr  <- ('*'/'/') Primary
    Primary  <- Parens / Number / Variable / '-' Primary

    Parens   <- '(' Expr ')'
    Number   <~ [0-9]+
    Variable <- Identifier
");
```

This will create the `arithmetic.d` file, with the necessary infrastructure (not much in fact, just an `import pegged.peg;` expression at the beginning).

Once you're done, you can use your grammar:

```d
import arithmetic;

enum input = "2/(8*7988+1*6196-y)";
enum parseTree = Expr.parse(input);
```

That should speed up the compiling, since the grammar is already generated.


Now, let's see [[Using the Parse Tree]].


* * * *

[[Pegged Tutorial]]
          