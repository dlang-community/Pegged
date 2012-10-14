Grammars a D Modules
====================

Before being mixed-in, a grammar output by `grammar` is just a string. If you do not want to generate your grammar anew each time your module is compiled, save the grammar code as a D module and import it. I found I needed this frequently enough to put the functionality in **Pegged**:

```d
asModule("arithmetic","
Arithmetic:
    Expr     <- Factor AddExpr*
    AddExpr  <- ('+'/'-') Factor
    Factor   <- Primary MulExpr*
    MulExpr  <- ('*'/'/') Primary
    Primary  <- Parens / Number / Variable / '-' Primary

    Parens   <- '(' Expr ')'
    Number   <~ [0-9]+
    Variable <- identifier
");
```

This will create the `arithmetic.d` file, with the necessary infrastructure (not much in fact, just an `import pegged.peg;` expression at the beginning).

Once you're done, you can use your grammar:

```d
import arithmetic;

enum input = "2/(8*7988+1*6196-y)";
enum parseTree = Arithmetic(input);
```

That should speed up the compiling, since the grammar is already generated.

Separating Module Name and File Name
------------------------------------

If you want the generated module to be put in a specific directory, there is an `asModule` overload for you:

```d
asModule("moduleName", "my/project/hierarchy/fileName", myGrammar);
```

For example **Pegged**'s own parser, the one used by `grammar` to parse the grammar-as-a-string the user provides, is generated like this:

```d
import pegged.examples.peggedgrammar; // contains Pegged grammar as a string, name PEGGEDgrammar
asModule("pegged.parser", "pegged/parser", PEGGEDgrammar);
```

* * * *

Next Lesson: [[Grammar Composition]].

* * * *

[[Pegged Tutorial]]

