Behind the Curtain: How Pegged Works
====================================

Plumbing
--------

Basically, **Pegged** constructs [expression templates](http://www10.informatik.uni-erlangen.de/~pflaum/pflaum/ProSeminar/exprtmpl.html) to associate parsers together according to PEG rules. That is to say, `a b / c` is rewritten as `Or!(Seq!(a,b), c)`. By the way, you _can_ assemble **Pegged**  parsers in this way, if you want to. Just import `pegged.peg` (where they are defined) and play. No need for the `pegged.grammar` module. This is what most C++ PEG library do: they give you a  collection of parsers which you can link together to construct complicated expressions. Just for fun, I did the same for types once, see [this module](http://svn.dsource.org/projects/dranges/trunk/dranges/docs/typepattern.html)). 

The power is the same than for **Pegged**, it's just that expressions rapidly become unreadable:

```d
(a b c / (d !e f))+
```

compared to:

```d
OneOrMore!( Or!( Seq!(a,b,c), Seq!(d, NegLookAhead!(e), f)))
```

And still, due to D nice template syntax and tuple template parameters, I can assure you it's _far_ nicer than the equivalent C++ code. But you can understand how it may be difficult for someone else to 'parse' (pun intended) your code afterwards. Have a look at `pegged.peg`, where pre-defined combinations are. 

Anyway, back to **Pegged** proper.

Parser
------

All **Pegged** predefined parsers or parser combinators defined in the `pegged.peg` module are classes that inherit from the `Parser` class. See [[Predefined Parsers]]. To make a class a **Pegged** parser, derive it from `Parser` and use a static `.parse` method. See [[User-Defined Parsers]] for a more in-depth look at this.

A grammar rule is a class that inherits from the parsing expression on the arrow's right. So `"Rule <- A B / C"` becomes:

```d
class Rule : Or!(Seq!(A,B), C) { ... }
```

This is where it becomes interesting. Why did I chose classes as the basic building block in **Pegged**: to allow recursive rules. With structs I can do:

```d
alias  Or!(Seq!(A,B), C) Rule;
```

But not:

```d
// "Parens <- '(' Parens ')' / Identifier"
alias Or!(Seq!(Lit!("("), Parens, Lit!(")")), Identifier) Parens; // bzzzt!
```

Because in the alias expression, `Parens` is not yet defined and the compiler rightfully complains. But with classes, we can use the [curiously recursive template pattern](http://en.wikipedia.org/wiki/Curiously_recurring_template_pattern), which was seen in C++ but works perfectly in D:

```d
class Parens : Or!(Seq!(Lit!("("), Parens, Lit!(")")), Identifier)
{ ... } //                         ^^^^^^!
```

Yes that works, because when the inherited expression is parsed, `Parens` already exists as a class name. (*shameless plug: I wrote a big tutorial on templates, it's another Github project of mine you can find [here](https://github.com/PhilippeSigaud/D-templates-tutorial), just select the pdf.*)

This means self-recursive parsers are OK (like the one before), or mutually recursive parsers:

```d
/*
Expr     <- Factor AddExpr*
AddExpr  <- ('+'/'-') Factor
Factor   <- Primary MulExpr*
MulExpr  <- ('*'/'/') Primary
Primary  <- Parens / Number / Variable / '-' Primary"
*/
class Expr : Seq!(Factor, ZeroOrMore!(AddExpr)) { ... }
class AddExpr : Seq!(Or!(Lit!("+"), Lit!("-")), Factor) { ... }
```

From this basis, it's easy to define the PEG terminals and operators.

Inside a Parser
---------------

After inheriting from `Parser`, the only real need is to define a static `.parse` method. `parse` takes an `Input` and returns an `Output` (this may change, I'm playing with the idea of a unique `State` struct. These types are structs defined in `pegged.peg`:

```d
class Rule : Or!(Seq!(A,B), C)
{
    static Output parse(Input input)
    {
     ...
    }
}
```

An `Input` brings everything a parser could need: an input string, and already named captures, if the parser need them. In a future evolution of **Pegged**, it might contain the current error stack also, for the parser to modify.

```d
struct Input
{
    dstring text;
    NamedCaptures namedCaptures;
    alias text this; // automatic 'conversion' into text, to simplify some expressions
    // + toString()
}
```

The `NamedCaptures` type is just an alias for an associative list of names (strings) and parse trees. You can retrieve a named capture with a `.namedCaptures["name"]` call. I plan to make `NamedCaptures` a stack also (along with being a set and an associative-array-like structure), to allow pushing/popping named captures.

`Output` is build along the same lines. It groups together the unconsumed rest of input (`next`), the named captures and the resulting parse tree. Parse trees are presented on their own page: [[Parse Trees]].

```d
struct Output
{
    dstring next;
    NamedCaptures namedCaptures;
    
    ParseResult parseTree;
    alias parseTree this; // automatic 'conversion' into parseTree, to simplify some expressions
    // + toString()
}
```

As of this writing, you'll notice `Input` and `Output` are not templated on the input type. They automatically take a `string`. That may change to allow the parsing of `wstring`s or `dstrings` (or I might use `dstring` internally, I do not know yet) or even ranges.

To define your own parsers, see [[User-Defined Parsers]]. For the previous classes that just inherit from an expression, they just delegate the parsing to their super class, appropriate its work by putting hteir name on it and pushing the parse tree as their child, and return:

```d
static Output parse(Input input)
{
    auto p = typeof(super).parse(input);
    p.parseTree.name = "AddExpr"; // or whatever its name is
    p.parseTree.children = [p];
    return p;
}
```

There are predefined mixins for this kind of thing in `pegged.peg`.

Bootstrapping
-------------

The only missing part is how to convert a *grammar-as-a-string* into the previous expression templates. Initially, it was done 'manually', so to speak: a specialized function took the input string and parsed it (with *many* bugs) to produce D code. So, given `"A <- B C / D"`, it recognized the `<-` as a rule definition marker, tokenized `B C / D` into `["B", "C", "OR", "D"]` and transformed it into `"Or!(Seq!(B, C), D)"`. The final string, `"class A : Or!(Seq!(B,C),D) { static Output parse(Input) { ... }}"` could be mixed in user code, as is the case right now.

Since PEG come with their own PEG-defined grammar, bootstrapping was easy: I just ate my own dogfood and followed the process described in [[Generating Code]]

1) I encoded the PEG grammar as a string

2) I generated the equivalent D code and dumped it in a module (see [[Grammars as D Modules]])

3) I added a semantic action (though they were not included in **Pegged** at this time, but the idea is the same) that took the new grammar `Output` and generated D code.

4) small wrappers to simplify and user code, and voilà, a complete, extensible, easy to understand PEG grammar.

The manual parser is still there, for my shame: it's `pegged.utils.manual`. The bootstrapping process was done in two steps: first with the standard PEG grammar and then with the [[Extended PEG Syntax]] **Pegged** uses.

When I add new functionalities in the **Pegged** grammar, I change the grammar in `pegged.examples.PEGGED`, parse it with **Pegged** and dump the resulting *grammar-to-make-grammars* in a new file, adding the functions from `pegged.development.grammarfunctions`. That newly-minted module then becomes the new `pegged.grammar`.

Yes, most of `pegged.grammar` was written by the D compiler, not by a human being. That's bootstrapping for you.

That means you can do so also: Say you do not like the PEG way to denote rules: `ruleName <- Expression` and want an EBNF-like syntax: `ruleName ::= Expression`, just change the corresponding line in `pegged.examples.PEGGED`:

```d
Definition <- RuleName Arrow Expression
Arrow      <- LEFTARROW / FUSEARROW / DROPARROW / ACTIONARROW
    LEFTARROW  <- "<-" S
    FUSEARROW  <- "<:" S
    DROPARROW  <- "<:" S
    ACTIONARROW <- "<">WithAction
```

into:

```d
Definition <- RuleName DefSymb Expression
DefSymb <- "::="
```

And presto, you have your own *grammar-to-make-grammars* to play with.

* * * *

Next Lesson : [[Optimizations]]

* * * *

[[Pegged Tutorial]]

