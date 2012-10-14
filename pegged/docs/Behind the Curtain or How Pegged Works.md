Behind the Curtain: How Pegged Works
====================================

Plumbing
--------

Basically, **Pegged** constructs [expression templates](http://www10.informatik.uni-erlangen.de/~pflaum/pflaum/ProSeminar/exprtmpl.html) to associate parsers together according to PEG rules. Every PEG terminal or operator is a function or a template, in standard D code, the global code for a rule being crafted by the `grammar` function in `pegged.grammar`.

For your information, here is a table to associate the PEG syntax and the D building blocks:

```
|---------------------------------------------------|
|     PEG                  |     PEGGED             |
|---------------------------------------------------|
|                Terminals                          |
|---------------------------------------------------|
| "abc" (literal)          | literal!"abc"          |
| 'abc' (literal)          | literal!"abc"          |
| `a-z` (char range)       | charRange!('a','z')    |
| eps (epsilon)            | eps                    |
| . (any char)             | any                    |
|---------------------------------------------------|
|                Operators                          |
|---------------------------------------------------|
| e* (zero or more e's)    | zeroOrMore!(e)         |
| e+ (one or more e's)     | oneOrMore!(e)          |
| e? (optional e)          | option!(e)             |
| &e (positive lookahead)  | posLookahead!(e)       |
| !e (negative lookahead)  | negLookahead!(e)       |
| a b(sequence, and)       | and!(a, b)             |
| a b c ...(sequence, and) | and!(a, b, c, ...)     |
| a / b (alternation, or)  | or!(a, b,)             |
| a / b / c / ...          | or!(a, b, c, ...)      |
| (a) (grouping)           | no equivalent          |
|---------------------------------------------------|
|                Misc.                              |
|---------------------------------------------------|
| naming a rule            | named!(expr, "name")   |
| :e (discarding a node)   | discard!(e)            |
| ^e (keeping a node)      | keep!(e)               |
| ;e (dropping a node)     | drop!(e)               |
| ~e (fusing matches)      | fuse!(e)               |
| e {act} (semantic action)| action!(e, act)        |
| space-discarding and     | spaceAnd!(a,b,c,...)   |
|---------------------------------------------------|
```

For example, given rules `a`, `b` and `c`, a sequence `a b c` in a grammar is converted by **Pegged** into `and!(a, b, c)`. In the same way, `"abc" / d*` becomes `or!(literal!"abc", zeroOrMore!d)`, with `d` being another rule. By the way, you *can* assemble **Pegged** parsers in this way, if you want to. Just import `pegged.peg` (where they are defined) and play, no need for the `pegged.grammar` module. This is what most C++ PEG libraries do: they give you a collection of parsers which you can link together to construct complicated expressions. Just for fun, I did the same for types once, see [this module](http://svn.dsource.org/projects/dranges/trunk/dranges/docs/typepattern.html)).

The power is the same as for **Pegged**, but expressions rapidly become unreadable:

```d
(a b c / (d !e f))+
```

compared to:

```d
oneOrMore!( or!( and!(a,b,c), and!(d, negLookAhead!(e), f)))
```

And still, due to D nice template syntax and tuple template parameters, I can assure you it's *far* nicer than the equivalent C++ code. But you can understand how it may be difficult for someone else to 'parse' (pun intended) your code afterwards. Have a look at `pegged.peg`, where pre-defined combinations are.

Anyway, back to **Pegged** proper.

Parsers
-------

All **Pegged** predefined parsers or parser combinators defined in the `pegged.peg` module are functions accepting a `ParseTree` or structs with a static `opCall`operator. See [[Predefined Parsers]]. To make your own parsers, see [[User-Defined Parsers]].

A grammar rule is a struct or a function that returns the code for the parsing expression on the arrow's right. So `"Rule <- A B / C"` becomes:

```d
ParseTree Rule(ParseTree p)
{
    return or!(and!(A,B), C)(p);
}
```

This new parser can be called by grammars without trouble, as long as `A`, `B`and `C` are themselves parsers visible in the local scope. If you intend to call the parser directly, you should also define a `string`-accepting overload:

```d
ParseTree Rule(string s)
{
    return or!(and!(A,B), C)(ParseTree(``,false,[], s));
}
```

Grammars
--------

Grammars are just structs whose methods are rules. A global `opCall`operator allows the grammar to be called like a rule and defers to the start rule (the first rule in a grammar definition). Grammars also add some machinery to accelerate the parsing and present a nice parse tree:

* The list of rules' names is stored and a `decimateTree` method is added to discard nodes external to the grammar. See [[Tree Decimation]].
* A special rule called `Spacing` is added if the user does not define one, to be called by `spaceAnd`, the space-dscarding version of `and`. See 'User-Defined Spacing' in [[Extended PEG Syntax]].
* If activated, memoizing stores all rules' results at all called parsing points. If a given rule was already called at a given index, then the result stored is used instead of parsing anew.

As a future extension to **Pegged**, I'll probably add some grammar introspection: giving access to the list of rules, the call graph, some metadata on each rule (recursive or not, ...).

Bootstrapping
-------------

The only missing part is how to convert a *grammar-as-a-string* into the previous expression templates. Initially, it was done 'manually', so to speak: a specialized function took the input string and parsed it (with *many* bugs) to produce D code. So, given `"A <- B C / D"`, it recognized the `<-` as a rule definition marker, tokenized `B C / D` into `["B", "C", "OR", "D"]` and transformed it into `"or!(and!(B, C), D)"`. The final string, `"ParseTree A(...) { return or!(and!(B,C),D)(p);}"` could be mixed in user code, as is the case right now.

Since PEGs come with their own PEG-defined grammar, bootstrapping was easy: I just ate my own dogfood and followed the process described in [[Generating Code]]

1) I encoded the PEG grammar as a string

2) I generated the equivalent D code and dumped it in a module (see [[Grammars as D Modules]])

3) I added a semantic action (though they were not included in **Pegged** at this time, but the idea is the same) that took the new grammar `ParseTree` and generated D code.

4) small wrappers to simplify and user code, and voilà, a complete, extensible, easy to understand PEG grammar.

Nowadays, when I add new functionalities in the **Pegged** grammar, I change the grammar in `pegged.examples.peggedgrammar`, parse it with **Pegged** and dump the resulting *grammar-to-make-grammars* in `pegged/parser.d`. That newly-minted module is then called by `pegged/grammar.d`. The module to regenerate the parser is `regenerate.d` and is mostly a one-liner.

Yes, most of `pegged/parser.d` was written by the D compiler, not by a human being. That's bootstrapping for you.

That means you can do so also: Say you do not like the PEG way to denote rules: `ruleName <- Expression` and want an EBNF-like syntax: `ruleName ::= Expression`, just change the corresponding line in `pegged.examples.PEGGED`:

```
Definition   <- LhsName Arrow Expression
Arrow        <- LEFTARROW / FUSEARROW / DISCARDARROW / SPACEARROW
LEFTARROW    <- '<-' Spacing
FUSEARROW    <- '<~' Spacing
DISCARDARROW <- '<:' Spacing
SPACEARROW   <- '<' Spacing
```

into:

```
Definition <- LhsName DefSymb Expression
DefSymb    <- "::="
```

And presto, you have your own *grammar-to-make-grammars* to play with. This means you can train **Pegged** into recognizing BNF, EBNF or other grammar formalisms (probably not two-levels grammars, though).

The Parsing Engine
------------------

Right now, what `pegged/grammar.d` generates is a standard top-down, recursive-descent parser with memoization and no backtracking, since it's the most natural parser for PEGs. Changing the parser generator would allow **Pegged** to accept an LL(1) or LALR(1) grammar, for example, and generate the corresponding parser. On my TODO list, there is trying GLR and LALR(1). Of course, in that case, the PEG formalism is lost, as PEG and LALR(1) grammars are not the same. **Pegged** would then become a more general parser generator and not a PEG parser generator. We will see.

* * * *

Next Lesson : [[Optimizations]]

* * * *

[[Pegged Tutorial]]
