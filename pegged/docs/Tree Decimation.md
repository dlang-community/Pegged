Tree Decimation
===============

The Way it Works
----------------

To avoid getting a ginormous parse tree, **Pegged** automatically cuts parts of the generated parse tree. The rule is simple: 'keep only nodes that are from the grammar'. So, given the PEG:

```
Gram:
    A <- B* Identifier
    B <- 'b'*
```

Then in a final `Gram` parse tree, there will be only nodes named `Gram.A` or `Gram.B`. `Identifier`, as a predefined parser (its full name is `Pegged.Identifier`) or `'b'` (that is, `Pegged.Lit(b)`) do not produce nodes in the final output. It's true even for `B*`, a `ZeroOrMore!(B)` internally, and which produces tree nodes named `Pegged.ZeroOrMore(B)` with children named `Gram.B`.

When an external rule is invoked and returns a parse tree, **Pegged** has a look at the grammar name of the returned node. If it's the current grammar name, the node is kept, along with its children. When the node is deemed external to the grammar, **Pegged** has a look at its children and keeps them only if they are from the grammar rules and so on, recursively. If children nodes are kept, they are lifted one level.

This process does *not* affect captures. In any case, if a rule consumed part of the input and produced captures, these are kept. Tree decimation is only concerned with the parse tree proper.

I'll try to explain with a diagram. Consider the following parse tree from grammar `Gram`:

```
Gram.A
  |
  +--Gram.B
  |    |
  |    +--Gram.C
  |
  +--Pegged.Identifier
  |    
  +--Pegged.OneOrMore(Gram.D)
       |
       +--Gram.D
       |
       +--Gram.D
```               

After decimation, the `Gram.B` branch is kept, the `Identifier` node is discarded. When **Pegged** discards `OneOrMore(Gram.D)`, it links the two `Gram.D` nodes to `OneOrMore(Gram.D)`'s parent: `Gram.A`. Which gives us:

```
Gram.A
  |
  +--Gram.B
  |    |
  |    +--Gram.C
  |
  +--Gram.D
  |
  +--Gram.D
```               

The Five Parsing Levels
-----------------------

Or course, sometimes you do not want tree decimation. In that case, you can use the `fullParse` method, which disables it. See [[Parsing Levels]] for more info on `fullParse` and its brethren.

Keeping Local Nodes / Rules
---------------------------

As described in [[Extended PEG Syntax]], you can use the 'promote/keep' operator, `^` to locally keep a node that would otherwise be discarded. `Keep!(T)` inner working is simple: it changes the returned node grammar name to that of the master grammar. The changed node is now a cuckoo and will be skipped by the decimation process.

Discarding Local Nodes / Rules
------------------------------

[[Extended PEG Syntax]] presents another operator, `:` (`Drop`), which drops the node from the final output. It's the opposite of `Keep`, in that it's used to force discarding of a node which would otherwise be kept: you want them to parse and consume the input, but you do not want their captures nor their presence in the final parse tree. It's useful for begin/end markers, for example. See [[Grammar Examples]] for some grammars using `:`.

The `Fuse` operators, `~`, which concatenates the matches also drops the children nodes and acts as a kind of cutting operator. I designed it this way because I realized that everytime I wanted to glue a rule matches, I was only interested in the global match and never needed the child nodes.

Both `:` and `~` are disabled by the fifth parsing level `fullestParse`. See [[Parsing Levels]].

* * * *

Next lesson: [[Extended PEG Syntax]]

* * * *

[[Pegged Tutorial]]