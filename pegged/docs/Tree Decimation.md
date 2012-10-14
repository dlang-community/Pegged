Tree Decimation
===============

The Way it Works
----------------

To avoid getting a ginormous parse tree, **Pegged** automatically cuts parts of the generated parse tree. The rule is simple: 'keep only nodes that comes from the grammar rules'. So, given the PEG:

```
Gram:
    A <- B* identifier
    B <- 'b'*
```

Then in a final `Gram` parse tree, there will be only nodes named `Gram.A` or `Gram.B`. `identifier`, as a predefined parser or `'b'` (that is, `literal!("b")`) do not produce nodes in the final output. It's true even for `B*`, a `zeroOrMore!(B)` internally, and which produces a node named `zeroOrMore!(B)` with children named `Gram.B`.

When an external rule is invoked and returns a parse tree, **Pegged** has a look at the name of the returned node. If it's from the grammar rules, the node is kept and the process continues with its children. When the node is deemed external to the grammar, **Pegged** has a look at its children and keeps them only if they are from the grammar rules and so on, recursively. If children nodes are kept in the latter case, they are lifted one level, because their parent node was just skipped.

This process does *not* affect matches. In any case, if a rule consumed part of the input and produced matches, these are kept. Tree decimation is only concerned with the parse tree proper.

I'll try to explain with a diagram. Consider the following parse tree from grammar `Gram`:

```
Gram.A
  |
  +--Gram.B
  |    |
  |    +--Gram.C
  |
  +--identifier
  |
  +--oneOrMore(Gram.D)
       |
       +--Gram.D
       |
       +--Gram.D
```

After decimation, the `Gram.B` branch is kept, the `identifier` node is discarded. When **Pegged** discards `oneOrMore(Gram.D)`, it links the two `Gram.D` nodes to `oneOrMore(Gram.D)`'s parent: `Gram.A`. Which gives us:

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

Keeping Local Nodes / Rules
---------------------------

As described in [[Extended PEG Syntax]], you can use the 'promote/keep' operator, `^` to locally keep a node that would otherwise be discarded. `keep!(T)` inner working is simple: it changes the returned node grammar name to that of the master grammar. The changed node is now a cuckoo and will be skipped by the decimation process.

Discarding Local Nodes / Rules
------------------------------

[[Extended PEG Syntax]] presents another operator, `:` (`discard`), which discards the node from the final output. It's the opposite of `keep`, in that it's used to force the discarding of a node which would otherwise be kept: you want them to parse and consume the input, but *you do not want their captures nor their presence in the final parse tree*. It's useful for begin/end markers, for example. See [[Grammar Examples]] for some grammars using `:`.

A second operator, `;` is used to drop a node, *but* keep its match in the final result. I recently realized I commonly had this need.

The `~` operator, which concatenates the matches also drops the children nodes and acts as a kind of cutting operator. I designed it this way because I realized that everytime I wanted to glue rule matches together, I was only interested in the global match and never needed the child nodes.

* * * *

Next lesson: [[Extended PEG Syntax]]

* * * *

[[Pegged Tutorial]]
