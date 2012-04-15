The Four Levels of Encapsulation
================================

OK, this is *not* implemented right now, because I played with it in a branch and found it was not such a good idea.

This article will most probably evolve in the near future.

Here is the original idea:

I want to let a grammar define encapsulation (for want of a better word). Let me explain:

- Level 0 is 'freeform': a grammar is an anonymous list of rules dropped where they are mixed in. The rules do not know one another. There is no parse tree simplification.
- Level 1 is 'open': a grammar is anonymous, but the rules know one another: they treat sister rules as important enough to be kept in a parse tree. Parse trees coming from rules outside the grammar can be cut, only their captures are interesting and kept. This is what **Pegged** does right now. I should probably explain that in more detail somewhere. External rules can refer to the grammar rules by their names.
- Level 2 is 'closed': a grammar is a parser in its own right, the rules are defined inside this parser. External rules calls from inside the grammar are cut (like level 1). External rules can only refer to the grammar rules by their qualified names: `grammar.rule`. Even though you can do that, it's slightly discouraged: the grammar is in effect more closed than the previous level.
- Level 3 is 'sealed': the rules are a private part of the grammar. External rules *cannot* access the inner rules. Only the grammar itself can be used for a `.parse()` call.

For levels 2 and 3, a grammar must have a name.

Possible syntax:

```d
mixin(grammar("
  MyGrammar1(closed)
    Rule1 <- ...
    Rule2 <- ...
"));

mixin(grammar("
  MyGrammar2(sealed)
    Rule3 <- ...
    Rule4 <- ...
"));

enum p1 = MyGrammar1.parse(input); // OK, equivalent to the following:
enum p2 = MyGrammar1.Rule1.parse(input); // OK
enum p3 = Rule1.parse(input); // NOK!

enum p4 = MyGrammar2.parse(input); // OK, the only call possible for a sealed grammar
enum p5 = MyGrammar2.Rule3.parse(input); // NOK!
```

What is Really Implemented
--------------------------

After playing for a while with the previous idea, I found out it was not so handy on real grammars. What I implemented is named grammars. A grammar can have a name, which is an identifier on the first line, followed by a colon:

```
MyGramm:
A <- Some Rule
B <- Another Rule
```

The rules are then enclosed in a `MyGramm` class, which inherits from `Parser`. As such, it's a Parser also and can be called by other grammars. Inner rules can be called with `MyGramm.A`. It's very near what is called a 'closed' grammar further up the page.

When parsing on `MyGramm`, the first inner rule is considered the root rule (as is usually the case in PEGs) and called on the input. So `MyGramm.parse(input)` is equivalent to `MyGramm.A.parse(input)`.

When a grammar is anonymous, the enclosing class is *still* created, but its name is the root rule name. So all the grammars presented in this tutorial (which are all anonymous) can still be used unchanged (whew!).

The only change that entails, is that for:

```
A <- Some Rule
B <- Another Rule
```

Calling `B` directly is not possible anymore, you *must* use `A.B.parse(input)`.

Cutting Nodes
-------------

Also, as presented in the first section, nodes coming from 'external' rules (the ones not defined in the grammar) are cut from the final parse tree. See [[Extended PEG syntax]] for the `^` operator disabling node's cutting.

See also [[Tree Decimation]].

* * * *

Next lesson: [[Behind the Curtain: How Pegged Works]]

* * * *

[[Pegged Tutorial]]