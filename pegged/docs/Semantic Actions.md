Semantic Actions
================

Syntax
------

Semantic actions are introduced with the `{ActionName}` syntax, just behind the expression you want to give the action to. When the expression returns from parsing, `ActionName` is called on the expression `Output`. An action can be any callable, as long as it has a name, accepts an `Output` as argument and returns an `Output`.

```
Rule1 <- Expr1 Expr2 {ActionA} Expr3 {ActionB)
Rule2 <- (Expr1 / Expr2) {ActionA} Expr3
Rule3 <{ActionB} Expr2 / Expr3 (!Expr1 Expr2)*
Rule4 <- (Expr1 {ActionA} Expr2 {ActionB}) {ActionC}
```

The previous example demonstrates some ways an action can be declared: 

* for `Rule1`, `ActionA` is called once `Expr2` finish parsing and then `ActionB` for `Expr3`.

* for `Rule2`, `ActionA` is called once the 'or' expression returns, be it with an `Expr1` or an `Expr2`. `Expr3` has no associated action.

* `Rule3` is an example of rule-level action: once the right-hand side expression returns, `ActionB` is called on its output.

* `Rule4` is an example of nested actions: `ActionA` is called after `Expr1`, `ActionB` after `Expr2` and than `ActionC` on the global sequence output.

Multiple Actions, One Rule
--------------------------

You can put multiple actions on one rule, if you need it:

```
Rule <- Expr {action1} {action2} {action3}
```

**Pegged** provides a shortcut notation:

```
Rule <- Expr {action1, action2, action3}
```


What Can Be Done With Semantic Actions
--------------------------------------

What are actions good for? They offer the user a window during the parsing process. Since they are passed the complete output of an expression, they can modify the output, or construct some value based on the passed argument. Let's demonstrate the two uses:

```d
Output cutChildren(Output o)
{
    o.children == null;
    return o;
}
```

`cutChildren` is an parse-tree-pruning action: it just nullifies the `children` array in the output's parse tree. Put it after expressions where you don't care for children expressions and just want to keep the captures.

This one keeps only the first capture in a rule:

```d
Output first(Output o)
{
    if (o.capture.length > 1)
        o.capture.length = 1;
    return o;
}
```

Now, let's use actions to validate some XML nodes. First, a grammar:

```d
mixin(grammar(`
    # Simple XML nodes:
    Node       <- OpeningTag (Node / Text)* ClosingTag
    OpeningTag <- :"<" Identifier :">" 
    Closingtag <- :"</" Identifier :">"
    Text       <~ (!OpeningTag !ClosingTag .)*  # Any char, as long as it's not a tag
`));
```

Suppose we want to validate the tags: any opening tag *must* be closed by an equivalent closing tag. We will use actions to push the tag identifier on a stack, which will be popped by closing tags.

```d
import std.array;
string[] nameStack;

Output opening(Output o)
{
    nameStack ~= o.capture[0];
    return o;
}

Output closing(Output o)
{
    if (nameStack.back != o.capture[0])
        o.success = false;
    else
        nameStack.popBack;
    return o;
}
```

And, adding action to the XML grammar:

```d
mixin(grammar(`
    Node       <- OpeningTag{opening} (Node / Text)* ClosingTag{closing}
    OpeningTag <- :"<" Identifier :">" 
    Closingtag <- :"</" Identifier :">"
    Text       <~ (!OpeningTag !ClosingTag .)*  # Any char, as long as it's not a tag
`));
```

Now, let's test it:

```d
assert( Node.parse("<a> Hello <b> World </b> ! </a>").success);
assert(!Node.parse("<a> Hello <b> World </c> ! </a>").success); // <b> closed by a </c>
assert(!Node.parse("<a> Hello <b> World </a> ! </b>").success); // <a> and <b> incorrectly nested
```

As you can see, correctly nested nodes get parsed, but not incorrectly closed and nested nodes. This means actions do validation while parsing and, if the parsing is successful, you can be sure the input is a correctly nested collection of nodes and that the parse tree is also correct for any following function to act upon.


Expression-Level or Rule-Level Actions?
---------------------------------------

There is no real difference between

```
Rule1 <- Expr1 {Action}
```

and 

```
Rule1 <{Action} Expr1
```

But there *is* a difference between

```
Rule1 <-  A {ActionA} B {ActionB}
Rule2 <-  B A
A <- ...
B <- ...
```

and

```
Rule1 <-  A B
Rule2 <-  B A
A <{ActionA} ...
B <{ActionB} ...
```

The latter means that `ActionA` gets called *every time* `A` is used, in `Rule1` as well as in `Rule2`. Whereas for the former grammar the actions are activated only for `Rule1`. Just decide what you want for your grammar.



Possible Extensions
-------------------

I'm playing with the following ideas concerning actions:

* Permitting other arguments, like this: `{dropChild(1)}`, which would call `dropChild(ruleOutput, 1)`. Heck, in DMD 2.059 with UFCS enabled, it's just `ruleOutput.dropChild(1)` which is *quite* easy to code.

* Defining some standard actions. The drop (`:`) and fuse (`~`) operators should be accessible as actions and also other basic tree operations.

* For now, actions are what I'd call *internal* actions: they act on the parse tree and any external action is a side-effect (assigning to external variables, for example). I could introduce 'external actions', for example with `<ActionName>` . These would return any D type and plug into one another during parsing.  This would allow **Pegged** to have the same run-of-the-mill example of calculus on arithmetic expressions. We'll se...

* * * *

Next lesson [[Generating Code]]

* * * *

[[Pegged Tutorial]]