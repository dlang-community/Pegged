Semantic Actions
================

Syntax
------

Semantic actions are introduced with the `{ActionName}` syntax, just behind the expression you want to give the action to. When the expression returns from parsing, `ActionName` is called on the expression parse tree. An action can be any callable, as long as it has a name, accepts a `ParseTree` as argument and returns a `ParseTree`.

```
Actions:
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


What Can Be Done With Semantic Actions
--------------------------------------

What are actions good for? They offer the user an opportunity to act on the parse tree *during* the parsing process. Since they are passed the complete output of an expression, they can modify the output, or construct some value based on the passed argument. Let's demonstrate the two uses:

```d
PT cutChildren(PT)(PT p)
{
    p.children == null;
    return p;
} 
```

`cutChildren` is a parse-tree-pruning action: it just nullifies the `children` array in the parse tree. Put it after expressions where you don't care for children expressions and just want to keep the captures.

Note that `cutChildren` is a function template. It's because it must be able to accept any kind of parse tree. (**TODO**: more explanations. For now, just make your semantic actions templates and everything will be alright).


Now, this action keeps only the first match of a rule:

```d
PT first(PT)(PT p)
{
    if (p.matches.length > 1)
        p.matches.length = 1;
    return p;
}
```

Now, let's use actions to validate some XML nodes. First, a grammar:

```d
mixin(grammar(`
XML:
    Node       <- OpeningTag (Node / Text)* ClosingTag
    OpeningTag <- "<"  identifier ">" 
    Closingtag <- "</" identifier ">"
    Text       <~ (!OpeningTag !ClosingTag .)*  # Any char, as long as it's not a tag
`));
```

Suppose we want to validate the tags: any opening tag *must* be closed by an equivalent closing tag. We will use actions to push the tag identifier on a stack, which will be popped by closing tags.

```d
import std.array;
string[] nameStack;

PT opening(PT)(PT p)
{
    nameStack ~= p.matches[0];
    return p;
}

PT closing(PT)(PT p)
{
    if (nameStack.back != p.matches[0])
        p.success = false; // Make the rule fail
    else
        nameStack.popBack;
    return p;
}
```

And, adding actions to the XML grammar:

```d
mixin(grammar(`
XML:
    Node       <- OpeningTag{opening} (Node / Text)* ClosingTag{closing}
    OpeningTag <- "<"  identifier ">" 
    Closingtag <- "</" identifier ">"
    Text       <~ (!OpeningTag !ClosingTag .)*  # Any char, as long as it's not a tag
`));
```

Now, let's test it:

```d
assert( XML("<a> Hello <b> World </b> ! </a>").success);
assert(!XML("<a> Hello <b> World </c> ! </a>").success); // <b> closed by a </c>
assert(!XML("<a> Hello <b> World </a> ! </b>").success); // <a> and <b> incorrectly nested
```

As you can see, correctly nested nodes get parsed, but not incorrectly closed and nested nodes. This means actions do validation *while parsing* and, if the parsing is successful, you can be sure the input is a correctly nested collection of nodes and that the parse tree is also correct for any following function to act upon.


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

The latter means that `ActionA` gets called *every time* `A` is used, in `Rule1` as well as in `Rule2`. Whereas for the former grammar, the actions are activated only for `Rule1`. Just decide what you want for your grammar.

Success and Failure
-------------------

Actions are called when a rule finishes parsing, successfully or not. Note that sequences stop once a rule fail. Like this:

```
Rule1 <- A {foo} B {bar} C {baz}
```

If `A` succeeds, `foo` is called on its result. If `B` then fail, `bar` is still called on its resulting parse tree. Unless `bar` changes `B.success` to `true` (a legal, but dangerous move), `B {bar}` will fail and `C {baz}` is not tested by **Pegged**.

A previous version of **Pegged** called actions only on successful parses. This forbade 'recovery rules', that'd accept a failed parse attempt and correct it to make it OK. So I'm testing letting rules be called after any parse result.

Side-Effects
------------

A consequence of the previous section is that side-effects (like storing a value in a variable) can be activated inside a failed rule. In the previous example, `foo` was called. If `foo` acts like `opening`, then something somewhere was changed. Even though `Rule1` failed, some partial modification was made. Maybe a cleaner solution would be to test each rule in a sandbox and undo any changes that were made by a failed rule. For actions acting on a grammar inner fields (something I plan to add), it's feasible: the current state can be duplicated, passed to a rule as a context variable and returned with the parse tree. If the rule fails, then the returned context is discarded. When the rule succeeds, the caller context is assigned the returned context.

Of course, all this copying and passing around'll probably slow the parsing process down and I cannot protect against any global variable modification.

Multiple Actions
----------------

You can have as many actions as you wish between the curly braces (min. 1, `{}` is not a valid action). Separate the actions names by a comma:

```
Rule1 <- A {foo, bar, baz} B {foo, baz}
```

Once `A` returns, `foo` is called on its result, then `bar` on `foo`'s result and then `baz`. For `B`, it'll be `foo` and then `baz`.


Predefined Actions
------------------

For now, **Pegged** has no predefined actions. Here is the minimum list I want to add:

* discardChildren (nullifies the `children` field of a parse tree)
* discardMatches (nullifies the `matches` field of a parse tree)
* fuseMatches (concatenates a parse tree's matches together)
* failure (parsetree.success = false)
* success (parsetree.success = true)
* discard (discards anything the parse tree achieved: reset `.end` to be equal to `.begin`, discards the matches and the children)

Possible Extensions
-------------------

I'm playing with the following ideas concerning actions:

* Permitting other arguments, like this: `{dropChild(1)}`, which would call `dropChild(ruleOutput, 1)`. Heck, in DMD with UFCS, it's just `ruleOutput.dropChild(1)` which is *quite* easy to code.

* For now, actions are what I'd call *internal* actions: they act on the parse tree and any external action is a side-effect (assigning to external variables, for example). I could introduce 'external actions', for example with `<ActionName>` . These would return any D type and plug into one another during parsing.  This would allow **Pegged** to have the same run-of-the-mill example of calculus on arithmetic expressions as other parser generators. We'll see...

* * * *

Next lesson [[Generating Code]]

* * * *

[[Pegged Tutorial]]