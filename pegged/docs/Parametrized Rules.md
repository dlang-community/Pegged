Parametrized Rules
==================

**Pegged** piggybacks on D nice template syntax to get parametrized rules: rules that take other parsing expressions as arguments. The syntax is quite simple:

```
List(Elem, Sep) <  Elem (Sep Elem)*
```

Which, as you might gather, means `List(Elem, Sep)` is a parser recognizing an `Elem` possibly followed by more `Sep`-separated `Elem`s (it also skips the spacing).

To use a parametrized rule, invoke it with parsing expressions, putting arguments inside parenthesis:

```
ArgList <- '(' List(Identifier, ',') ')'
```

So, an `ArgList` is a comma-separated list of identifiers enclosed between parenthesis. **Pegged** is smart enough to produce the correct code for parsing expressions used as argument, which can be as complicated as you want (any parsing expression will do). In the previous example, `','` is correctly transformed into the comma-matching parser.

What Parametrized Rules Can Do For You
--------------------------------------

That means any repetitive pattern you see in your grammar can be extracted and abstracted away as a parametrized rule. For example, it's common in PEG to see:

```
Rule <~ (!Expr .)*
```

Which means 'any char, as long as it's not an `Expr`'. To get text up to an end-of-line marker, for example:

```
Line <~ (!EOL .)* (EOL / EOI)
```

(`EOL` and `EOI` are [[Predefined Parsers]]).

Let's call this pattern AllUntil:

```
AllUntil(Pred) <~ (!Pred .)*
```

This can be generalized further:

```
Until(Expr, Pred) <- (!Pred Expr)*
AllUntil(Pred)    <~ Until(., Pred)
```

**Pegged** parsers are classes (see [[Behind the Curtain: How Pegged Works]]). As you may gather, parametrized rules are just class templates:

```d
class Until(Expr, Pred) : ZeroOrMore!(Seq!(NegLookAhead!(Pred), Expr)) { ... }
```

Which means they can be directly invoked by the user, like this:

```d
auto p = AllUntil!(EOL).parse("Hello
World");

assert(p.capture[0] == "Hello");
```

That also means it's possible to define parametrized rules with different arities (number of args):

```d
List(Elem, Sep) <- Elem (Sep Elem)*
List(Elem)      <- List(Elem, :',') # Comma-sep, drop the commas
```

Caveat
------

Parametrized rules are inner class templates in the bigger class (the grammar itself). This means these definitions must be placed first in the class code. **Pegged** does this automatically for you: it scans the grammar definitions and reorders them accordingly. You do not have to worry about it: just define your grammar in the order you wish.

But there is a catch: **Pegged** considers that the root rule of a grammar (the one that's called when you just use the grammar name in the `.parse()` call) is the very first rule. This means **Do not put a parametrized rule as the first rule of a grammar**. Else, the inner code won't know which argument should be passed to the template.

```d
// This creates the 'List' grammar, with one rule called 'List' also.
mixin(grammar(`
List(E, Sep) <- E
`));

void main()
{
    List.parse("A BC D"); // Uh? In List!(AAA, BBB), what should be AAA and BBB?
}
```

In the previous example, that may seem obvious. But sometimes, while adding rules to an already existing grammar, you may forget it. So, don't write a parametrized rule as

Other Examples
--------------

There is a module presenting different parametrized rules: [here](https://github.com/PhilippeSigaud/Pegged/blob/master/pegged/examples/parametrized.d).  

As it contains *only* parametrized rules, do not use it 'raw', see the Caveat section.

Future Extensions
-----------------

**Todo**: For now, **Pegged** does not recognize template tuple parameters. You _cannot_ do:

```
# For example
Rule(Exprs...) <- Exprs[0] !(Exprs[1]) Exprs[$-1]
```

That would also mean giving a user access to D syntax for calling parameters, $ calls or even slices. I'm not up to it right now, because it'll add another level of complexity to the **Pegged** grammar.

Also, there is no default parameter. That, I might code it as it seems easy to do.


Another very interesting thing would be to allow numerical arguments (2, as opposed to the literal '2'). For example: `Repeat(e, 2,4)`. But it's a Pandora box: to define `Repeat` I'd need to add `if` expressions. It's doable, but I'm leery of transforming the **Pegged** syntax into a full-fledged programming language syntax. It won't be far from Turing-completeness and that's definitely something I *do not want*. I mean, I already have D for that.

```
Repeat(Expr, n) <- Expr  if(n > 1)(Repeat(Expr, n-1))
```

Also, associated to that, guards/constraints on parametrized rules:

```
Repeat(Expr,n) if (n> 0) <- Expr   if(n > 1)(Repeat(Expr, n-1))
```

* * * *

Next Lesson: [[Named Captures]]


* * * *

[[Pegged Tutorial]]