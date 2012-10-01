Parameterized Rules
===================

**Pegged** piggybacks on D's nice template syntax to get parameterized rules: rules that take other parsing expressions as arguments. The syntax is quite simple, just put a comma-separated list right after the rule name (without any space between the list and the name):

```
List(Elem, Sep) <  Elem (Sep Elem)*
```

Which, as you might gather, means `List(Elem, Sep)` is a parser recognizing an `Elem` possibly followed by more `Sep`-separated `Elem`s (it also skips the spacing).

To use a parameterized rule, invoke it with parsing expressions, putting arguments inside parenthesis:

```
ArgList <- '(' List(Identifier, ',') ')'
```

So, an `ArgList` is a comma-separated list of identifiers enclosed between parenthesis. **Pegged** is smart enough to produce the correct code for parsing expressions used as argument, which can be as complicated as you want (any parsing expression will do). In the previous example, `','` is correctly transformed into the comma-matching parser.

What Parameterized Rules Can Do For You
---------------------------------------

This way, any repetitive pattern you see in your grammar can be extracted and abstracted away as a parameterized rule. For example, it's common in PEG to see:

```
Rule <~ (!Expr .)*
```

Which means 'any char, as long as it's not an `Expr`'. To get text up to an end-of-line marker, for example:

```
Line <~ (!endOfLine .)* (endOfLine / endOfInput)
```

(`endOfLine` and `endOfInput` are [[Predefined Parsers]]).

Let's call this pattern AllUntil:

```
AllUntil(Pred) <~ (!Pred .)*
```

This can be generalized further:

```
Until(Expr, Pred) <- (!Pred Expr)*
AllUntil(Pred)    <~ Until(., Pred)
```

**Pegged** parsers are structs (see [[Behind the Curtain: How Pegged Works]]). As you may gather, parameterized rules are just struct templates. Hence, they can be directly invoked by the user, like this:

```d
auto p = AllUntil!(EOL)("Hello
World");

assert(p.matches[0] == "Hello");
```

Rules Overloads
---------------

That also means it's possible to define parameterized rules with different arities (number of args):

```d
Lists:
    List(Elem, Sep) <- Elem (Sep Elem)*
    List(Elem)      <- List(Elem, :',') # Comma-sep, drop the commas
```

```d
ParseTree p1 = Lists.List!(identifier); // Calls the second, 1-argument, version.
ParseTree p2 = Lists.List!(identifier, literal!"."); // Calls the first version.
```

Default Values
--------------

Parameters can have default values, standard **Pegged** expressions. Here is for example a `List` rule which defaults to comma-separated when given only one argument:

```
List(Elem, Sep = ',')  <  Elem (:Sep Elem)*
```

The default value is internally converted into a standard **Pegged** parser, and then the D engine for template default parameters does the rest of the job.

Note that the standard D disimbiguation rules apply. Given the two following rules:
 
```
Lists:
    List(Elem) < Elem (:' '* Elem)*          # Space-separated list
    List(Elem, Sep =',') < Elem (Sep Elem)* # User-defined separation, defaults to comma-separated
```

```
List!(identifier)("abc def");
```

Will call the first version, because D chooses the 1-parameter version in this case. Translated into a grammar rule, that means:

```
MyRule <- Lists.List(AnotherRule)
```

Will also call `Lists.List` first overload.


Parameterized Grammars
---------------------

Entire grammars can be parameterized and can use the passed expressions. The syntax is the same as for rules: put a comma-separated list right after the rule name, before the colon:

```d
mixin(grammar(`
Arithmetic(Atom) :
    Expr     <  Factor  (('+'/'-') Factor)*
    Factor   <  Primary (('*'/'/') Primary)*
    Primary  <  '(' Expr ')' / '-' Expr / Atom
`));
```

To use the parameterized grammar directly, call it with parsing expressions as arguments:

```d
alias or!(identifier, fuse!(oneOrMore!(digit))) Atom; // more or less equivalent to Atom <- Identifier / ~Digit+
auto tree = Arithmetic!(Atom)("(x-1)*(x+1)");
```

The previously-defined `Atom` matches both identifiers and simple numbers. That way, `Arithmetic` is now a grammar template (well, duh!) that provides a 'skeleton': it recognizes expressions of the form `[] + ([] - [])*[]`, for any expression filling the slots.

This opens a new genericity in grammars: `Arithmetic` can now be used in different contexts and by different grammars. To use it in another grammar, just call it with an argument. Say we have a grammar for relations (`e == f`, `e <= f`, and so on). The compared expression can be arithmetic expressions:

```d
mixin(grammar(`
Relation:
Rel   <  ^Arithmetic(Atom) RelOp ^Arithmetic(Atom)
RelOp <- "==" / "!=" / "<=" / ">=" / "<" / ">"
Atom  <- identifier / ~digit+
`));

void main()
{
    writeln(Relation("(x-1)*(x+1) == x*x -1"));
}
```

Note the `^` (promote aka 'keep') operator before the `Arithmetic(Atom)` call, to keep the resulting parse node, external to the grammar.

This gives us the following parse tree:

```
Relation  [0, 21]["(", "x", "-", "1", ")", "*", "(", "x", "+", "1", ")", "==", "x", "*", "x", "-", "1"]
 +-Relation.Rel  [0, 21]["(", "x", "-", "1", ")", "*", "(", "x", "+", "1", ")", "==", "x", "*", "x", "-", "1"]
    +-Arithmetic  [0, 12]["(", "x", "-", "1", ")", "*", "(", "x", "+", "1", ")"]
    |  +-Arithmetic.Expr  [0, 12]["(", "x", "-", "1", ")", "*", "(", "x", "+", "1", ")"]
    |     +-Arithmetic.Factor  [0, 12]["(", "x", "-", "1", ")", "*", "(", "x", "+", "1", ")"]
    |        +-Arithmetic.Primary  [0, 5]["(", "x", "-", "1", ")"]
    |        |  +-Arithmetic.Expr  [1, 4]["x", "-", "1"]
    |        |     +-Arithmetic.Factor  [1, 2]["x"]
    |        |     |  +-Arithmetic.Primary  [1, 2]["x"]
    |        |     +-Arithmetic.Factor  [3, 4]["1"]
    |        |        +-Arithmetic.Primary  [3, 4]["1"]
    |        +-Arithmetic.Primary  [6, 12]["(", "x", "+", "1", ")"]
    |           +-Arithmetic.Expr  [7, 10]["x", "+", "1"]
    |              +-Arithmetic.Factor  [7, 8]["x"]
    |              |  +-Arithmetic.Primary  [7, 8]["x"]
    |              +-Arithmetic.Factor  [9, 10]["1"]
    |                 +-Arithmetic.Primary  [9, 10]["1"]
    +-Relation.RelOp  [12, 14]["=="]
    +-Arithmetic  [15, 21]["x", "*", "x", "-", "1"]
       +-Arithmetic.Expr  [15, 21]["x", "*", "x", "-", "1"]
          +-Arithmetic.Factor  [15, 19]["x", "*", "x"]
          |  +-Arithmetic.Primary  [15, 16]["x"]
          |  +-Arithmetic.Primary  [17, 19]["x"]
          +-Arithmetic.Factor  [20, 21]["1"]
             +-Arithmetic.Primary  [20, 21]["1"]
```

Notice how the different grammars are intermingled: `Relation.*` for `Relation`'s nodes and `Arithmetic.*` for `Arithmetic`'s nodes. In this case, the `Relation.RelOp` node matching `==` is between two `Arithmetic` nodes.

We can continue this matrioshka construction even further: `Relation` itself can be parameterized and used into a Boolean grammar, recognizing sentences like `(x < 1) || (x+y == 0)`:

```d
mixin(grammar(`
Arithmetic(Atom) :
    Expr     <  Factor  (^('+'/'-') Factor)*
    Factor   <  Primary (^('*'/'/') Primary)*
    Primary  <  '(' Expr ')' / '-' Expr / Atom
`));

mixin(grammar(`
Relation(Atom):
    Expr     <  ^Arithmetic(Atom) RelOp ^Arithmetic(Atom)
    RelOp    <- "==" / "!=" / "<=" / ">=" / "<" / ">"
`));

mixin(grammar(`
Boolean:
    BoolExpr < AndExpr ("||" AndExpr)*
    AndExpr  < Primary ("&&" Primary)*
    Primary  <  '(' BoolExpr ')' / '!' BoolExpr / ^Relation(Atom)

    Atom     <- identifier / Number
    Number   <~ [0-9]+
`));

void main()
{
    auto tree = Boolean("x < 0 || x+y == 1");
    writeln(tree);
}
```

So, `Boolean` calls `Relation`, which in turn calls `Arithmetic`. Isn't that cool?


Parameterized First Rules
-------------------------

There is something you should be aware of: **Pegged** considers that the start rule of a grammar (the one that's called first when you just use the grammar name to call it) is the very first rule in the grammar definition. When this first rule is parameterized, there is no way for **Pegged** to determine how to invoke the grammar, so no `opCall` is created:

```d
mixin(grammar(`
Gram:
    Rule(B) <- B B
    `));

void main()
{
    auto error = Gram("bbb"); // Uh? What is B in this case?
}
```

The correct way to call such a grammar is to call the rule directly, while providing the template parameter yourself:

```d
void main()
{
    alias literal!"b" b;
    auto correct = Gram!(b)("bbb"); // Ok, calls the equivalent of: Rule <- 'b' 'b'
}
```

This is also a possibility for other parameterized rules: as with standard rule, you can call them directly if you wish, as long as you bring the parameters along.

Parameterized Rules inside Parameterized Grammars
-------------------------------------------------

You can have parameterized rules inside otherwise parameterized grammars:

```d
mixin(grammar(`
A(B):
    Rule1    <- B*
    Rule2(C) <- B C
`));
```

This means the global `A` grammar depends on `B` (which appears in both inner rules). But rule `Rule2` *also* depends on a parameter named `C`:

```d
void main()
{
    alias literal!"b" b;
    alias literal!"c" c;
    
    writeln(A!(b)("bbb")); // calls Rule1, matches all b's
    writeln(A!(b).Rule2!(c)("bc"); // matches b and then c.
}
```

As seen in the previous section, if the start rule is parameterized, no general grammar `opCall` is generated:

```d
mixin(grammar(`
A(B):
    Rule1(C) <- B C
`));

void main()
{
    alias literal!"b" b;
    alias literal!"c" c;
    
    writeln(A!(b)("bbb")); // error! C is not defined.
    writeln(A!(b).Rule1!(c)("bc"); // matches b and then c.
}
```

Other Examples
--------------

There is a module presenting different parameterized rules: [here](https://github.com/PhilippeSigaud/Pegged/blob/master/pegged/examples/parameterized.d).  

As it contains *only* parameterized rules, do not use it 'raw'.

Future Extensions
-----------------

**Todo**: For now, **Pegged** does not recognize template tuple parameters. You _cannot_ do:

```
# For example
Rule(Exprs...) <- Exprs[0] !(Exprs[1]) Exprs[$-1]
```

That would also mean giving a user access to D syntax for calling parameters, $ calls or even slices. I'm not up to it right now, because it'll add another level of complexity to the **Pegged** grammar, which I find to be already quite complex.

Another very interesting thing would be to allow numerical arguments (2, as opposed to the literal '2'). For example: `Repeat(e, 2,4)` for a rule that would accept 2, 3 or 4 `e` matches. But it's a Pandora box: to define `Repeat` I'd need to add `if` expressions. It's doable, but I'm leery of transforming the **Pegged** syntax into a full-fledged programming language syntax. It won't be far from Turing-completeness and that's definitely something I *do not want*. I mean, I already have D for that.

```
Repeat(Expr, n) <- Expr  if(n > 1)(Repeat(Expr, n-1))
```

Also, associated to that, guards/constraints on parameterized rules:

```
Repeat(Expr,n) if (n> 0) <- Expr   if(n > 1)(Repeat(Expr, n-1))
```

Or maybe like this?

```
Repeat(Expr, 0) <- eps
Repeat(Expr, n) <- Expr Repeat(Expr, n-1)
```

But then I'd need a way to indicate `n` will be numerical value, not a **Pegged** expression.

* * * *

Next Lesson: [[Semantic Actions]]


* * * *

[[Pegged Tutorial]]