User-Defined Parsers
====================

To define your own parser, inherit from the `Parser` class and add a static `parse` member. `parse` should accept an `Input` as argument and must return an `Output`.

If you have a look at `pegged.peg`, you'll see the parsers in there use two functions, `ok` and `fail`, to indicate success and failure respectively. You can use these also. **Pegged**  defines some mixin in `pegged.peg` to help with the plumbing. Use `mixin(okfailMixin());` to generate the `ok` and `fail` functions automatically in your class. Also, there is the `inputToStringMixin` mixin, which provides the definition for `.parse(string s)` and `.parse(Output o)` methods. Just define `.parse(Input i)`, **Pegged** will help you with the rest.

Let's define our own new parser. Since we are big boys and girls, it won't be a terminal, we'll directly create an operator. Let's make `Not(Expr)`, a parser that returns the opposite of `Expr`, but *consumes input*, contrary to the `!e` negative lookahead operator.

```d
class Not(Expr) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Not!(" ~ Expr.stringof ~ ")"));
        (...)
    }
    mixin(stringToOutputMixin());
}
```

The plumbing is there, now let's code the real thing: `Not` delegates the parsing to `Expr`, its super type. Here is what we could write instead of `(...)`

```d
auto p = typeof(super).parse(input);
p.success = !p.success;
return p;
```

And this is it. This is a fully functional parser, able to plug into any **Pegged** expression and be used in the same way. The need is common enough that I internally use the `wrapMixin` and `inheritMixin` mixins to simplify the coding. After all, in D, boilerplate is best left to the compiler to do for you (your time is too precious for that).

The only thing left is to modify the **Pegged** grammar to accept a new syntax to represent `Not`. That may be the subject of another tutorial lesson one day.

* * * *

Next lesson [[Four Levels of Encapsulation]]

* * * *

[[Pegged Tutorial]]
