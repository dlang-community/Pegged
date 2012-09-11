User-Defined Parsers
====================

A **Pegged** parser is just a callable accepting and returning a `ParseTree`. You can assemble it from the predefined parsers, by calling upon other grammar's rules or from completly new code

**Pegged** only needs this `ParseTree`-accepting version. If you want to have a nicer to use `string`-accepting version, to begin the parsing by just calling your parser with a string, you can overload it:

```d
ParseTree mine(ParseTree p)
{
    (...)
	
}

ParseTree mine(string s)
{
    return mine(ParseTree("",false,null,s));
}
```

See the definition of `ParseTree` in [[Parse Trees]].

Let's define our own new parser. Since we are big boys and girls, it won't be a terminal, we'll directly create an operator. Let's make `permutation(expr1, expr2)`, a parser that succeeds if it encounters `expr1` followed by `expr2` or `expr2` followed by `expr1`.

```d
ParseTree permutation(alias expr1, alias expr2)(ParseTree p)
{
	(...)
}
```

The plumbing is there, now let's code the real thing: `permutation` delegates the parsing to `expr1` and `expr2`. Here is what we could write instead of `(...)`

```d
ParseTree permutation(alias expr1, alias expr2)(ParseTree p)
{
	ParseTree result = and!(expr1,expr2)(p);
	if (result.successful)
		return result;
	else
		return and!(expr2,expr1)(p);
}
```

Of course, since `permutation(a,b)` is just `a b / b a`, it can be written simply like this:

```d
ParseTree permutation(alias expr1, alias expr2)(ParseTree p)
{
	return or!( and!(expr1,expr2),
	            and!(expr2,expr1)
			   )(p);
}
```

And this is it. This is a fully functional parser, able to plug into any **Pegged** expression and be used in the same way:

```
// Called by a grammar:
mixin(grammar(`
Gram:
	A <- permutation(B,C)
	B <- 'b'
	C <- 'c'
`));

assert( Gram("bc").successful);
assert( Gram("cb").successful);
assert(!Gram("bb").successful);
assert(!Gram("cc").successful);
```

Now, what about extending it? It should be possible to call `permutation(a,b,c,d)` and to have it match `a b c d`, `a b d c` and all other 22 permutations (4! == 24). The easiest way is to use a template tuple parameter:

```d
ParseTree permutation(exprs...)(ParseTree p)
{
    ...
}
```

Now, for the parsing code, it should be a big `or` enclosing a list of `and`s, each representing a possible permutation. The tricky part is getting all permutations of a tuple's elements. I'll show the code here, even though it's a tutorial on **Pegged** and not on templates. The parsing part is quite simple, in fact.

```d
struct Group(T...)
{
    alias T Types;
}

template Dispatch(size_t i, alias First, Rest)
{
    static if (i < Rest.Types.length)
        alias TypeTuple!(Group!(Rest.Types[0..i], First, Rest.Types[i..$]), Dispatch!(i+1, First, Rest)) Dispatch; 
    else
        alias TypeTuple!(Group!(Rest.Types[0..i], First)                                               ) Dispatch; 
}

template PutEverywhere(alias First)
{
    template PutEverywhere(Rest)
    {
        alias Dispatch!(0,First,Rest) PutEverywhere;
    }
}

template Distribute(alias First, List...)
{
    alias staticMap!(PutEverywhere!First, List) Distribute;
}

template staticPermutation(T...) if (T.length > 0)
{
    static if (T.length == 1)
        alias Group!(T[0]) staticPermutation;
    else
        alias Distribute!(T[0], staticPermutation!(T[1..$])) staticPermutation;
}

template andForGroup(T...)
{
    static if (T.length)
        alias TypeTuple!(and!(T[0].Types), andForGroup!(T[1..$])) andForGroup;
    else
        alias TypeTuple!() andForGroup;
}

ParseTree permutation(exprs...)(ParseTree p) if (exprs.length > 0)
{
    return or!( andForGroup!(staticPermutation!(exprs)))(p);
}
```

Whee, that was a mouthful! Most of it is getting permutations. `andForGroup` is just there to transform the helper struct `Group` (holding types) into an `and!(...)` call. The enclosing `or` is then trivial.

And it works:

```d
// The last literal to show expressions can consume as much as they want.
alias permutation!(literal!"a", literal!"b", literal!"c", literal!"def");

assert(perm("abcdef").successful);
assert(perm("abdefc").successful);
assert(perm("cdefab").successful);
assert(!perm("deafcb").successful);
```

* * * *

Next lesson [[Writing Your Own Grammar]]

* * * *

[[Pegged Tutorial]]
