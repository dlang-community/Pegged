Generating Code
===============

Once a grammar is defined and a parse tree was generated, how can it be used? We already saw how to use the parse tree in [[Using the Parse Tree]], at runtime or compile-time. But since **Pegged** can parse at compile-time, the information can also be used to generate during compilation a string containing D code, which can in turn be mixed in. 

So, what's the difference with what we saw in lesson [[Using the Parse Tree]]? None whatsoever, except be sure to do everything at compile-time, since you cannot mix runtime strings.

This is *exactly* what **Pegged** itself does with the input you give it. So let's use **Pegged** as an example.

Part of the PEG grammar is :

```
Grammar      <- Spacing GrammarName Definition+ :eoi
Definition   <- LhsName Arrow Expression
Expression   <- Sequence (:OR Sequence)*
Sequence     <- Prefix+
Prefix       <- (POS / NEG / FUSE / DISCARD / KEEP / DROP)* Suffix
Suffix       <- Primary (OPTION / ZEROORMORE / ONEORMORE / Action)*
(...)
```

A `Grammar` is a bunch of `Definition`s up to the end of the input (to let nothing unparsed and provoke an error if something is left unparsed. A `Definition` is a name, an `Arrow` and an `Expression` and so on. Pretty standard stuff for you, now. Let's see how that can be used to create code.

As said in [[Behind the Curtain: How Pegged Works]], the basic PEG parsers are structs and functions like `and` (sequences), `or` (choices) or `literal` (literals). A PEG expression like:

```
A <- B C D+ E?
```

Is implemented in D code as:

```d
ParseTree A (ParseTree p)
{ 
    return  and!(B, C, oneOrMore!(D), option!(E))(p);
}
``` 

So really, to create such a code from a parse tree we have to

1) Get the rule name

2) (Optionally, get the arrow type, not described here)

3) Get the parsing expression parse tree

4) Transform the expression into something like `and!(B, C, oneOrMore!(D), option!(E))`

5) concatenate the parts : name + inheritance + parsing stuff inside the struct.

6) (Optionally, collect the rule names to get better parse tree deforestation: cutting rules external to the grammar in the final parse tree)

Step 1) is standard identifier recognition. I'll focus on steps 3) and 4). If you look into `pegged.grammar`, you'll find the inner `generateCode` function, inside the `grammar` function. Its role is to transform a parse tree into D code. It's mainly a big switch on the root node name and recursion on the children. **Pegged** see parsing expression as choices (`Expr1 / Expr2 / Epxr3 ...`), composed of sequences (`Expr1a Expr1b Expr1c ...`), themselves composed of a prefix followed by a suffix and so on.  Let's zoom on expressions and sequences:

```d
case "Pegged.Expression":
	if (p.children.length > 1) // OR expression
	{
		result = "or!(";
		foreach(seq; p.children)
			result ~= generateCode(seq) ~ ", ";
		result = result[0..$-2] ~ ")";
	}
	else // One child -> just a sequence, no need for an or!( , )
	{
		result = generateCode(p.children[0]);
	}
	break;
```

When there is a choice node, its children are the different sub-rules. The final code must look something like `or!(Expr1, Expr2, Expr3)`, which is exactly what the foreach loop build, invoking `PEGtoCode` again on the children nodes. There is a slight simplification for 1-element choices: no need to create `or!(Expr)`, just return `Expr`. Note that the way the **Pegged** grammar is built, any lone top level expression is seen as 1-element choice (and then a 1-element sequence), so this simplification is used quite frequently.

The same goes for sequences:

```d
case "Pegged.Sequence":
	if (p.children.length > 1) // real sequence
	{
		result = "and!(";
		foreach(seq; p.children)
		{
			string elementCode = generateCode(seq);
			if (elementCode[0..5] == "and!(") // flattening inner sequences
				elementCode = elementCode[5..$-1]; // cutting 'and!(' and ')'
			result ~= elementCode ~ ", ";
		}
		result = result[0..$-2] ~ ")";
	}
	else // One child -> just a Suffix, no need for a and!( , )
	{
		result = generateCode(p.children[0]);
	}
	break;
```

The difference here is that sequence of sequences are flattened into a single sequence: `A (B C) D` is rewritten into (the equivalent of) `A B C D`, that is `and!(A, B, C, D)`.

You now understand how, given a rule such as:

```
A <- B C
   / D
   / E F (G / H)
```

**Pegged** generates `or!(and!(B,C), D, and!(E, F, or!(G, H)))`.

The same process can be done in your code: define a DSL in **Pegged**, write a parse-tree-to-D-code function and mix it in:

```d
mixin(sql(`

USE mydatabase;

SELECT customer, SUM(quantity) AS "Total Items"
FROM orders
GROUP BY customer;

`));
```

* * * *

Next lesson: [[User-Defined Parsers]]

* * * *

[[Pegged Tutorial]]