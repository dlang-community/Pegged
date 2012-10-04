Extended PEG Syntax
===================

As we saw in [[PEG Basics]] and [[Declaring a Grammar]], **Pegged** implements the entire PEG syntax, exactly as it was defined by its author.

Now, I felt the need to extend this a little bit. At that time, semantic actions were not implemented in **Pegged** so now that they are, these extensions are not strictly necessary, but they are useful shortcuts.

Discarding a Node
-----------------

Given a parsing expression `e`, `:e` will discard `e`'s parse result. The mother expression calling `e` will forget `e` result (that's deliberate). No node nor match will be in the final parse tree. It allows one to write:

```d
mixin(grammar("
    Rule1 <- Rule2 :Spaces Rule3
    
    # Rest of grammar ...
"));
```

On the first rule, see the colon before `Spaces`. That means that the parse tree will contain only a `Rule2` and a `Rule3` node, and no `Spaces` node. It's a way to cut the parse tree down, so as to keep only the interesting parts, not the syntactic signs necessary to structure a grammar.

Note that **Pegged** automatically drops the rules that are outside a grammar (See [[Four Levels of Encapsulation]] for more thoughts on this). In particular, char and string literals terminals are always dropped unless **Pegged** is told otherwise (see next extension). That allows one to write:

```
JSON:
    JSONObject <  '{' (Pair (',' Pair)*)? '}'
    Pair       <  String ':' Value
    Array      <  '[' (Value (',' Value)* )? ']'
    
    # Rest of JSON grammar...
```

And given an input such as `{"Hello":42, "World":0}`, the parse tree will look like this:

```
JSON(
	JSONObject(
		Pair(
			String (...),
			Number (...)),
		Pair(
			String (...),
			Number (...))))
```

And no '{' nor ':' will be in the final output.  

Dropping Nodes
--------------

Completly discarding a node and its matches is sometimes a bit extreme. Since I regularly needed to cut a node, *but* keep its matches in the parent node, I added the `;` operator (cousin to `:`) to do just that. It's an intermediate between the default behavior and `:`'s behavior.

Keeping Nodes
-------------

If you want to keep literals (or any other parsing expression), the standard way is to make it a rule, as it's done in the JSON grammar:

```
    Value  <  String 
            / Number 
            / JSONObject 
            / Array 
            / True 
            / False 
            / Null
    True   <- "true"
    False  <- "false"
    Null   <- "null"
```

Here, `"true"` and co are kept through the `True`, `False` and `Null` rules.

Another way is the `^` operator, that tells **Pegged** to always keep a node, even when it should be dropped. It's a way to keep calls to external rules, like the predefined ones (`identifier` for example) or the literals. For example, given the two grammars `Gram1` and `Gram2`:

```
Gram1:
    A <- B*
    B <- 'b'

Gram2:
    A <- B*
    B <- ^'b'
```

When given `"bbb"` as input, `Gram1` will return a parse tree containing only `A` and `B` nodes:

```
A ["b", "b", "b"]
    B ["b"]
    B ["b"]
    B ["b"]
```

Whereas `Gram2` will keep the literal child nodes:

```
A ["b", "b", "b"]
    B ["b"]
        literal!(b) ["b"]
    B ["b"]
        literal!(b) ["b"]
    B ["b"]
        literal!(b) ["b"]
```

It's useful when you want the parse tree structure to keep the syntactic structure, with all markers reproduced as nodes. Also, in a programming language, it allows one to keep keywords. For example:

```
ReturnStatement <- ^"return" Expression? ';'
```

Given `"return i;"`, the previous rule will return a parse tree with "return" as a node, but the semicolon dropped.

```
ReturnStatement ["return", "i"]
    literal!(return) ["return"]
    Expression ["i"]
        (... lots of intermediary nodes ...)
```

Note the subtile difference between

```
AddExpr  < ^('+'/'-') Factor 
```

and

```
AddExpr  <  (^'+'/^'-') Factor 
```

In the former case the entire choice node (`pegged.peg.or` behind the scenes) is kept in the parse tree. In the latter case, the `or` node will be discarded and only the literal will be kept.

Fusing Matches
--------------

The `~` (tilde) operator concatenates an expression's matches in one string. It was chosen for its proximity with the equivalent D operator. It's useful when an expression would otherwise return a long list of individual matchss, whereas you're interested only in the global result:

```d
mixin(grammar("
Strings:
    String <- doublequote ~(Char*) doublequote
    Char   <- !doublequote . # Anything but a double quote
"));
```

Without the tilde operator, using `String` on a string would return a list of `Char` results. With tilde, you get the string content, which is most probably what you want:

```d
auto p = Strings(q{"Hello World!"});
assert(p.matches == ["Hello World!"];
// without tilde: p.matches == ["H", "e", "l", "l", "o", " ", "W", ...]
```

The same goes for number-recognizers:

```
Numbers:
	Number <- ~(Digit+)
	Digit  <- [0-9]
```

```d
auto n = Numbers("1234");
assert(n.matches == ["1234"]);
// without tilde: n.matches == ["1", "2", "3", "4"]
```

Internally, it's used by `identifier` and `qualifiedIdentifier`.

Semantic Actions
----------------

Semantic actions are enclosed in curly braces and put behind the expression they act upon:

```
XMLNode <- OpeningTag {OpeningAction} (Text / Node)* ClosingTag {ClosingAction}
```

You can use any delegate from `ParseTree` to `ParseTree` as a semantic action. See [[Semantic Actions]].

Range of Chars Extension
------------------------

The characters `-` (dash), `[` (opening square brackets) and `]` (closing square bracket) have special meaning in char ranges (the `[a-z]` syntax). In **Pegged** they can be escaped with `\` to represent themselves. As usual, `\` is thus `\\`. Use them like this:

* `[-+]` `-` in first position is OK. Already possible in PEG. 

* `[+\-]` Matches `+` and `-`

* `[-\\\]]` Matches `-`, `\` and `]`

* `[\r\n\t]` Standard escapes are there, too.

Also, the char semantics were extended to deal with Unicode characters (and not only UTF-8). Basic PEG allows a char to be represented as an ASCII octal sequence: `\040`. **Pegged** also recognizes:

*  `\x41`: hexadecimal UTF-8 chars.

*  `\u0041`: hexadecimal UTF-16 chars.

*  `\U00000041`: hexadecimal UTF-32 chars.

I added them to deal with the W3C XML spec.

Other Extensions
----------------

I plan to use other symbols, such as `=`, `@` or `$` but these are in flux right now and I'll wait for the design to stabilize before documenting them.

Rule-Level Extensions
---------------------

All the previously-described extensions act upon expressions. When you want an operator to act upon an entire rule, it's possible to enclose it between parenthesis:

```
Rule <- ~(complicated expression I want to fuse)
```

This need is common enough for **Pegged** to provide a shortcut: put the operator in the arrow:

`<~` (squiggly arrow) concatenates the captures on the right-hand side of the arrow.

`<:` (colon arrow) drops the entire rule result (useful to ignore comments, for example)

`<^` (keep arrow) that calls the 'keep' operator to all subelements in a rule.

For example:

```
Number <~ Digit+
Digit  <- [0-9]

# Nested comments
Comment <: "/*" (Comment / Text)* "*/"
Text    <~ (!("/*"/"*/") .)* # Anything but begin/end markers
```

That makes `Number` expression a bit more readable.

Space Arrow and User-Defined Spacing
------------------------------------

There is another kind of space-level rule, it's the 'space arrow', just using `< ` (lower-than followed by a space) as an arrow. Instead of then treating the parsing expression as a standard non-space-consuming PEG sequence, it will consume spaces before, between and after elements.

So, given:

```
Rule1 <- A B C
Rule2 <  A B C

A <- 'a'
B <- 'b'
C <- 'c'
```

`Rule1` will parse `"abc"` but not `"a   b c"`, where `Rule2` will parse the two inputs (and will output a parse tree holding only an `A` node, a `B` and a `C` node: no space node. `Rule2` will also parse `"   ab c    "` up to `d`.

You can select what pattern is considered as blank: define a rule called `Spacing` in your grammar. This is the rule that will be called by **Pegged** for this grammar to consume space when the `< `arrow is used. If no user-defined `Spacing` is provided, **Pegged** use the predefined `spacing` rule that parse blank chars (whitespace, tabulation, etc).

If you're using **Pegged** to parse a programming language with comments, a nice trick is to put a call to both whitechars and comments rules in `Spacing` (see **Pegged** own grammar for an example). That way, you can put comments anywhere a space can be and they will be recognized and discarded while parsing.

Parameterized Rules
-------------------

Parameterized rules are another PEG extension, described in [[Parameterized Rules]].

* * * *
Next lesson: [[Parametrized Rules]]

* * * *

[[Pegged Tutorial]]