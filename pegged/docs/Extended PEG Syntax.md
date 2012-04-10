Extended PEG Syntax
===================

As we saw in [[PEG Basics]] and [[Declaring a Grammar]], **Pegged** implements the entire PEG syntax, exactly as it was defined by its author.

Now, I felt the need to extend this a little bit. At that time, semantic actions were not implemented in **Pegged** so now that they are, these extensions are not strictly necessary, but they are useful shortcuts.


Dropping a Node
---------------

Given a parsing expression `e`, `:e` will drop `e`'s captures. And, due to the way sequences are implemented in **Pegged**, the mother expression will forget `e` result (that's deliberate). It allows one to write:

```d
mixin(grammar("
    Rule1 <- Rule2 :Spaces Rule3
    
    # Rest of grammar ...
"));
```

On the first rule, see the colon before `Spaces`. That means that the parse tree will contain only a `Rule2` and a `Rule3` node, and no `Spaces` node. It's a way to cut the parse tree so as to keep only the interesting parts, not the syntactic signs necessary to structure a grammar.

Note that **Pegged** automatically drops the rules that are outside a grammar (See [[Four Levels of Encapsulation]] for more thoughts on this). In particular, char and string literals are always dropped unless **Pegged** is told otherwise (see next extension). That allows one to write:

```
JSON:
    JSONObject <  '{' (Pair (',' Pair)*)? '}'
    Pair       <  String ':' Value
    Array      <  '[' (Value (',' Value)* )? ']'
    
    # Rest of JSON grammar...
```

And given an input such as `{"Hello":42, "World":0}`, the parse tree will look like this:

```
ParseTree("JSONObject",
    ParseTree("Pair", ...
        ParseTree("String", ...)
        ParseTree("Number", ...)),
     ParseTree("Pair",
        ParseTree("String", ...)
        ParseTree("Number", ...))  
)
```

And no '{' nor ':' will be in the final output.  If you want to keep literals (or any other parsing expression), the standard way is to make it a rule, as it's done in the JSON grammar:

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

Another way is the next extension:

Keeping Nodes
-------------

A recent addition to **Pegged** is the `^` operator, that tells **Pegged** to always keep a node, even when it should be dropped. It's a way to keep calls to external rules, like the predefined ones (`Identifier` for example) or the literals. For examples, given the two grammars `Gram1` and `Gram2`:

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
        Lit(b) ["b"]
    B ["b"]
        Lit(b) ["b"]
    B ["b"]
        Lit(b) ["b"]
```

It's useful when you want the parse tree structure to keep the syntactic structure, with all markers reproduced as nodes. Also, in a programming language, it allows one to keep keywords. For example:

```
ReturnStatement <- ^"return" Expression? ';'
```

Given `"return i;"`, the previous rule will return a parse tree with "return" as a node, but the semicolon dropped.

```
ReturnStatement ["return", "i"]
    Lit(return) ["return"]
    Expression ["i"]
        (... lots of intermediary nodes ...)
```

Note that, as this is a recent addition, the example grammars given in [[Grammar Examples]] do not integrate this change. The main change will be a net decrease in the `:` operator used, as literals are now dropped from the parse tree.

Fusing Captures
---------------

The `~` (tilde) operator concatenates an expression's captures in one string. It was chosen for its proximity with the equivalent D operator. It's useful when an expression would otherwise return a long list of individual parses, whereas you're interested only in the global result:

```d
mixin(grammar("
    # See the ':' before DoubleQuote
    # And the '~' before (Char*)
    String <- :DoubleQuote ~(Char*) :DoubleQuote
    Char <- !DoubleQuote . # Anything but a double quote
"));
```

Without the tilde operator, using `String` on a string would return a list of `Char` results. With tilde, you get the string content, which is most probably what you want:

```d
auto p = String.parse(q{"Hello World!"});
assert(p.capture == ["Hello World!"];
// without tilde: p.capture == ["H", "e", "l", "l", "o", " ", "W", ...]
```

The same goes for number-recognizers:

```
Number <- ~(Digit+)
Digit <- [0-9]
```

```d
auto n = Number.parse("1234");
assert(n.capture == ["1234"]);
// without tilde: n.capture == ["1", "2", "3", "4"]
```

Internally, it's used by `Identifier` and `QualifiedIdentifier`.

Named Captures
--------------

The `=name` (equal) operator is used to name a particular capture. it's defined in [[Named Captures]]. But here is the idea:

```
Email <- QualifiedIdentifier=name :'@' QualifiedIdentifier=domain
```

```d
enum p = Email.parse("john.doe@example.org");
assert(p.namedCaptures["name"] == "John.Doe");
assert(p.namedCaptures["domain"] == "example.org");
```

Semantic Actions
----------------

Semantic actions are enclosed in curly braces and put behind the expression they act upon:

```
XMLNode <- OpeningTag {OpeningAction} (Text / Node)* ClosingTag {ClosingAction}
```

You can use any delegate from `Output` to `Output` as a semantic action. See [[Semantic Actions]].

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

I plan to add other extensions, such as `@` or `$` but these are in flux right now and I'll wait for the design to stabilize before documenting them.

Rule-Level Extensions
---------------------

All the previously-described extensions act upon expressions. When you want an operator to act upon an entire rule, it's possible to enclose it between parenthesis:

```
Rule <- ~(complicated expression I want to fuse)
```

This need is common enough for **Pegged** to provide a shortcut: put the operator in the arrow:

`<~` (squiggly arrow) concatenates the captures on the right-hand side of the arrow.

`<:` (colon arrow) drops the entire rule result (useful to ignore comments, for example)

`<{Action}` associates an action with a rule.

(There is no `<^` (keep arrow) that would distribute the 'keep' operator to all subelements in a rule. I might add it if the needs is strong enough.)

For example:

```
Number <~ Digit+
Digit  <- [0-9]

# Nested comments
Comment <: "/*" (Comment / Text)* "*/"
Text    <~ (!("/*"/"*/") .)* # Anything but begin/end markers
```

That makes `Number` expression a bit more readable (if you use a font that rightly distinguishes between ~ and -, as GitHub does not really do...)

Space Arrow
-----------

There is another kind of space-level rule, it's the 'space arrow', just using `< ` as an arrow. Instead of then treating the parsing expression as a standard non-space-consuming PEG sequence, it will consume spaces between elements.

So, given:

```
Rule1 <- A B C
Rule2 <  A B C

A <- 'a'
B <- 'b'
C <- 'c'
```

`Rule1` will parse `"abc"` but not `"a   b c"`, where `Rule2` will parse the two inputs (and will output a parse tree holding only an `A` node, a `B` and a `C` node: no space node.

The space-consuming is done by the predefined `Spacing` parser (see [[Predefined Parsers]]), which munches blank chars, tabs and co and line terminators.

As a TODO, I plan to let the user define its own `Space` parser (that could for example consume both spaces and comments, as is done in the PEG grammar itself), which the space-sequence would call behind the scene. That way, the space consuming could be customized for each grammar. **Pegged** is not there yet.


* * * *
Next lesson: [[Parametrized Rules]]

* * * *

[[Pegged Tutorial]]