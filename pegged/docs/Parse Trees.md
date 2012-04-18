Parse Trees
===========

In **Pegged**, a parse tree is a very simple recursive struct:

```d
struct ParseTree
{
    dstring grammarName;
    dstring ruleName;
    bool success;
    dstring[] capture;
    Pos begin, end;
    ParseTree[] children;
// toString()
}
```

`grammarName` and `ruleName` contain respectively the name of the grammar and the name of the rule that created the node: "Expr", "line", etc. There is a `ParseTree` property called `.name` which returns `grammarName ~ "." ~ ruleName`. It's useful to switch on nodes to generate D code, see [[Generating Code]].

`success` indicates whether or not the parse was successful.

`capture` contains the matches. It's a `string[]` and not a `string` since a complete parsing expression can produce many matches. For examples `a*` will contain all the successive matches of rule `a`, which may well be different at each call.

`begin` and `end` are `Pos`s, a small struct representing positions in the input (see below). End is excluding, like for D slices.

Finally, `children` contains an array of component parse results. In general, child expressions in a parsing expression will all contribute their mother node parse result. When defining the parsing expression `a b c`, its parse result will contain a parse result for `a`, another for `b` and another for `c`. The standard aggregating **Pegged** parsers all duplicate their children's captures in their main `capture` member, so as not to force the user to explore the entire tree to get the parsed substrings.
For example:

```d
mixin(grammar("
    Ids <- (Identifier Spacing)* # 'Spacing' does not return any capture
"));

auto input = "This is a sentence.";
auto words = Ids.parse(input); // 

assert(words.capture == ["This", "is", "a", "sentence"]);
```

In the previous example, `Ids` is a single parser that can produce multiple captures.

Positions
---------

The `Pos` struct is defined like this:

```d
struct Pos
{
    size_t index; // linear index
    size_t line;  // input line
    size_t col;   // input column
}
```

Its three fields reprensent respectively:

* the position's index (in a standard D array way, from 0 of course). Use this to obtain slices of input: `input[parseTree.begin.index..parseTree.end.index]`

* the line, also **starting at 0 to be coherent**. This is different from the usual way to deal with lines, but I found it easier to deal with. If people complain, I will change it to 1-indexed lines.

* the column, also starting at 0.

For example, in [[Parse Result]], you'll see:

```
 Arithmetic.Primary: [[index: 8, line: 0, col: 8] - [index: 15, line: 0, col: 15]]["(", "3", "*", "x", "-", "5", ")"]
```

Which means that `Arithmetic`'s rule `Primary` matched the input from index 8 up to (and excluding) index 15. Only the first line is concerned (line 0) and the columns in this case just follow the index.

The line counter is incremented every time a standard end-of-line character is encountered: `\n`, `\r` or `\r\n`. 

* * * *

See also [[Parsing Levels]], [[Using the Parse Tree]] and [[Generating Code]]