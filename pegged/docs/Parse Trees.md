Parse Trees
===========

In **Pegged**, a parse tree is a very simple recursive struct:

```d
struct ParseTree
{
    string grammarName;
    string ruleName;
    bool success;
    string[] capture;
    ParseTree[] children;
// toString()
}
```

`grammarName` and `ruleName` contain respectively the name of the grammar and the name of the rule that created the node: "Expr", "line", etc. There is a `ParseTree` property called `.name` which returns `grammarName ~ "." ~ ruleName`. It's useful to switch on nodes to generate D code, see [[Generating Code]].

`success` indicates whether or not the parse was successful.

`capture` contains the matches. It's a `string[]` and not a `string` since a complete parsing expression can produce many matches. For examples `a*` will contain all the successive matches of rule `a`, which may well be different at each call.

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

Finally, `children` contains an array of component parse results. In general, child expressions in a parsing expression will all contribute their mother node parse result. When defining the parsing expression `a b c`, its parse result will contain a parse result for `a`, another for `b` and another for `c`. The standard aggregating **Pegged** parsers all duplicate their children's captures in their main `capture` member, so as not to force the user to explore the entire tree to get the parsed substrings.

See also [[Using the Parse Tree]] and [[Generating Code]]