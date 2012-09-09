Parse Trees
===========

In **Pegged**, a parse tree is a very simple recursive struct:

```d
struct ParseTree
{
    string name;
    bool success;
    string[] matches;
    
    string input;
    size_t begin, end;
    ParseTree[] children;
// toString()
}
```

`name` contains the qualified name of the rule that created the node: "Arithmetic.Expr", "Input.line", etc. It's useful to switch on nodes to generate D code, see [[Generating Code]]. The `grammarName.ruleName` was chosen to help disambiguate nodes created by different parsers. Predefined rules have a simple name, like `digit` or `identifier`.

`success` indicates whether or not the parse was successful.

`matches` contains the matches. It's a `string[]` and not a `string` since a complete parsing expression can produce many matches. For examples `a*` will contain all successive matches of rule `a`, which may well be different at each call.

`input` is just the input string, to allow for easy slicing and to make a `ParseTree` a valid input for another rule: the input string is already included.

`begin` and `end` represent indices in the input. `input[begin..end]` is the part that was matched while parsing.

Finally, `children` contains an array of component parse results. In general, child expressions in a parsing expression will all contribute their mother node parse result. When defining the parsing expression `a b c`, its parse result will contain a parse result for `a`, another for `b` and another for `c`. The standard aggregating **Pegged** parsers all duplicate their children's matches in their main `matches` member, so as not to force the user to explore the entire tree to get the parsed substrings.

For example:

```d

mixin(grammar("
Identifiers:
    Ids <- (identifier :Spacing)* # ':Spacing' is cut from the parse tree
"));

auto input = "This is a sentence.";
auto words = Ids(input); // 

assert(words.matches == ["This", "is", "a", "sentence"]);
```

In the previous example, `Identifiers` is a single parser that can produce multiple matches.

* * * *

See also [[Using the Parse Tree]] and [[Generating Code]]