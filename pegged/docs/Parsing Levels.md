Parsing Levels
==============

The **Pegged** docs mainly refer to the `.parse` static method, implemented in all rules and grammars. This method accepts an `Input` or any kind of string and returns an `Output`. But, sometimes, a full `Output` is a bit too much for what you need. If what you want is just the matches or validating the input (that is, seeing whether it's structured as the grammar defines it), why generate a parse tree?

For this need **Pegged** defines five different levels of parsing, members of the `ParseLevel` enum. This enum is silently passed as a template parameter for `parse` and any rule or combinator transmits the parse level to its child rules.

* `ParseLevel.validating`: discards any parse tree and match results (the captures). Only `success` is processed by the child rule and produced by the parser. In the end, as a user, the only info you get is whether or not the input was successfully parsed or not. Use it like this: 

```d
if (XML.parse!(ParseLevel.validating)(input).success)
{ 
    /*...*/ 
}
```

* `ParseLevel.matching`: keeps only `success` and `capture`. No parse tree is generated.

```d
assert(Arithmetic.parse!(ParseLevel.matching)(input).capture = ["2"d, "+"d, "3"d]);
```

* `ParseLevel.parsing` is the default **Pegged** state. Captures are collected, and a decimated parse tree is produced. As said in [[Tree Decimation]], only the nodes coming from the grammar rules are kept. Any external node is discarded, and recursively so for its children. Use the `^` (promote, aka keep) operator to locally disable node-cutting. See [[Extended PEG Syntax]].


* `ParseLevel.noDecimation`: like `.parsing` above, but no decimation is done. All nodes are kept, as long as they reach the rule. It can be useful to debug or if you do not like the way **Pegged** simplifies the parse trees. Be aware that parse trees generated this way can be quite large.

* `ParseLevel.fullest`: like `.noDecimation`, it does not discard any node and also disable any content-dropping operators, like `:` (`Drop`) or `~` (`Fuse`, which concatenates captures but also cut the children). If I eve add other 'discarding' operators, I'll have them do nothing if `ParseLevel.fullest` is passed as an argument.

Aliases
-------

Since typing `Grammar.parse!(ParseLevel.validating)(myInput)` can be a bit tedious, **Pegged** defines four different aliases to help you. These are all static methods in the grammars and rules, and can all accept `Input`s or any kind of strings.

* `validate`: equivalent to `parse!(ParseLevel.validating)`. 

* `match`: equivalent to `parse!(ParseLevel.matching)`.

* `fullParse`: equivalent to `parse!(ParseLevel.noDecimation)`.

* `fullestParse`: equivalent to `parse!(ParseLevel.fullest)`.

Of course `parse` itself is equivalent to `parse!(ParseLevel.parsing)`, the middle level.

Example:
--------

Let's define a small grammar that recognizes double-quote-delimited strings. See that the first call to `DoubleQuote` (a predefined parser recognizing `"`) is not dropped, while the second call uses the 'drop' (`:`) operator, to demonstrate the parsing levels:

```d
mixin(grammar(`
Test:
    A <- DoubleQuote ~Text :DoubleQuote
    Text <- (!DoubleQuote .)*
`));
```

If you test the five parsing levels, like this:

```d
void main()
{
    writeln(Test.validate(`"Hello, World!"`));
    writeln(Test.match(`"Hello, World!"`));
    writeln(Test.parse(`"Hello, World!"`));
    writeln(Test.fullParse(`"Hello, World!"`));
    writeln(Test.fullestParse(`"Hello, World!"`));
}
```

Then you'll get the following `Output`s:

For `validate`:

```
Parse output: success
named captures: []
position: [index: 15, line: 0, col: 15]
parse tree:
Pegged.Seq!(Drop, Fuse, Drop): [[index: 0, line: 0, col: 0] - [index: 15, line: 0, col: 15]][]
```

The only info left (or even computed) is that the input was successfully parsed, up to index 15 (which is the end).

**Bug**: hmm, OK, the rule should be called `Test.A`, not `Seq!(something)`.

For `match`, one gets the two matches: `"` and `Hello, World!`:

```
Parse output: success
named captures: []
position: [index: 15, line: 0, col: 15]
parse tree:
Pegged.Seq!(DoubleQuote, Fuse, Drop): [[index: 0, line: 0, col: 0] - [index: 15, line: 0, col: 15]]["\"", "Hello, World!"]
```

`parse` gives the standard **Pegged** parse tree: rule `Test.A` was called, along with its child `Test.Text`. No `DoubleQuote` node is left in the parse tree.

```
Parse output: success
named captures: []
position: [index: 15, line: 0, col: 15]
parse tree:
Test.A: [[index: 0, line: 0, col: 0] - [index: 15, line: 0, col: 15]]["\"", "Hello, World!"]
  Test.Text: [[index: 1, line: 0, col: 1] - [index: 14, line: 0, col: 14]]["Hello, World!"]
```

With `fullParse`, the intermediate call to an external rule like `Pegged.DoubleQuote` is kept:

```
Parse output: success
named captures: []
position: [index: 15, line: 0, col: 15]
parse tree:
Test.A: [[index: 0, line: 0, col: 0] - [index: 15, line: 0, col: 15]]["\"", "Hello, World!"]
  Pegged.Seq!(DoubleQuote, Fuse, Drop): [[index: 0, line: 0, col: 0] - [index: 15, line: 0, col: 15]]["\"", "Hello, World!"]
      Pegged.DoubleQuote: [[index: 0, line: 0, col: 0] - [index: 1, line: 0, col: 1]]["\""]
          Pegged.Lit!("): [[index: 0, line: 0, col: 0] - [index: 1, line: 0, col: 1]]["\""]
      Test.Text: [[index: 1, line: 0, col: 1] - [index: 14, line: 0, col: 14]]["Hello, World!"]
```

And finally, with `fullestParse`, the 'drop' and 'fuse' operators are disabled, giving this monster:

```
Parse output: success
named captures: []
position: [index: 15, line: 0, col: 15]
parse tree:
Test.A: [[index: 0, line: 0, col: 0] - [index: 15, line: 0, col: 15]]["\"", "Hello, World!", "\""]
  Pegged.Seq!(DoubleQuote, Fuse, Drop): [[index: 0, line: 0, col: 0] - [index: 15, line: 0, col: 15]]["\"", "Hello, World!", "\""]
      Pegged.DoubleQuote: [[index: 0, line: 0, col: 0] - [index: 1, line: 0, col: 1]]["\""]
          Pegged.Lit!("): [[index: 0, line: 0, col: 0] - [index: 1, line: 0, col: 1]]["\""]
      Test.Text: [[index: 1, line: 0, col: 1] - [index: 14, line: 0, col: 14]]["Hello, World!"]
          Pegged.ZeroOrMore!(Seq!(NegLookAhead, Any)): [[index: 1, line: 0, col: 1] - [index: 14, line: 0, col: 14]]["H", "e", "l", "l", "o", ",", " ", "W", "o", "r", "l", "d", "!"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 1, line: 0, col: 1] - [index: 2, line: 0, col: 2]]["H"]
                  Pegged.Any: [[index: 1, line: 0, col: 1] - [index: 2, line: 0, col: 2]]["H"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 2, line: 0, col: 2] - [index: 3, line: 0, col: 3]]["e"]
                  Pegged.Any: [[index: 2, line: 0, col: 2] - [index: 3, line: 0, col: 3]]["e"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 3, line: 0, col: 3] - [index: 4, line: 0, col: 4]]["l"]
                  Pegged.Any: [[index: 3, line: 0, col: 3] - [index: 4, line: 0, col: 4]]["l"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 4, line: 0, col: 4] - [index: 5, line: 0, col: 5]]["l"]
                  Pegged.Any: [[index: 4, line: 0, col: 4] - [index: 5, line: 0, col: 5]]["l"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 5, line: 0, col: 5] - [index: 6, line: 0, col: 6]]["o"]
                  Pegged.Any: [[index: 5, line: 0, col: 5] - [index: 6, line: 0, col: 6]]["o"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 6, line: 0, col: 6] - [index: 7, line: 0, col: 7]][","]
                  Pegged.Any: [[index: 6, line: 0, col: 6] - [index: 7, line: 0, col: 7]][","]
              Pegged.Seq!(NegLookAhead, Any): [[index: 7, line: 0, col: 7] - [index: 8, line: 0, col: 8]][" "]
                  Pegged.Any: [[index: 7, line: 0, col: 7] - [index: 8, line: 0, col: 8]][" "]
              Pegged.Seq!(NegLookAhead, Any): [[index: 8, line: 0, col: 8] - [index: 9, line: 0, col: 9]]["W"]
                  Pegged.Any: [[index: 8, line: 0, col: 8] - [index: 9, line: 0, col: 9]]["W"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 9, line: 0, col: 9] - [index: 10, line: 0, col: 10]]["o"]
                  Pegged.Any: [[index: 9, line: 0, col: 9] - [index: 10, line: 0, col: 10]]["o"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 10, line: 0, col: 10] - [index: 11, line: 0, col: 11]]["r"]
                  Pegged.Any: [[index: 10, line: 0, col: 10] - [index: 11, line: 0, col: 11]]["r"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 11, line: 0, col: 11] - [index: 12, line: 0, col: 12]]["l"]
                  Pegged.Any: [[index: 11, line: 0, col: 11] - [index: 12, line: 0, col: 12]]["l"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 12, line: 0, col: 12] - [index: 13, line: 0, col: 13]]["d"]
                  Pegged.Any: [[index: 12, line: 0, col: 12] - [index: 13, line: 0, col: 13]]["d"]
              Pegged.Seq!(NegLookAhead, Any): [[index: 13, line: 0, col: 13] - [index: 14, line: 0, col: 14]]["!"]
                  Pegged.Any: [[index: 13, line: 0, col: 13] - [index: 14, line: 0, col: 14]]["!"]
      Pegged.DoubleQuote: [[index: 14, line: 0, col: 14] - [index: 15, line: 0, col: 15]]["\""]
          Pegged.Lit!("): [[index: 14, line: 0, col: 14] - [index: 15, line: 0, col: 15]]["\""]
```

I mean, 34 nodes for such a simple input and such a simple grammar...
