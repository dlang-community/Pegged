Using a Grammar
===============

To use a grammar in **Pegged**, just call it with a string:

```d
// Continuing with the previous example:
enum input = [
"01-01-2000 abc 109203 AGT",
"01-01-2000 def 1910 ACGAT AGGGAT CGA",
"01-01-2000 def 18629 ACGTTTT GGGGAT",
"02-01-2000 error 0000"
];

void main()
{
    foreach(entry; input)
        writeln(Input(entry));
}
```

`Input` is the name of the grammar. The first rule is the start rule
is the name of the master parser, the one used to call all other rules in the grammar.

Compile-Time Parsing
--------------------

**Pegged** can parse input strings and generate a parse tree at compile time. Just force compile-time evaluation like you'd do for any other D value: either by defining them as an `enum` or by using them as a template parameter.

```d
enum firstLine = line(input[0]);
pragma(msg, firstLine.matches); // CT message
```

As you can see in the above example, the parsing, matching, input deconstruction and parse tree assembly are all done at compile-time without any problem, thanks to D powerful CT evaluation capabilities. See [[Generating Code]] for what can be done with it.

The downside is that it's much slower than runtime parsing (between one and two orders of magnitude slower, in my own tests). But then, it's already calculated once the code is run, so the question is more: 'where do you spend more time? Compiling or running?'.

Runtime Parsing
---------------

To parse at runtime, just call the grammar in a standard way:

```d
ParseTree p = line(input[0]);
```

Calling a Rule
--------------

Though this is not usual, you can call a grammar rule directly, by qualifying its name with the grammar's name.

```
ParseTree p = Input.date("01-01-1970");

writeln(p);
```

Which prints:

```
Input.date  [0, 10]["0", "1", "-", "0", "1", "-", "1", "9", "7", "0"]
 +-Input.digit  [0, 1]["0"]
 +-Input.digit  [1, 2]["1"]
 +-literal(-)  [2, 3]["-"]
 +-Input.digit  [3, 4]["0"]
 +-Input.digit  [4, 5]["1"]
 +-literal(-)  [5, 6]["-"]
 +-Input.digit  [6, 7]["1"]
 +-Input.digit  [7, 8]["9"]
 +-Input.digit  [8, 9]["7"]
 +-Input.digit  [9, 10]["0"]
```

First, this works (wew!).

Second, by using the 'concatenate' operator (`~`) in `date` definition, we would have a more readable and usable result (namely `Input.date  [0,10]["01-10-1970"]`). PEG extensions are described in [[Extended PEG Syntax]].

Then, please notice how the parse tree contains nodes from the `Input` grammar (`Input.digit`) and nodes from **Pegged** (`literal`). If you want to get a standard, decimated parse tree, call the grammar's `decimateTree` method:

```
ParseTree p = Input.decimateTree(Input.date("01-01-1970")));
writeln(p);
```

Which prints:

```
Input.date  [0, 10]["0", "1", "-", "0", "1", "-", "1", "9", "7", "0"]
 +-Input.digit  [0, 1]["0"]
 +-Input.digit  [1, 2]["1"]
 +-Input.digit  [3, 4]["0"]
 +-Input.digit  [4, 5]["1"]
 +-Input.digit  [6, 7]["1"]
 +-Input.digit  [7, 8]["9"]
 +-Input.digit  [8, 9]["7"]
 +-Input.digit  [9, 10]["0"]
```

There is another lesson on [[Tree Decimation]].

* * * *

Next step: [[Memoization]]

* * * *

[[Pegged Tutorial]]
