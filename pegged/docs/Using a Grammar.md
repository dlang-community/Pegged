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
auto p = line(input[0]);
```

Next step: [[Memoization]]

* * * *

[[Pegged Tutorial]]