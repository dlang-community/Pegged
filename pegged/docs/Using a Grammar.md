Using a Grammar
===============

To use a grammar in **Pegged**, use the static `.parse` method, present in all parser (in a way, defining that something is a **Pegged** parser):

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
        writeln(line.parse(entry));
}
```

`line` is the name of the master parser, the one used to call all other rules in the grammar.

Compile-Time Parsing
--------------------

**Pegged** can parse input strings and generate a parse tree at compile time. Just force compile-time evaluation like you'd do for any other D value: either by defining them as an `enum` of by using them as a template parameter.

```d
enum firstLine = line.parse(input[0]);
pragma(msg, firstLine.capture); // CT message
```

As you can see in the above example, the parsing, capture, input deconstruction and parse tree assembly are  all done at compile-time without any problem, thanks to D powerful CT evaluation capabilities. See [[Generating Code]] for what can be done with it.

The downside is that it's much slower than runtime parsing (more than one order of magnitude slower, in my own tests). But then, it's already calculated once the code is run, so the question is more: 'where do you spend more time? Compiling or running?'.

Runtime Parsing
---------------

To parse at runtime, just call `parse` in a standard way:

```d
auto p = line.parse(input[0]);
```

Next step: using [[Grammars as D Modules]]

* * * *

[[Pegged Tutorial]]