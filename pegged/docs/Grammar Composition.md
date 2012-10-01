Grammar Composition
===================

As seen in [[Using a Grammar]], a grammar is a standard **Pegged** parser. That means you can call a grammar in another grammar, as you do for the predefined parsers. In fact, a grammar is nothing but a parser with a lot of plumbing inside. That opens whole vistas for modularization and code re-use. If you see in a grammar any functionality that could be extracted and stand by itself, make it another grammar.

Have a look for example at the `pegged.examples.strings` and `pegged.examples.numbers` modules. They define grammars that recognize respectively double-quote-delimited strings and standard integer and floating point numbers. To use them in your own grammar definitions, just import the modules to make the symbols visible.

```d
module my.grammar;

import pegged.grammar;
import pegged.examples.strings, pegged.examples.numbers;

mixin(grammar(`
LOG:
    LogFile <- LogLine+ eoi
    LogLine <  String ':' Numbers (',' Numbers)* eol?
`));

void main()
{
	auto log = 
"File1: 0.00, 0.01, 0.00, 0.00
File2: 1.0, 2.0, 3.14
File3: 0.00, 10";

	writeln(LOG(log));
}
```

`Numbers` also defines an `Hexa` rule to recognize hexadecimal numbers. It's never invoked directly by `Numbers` itself, but, as seen in [[Using a Grammar]], it can be called by qualifiying its name: `Numbers.Hexa("A73FEC384CBB")`. If two grammars define a rule with the same name, you can also use their qualified names to distinguish them. **Pegged** allows rule invokation (the rhs in a rule definition) to contain qualified identifiers:

```d
module my.grammar;

import std.stdio;
import pegged.grammar;
import pegged.examples.strings, pegged.examples.numbers;

enum g = grammar(`
LOG:
    LogFile <- LogLine+ EOI
    LogLine < String Numbers.Hexa ':' Numbers (',' Numbers)*
`);
mixin(g);

enum log = 
`"File1" 123AC7AF   : 123, 78.265, 0.00
"File2" 31F039DC9BE : 49.45, 42.220, 0.02, -22.3
"File3" D0043869930 : 0
`;

void main()
{
    writeln(LOG(log));
}
```

**Pegged** does not allow rule *names* (lhs in a definition) to be qualified. 

* * * *

Next Lesson: [[Using the Parse Tree]].

* * * *

[[Pegged Tutorial]]
          
