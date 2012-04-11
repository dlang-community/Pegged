Grammar Composition
===================

As seen in [[Using a Grammar]], a grammar is a standard **Pegged** parser with a `.parse( )` static method. That means you can call a grammar in another grammar, as you do for the predefined parsers. In fact, a grammar is nothing but a parser with a lot of plumbing inside. That opens whole vistas for modularization and code re-use. If you see in a grammar any functionality in a grammar that could be extracted and stand by itself, make it another grammar.

Have a look for example at the `pegged.examples.strings` and `pegged.examples.numbers` modules. The define grammars that recognize respectively double-quote-delimited strings and standard integer and floating point numbers. To use them in your own grammar definitions, just import the modules to make the symbols visible.

```d
module my.grammar;

import pegged.grammar;
import pegged.examples.strings, pegged.examples.numbers;

mixin(grammar(`
LOG:
    LogFile <- LogLine+ EOI
    LogLine <  String ':' Numbers (',' Numbers)* EOL?
`));

void main()
{
	auto log = 
"File1: 0.00, 0.01, 0.00, 0.00
File2: 1.0, 2.0, 3.14
File3: 0.00, 10";

	LOG.parse(log);
}
```

`Numbers` also define a `Hexa` rule to recognize hexadecimal numbers. It's never invoked directly by `Numbers` itself, but can be called by qualifiying its name: `Numbers.Hexa.parse("A73FEC384CBB")` (see [[Four Levels of Encapsulation]]). If two grammars define each a rule with the same name, you can also use their qualified names to distinguish them. **Pegged** allow rule invokation (the rhs in a rule definition) to contain qualified identifiers:

```d
module my.grammar;

import pegged.grammar;
import pegged.examples.strings, pegged.examples.numbers;

mixin(grammar(`
LOG:
    LogFile <- LogLine+ EOI
    LogLine <  String '(' Numbers.Hexa ')' ':' (',' Numbers)* EOL?
`));

void main()
{
	auto log = 
"File1(000A): 0.00, 0.01, 0.00, 0.00
File2(31F0): 1.0, 2.0, 3.14
File3(D004): 0.00, 10";

	LOG.parse(log);
}
```

**Pegged** does not allow rule *names* (lhs in a definition) to be qualified. 

