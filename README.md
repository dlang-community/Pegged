PEGGED
======

**Pegged** is a parsing expression grammar (PEG) generator implemented in the D programming language.

The idea is to give the generator a [PEG](http://en.wikipedia.org/wiki/Parsing_expression_grammar), with the syntax presented in [the reference article ](http://bford.info/pub/lang/peg). From this grammar definition a set of related parsers will be created, to be used at runtime or compile time.

Usage
-----

To use **Pegged**, just call the `grammar` function with a PEG and mix it in. For example:


```d
import pegged.grammar;

mixin(grammar(`
Arithmetic:
    Term     < Factor (Add / Sub)*
    Add      < "+" Factor
    Sub      < "-" Factor
    Factor   < Primary (Mul / Div)*
    Mul      < "*" Primary
    Div      < "/" Primary
    Primary  < Parens / Neg / Pos / Number / Variable
    Parens   < "(" Term ")"
    Neg      < "-" Primary
    Pos      < "+" Primary
    Number   < ~([0-9]+)

    Variable <- identifier
`));
```

This creates the `Arithmetic` grammar, with the `Expr`, `Add`, `Factor` (and so on) rules for basic arithmetic expressions with operator precedence ('*' and '/' bind stronger than '+' or '-'). `identifier` is a pre-defined parser recognizing your basic C-style identifier (first a letter or underscore, then digits, letters or underscores). In the rest of this document, I'll call 'rule' a `Parser <- Parsing Expression` expression and I'll use 'grammar' to designate the entire group of rules given to `grammar`.

To use a grammar, call it with a string. It will return a parse tree containing the calls to the different rules:

```d
// Parsing at compile-time:
enum parseTree1 = Arithmetic("1 + 2 - (3*x-5)*6");

pragma(msg, parseTree1.matches);
assert(parseTree1.matches == ["1", "+", "2", "-", "(", "3", "*", "x", "-", "5", ")", "*", "6"]);
writeln(parseTree1);

// And at runtime too:
auto parseTree2 = Arithmetic(" 0 + 123 - 456 ");
assert(parseTree2.matches == ["0", "+", "123", "-", "456"]);
```

Even for such a simple grammar and such a simple expression, the resulting parse tree is a bit long to be shown here. See [the result here](https://github.com/PhilippeSigaud/Pegged/wiki/Parse-Result)

By default, the grammars do not silently consume spaces, as this is the standard behaviour for PEGs. There is an opt-out though, with the simple `< ` arrow instead of `<-` (you can see it in the previous example).

How to get Pegged
-----------------

Pegged is a github project, hosted at <https://github.com/PhilippeSigaud/Pegged>

To get it:

```bash
$ git clone https://github.com/PhilippeSigaud/Pegged
```

The `/docs` directory contains an empty `/wiki` directory, linked to the github wiki as a git submodule.
Here is how to get it:

```bash
$ cd <pegged directory>
$ git submodule init
$ git submodule update
```

This should give you a `/docs/wiki` directory full of markdown files, right from the online wiki.

Tutorial and docs
-----------------

The **Pegged** wiki is [here](https://github.com/PhilippeSigaud/Pegged/wiki/). It contains a growing [tutorial](https://github.com/PhilippeSigaud/Pegged/wiki/Pegged-Tutorial). All the wiki pages are also present (as Markdown files) in the `docs` directory.

Features
--------

* The complete set of operators described [here](http://en.wikipedia.org/wiki/Parsing_expression_grammar) are implemented, with the 'traditional' PEG syntax. See [Peg Basics](https://github.com/PhilippeSigaud/Pegged/wiki/PEG-Basics).
* **Pegged** can parse its input at compile time and generate a complete parse tree at compile time. In a word: compile-time string (read: D code) transformation and generation. See [Generating Code](https://github.com/PhilippeSigaud/Pegged/wiki/Generating-Code) for example.
* You can parse at runtime also, you lucky you. ([Using the Parse Tree](https://github.com/PhilippeSigaud/Pegged/wiki/Using-the-Parse-Tree))
* Use a standard and readable PEG syntax as a DSL, not a bunch of templates that hide the parser in noise.
* But you can use expression templates if you want, as parsers are all available as such. **Pegged** is implemented as an expression template, and what's good for the library writer is sure OK for the user too. ([Behind the Curtain: How Pegged Works](https://github.com/PhilippeSigaud/Pegged/wiki/Behind-the-Curtain%3A-How-Pegged-Works)
* Some useful additional operators are there too: a way to discard matches (thus dumping them from the parse tree), to push captures on a stack, to accept matches that are equal to another match: see [PEG Additions](https://github.com/PhilippeSigaud/Pegged/wiki/Extended-PEG-Syntax).
* Adding new parsers is easy. See [User-Defined Parsers](https://github.com/PhilippeSigaud/Pegged/wiki/User-Defined-Parsers) to see how to do that.
* Grammars are composable: you can put different `mixin(grammar(rules));` in a module and then grammars and rules can refer to one another. That way, you can have utility grammars providing their functionalities to other grammars. [Grammar Composition](https://github.com/PhilippeSigaud/Pegged/wiki/Grammar-Composition)
* That's why **Pegged** comes with some pre-defined grammars (JSON, C, XML, CSV, D, the PEG grammar itself, etc). See [Grammar Examples](https://github.com/PhilippeSigaud/Pegged/wiki/Grammar-Examples).
* Grammars can be dumped in a file to create a module. Use the `asModule(string moduleName, string gram)` function in `pegged.grammar` to do that. See [Grammars as Modules](https://github.com/PhilippeSigaud/Pegged/wiki/Grammars-as-D-Modules).

More advanced features, outside the standard PEG perimeter are there to bring more power in the mix:

* **Parametrized rules**: `"List(E, Sep) <- E (Sep E)*"` is possible. The previous rule defines a parametrized parser taking two other parsers (namely, `E` and `Sep`) to match a `Sep`-separated list of `E`'s.  Entire grammars can be parametrized, too. See [Parametrized Rules](https://github.com/PhilippeSigaud/Pegged/wiki/Parametrized-Rules) to see what's possible.
* **Semantic actions** can be added to any rule in a grammar. Once a rule has matched, its associated action is called on the rule output and passed as final result to other parsers further up the grammar. Do what you want to the parse tree. If the passed actions are delegates, they can access external variables. See [Semantic Actions](https://github.com/PhilippeSigaud/Pegged/wiki/Semantic-Actions).

References
----------

Articles:

* [The base PEG article from Bryan Ford](http://bford.info/pub/lang/peg).
* [Packrat parsing](http://pdos.csail.mit.edu/~baford/packrat/icfp02/).
* [OMeta](http://www.vpri.org/pdf/tr2007003_ometa.pdf).

D Code:

* Hisayuki Mima's [CTPG](https://github.com/youkei/ctpg), very similar, also done in D. Have a look!
* Nick Sabalausky's [Goldie](http://www.dsource.org/projects/goldie).
* Benjamin Shropshire's [dparser](http://dsource.org/projects/scrapple/browser/trunk/dparser).
* Martin Nowak put these gists on the D newsgroup:
    - https://gist.github.com/1255439 - lexer generator
    - https://gist.github.com/1262321 - complete and fast D lexer

Other languages:

* [pegtl](http://code.google.com/p/pegtl/), the PEG Template Library, in C++.
* [chilon::parser](http://chilon.net/library.html) in C++ also.
* [metaparse](http://abel.web.elte.hu/mpllibs/metaparse/index.html), in C++, is able to parse at compile-time.
* [Parslet](http://kschiess.github.com/parslet/) in Ruby and [Treetop](http://treetop.rubyforge.org/), in Ruby also.

License
-------

**Pegged** is released with the Boost license (like most D projects). See [here](http://www.boost.org/LICENSE_1_0.txt) for more details.
