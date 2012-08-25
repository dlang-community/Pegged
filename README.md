PEGGED
======

**Pegged** is a parsing expression grammar (PEG) generator implemented in the D programming language. 

**This is the README file for the 'speedup1' branch**. I'll highlight here the differences with the 'master' branch.

Speedup1 Branch
---------------

The main difference for the end-user is that the global parsing (and hence, grammar generation) speed is one-to-two orders of magnitude faster. I regularly obtain speed-ups of x30 to x50, while comparing the master branch and this one. This is true for runtime parsing and also for compile-time parsing. All in all, it makes for a much-improved user experience.

Most of **Pegged** features exist in this branch:

- Just use `mixin(grammar(" you grammar here "));` to create the parser in your code.
- The standard PEG syntax is used.
- Most usual **Pegged** syntax extensions are there: fusing matches (`~`), discarding nodes (`:`), keeping them (`^`), and so on.
- Semantic actions are there. They are just functions from `ParseTree` to `ParseTree`, like rules and grammars. There is a certain elegance in that.
- Parametrized rules and parametrized grammars are there also. With default arguments, too.
- Grammar composition is done as before: just call the grammar by its name from another grammar. Or call a specific rule with the `grammarName.ruleName` syntax.

- A very nice side effect is that the generated C parser and D parser now work: they compile and parse alright, for the (admittedly limited) tests I've done. The D grammar is still as gigantic as ever, to be prepared for some truly big parse trees. 

Some parts have been simplified: 

- to parse with a grammar, just call it like a function. In fact, most predefined rules and a grammar inner rules are just functions now.
- rules take a `ParseTree` or a `string` and return a `ParseTree`.

Some slight changes:

- The `capture` field is now called `matches`, since it corresponds much better to what it is.
- Grammar *must* be named. That alone allowed for a nice simplication in the generating code.
- A new operator, `;` is used to drop a node from the parse tree, but keep its matches. To be compared to the `:` operator that completly erases the node (no matche is kept).
- behind the scene, the code is vastly simplified and the generated parsers are far smaller. It's still just a top-down recursive-descent parser, no GLR or LALR(1).
- As rules are functions, I renamed the predefined rules to be in lower caps (`identifier`, `doublequote`, `backslash`, etc). I'll have to make a pass on this again to polish this a bit more.

Some missing features:

- No `wstring` or `dstring` parsing for now. Everything is down-to-earth `string`.
- Positions of matches are just two indices (begin-end): no line/column counting for now. That might very well partially explain the speed-up...
- Since this branch was entirely rewritten from scratch, I didn't add (for now) the parametrization on the parse tree's type that was added by *chadjoan*.
- No named captures (operators `=` and `@` in master branch). I do not use them much, so they are not high priority for me. Also, dropping them drastically simplified the code. Use semantic actions.
- No level of parsing (from simple recognizing to full non-decimated parse tree). Only a standard parse tree is created.
- No level of encapsulation. All grammars are called by their names and their inner rules are called by `grammarName.ruleName`, if needed.
- Rules nodes are named by their rule name (`"Expr"`), not their full, qualified, grammar.rule name (`"Arithmetic.Expr"`). That will probably change, though.

- Not all examples were recompiled with the new engine. I'll convert them as time permits. For now, on this branch, there is `Arithmetic`, `JSON`, `XML` (not `XML2.d`), `C`and `D`. Checking the rest is on my todo list. Most of the time, it's just changing predefined rule names and some testing code.

All in all, the vastly improved parsing speed re-ignited my interest in **Pegged**, so stay tuned!

Future features:
----------------

- 'hooked' rules: rules that can be extended at runtime, or modified. I want to have a D parser that accepts syntactic extensions. I have these hooked rules, they work, but the end-user code is not as polished as I'd like.
- parsing a range.
- converting/checking all examples.
- a function that reconstitutes D code from a parse tree.
- tree functions (at least filtering, reducing and matching on trees).
- D code lowerings.

License
-------

**Pegged** is released with the Boost license (like most D projects). See [here](http://www.boost.org/LICENSE_1_0.txt) for more details.
