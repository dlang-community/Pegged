Pegged TODO:
============

Short term:

TODO: doc update to explain the modifications introduced by the speedup1 branch.
(for example, a new operator, `;` is used to drop a node from the parse tree, but keep its matches. To be compared to the `:` operator that completly erases the node (no matche is kept).)
TODO: update docs to explain spacing can be user-defined (spaceAnd will call it)
TODO: see what name to keep for the predefined rules (EOI, eoi, endOfInput?)
TODO: qualified names for rules (grammarName.ruleName)
TODO: Directly reading a grammar from a file.
TODO: Explain the tree decimation rules.

Long term:

TODO: Better error report: explaining what was expected "rule Digit failed at XXX, expected '0'-'9'"
TODO: Add a debug mode that logs everything a parser does: which rule was tested by sequences and choices, which was unsuccessful, and so on.
TODO: add an enum inside ParseTree's, containing the rules's name, to enable final switch selection
TODO: Add a boolean grammar (&&, ||, !) and a comparison grammar( ==, !=, <, >, <=, >=), calling arithmeric expressions.
TODO: inlining
TODO: option infastructure (easier for the user: grammar!(memoizing, inlining, debug))
TODO: import expressions in grammars?
TODO: rules parameterized on numbers
TODO: external rules, introduced by < name > or automatically recognized.
TODO: direct D code, with {{ }} ? Or use < > for actions and { } for D code
TODO: [ ] after rule names and grammars, for options. 
TODO: memoization as a parse-time option and not a generation-time option
TODO: new options: logging, debugging, inlining
TODO: adding [ ] after a grammar name for importing other grammars.
TODO: another option for grammars, coupled to external rules: creating inner variables.
TODO: drop the static methods and try standard methods?
