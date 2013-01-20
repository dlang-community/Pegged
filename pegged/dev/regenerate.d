/**
 * Recreate the Pegged parser from the grammar.
 */
module pegged.dev.regenerate;

import pegged.grammar;
import pegged.examples.peggedgrammar;
import pegged.examples.testergrammar;

void main()
{
    asModule!(Memoization.no)("pegged.parser", "../parser", PEGGEDgrammar);
    asModule!(Memoization.no)("pegged.tester.testerparser", "../tester/testerparser", testerGrammar);
}
