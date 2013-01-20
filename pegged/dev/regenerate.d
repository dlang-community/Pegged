/**
 * Recreate the Pegged parser from the grammar.
 */
module pegged.regenerate;

import pegged.grammar;
import pegged.examples.peggedgrammar;
import pegged.examples.testergrammar;

void main()
{
    asModule!(Memoization.no)("pegged.parser", "pegged/parser", PEGGEDgrammar);
    asModule!(Memoization.no)("pegged.testerparser", "pegged/testerparser", testerGrammar);
}
