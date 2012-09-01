/**
 * Recreate the Pegged parser from the grammar.
 */
module pegged.regenerate;

import pegged.grammar;
import pegged.examples.peggedgrammar;

void main()
{
    asModule!(Memoization.no)("pegged.parser", "pegged/parser", PEGGEDgrammar);
}