/**
 * Ultra-simple XML grammar
 */
module pegged.examples.xml;

import pegged.grammar;

import std.array;
string[] nameStack;

/// Semantic action to push a tag name on a stack
Output opening(Output o)
{
    nameStack ~= o.capture[0];
    return o;
}

/** 
 * Semantic action to pop the name stack and compare a tag name
 * The parsing fails if the tags do not nest correctly (See docs, the Semantic Actions wiki page)
 */ 
Output closing(Output o)
{
    if (nameStack.back != o.capture[0])
        o.success = false;
    else
        nameStack.popBack;
    return o;
}

mixin(grammar(`    
    Node       <- OpeningTag{opening} (Node / Text)* ClosingTag{closing}
    OpeningTag <- :"<">Identifier>:">" 
    ClosingTag <- :"</">Identifier>:">"
    Text       <~ (!OpeningTag>!ClosingTag>.)*
`));
