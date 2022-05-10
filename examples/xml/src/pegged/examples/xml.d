/**
 * Ultra-simple XML grammar
 */
module pegged.examples.xml;

import pegged.grammar;

import std.array;

string[] nameStack;

/// Semantic action to push a tag name on a stack
O opening(O)(O o)
{
    if (o.successful)
        nameStack ~= o.matches;
    return o;
}

/**
 * Semantic action to pop the name stack and compare a tag name
 * The parsing fails if the tags do not nest correctly (See docs, the Semantic Actions wiki page)
 */
O closing(O)(O o)
{
    if (o.matches.empty || nameStack.back != o.matches[0])
        o.successful = false;
    else
        nameStack.popBack();
    return o;
}

/**
 * Semantic action to flush the name stack
 */
O flush(O)(O o)
{
    nameStack = null;
    return o;
}

mixin(grammar(`
XML:
    Node        <- OpeningTag{opening} (Node / Text)* ClosingTag{closing}
    OpeningTag  <- :"<" identifier :">"
    ClosingTag  <- :"</" identifier :">"
    Text        <~ (!OpeningTag !ClosingTag .)+
`));

unittest
{
    auto p1 = XML("<a></a>");
    assert(p1.successful);
    assert(p1.matches == ["a", "a"]);
    assert(p1.children[0].children.length == 2);

    assert(p1.children[0].name == "XML.Node");
    assert(p1.children[0].children[0].name == "XML.OpeningTag");
    assert(p1.children[0].children[1].name == "XML.ClosingTag");

    assert(!XML("<b></>").successful); // incomplete closing tag
    assert(!XML("<c></d>").successful); // unmatched tag
    assert( XML("<e><f></f></e>").successful); // OK
    assert(!XML("<g><h></g></h>").successful); // badly enclosed tags

    auto p2 = XML("<text>Hello World! This is an <emph>important</emph> announcement!</text>");
    assert(p2.successful);

    assert(p2.matches == ["text", "Hello World! This is an ", "emph", "important", "emph", " announcement!", "text"]);
    assert(p2.children[0].children[0].matches == ["text"]);
    assert(p2.children[0].children[1].matches == ["Hello World! This is an "]);
    assert(p2.children[0].children[$-1].matches == ["text"]);
}
