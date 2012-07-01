/**
 * Ultra-simple XML grammar
 */
module pegged.examples.xml;

import pegged.grammar;

import std.array;
dstring[] nameStack;

//alias pegged.peg.Output!(ParseTree) Output;

/// Semantic action to push a tag name on a stack
O opening(O)(O o)
{
    nameStack ~= o.capture[0];
    return o;
}

/** 
 * Semantic action to pop the name stack and compare a tag name
 * The parsing fails if the tags do not nest correctly (See docs, the Semantic Actions wiki page)
 */ 
O closing(O)(O o)
{
    if (nameStack.back != o.capture[0])
        o.success = false;
    else
        nameStack.popBack();
    return o;
}

mixin(grammar(`
XML:
    Node       <- OpeningTag{opening} (Node / Text)* ClosingTag{closing}
    OpeningTag <- :"<" Identifier :">" 
    ClosingTag <- :"</" Identifier :">"
    Text       <~ (!OpeningTag !ClosingTag .)+
`));

unittest
{
    import std.stdio;

    auto p1 = XML.parse("<a></a>");
    assert(p1.success);
    assert(p1.capture == ["a"d, "a"d]);
    assert(p1.children.length == 2);
    
    assert(p1.name == "XML.Node"d);
    assert(p1.children[0].name == "XML.OpeningTag"d);
    assert(p1.children[1].name == "XML.ClosingTag"d);

    assert(!XML.parse("<a></>").success); // incomplete closing tag
    assert(!XML.parse("<a></b>").success); // unmatched tag
    assert( XML.parse("<a><b></b></a>").success); // OK
    assert(!XML.parse("<a><b></a></b>").success); // badly enclosed tags
    
    auto p2 = XML.parse("<text>Hello World! This is an <emph>important</emph> announcement!</text>");
    assert(p2.success);
    
/*    
    parse tree:
XML: [[index: 0, line: 0, col: 0] - [index: 73, line: 0, col: 73]]["text", "Hello World! This is an ", "emph", "important", "emph", " announcement!", "text"]
  Node: [[index: 0, line: 0, col: 0] - [index: 73, line: 0, col: 73]]["text", "Hello World! This is an ", "emph", "important", "emph", " announcement!", "text"]
      OpeningTag: [[index: 0, line: 0, col: 0] - [index: 6, line: 0, col: 6]]["text"]
      Text: [[index: 6, line: 0, col: 6] - [index: 30, line: 0, col: 30]]["Hello World! This is an "]
      Node: [[index: 30, line: 0, col: 30] - [index: 52, line: 0, col: 52]]["emph", "important", "emph"]
          OpeningTag: [[index: 30, line: 0, col: 30] - [index: 36, line: 0, col: 36]]["emph"]
          Text: [[index: 36, line: 0, col: 36] - [index: 45, line: 0, col: 45]]["important"]
          ClosingTag: [[index: 45, line: 0, col: 45] - [index: 52, line: 0, col: 52]]["emph"]
      Text: [[index: 52, line: 0, col: 52] - [index: 66, line: 0, col: 66]][" announcement!"]
      ClosingTag: [[index: 66, line: 0, col: 66] - [index: 73, line: 0, col: 73]]["text"]
*/      
    assert(p2.capture == ["text"d, "Hello World! This is an "d, "emph"d, "important"d, "emph"d, " announcement!"d, "text"d]);
    assert(p2.children[0].capture == ["text"d]);
    assert(p2.children[1].capture == ["Hello World! This is an "d]);
    assert(p2.children[$-1].capture == ["text"d]);
}