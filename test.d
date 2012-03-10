module pegged.test;

import std.algorithm;
import std.conv;
import std.stdio;
import std.traits;
import std.typecons;
import std.typetuple;

import pegged.grammar;


import std.array;
string[] nameStack;

Output opening(Output o)
{
    nameStack ~= o.capture[0];
    return o;
}

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

mixin(grammar(`

List(Elem, Sep) <- Elem (Sep Elem)*
List(Elem)      <- List(Elem, :',')
`));

void main()
{
    auto p = Node.parse("<a> Hello <b> World </b> ! </a>");
    writeln(p);
    writeln(nameStack);
    
    writeln(List!Identifier.parse("Hello,World,!"));
}