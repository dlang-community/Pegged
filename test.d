/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.grammar;
import pegged.parser;

mixin(grammar(`
R:
    ArgList <- '(' List(identifier, ',') ')'
    List(Elem, Sep) <  Elem (Sep Elem)*
`));

void main()
{
    writeln(R("(abc,def,g,h)"));
}
