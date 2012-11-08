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
import pegged.examples.xml;

void main()
{
    auto p1 = XML("<a>Test<b>B</b></a>");
    writeln(p1);
    writeln(pegged.examples.xml.nameStack);
}
