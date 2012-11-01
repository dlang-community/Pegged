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
import pegged.introspection;

string g =
`
    Recursive:
        A <- B 'a' / eps
        B <- A C
        C <- 'c'
`;

void main()
{
    writeln(ruleInfo(g));
    //writeln(Pegged(g));
}
