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
    Recursive:
        Rule1 <- 'a' Rule2
        Rule2 <- Rule1?
`));

void main()
{
    writeln(Pegged("Recursive:
        Rule1 <- 'a' Rule2
        Rule2 <- Rule1?"));
}
