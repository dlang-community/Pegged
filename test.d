module test;

import std.algorithm;
import std.array;
import std.conv;
import std.range;
import std.stdio;
import std.typecons;

import pegged.grammar;

enum code = (grammar(`
TEST:
A <- 'a'*
`));

mixin(code);

void main()
{
    writeln(TEST.parse("aaa bbb"));
}


