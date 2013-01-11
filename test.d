/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.functional;
import std.range;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.grammar;

enum g = grammar("Test:
    A <- 'a'
");

void main()
{
    mixin(g);
    writeln(Test("b"));
    Test.ABefore = &literal!"b";
    writeln(Test("b"));
}

