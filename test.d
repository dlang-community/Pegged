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
import pegged.examples.json;


void main()
{
    int N = 1000;
    auto b = benchmark!(()=>JSON(example4))(N);
    writeln(b[0].to!("msecs", float)/N, " msecs/call.");
}
