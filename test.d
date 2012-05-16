module test;

import std.algorithm;
import std.array;
import std.conv;
import std.range;
import std.stdio;
import std.typecons;

import pegged.grammar;

mixin(grammar(`Number < [0-9]*`));
enum result1 = Number.parse("123");


void main()
{
    writeln("CT: ", result1);
    writeln("RT: ", Number.parse("123"));
}


