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
import pegged.grammartester;

import std.uni: isAlpha;

ParseTree log(ParseTree p)
{
    writeln("called on ", p.name);
    return p;
}

mixin(grammar(`Test:
    A <{log, log} 'a' { log }
    `));

void main()
{
    writeln(Test("abc"));
}

