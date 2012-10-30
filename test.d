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

mixin(grammar!(Memoization.yes)(`
Test:
    Rule1 <- 'a' Rule2
    Rule2 <- 'b'
`));

void main()
{
    writeln(Test("ab"));
    enum Success = Test("ab");
    writeln("enum:\n",Success);
    pragma(msg, "Pragma:\n" ~ to!string(Success));

    writeln(Test("ac"));
    enum Fail = Test("ac");
    writeln("enum:\n",Fail);
    pragma(msg, "Pragma:\n" ~ to!string(Fail));

}
