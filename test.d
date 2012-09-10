/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.typecons;

import pegged.grammar;
import pegged.examples.peggedgrammar;

void main()
{
mixin(grammar(`
A(B):
    Rule1(C) <- B C
    Rule2 <- B*
    `));

alias literal!"b" b;
alias literal!"c" c;
writeln(A!(b).Rule1!(c)("bcb"));
}
