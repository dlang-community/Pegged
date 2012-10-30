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

void main()
{
    mixin(grammar(`
    Test:
        Rule1 <- 'a' Rule2
        Rule2 <- 'b'
    `));

    // Equality on success
    ParseTree result = Test("ab");
    enum CTsuccess = Test("ab");

    assert(CTsuccess == result, "Compile-time parsing is equal to runtime parsing on success.");

    // Equality on failure
    result = Test("ac");
    enum CTfailure = Test("ac");

    assert(CTfailure = result, "Compile-time parsing is equal to runtime parsing on failure.");
}
