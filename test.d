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
        A <- B %C D
        B <- 'b'
        C <- E F
        D <- 'd'
        E <- 'e'
        F <- 'f'
    `));

    writeln(Test("befd"));

}

