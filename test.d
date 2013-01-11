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

enum g = (grammar(`
    Parameterized:
        # Different arities
        Root <- Rule1('a', 'b') 'c'
        Rule1(A)     <- A+
        Rule1(A,B)   <- (A B)+
        Rule1(A,B,C) <- (A B C)+
    `));

/// TODO: add a number after a param rule, for the number of parameters
///       *or* make param rules non hooked
void main()
{
    mixin(g);

    alias A = Parameterized;
    enum e = A("abababc");
    writeln(e);
}

