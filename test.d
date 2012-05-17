module test;

import std.algorithm;
import std.array;
import std.conv;
import std.range;
import std.stdio;
import std.typecons;

import pegged.grammar;
import pegged.examples.dgrammar;

enum input = //JSONGrammar;
`
TEST:
A <- 'a'
B <- A?
C <- [0-9]+
D <- C B*

`;


void main()
{
    auto c = checkGrammar(input, ReduceFurther.Yes);
    writeln(c);
    foreach(k,v;c)
        if (v != Diagnostic.NoRisk) writeln(k,":",v);
}


