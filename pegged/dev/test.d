/// Testing Pegged modifications.
module pegged.dev.test;

//import std.algorithm;
//import std.array;
//import std.conv;
import std.datetime;
//import std.functional;
//import std.range;
import std.stdio;
//import std.typecons;
//import std.typetuple;

import pegged.grammar;
import pegged.dynamic.peg;
import pegged.dynamic.grammar;

import pegged.examples.arithmetic;

void main()
{
    writeln(Arithmetic("1+2*x"));
    Arithmetic.addRuleBefore("Variable", "Variable <- '&&'");
    writeln(Arithmetic("1+2*&&"));
    writeln(Arithmetic("1+2*x"));
    Arithmetic.addRuleBefore("Variable", "Variable <- '__'");
    writeln(Arithmetic("1+2*__"));
    writeln(Arithmetic("1+2*&&"));
    writeln(Arithmetic("1+2*x"));
}

