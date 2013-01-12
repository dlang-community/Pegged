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
import pegged.dynamic;

//import pegged.examples.dgrammar;
//import dparser;

enum g = (grammar("Test:
    A <- 'a'
"));

mixin(g);

void main()
{
    //writeln(g);
    ParseTree delegate(ParseTree) first = p => literal!"b"(p);
    ParseTree delegate(ParseTree) second = p => literal!"c"(p);
    
    Test.beforeA = dynamicOr(dynamicLiteral("b"), dynamicLiteral("c"), p => oneOrMore!(p => dynamicLiteral("d")(p))(p));
    writeln(Test("e"));
    //writeln(Test("b"));
}

