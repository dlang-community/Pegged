/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.typecons;

//import pegged.examples.xml2;
import pegged.grammar;
import dparser;

void main()
{
    writeln(D("
module test.foo;

int i,j;

class C { this() {} }

struct S(T) 
{
    T t;
}

void main()
{
    double[] array;
}
    "));
}