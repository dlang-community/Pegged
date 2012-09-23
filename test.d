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
import std.variant;

import pegged.grammar;
import pegged.parser;
import pegged.introspection;

//import pegged.examples.pattern;


import pegged.examples.Dgrammar;

import pegged.examples.dparser;

void main()
{
    //asModule("dparser", DGrammar);
	writeln(D.decimateTree(and!(D.BasicType, spacing, D.Declarator)(ParseTree("",false,[],"Tuple!(int,T) a"))));
}