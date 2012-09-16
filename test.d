/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
//import std.file;
//import std.path;
import std.range;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.grammar;
import pegged.parser;
import pegged.introspection;

import pegged.examples.pattern;


void main()
{
    alias TypeTuple!(byte,int,double,int) I;
    alias OneOrMore!(SubType!(ulong)) P;
    alias P.Match!(I) R;
    writeln(R.successful,": ", R.Types.stringof, " / ", R.Rest.stringof, "\n");
    
    writeln(Pattern(`. {. first}`));
}