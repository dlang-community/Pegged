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
import GramTest;

void main()
{
/+    
    asModule("GramTest", 
"Gram:
    A <- B C
    B <- 'b'
    C <- 'c'");+/
    writeln(Gram("bc"));
}