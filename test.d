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
//import pegged.examples.peggedgrammar;

//import PeggedParser;
//import _grammar;

void main()
{
    //asModule("_grammar", PEGGEDgrammar);

    mixin(grammar("
    Test:
        A <- B C*
        B <- 'b'
        C <- 'c'
    "));
        
    writeln(Test("bcccd"));
    
}