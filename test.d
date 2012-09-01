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
import pegged.examples.peggedgrammar;


mixin(grammar(`
Gram(a):
    A <- a
`));

    
void main()
{
    
}