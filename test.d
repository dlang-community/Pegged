module pegged.test;

import std.algorithm;
import std.conv;
import std.stdio;
import std.traits;
import std.typecons;
import std.typetuple;

import pegged.grammar;

import pegged.examples.constraints;

mixin(grammar(
    "A <- [0-9]"
      ));


void main()
{
    writeln(A.parse("0a"));
}