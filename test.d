module test;

import std.algorithm;
import std.array;
import std.conv;
import std.stdio;
import pegged.grammar;

mixin(grammar("Test: A <- 'a' 'é' '\u237A' '\U0000237A'"));

void main()
{
    writeln(Test.parse(`aé⍺⍺ù`));
    
}


