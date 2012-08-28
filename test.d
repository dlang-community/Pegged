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

mixin(grammar!(Memoization.no)(`
Test1:
    expr <- num ("-" num)* "+" num
          / num ("-" num)*
    num <- [0-9]+
    `));


mixin(grammar(`
Test2:
    expr <- num ("-" num)* "+" num
          / num ("-" num)*
    num <- [0-9]+
    `));


void main()
{
    string input = "11111111111111111111-11111111111111111111";
    float[] ratio;
    foreach(i; 0..3)
    {
        auto b = benchmark!(() => Test1(input), () => Test2(input))(256);
        writeln(input.length, ", ", b[0].to!("msecs",float),", ", b[1].to!("msecs",float));
        input ~= "-"~input;
    }
}