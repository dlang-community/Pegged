module test;

import std.algorithm;
import std.array;
import std.conv;
import std.stdio;
//import pegged.peg;
import pegged.grammar;
//import pegged.examples.PEGGED;

enum G = `
Test:
    A <- :DoubleQuote ~Text :DoubleQuote
    Text <- (!DoubleQuote .)*
`;


mixin(grammar(G));

void main()
{
    writeln(Test.validate(`"Hello, World!"`));
    writeln(Test.match(`"Hello, World!"`));
    writeln(Test.parse(`"Hello, World!"`));
    writeln(Test.fullParse(`"Hello, World!"`));
    writeln(Test.fullestParse(`"Hello, World!"`));
}


