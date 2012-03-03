module pegged.test;

import std.conv;
import std.stdio;
import std.traits;
import std.typecons;
import std.typetuple;

import pegged.grammar;
import pegged.examples.arithmetic;
import pegged.examples.arithmeticExample;

void main()
{
    enum parseTree1 = Expr.parse(example1);
    pragma(msg, parseTree1.capture);
    writeln(parseTree1);
    
    auto parseTree2 = Expr.parse(" 0 + 123 - 456 ");
    assert(parseTree2.capture == ["0", "+", "123", "-", "456"]);
}