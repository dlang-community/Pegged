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

struct Test1
{
    enum names = [`expr`:true, `num`:true];

    mixin decimateTree;

    alias spacing Spacing;

    static ParseTree expr(ParseTree p)
    {
        return named!(or!(and!(num, zeroOrMore!(and!(literal!("-"), num)), literal!("+"), num), and!(num, zeroOrMore!(and!(literal!("-"), num)))), "expr")(p);
    }

    static ParseTree num(ParseTree p)
    {
        return named!(and!(oneOrMore!(charRange!('0', '9'))), "num")(p);
    }

    static ParseTree opCall(ParseTree p)
    {
        ParseTree result = decimateTree(expr(p));
        result.children = [result];
        result.name = "Test1";
        return result;
    }

    static ParseTree opCall(string input)
    {
        return Test1(ParseTree(``, false, [], input, 0, 0));
    }
}

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
    foreach(i; 0..2)
    {
        auto b = benchmark!(() => Test1(input), () => Test2(input))(256);
        writeln(input.length, ", ", b[0].to!("msecs",float),", ", b[1].to!("msecs",float));
        input ~= "-"~input;
    }
}