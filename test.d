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

struct Test2
{
    enum names = [`expr`:true, `num`:true];

    mixin decimateTree;

    alias spacing Spacing;

    static ParseTree[Tuple!(string, size_t)] memo;

    static ParseTree expr(ParseTree p)
    {
        auto m = tuple("expr",p.end) in memo;
        if (m)
            return *m;
        else
        {
            ParseTree result = 
                named!(or!(and!(num, zeroOrMore!(and!(literal!("-"), num)), literal!("+"), num), and!(num, zeroOrMore!(and!(literal!("-"), num)))), "expr")(p);
            memo[tuple("expr", p.end)] = result;
            //writeln("storing");
            return result;
        }
    }

    static ParseTree num(ParseTree p)
    {
        auto m = tuple("num",p.end) in memo;
        if (m)
            return *m;
        else
        {
            ParseTree result = 
                named!(and!(oneOrMore!(charRange!('0', '9'))), "num")(p);
            memo[tuple("num", p.end)] = result;
            return result;
        }
    }

    static ParseTree opCall(ParseTree p)
    {
        ParseTree result = decimateTree(expr(p));
        result.children = [result];
        result.name = "Test2";
        return result;
    }

    static ParseTree opCall(string input)
    {
        memo = null; // Else the static-ity of memo doesn't play nice with the benchmark
        return Test2(ParseTree(``, false, [], input, 0, 0));
    }
}


void main()
{
    string input = "123456789-123456789";
    float[] ratio;
    foreach(i; 0..10)
    {
        auto b = benchmark!(() => Test1(input), () => Test2(input))(100);
        writeln(input.length, ": ", b[0].to!("msecs",float),", ", b[1].to!("msecs",float));
        input ~= "-"~input;
    }
}