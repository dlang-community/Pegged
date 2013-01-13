/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.functional;
import std.range;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.grammar;
import pegged.dynamic;

//import pegged.examples.dgrammar;
//import dparser;


enum g = grammar(`Test:
    A <~ 'a'* B*
    B <- 'b'
`);

mixin(g);

struct Test2
{
    static ParseTree delegate(ParseTree) A;// = named(literal("b"), "Test.B"); //named(and(zeroOrMore(literal("a")), zeroOrMore(B)), "Test.A");
    static ParseTree delegate(ParseTree) B;// = named(literal("b"), "Test.B");

    static this()
    {
        A = fuse(named(and(zeroOrMore(literal("a")), zeroOrMore(B)), "Test.A"));
        B = named(literal("b"), "Test.B");
    }
    static bool isRule(string s)
    {
        switch(s)
        {
            case "Test.A":
            case "Test.B":
                return true;
            default:
                return false;
        }
    }
    mixin decimateTree;
    alias spacing Spacing;

    static ParseTree opCall(ParseTree p)
    {
        ParseTree result = decimateTree(A(p));
        result.children = [result];
        result.name = "Test";
        return result;
    }

    static ParseTree opCall(string input)
    {
        return Test(ParseTree(``, false, [], input, 0, 0));
    }

}


void main()
{
    string input = "";
    int N = 1_000;
    writeln(Test2(input));
    foreach(n;0..100)
    {
        auto b = benchmark!(()=> Test(input), ()=> Test2(input))(N);
        auto t1 = b[0].to!("usecs", float)/N;
        auto t2 = b[1].to!("usecs", float)/N;
        writefln("%s: %.1f and %.1f => %.1f times faster", input.length, t1, t2, t1/t2);
        input = "a" ~ input ~ "b";
    }
    // Dynamically changing the rules
    writeln(Test2(input));
    
}

