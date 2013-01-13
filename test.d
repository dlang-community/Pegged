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
    Root <~ A* B C
    A <- 'a'
    B <- 'b'
    C <- 'c'
`);

mixin(g);

struct Test2
{
    static ParseTree delegate(ParseTree) Root;
    static ParseTree delegate(ParseTree) A;
    static ParseTree delegate(ParseTree) B;
    static ParseTree delegate(ParseTree) C;

    static this()
    {
        Root = named(fuse(pegged.dynamic.and( zeroOrMore(p=>A(p)), (ParseTree p)=>B(p), (ParseTree p)=>C(p))), "Test");
        A = named(literal("a"), "Test.A");
        B = named(literal("b"), "Test.B");
        C = named(literal("c"), "Test.C");
    }

    static ParseTree opCall(ParseTree p)
    {
        ParseTree result = Root(p);
        result.children = [result];
        result.name = "Test";
        return result;
    }

    static ParseTree opCall(string input)
    {
        return Root(ParseTree(``, false, [], input, 0, 0));
    }
}


void main()
{
    string input = "";
    int N = 1000;

    writeln(Test2(ParseTree("", true, [], input)));
    foreach(n;0..20)
    {
        auto b = benchmark!(()=> Test(input), ()=> Test2(input))(N);
        auto t1 = b[0].to!("usecs", float)/N;
        auto t2 = b[1].to!("usecs", float)/N;
        writefln("%s: %.1f and %.1f => %.1f times faster", input.length, t1, t2, t1/t2);
        input = "aaaaaaaaaaaa" ~ input ~ "b";
    }
    //writeln(Test2(input));
}

