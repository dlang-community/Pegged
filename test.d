/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
//import std.file;
//import std.path;
import std.range;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.grammar;
import pegged.parser;
import pegged.introspection;

struct GenericTest(TParseTree)
{
    struct Test
    {
    enum name = "Test";
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

    static TParseTree A(TParseTree p)
    {
        return named!(literal!("a"), name ~ ".A")(p);
    }

    static TParseTree A(string s)
    {
        return named!(literal!("a"), name ~ ".A")(TParseTree("", false,[], s));
    }

    template B(alias b)
    {
        static TParseTree B(T)(T t) if (is(T == TParseTree) || is(T == string))
        {
            static if (is(T == TParseTree))
                return named!(b, name ~ ".B")(t);
            else
                return named!(b, name ~ ".B")(TParseTree("", false,[], t));
        }

        
    }
    
    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(A(p));
        result.children = [result];
        result.name = "Test";
        return result;
    }

    static TParseTree opCall(string input)
    {
        return Test(TParseTree(``, false, [], input, 0, 0));
    }
    }
}

alias GenericTest!(ParseTree).Test Test;

/// TODO: modify the way generic rules are written, again.
/// Caution: memoization code
void main()
{
    writeln(grammar(`
    Test:
        A <- "a"
        B(b) <- b
        `));
    
    writeln(Test("a"));
    writeln(and!(Test.A, Test.B!(literal!"b"))(ParseTree("",false,[],"ab")));
}