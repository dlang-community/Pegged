/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.grammar;
import pegged.parser;

struct GenericTest(TParseTree)
{
    struct Test
    {
    enum name = "Test";
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;

    static RuleIntrospection[string] internal;
    static RuleIntrospection[string] external;

    static this()
    {
        external["identifier"] = identifier(RuleIntrospection());
        internal = grammarIntrospection(Pegged(
            `Test:
                A <- identifier`)
        , external);
    }


    static bool isRule(string s)
    {
        switch(s)
        {
            case "Test.A":
                return true;
            default:
                return false;
        }
    }
    mixin decimateTree;
    alias spacing Spacing;

    static TParseTree A(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(identifier, name ~ `.`~ `A`)(p);
        }
        else
        {
            if(auto m = tuple(`A`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(identifier, name ~ `.`~ `A`)(p);
                memo[tuple(`A`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree A(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(identifier, name ~ `.`~ `A`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(identifier, name ~ `.`~ `A`)(TParseTree("", false,[], s));
        }
    }
    static string A(GetName g)
    {
        return name ~ `.`~ `A`;
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
        if(__ctfe)
        {
            return Test(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            memo = null;
            return Test(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "Test";
    }

    }
}

alias GenericTest!(ParseTree).Test Test;

enum g =
`
    Test:
        A <- identifier
`;

/**
 TODO: add 'expected' info in introspection (beware of left-recursive rules)
 TODO: roll my sleeves up and inject introspection information into grammars.
*/

void main()
{
    writeln(Test.internal);
    //writeln(identifier(RuleIntrospection()));
    //writeln(Test("ad"));
    //writeln(Recursive.A(RuleIntrospection()));

}
