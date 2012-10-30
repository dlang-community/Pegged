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

struct GenericTest(TParseTree)
{
    struct Test
    {
    enum name = "Test";
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static bool isRule(string s)
    {
        switch(s)
        {
            case "Test.Rule1":
            case "Test.Rule2":
                return true;
            default:
                return false;
        }
    }
    mixin decimateTree;
    alias spacing Spacing;

    static TParseTree Rule1(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`a`), Rule2), name ~ `.`~ `Rule1`)(p);
        }
        else
        {
            if(auto m = tuple(`Rule1`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`a`), Rule2), name ~ `.`~ `Rule1`)(p);
                memo[tuple(`Rule1`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Rule1(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`a`), Rule2), name ~ `.`~ `Rule1`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`a`), Rule2), name ~ `.`~ `Rule1`)(TParseTree("", false,[], s));
        }
    }
    static string Rule1(GetName g)
    {
        return name ~ `.`~ `Rule1`;
    }

    static TParseTree Rule2(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!(`b`), name ~ `.`~ `Rule2`)(p);
        }
        else
        {
            if(auto m = tuple(`Rule2`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.literal!(`b`), name ~ `.`~ `Rule2`)(p);
                memo[tuple(`Rule2`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Rule2(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!(`b`), name ~ `.`~ `Rule2`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.literal!(`b`), name ~ `.`~ `Rule2`)(TParseTree("", false,[], s));
        }
    }
    static string Rule2(GetName g)
    {
        return name ~ `.`~ `Rule2`;
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Rule1(p));
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

void main()
{
    mixin(grammar(`
    Test:
        Rule1 <- 'a' Rule2('b')
        Rule2(B) <- B
    `));

    // Equality on success
    ParseTree result = Test("ab");

    enum CTsuccess = Test("ab");

    assert(CTsuccess == result, "Compile-time parsing is equal to runtime parsing on success.");

    // Equality on failure
    result = Test("ac");
    enum CTfailure = Test("ac");

    assert(CTfailure == result, "Compile-time parsing is equal to runtime parsing on failure.");
}
