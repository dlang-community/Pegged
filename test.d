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
import pegged.introspection;

enum g =
`
    Recursive:
        A <- 'a' / eoi
`;

struct GenericRecursive(TParseTree)
{
    struct Recursive
    {
    enum name = "Recursive";
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static bool isRule(string s)
    {
        switch(s)
        {
            case "Recursive.A":
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
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(`a`), eoi), name ~ `.`~ `A`)(p);
        }
        else
        {
            if(auto m = tuple(`A`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(`a`), eoi), name ~ `.`~ `A`)(p);
                memo[tuple(`A`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree A(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(`a`), eoi), name ~ `.`~ `A`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(`a`), eoi), name ~ `.`~ `A`)(TParseTree("", false,[], s));
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
        result.name = "Recursive";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return Recursive(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            memo = null;
            return Recursive(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "Recursive";
    }

    }
}

alias GenericRecursive!(ParseTree).Recursive Recursive;

void main()
{
    auto ri = grammarIntrospection(Pegged(`Recursive:
        A <- B 'a' / eps`));
    writeln(ri);

    writeln(and!(literal!"abc", Recursive.A)(RuleIntrospection()));
}
