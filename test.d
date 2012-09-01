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
import pegged.examples.peggedgrammar;

version(none)
{
struct Parameterized
{
    import std.typecons:Tuple, tuple;
    static ParseTree[Tuple!(string, uint)] memo;
    enum names = [`Nested`:true];
    mixin decimateTree;
    alias spacing Spacing;

    static ParseTree Nested(alias Elem, T)(T p) if (is(T == ParseTree))
    {
        if(auto m = tuple("Nested",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(discard!(literal!("(")), 
                                               Nested!(Elem,ParseTree), 
                                               discard!(literal!(")"))),
                                          identifier
                                         ), "Nested")(p);
            memo[tuple("Nested",p.end)] = result;
            return result;
        }
    }

    static ParseTree Nested(alias Elem, T)(T p) if (is(T:string))
    {
        memo = null;        
        return Nested!(Elem)(ParseTree("", false,[], p));
    }

}

struct Gram(alias a)
{
    import std.typecons:Tuple, tuple;
    static ParseTree[Tuple!(string, uint)] memo;
    enum names = [`A`:true, `rule`:true];
    mixin decimateTree;
    alias spacing Spacing;

    static ParseTree A(ParseTree p)
    {
        if(auto m = tuple("A",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("a")), "A")(p);
            memo[tuple("A",p.end)] = result;
            return result;
        }
    }

    static ParseTree A(string s)
    {
        memo = null;        
        return A(ParseTree("", false,[], s));
    }

    static ParseTree rule(alias b, T : ParseTree)(T p)
    {
        if(auto m = tuple("rule",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(a, b), "rule")(p);
            memo[tuple("rule",p.end)] = result;
            return result;
        }
    }

    static ParseTree rule(alias b, T : string)(T p)
    {
        memo = null;        
        ParseTree result = named!(and!(a, b), "rule")(ParseTree("",false,[],p));
        memo[tuple("rule",0)] = result;
    }

    static ParseTree opCall(T)(T p) // !
    {
        memo = null; // !
        ParseTree result = decimateTree(A(p));
        result.children = [result];
        result.name = "Gram";
        return result;
    }

}
}

void main()
{
}