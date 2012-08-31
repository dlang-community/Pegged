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

/**
 * TODO: when a rule is templated, both the string and the parse string version must be templated
 *       Add a ,T : ParseTree)(T p) or ,T : string)(T p) at the end of the name 
 *       For the string case: 
 *     static ParseTree rule(alias b, T : string)(T p)
    {
        memo = null;        
        ParseTree result = named!(code, "rule")(ParseTree("",false,[],p));
        memo[tuple("rule",0)] = result;
    }

    TODO: The same for parameterized grammars. opCall must be modified.
    TODO: updates doc to explain spacing can be user-defined (spaceAnd will call it)
    TODO: add an enum inside ParseTree's, containing the rules's name, to enable final switch selection
    TODO: qualified names for rules (grammarName.ruleName)
    TODO: parameterize the grammars on a ParseTree type
    TODO: inlining
    TODO: modify the gen_grammar to regenerate correctly the new engine
    TODO: modify the makefile
    TODO: fuse with the master branch
    TODO: grammar introspection: grammar name, rule names, call graph, 
**/
import pegged.introspection;

void main()
{
    auto g = callGraph(`
Rec1:
    A <- 'a' B
    B <- 'b' A C
    C <- 'c'
`);
    
    auto g2 = callGraph(`
Rec2:
    A <- 'a' B
    B <- 'b' A 'c'
    C <- 'c'
`);

    auto g3 = callGraph(`
Rec3:
    A <- 'a' B
    B <- 'b' 'a' B 'c'
    C <- 'c'
`);

    auto g4 = callGraph(`
Rec4:
    A <- 'a' 'b' 'a' B 'c'
    B <- 'b' 'a' B 'c'
    C <- 'c'
`);
    writeln(g);
    writeln(closure(g));
    writeln(g);
    writeln(recursions(g));
    writeln(recursions(g2));
    writeln(recursions(g3));
    writeln(recursions(g4));
}