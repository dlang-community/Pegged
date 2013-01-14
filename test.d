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


enum g = grammar(`

Test:
    Root <- (A B C)*
    A <- 'a'
    B <- 'b'
    C <- 'c'
    Many(R) <- R*

`);

mixin(g);

struct ParameterizedRule
{
    size_t numArgs;
    
    Dynamic opCall(D...)(D rules)
    {
    
    }
}

struct DynamicGrammar
{
    Dynamic[string] rules;
    string startingRule;
    Dynamic delegate(Dynamic[] a...)[string] paramRules;
    string grammarName;
    
    ParseTree decimateTree(ParseTree p)
    {
        if(p.children.length == 0) return p;

        ParseTree[] filterChildren(ParseTree pt)
        {
            ParseTree[] result;
            foreach(child; pt.children)
            {
                if (  (child.name.startsWith(grammarName) && child.matches.length != 0)
                   || !child.successful && child.children.length == 0)
                {
                    child.children = filterChildren(child);
                    result ~= child;
                }
                else if (child.name.startsWith("keep!(")) // 'keep' node are never discarded.
                                               // They have only one child, the node to keep
                {
                    result ~= child.children[0];
                }
                else // discard this node, but see if its children contain nodes to keep
                {
                    result ~= filterChildren(child);
                }
            }
            return result;
        }
        p.children = filterChildren(p);
        return p;
    }
    
    ParseTree opCall(ParseTree p)
    {
        ParseTree result = decimateTree(rules[startingRule](p));
        result.children = [result];
        result.name = grammarName;
        return result;
    }

    ParseTree opCall(string input)
    {
        ParseTree result = decimateTree(rules[startingRule](ParseTree(``, false, [], input, 0, 0)));
        result.children = [result];
        result.name = grammarName;
        return result;
    }
    
    void opIndexAssign(D)(D code, string s)
    {
        rules[s]= named(code, "Test." ~ s);
    }
    
    Dynamic opIndex(string s)
    {
        return rules[s];
    }
}

DynamicGrammar Test2()
{
    DynamicGrammar dg;
    
    dg.grammarName = "Test";
    // Many(R) <- R*
    dg.paramRules["Many"] = (Dynamic...) => zeroOrMore(R[0]);
    // Pair(A,B) <- A B
    dg.paramRules["Pair"] = (Dynamic[] R...) => and(R[0], R[1]);
    dg["Root"] = dg.paramRules["Many"](and(()=>dg["A"], ()=>dg["B"], ()=>dg["C"]));
    dg["A"] = literal("a");
    dg["B"] = literal("b");
    dg["C"] = literal("c");
    
    dg.startingRule = "Root";
    
    return dg;
}


/**
Dynamic grammars:
    - no CT parsing
    - no memoization (could be added)
    - slightly less handy parameterized rules => ajouter un overload de and and or
        qui accepte les Dynamic[],
    
Advantages:
    - fully runtime configurable: change rules, add rules, delete rules.
*/
void main()
{
    DynamicGrammar test2 = Test2();
    ParseTree e = (test2("abcabcabc"));
    writeln(e);
    string input = "";
    int N = 200;
    
    // Rule1 = Rule2; // Weak linking. If Rule2 changes, Rule1 will point to the old Rule2.
    // Rule1 = (ParseTree p) => Rule2(p) : Strong linking. If Rule2 evolves, Rule1 will still point to it.

    foreach(n; 0..10)
    {
        auto b = benchmark!(()=> Test(input), ()=>test2(input))(N);
        auto t1 = b[0].to!("usecs", float)/N;
        auto t2 = b[1].to!("usecs", float)/N;
        writefln("%.1f %.1f => %.2f", t1, t2, t2/t1);
        input ~= "abcabcabcabcabcabcabcabc";
    }

}

