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
import pegged.dynamicpeg;
import pegged.dynamicgrammar;

//import pegged.examples.dgrammar;
//import dparser;

/+
enum g = grammar(`

Test:
    Root <- (A B C)*
    A <- 'a'
    B <- 'b'
    C <- 'c'
    Many(R) <- R*

`);

mixin(g);
+/
DynamicGrammar Test2()
{
    DynamicGrammar dg;
    dg.grammarName = "Test";
    // Many(R) <- R*
    dg.paramRules["Many"] = parameterizedRule(1, (Dynamic[] d) => zeroOrMore(d[0]));
    // Pair(A,B) <- A B
    dg.paramRules["Pair"] = parameterizedRule(2, (Dynamic[] d) => and(d[0], d[1]));
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
    - slightly less handy parameterized rules (could be better)
    - no semantic actions (drat, this one is the worse)

Advantages:
    - fully runtime configurable: change rules, add rules, delete rules.
*/
void main()
{
    DynamicGrammar test2 = Test2();
    ParseTree e = (test2("abcabcabc"));
    writeln(e);
    string input = "";
    int N = 1000;
    // Rule1 = Rule2; // Weak linking. If Rule2 changes, Rule1 will point to the old Rule2.
    // Rule1 = (ParseTree p) => Rule2(p) : Strong linking. If Rule2 evolves, Rule1 will still point to it.
/+
    foreach(n; 0..10)
    {
        auto b = benchmark!(()=> Test(input), ()=>test2(input))(N);
        auto t1 = b[0].to!("usecs", float)/N;
        auto t2 = b[1].to!("usecs", float)/N;
        writefln("%d: %.1f %.1f => %.2f", input.length, t1, t2, t2/t1);
        input ~= "abcabcabcabcabcabcabcabc";
    }
+/
}

