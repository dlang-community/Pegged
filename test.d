/// Testing Pegged modifications.
module test;

//import std.algorithm;
//import std.array;
//import std.conv;
import std.datetime;
import std.functional;
//import std.range;
import std.stdio;
//import std.typecons;
//import std.typetuple;

import pegged.grammar;

import pegged.examples.dgrammar;
//import dparser;
import pegged.dynamicpeg;
import pegged.dynamicgrammar;

import ab2;

void main()
{
    //asModule!(Memoization.no)("ab2", "ab2", "Test: A<-B B <- 'b'/'c'");
    //Test.before["B"]= named(oneOrMore(literal("b")), "Test.Addon");
    //asModule!(Memoization.no)("dparser", "dparser", Dgrammar);
    auto space = zeroOrMore(or(literal(" "), literal("\t"), literal("\n"), literal("\r\n"), literal("\r")));
    Test.addRuleAfter("B", "Root < 'd'*");
    writeln(Test.rules.keys);
    writeln(Test.before.keys);
    writeln(Test.after.keys);
    writeln(Test("d d d "));
    //writeln(D("int main() { if(e) {} }"));

    // Adding a new statement
    /+
    D.before["Statement"] =
        named( and(literal("unless"), space, literal("("),space, &D.IfCondition, space, literal(")"),
                   space, &D.BlockStatement)
             , "D.UnlessStatement");
    writeln(D("int main() { unless(e) {} }"));
    +/
/+
    foreach(n; 0..6)
    {
        int N = 100;
        writeln(input.length, ":");
        auto b = benchmark!(()=> dg(input), ()=>dg2.rules["Arithmetic"](input),()=>Arithmetic(input))(N);
        auto b0 = b[0].to!("msecs", float)/N;
        auto b1 = b[1].to!("msecs", float)/N;
        auto b2 = b[2].to!("msecs", float)/N;

        writefln("dg1: %.2f    dg2: %.2f    Arithmetic: %.2f    ratio: %.2f and %.2f", b0, b1, b2, b0/b2, b1/b2);
        //writeln(dg2.rules["Term"](input));
        //writeln(Arithmetic.Term(input));
        input = input ~ "+" ~ input;
    }
+/
}

