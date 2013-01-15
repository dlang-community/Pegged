/// Testing Pegged modifications.
module test;

//import std.algorithm;
//import std.array;
//import std.conv;
import std.datetime;
//import std.functional;
//import std.range;
import std.stdio;
//import std.typecons;
//import std.typetuple;

import pegged.grammar;
import pegged.dynamicpeg;
import pegged.dynamicgrammar;

import pegged.examples.c;
/**
Dynamic grammars:
    - no CT parsing
    - no memoization (could be added)
    - slightly less handy parameterized rules (could be better)
    - no semantic actions (drat, this one is the worse)
	- no calling from other grammars?

Advantages:
    - fully runtime configurable: change rules, add rules, delete rules.
*/

/**
TODO: test rule one node.
*/
void main()
{
    //writeln(makeSwitch(40));
    Dynamic[string] predefined =
    [ "quote":      (ParseTree p) => literal("'")(p)
    , "doublequote":(ParseTree p) => literal("\"")(p)
    , "backquote":  (ParseTree p) => literal("`")(p)
    , "slash":      (ParseTree p) => literal("/")(p)
    , "backslash":  (ParseTree p) => literal("\\")(p)
    , "endOfLine":  (ParseTree p) => or(literal("\n"), literal("\r\n"), literal("\r"))(p)
    , "space":      (ParseTree p) => or(literal(" "), literal("\t"), literal("\n"), literal("\r\n"), literal("\r"))(p)
    , "digit":      (ParseTree p) => charRange('0', '9')(p)
    , "identifier": (ParseTree p) => fuse(and( or(charRange('a','z'), charRange('A','Z'), literal("_"))
                                             , oneOrMore(or(charRange('a','z'), charRange('A','Z'), literal("_"), charRange('0', '9')))))(p)
    ];
    DynamicGrammar dg = pegged.dynamicgrammar.grammar(Cgrammar, predefined);
    auto space = zeroOrMore(or(literal(" "), literal("\t"), literal("\n"), literal("\r\n"), literal("\r")));
    dg["UnlessStatement"] = and(literal("unless"), space, literal("("), space, dg["Expression"], space, literal(")"), dg["Statement"]);
    dg["Statement"] = or(dg["UnlessStatement"], dg["Statement"]);
    writeln(dg(
"int main()
{
    unless (e) { writeln(e);}
}"));
    //writeln(makeSwitch(10));
    /+
    string input = "1";
	writeln(dg(input));
    writeln(Arithmetic(input));

    int N = 100;
    foreach(n; 0..50)
    {
        auto b = benchmark!(()=> Arithmetic(input), ()=>dg(input))(N);
        auto t1 = b[0].to!("usecs", float)/N;
        auto t2 = b[1].to!("usecs", float)/N;
        writefln("%d: %.1f %.1f => %.2f", input.length, t1, t2, t2/t1);
        input ~= "+1";
    }
    +/
}

