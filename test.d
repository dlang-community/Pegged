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

import pegged.examples.arithmetic;

import pegged.dynamicpeg;
import pegged.dynamicgrammar;

struct Hook
{
    ParseTree delegate(string)[string] rules;
    /+
    ParseTree opCall(string name, string s)
    {
        switch(name)
        {
            case "Term": return Arithmetic.Term(s);
            default: return fail(s);
        }
    }

    ParseTree opCall(string name, ParseTree p)
    {
        switch(name)
        {
            case "Term": return Arithmetic.Term(p);
            default: return fail(p);
        }
    }
    +/
}

void main()
{
    //writeln(pegged.grammar.grammar("Test: A <- 'a'"));
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
    , "identifier": (ParseTree p) =>pegged.peg.identifier(p)
    ];

    StopWatch sw;
    writeln("Generating the dynamic parser...");
    sw.start();
    DynamicGrammar dg = pegged.dynamicgrammar.grammar("
Arithmetic:
    Term     < Factor (Add / Sub)*
    Add      < '+' Factor
    Sub      < '-' Factor
    Factor   < Primary (Mul / Div)*
    Mul      < '*' Primary
    Div      < '/' Primary
    Primary  < Parens / Neg / Number / Variable
    Parens   < :'(' Term :')'
    Neg      < '-' Primary
    Number   <~ [0-9]+
    Variable <- identifier

    ", predefined);
    sw.stop();
    auto last = sw.peek().msecs;
    writeln("Done, generated in ", last, " ms.");

    ///////////// I have to write dynamicpeg overloads for strings
    Hook dg2;
    dg2.rules["Factor"] = (string s) => Arithmetic.Factor(s);
    dg2.rules["Add"] = (string s) => and(literal("+"), dg2.rules["Factor"])(ParseTree("",false,null,s));
    dg2.rules["Sub"] = (string s) => and(literal("-"), dg2.rules["Factor"])(ParseTree("",false,null,s));
    dg2.rules["Term"] = (string s) => and(dg2.rules["Factor"], zeroOrMore(or(dg2.rules["Add"], dg2.rules["Sub"])))(ParseTree("",false,null,s));
    dg2.rules["Arithmetic"] = (string s) => Arithmetic(s);

    string input = "1";

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

