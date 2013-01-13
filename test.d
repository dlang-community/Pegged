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

import pegged.examples.dgrammar;
import dparser;

/+
ParseTree foo(ParseTree p)
{
    writeln("called!");
    writeln("[", p.input, "] ", p.begin, " ", p.end);
    return p;
}

enum g = (grammar!(Memoization.no)(`Test:
    A <- 'a'
    B <- eps {foo} 'b' { foo }
`));

mixin(g);
+/
void main()
{
    //asModule!(Memoization.no)("dparser", "dparser", Dgrammar);
    //writeln(g);
    Dynamic sp = named(discard(zeroOrMore(or(literal(" "), literal("\t"), literal("\n")))), "sp");
    //writeln(getName(sp));
    Dynamic identifier = fuse(oneOrMore(charRange('a', 'z')));
    Dynamic unless = and( sp, literal("unless")
                        , sp, literal("("), sp, p => D.IfCondition(p), sp, literal(")")
                        , sp, p => D.ThenStatement(p));


    writeln(unless(ParseTree("", true, [], "unless (expr) {}")));
    //D.beforeIfStatement = unless;
    ParseTree result = D("
void main()
{
    if (expr) {}
}");
    writeln(result);
    /+Test.afterA = pegged.dynamic.zeroOrMore(toDelegate(&Test.B));
    auto e = Test("bbba");
    writeln(e);+/
}

