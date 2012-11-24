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

void main()
{
        enum gram = `
        P:
        Rule1 <- (w 'a' w)*
        Rule2 <- (wx 'a' wx)*
        w <- :(' ' / '\n' / '\t' / '\r')*
        wx <- (:' ' / '\n' / '\t' / '\r')*
        `;

    mixin(grammar(gram));

    string input = "   a   a   a a  a a ";

    ParseTree p1 = P.decimateTree(P.Rule1(input));
    ParseTree p2 = P.decimateTree(P.Rule2(input));
    assert(softCompare(p1,p2));

    input = " a\n  \011\012 a\n\t  a\x09\x0A a ";
    p1 = P.decimateTree(P.Rule1(input));
    p2 = P.decimateTree(P.Rule2(input));
    assert(p1.end == input.length); // Parse the entire string
    assert(p2.end == input.length);
}

