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


enum gram = `
P:
Rule1 <- (w 'a' w)*
Rule2 <- (wx 'a' wx)*
w <- :(' ')*
wx <- (:' ')*
Rule3 <- w 'a' w
Rule4 <- wx 'a' wx
`;

mixin(grammar(gram));

string input = "   a   a   a a  a a ";


void main()
{
    ParseTree p1 = P.decimateTree(P.Rule1(input));
    ParseTree p2 = P.decimateTree(P.Rule2(input));
    writeln(p1);
    writeln(p2);

    ParseTree p3 = P.decimateTree(P.Rule3(input));
    ParseTree p4 = P.decimateTree(P.Rule4(input));
    writeln(p3);
    writeln(p4);
}

