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

enum test =
`
P:
Rule1 <- (w 'a' w)*
Rule2 <- (wx 'a' wx)*
w <- :(' ' / '\n')*
wx <- (:' ' / '\n')*
`;

mixin(grammar!(Memoization.no)(test));

void main()
{
    string input = "   a \n a  a a  a\n a ";
    writeln(P.decimateTree(P.Rule1(input)));
    writeln(P.decimateTree(P.Rule2(input)));
    //writeln(P.decimateTree(P.n(input)));
    
}
