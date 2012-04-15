module test;

import std.algorithm;
import std.array;
import std.conv;
import std.stdio;
import pegged.grammar;

mixin(grammar(`
Arithmetic(Atom) :
    Expr     <  Factor  (('+'/'-') Factor)*
    Factor   <  Primary (('*'/'/') Primary)*
    Primary  <  '(' Expr ')' / '-' Primary / Atom
`));

mixin(grammar(`
Relation(Atom):
Expr     <  Arithmetic(Atom) RelOp Arithmetic(Atom)
RelOp    <- "==" / "!=" / "<=" / ">=" / "<" / ">"
`));

mixin(grammar(`
Boolean < AndExpr (^"||" AndExpr)*
AndExpr < NotExpr (^"&&" NotExpr)*
NotExpr < ^"!"? Primary
Primary <  '(' Boolean ')' / ^Relation(Atom)
Atom    <- Identifier / Number
Number  <~ [0-9]+
`));


void main()
{
    auto tree = Boolean.parse("x < 0 || !(x+y == 1)");
    writeln(tree);
}

