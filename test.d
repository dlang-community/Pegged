/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
//import std.file;
//import std.path;
import std.range;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.grammar;


/// TODO: modify the way generic rules are written, again.
/// Caution: memoization code

mixin(grammar(`
TEST:
    recordDeclaration < :"rec" identifier '{' '}'

    keywords <- "rec" / "foo" / "bar"
    identifier <- !(keywords Spacing) ident
    ident <~ [a-zA-Z_] [a-zA-Z_0-9]*
`));



void main()
{
    writeln(TEST("rec record { []}"));
}