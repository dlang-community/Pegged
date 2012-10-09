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
    mixin(grammar(`
    Test:
            program <- decl* :eoi
            useDecl < "use" qualifiedIdentifier
            recDecl < "record" identifier "{" "}"
			classDecl < "class" identifier "{" "}"
            decl < recDecl/useDecl/classDecl
    `));

    writeln(Test(`use foo.bar

                    rec foo {
                    }`));

}