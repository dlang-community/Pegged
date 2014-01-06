/// Testing Pegged modifications.
module pegged.dev.test;

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


void main()
{
    enum gram = `
    G:
        A <- B
        B <- C
        C <- 'c' D
        D <- 'd'
    `;

    mixin(grammar(gram));

    string input = "cd";

    ParseTree p = G(input);
    writeln(p);
    assert(p.successful);
    assert(p.name == "G");
    assert(p.children.length == 1);
    assert(p.children[0].name == "G.A");
    assert(p.children[0].children.length == 1);
    assert(p.children[0].children[0].name == "G.B");
    assert(p.children[0].children[0].children.length == 1);
    assert(p.children[0].children[0].children[0].name == "G.C");
    assert(p.children[0].children[0].children[0].children.length == 1);
    assert(p.children[0].children[0].children[0].children[0].name == "G.D");
    assert(p.children[0].children[0].children[0].children[0].children.length == 0);
}

