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
    Terminals:
        Literal1 <- "abc"
        Literal2 <- 'abc'
        EmptyLiteral1 <- ""
        EmptyLiteral2 <- ''
        Any <- .
        Eps <- eps
        Letter <- [a-z]
        Digit  <- [0-9]
        ABC    <- [abc]
        Alpha1  <- [a-zA-Z_]
        Alpha2  <- [_a-zA-Z]
    `));

    ParseTree result = Terminals("abc");

    assert(result.name == "Terminals", "Grammar name test.");
    assert(result.children[0].name == "Terminals.Literal1", "First rule name test.");
    assert(result.begin == 0);
    assert(result.end == 3);
    assert(result.matches == ["abc"]);    
}

