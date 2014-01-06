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
    alias or!(literal!("abc"), charRange!('0','9')) rule;
    alias defined!(rule, "myRule") myRule;

    assert(getName!(rule)() == `or!(literal!("abc"), charRange!('0','9'))`);
    assert(getName!(myRule)() == "myRule");

    // Equality on success (except for the name)
    ParseTree result = rule("abc0");
    ParseTree myResult = myRule("abc0");

    assert(myResult.successful && result.successful);
    assert(myResult.name == "myRule");
    assert(myResult.matches == result.matches);
    assert(myResult.begin == result.begin);
    assert(myResult.end == result.end);
    assert(myResult.children[0] == result);

    // Equality on failure (except for the name)
    result = rule("_abc");
    myResult = myRule("_abc");

    assert(!myResult.successful && !result.successful);
    assert(myResult.name == "myRule");
    assert(myResult.matches == result.matches);
    assert(myResult.begin == result.begin);
    assert(myResult.end == result.end);
    assert(myResult.children[0] == result);
}

