module pegged.test;

import std.algorithm;
import std.conv;
import std.stdio;
import std.traits;
import std.typecons;
import std.typetuple;

import pegged.grammar;

import pegged.examples.json;
import pegged.examples.jsonExample;


void main()
{
    writeln(JSON.parse(example2));
}