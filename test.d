/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.typecons;

import pegged.examples.json;

import std.json;

void main()
{
    int N = 100;
    auto b = benchmark!(() => parseJSON(example4), () => pegged.examples.json.JSON(example4))(N);
    writeln(b[0].to!("msecs", float)/N, " msecs/call <-> ", b[1].to!("msecs", float)/N, " msecs.call");
}