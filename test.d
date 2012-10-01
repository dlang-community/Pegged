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
import std.stdio;
import pegged.grammar;

mixin(grammar(`
    Parse:
        Line < (Keyword )*
        Keyword <- "one" / "two" / "three"
`));

void main()
{
    
    string input =  "one two three ";
    auto res = Parse(input);
    writeln(res);
}
