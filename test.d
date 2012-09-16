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
import pegged.parser;
import pegged.introspection;

import pegged.examples.pattern;


void main()
{
    //alias TypeTuple!(byte,int,double,int) I;
    //alias OneOrMore!(SubType!(ulong)) P;
    //alias P.Match!(I) R;
    //writeln(R.successful,": ", R.Types.stringof, " / ", R.Rest.stringof, "\n");
    
    //writeln(Pattern(`. {. first}`));
    alias zeroOrMore!(or!(literal!"abc", literal!"d")) rule; // in PEG-speak:  '("abc" / "d")*'
ParseTree input = ParseTree("",false,[], "abcdabce");

ParseTree result = rule(input);

assert(result.successful);
assert(result.matches == ["abc", "d", "abc"]);
assert(result.end == 7); // matched "abcdabce"[0..7] => "abcdabc". "e" at the end is not part of the parse tree.
assert(result.children.length == 3);
writeln(result);
/+
writes:
+/

input = ParseTree("",false,[], "efgh");
result = rule(input);
assert(result.successful); // succeed, even though all patterns failed.
assert(result.children.length == 0);
}