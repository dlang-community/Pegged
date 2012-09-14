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
import pegged.parser;
import pegged.introspection;

import G;

void main()
{
	enum g = "
	Gram:
		A <- 'a'? B*
		B <- 'c'? A 'c'*
		";
	
	writeln(G.Gram.info);
	//asModule("G", g);
	//mixin(grammar(g));
	//writeln(Gram("abccc"));
	//writeln(Gram.info);
	//writeln(Pegged(G));
	//writeln(callGraph(G));
	//writeln(ruleInfo(G));
	//writeln(ruleInfo(G));
	
	//writeln(grammar(G));
	//asModule("G2", g);
	//alias Gram.A!(literal!"b") Rule;
	
	//writeln(Rule("abbbc"));
	//writeln(Gram.A!(literal!"b")("abbbc"));
	
	//writeln(S.A!("abc","def")("abc"));
	
	//writeln(Gram.ruleNames);
	//writeln(Gram.info["A"].recursion);
	//writeln(Gram.info["A"].nullMatch);
	//writeln(Gram.info["A"].infiniteLoop);
}