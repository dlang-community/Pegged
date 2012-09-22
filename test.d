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

struct MatchFailure {};

auto pair(Args...)(Args args) 
{
	alias isRange		 	P0;
	alias DiscardMatch!Any	P1;
	alias ZeroOrMore!(Any)	P2;
	alias And!(P0,P1,P2) 	P;
	
	alias P.Match!(Args) M;
	
	static if(M.successful)
	{
		alias P0.Match!(Args) 		M0;
		alias P1.Match!(M0.Rest) 	M1; // discarded
		alias P2.Match!(M1.Rest) 	M2;
		
		struct MatchResult
		{
			M0.Types a;
			M2.Types b;
		}
		return MatchResult(args[M0.begin .. M0.end], 
		                   args[M0.end+M1.end+M2.begin .. M0.end+M1.end+M2.end]);
	}
	else
		return MatchFailure();
}



class C
{
	int[] i;
	double d;
	string s;
	char c;
	
	this(int[] i, double d, string s, char c)
	{
		this.i = i;
		this.d = d;
		this.s = s;
		this.c = c;
	}
}


void main()
{
    	
	writeln(literalValue!(0,1)(0,1,2));
}