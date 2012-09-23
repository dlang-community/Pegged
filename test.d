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
import std.variant;

import pegged.grammar;
import pegged.parser;
import pegged.introspection;

import pegged.examples.pattern;

struct Literal(alias lit)
{
    alias typeof(lit) Type;
    
    struct MatchResult
    {
        alias typeof(lit) M;
        
        bool successful;
        M _match;
        enum size_t end = 1;
    }
    
    static MatchResult match(T...)(T t)
    {
        static if (t.length > 0 && is(typeof(t[0] == lit)))
            if (t[0] == lit)
                return MatchResult(true, lit);
            else
                return MatchResult(false);
        else
            return MatchResult(false);
    }    
}

struct And(alias p1, alias p2)
{
    alias TypeTuple!(p1.Type, p2.Type) Type;
    
    static match(T...)(T t)
    {
        struct MatchResult
        {
            /// TODO
            alias TypeAnd!(TypeLiteral!(p1.Type), TypeLiteral!(p2.Type)).Match!(T) P;
            alias P.Types M;
            
            bool successful;
            M _match;
            enum size_t end = P.end;
        }

        auto m1 = p1.match(t);
        if (m1.successful)
        {
            auto m2 = p2.match(t[m1.end..$]);
            if (m2.successful)
                return MatchResult(true, tuple(m1._match, m2._match).expand);
        }
        return MatchResult(false);
    }
}

struct Or(alias p1, alias p2)
{

    static match(T...)(T t)
    {
        struct MatchResult
        {
            /// TODO
            alias TypeOr!(TypeLiteral!(p1.Type), TypeLiteral!(p2.Type)).Match!(T) P;
            alias P.Types M;
            
            bool successful;
            M _match;
            enum size_t end = P.end;
        }
    
        alias TypeLiteral!(p1.Type).Match!(T) M1;
        alias TypeLiteral!(p2.Type).Match!(T) M2;
        static if (M1.successful)
        {
            auto m1 = p1.match(t);
            if (m1.successful)
            {
                return MatchResult(true, m1._match);
            }
            else
                return MatchResult(false);
        }
        else static if (M2.successful)
        {
            auto m2 = p2.match(t);
            if (m2.successful)
                return MatchResult(true, m2._match);
            else
                return MatchResult(false);
        }
        else
            return MatchResult(false); // What type?
    }
}



void main()
{
	alias Or!(And!(Literal!123, Literal!"abc"), Literal!"abc") P;
    auto m = P.match(123);
    writeln(m);
}