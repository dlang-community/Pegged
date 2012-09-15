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

enum g = `
Pattern:
    Choice     <  Sequence ('/' Sequence)* eoi
    Sequence   <  Primary+
    Primary    <  (Aggregate / Array / Identifier / Literal / REST / ANY) Name?
    Name       <  identifier
    Aggregate  <  (Identifier / ANY) '{' (Primary (',' Primary)*)? '}'
    Array      <  '[' (Primary (',' Primary)*)? ']'
    Identifier <  identifier
    Literal    <  Number / Char / String
    Number     <~ Digit+ ('.' Digit*)?
    Digit      <  [0-9]
    Char       <~ quote . quote
    String     <~ doublequote (!doublequote .)* doublequote
    REST       <  "..." Spacing
    ANY        <  "." Spacing
    Spacing    <: spacing
`;

template Failure(T...)
{
    enum successful = false;
    alias TypeTuple!() Types;
    alias T Rest;
}

struct Any
{
    template Match(T...)
    {
        static if (T.length == 0)
            mixin Failure!(T);
        else
        {
            enum successful = true;
            alias T[0..1] Types;
            alias T[1..$] Rest;
        }
    }
}

struct End
{
    template Match(T...)
    {
        static if (T.length == 0)
        {
            enum successful = true;
            alias TypeTuple!() Types;
            alias T Rest;
        }
        else
            mixin Failure!(T);
    }
}

struct Eps
{
    template Match(T...)
    {
        enum successful = true;
        alias TypeTuple!() Types;
        alias T Rest;
    }
}

struct Literal(U...)
{
    template Match(T...)
    {
        static if (T.length < U.length || !is(T[0..U.length] == U))
            mixin Failure!(T);
        else
        {
            enum successful = true;
            alias T[0..U.length] Types;
            alias T[U.length..$] Rest;
        }
    }
}

template isSubtype(T...)
{
    static if (T.length == 0)
        enum isSubtype = true;
    else static if (is(T[0] : T[$/2]))
        enum isSubtype = isSubtype!(T[1..$/2],T[$/2+1..$]);
    else
        enum isSubtype = false;
}

struct SubType(U...)
{
    template Match(T...)
    {
        static if (T.length < U.length || !isSubtype!(T[0..U.length],U))
            mixin Failure!(T);
        else
        {
            enum successful = true;
            alias T[0..U.length] Types;
            alias T[U.length..$] Rest;
        }
    }
}

struct Or(alias Pattern1, alias Pattern2)
{
    template Match(T...)
    {
        alias Pattern1.Match!(T) P1;
        static if (P1.successful)
            alias P1 Match;
        else
            alias Pattern2.Match!(T) Match;
    }
}

template Or(Patterns...) if (Patterns.length > 2)
{
    alias Or!(Patterns[0], Or!(Patterns[1..$])) Or;
}

template Or(Patterns...) if (Patterns.length == 1)
{
    alias Patterns[0] Or;
}

struct And(alias Pattern1, alias Pattern2)
{
    template Match(T...)
    {
        alias Pattern1.Match!(T) P1;
        alias Pattern2.Match!(Pattern1.Match!(T).Rest) P2;
        static if (P1.successful && P2.successful)
        {
            enum successful = true;
            alias TypeTuple!(P1.Types, P2.Types) Types;
            alias P2.Rest Rest;
        }
        else
            mixin Failure!(T);
    }
}

template And(Patterns...) if (Patterns.length > 2)
{
    alias And!(Patterns[0], And!(Patterns[1..$])) And;
}

template And(Patterns...) if (Patterns.length == 1)
{
    alias Patterns[0] And;
}

struct Option(alias Pattern)
{
    template Match(T...)
    {
        alias Pattern.Match!(T) P;
        static if (P.successful)
            alias P Match;
        else
        {
            enum successful = true;
            alias TypeTuple!() Types;
            alias T Rest;
        }
    }
}

struct ZeroOrMore(alias Pattern)
{
    template Match(T...)
    {
        alias Pattern.Match!(T) P;
        static if (P.successful)
        {
            enum successful = true;
            alias TypeTuple!(P.Types, ZeroOrMore!(Pattern).Match!(P.Rest).Types) Types;
            alias ZeroOrMore!(Pattern).Match!(P.Rest).Rest Rest;
        }
        else
        {
            enum successful = true;
            alias TypeTuple!() Types;
            alias T Rest;
        }
    }
}

struct OneOrMore(alias Pattern)
{
    template Match(T...)
    {
        alias Pattern.Match!(T) P;
        static if (P.successful)
        {
            enum successful = true;
            alias TypeTuple!(P.Types, ZeroOrMore!(Pattern).Match!(P.Rest).Types) Types;
            alias ZeroOrMore!(Pattern).Match!(P.Rest).Rest Rest;
        }
        else
            mixin Failure!(T);
    }
}



void main()
{
    mixin(grammar(g));
    //alias TypeTuple!(byte,int,double,int) Input;
    //alias SubType!(ulong) Pattern;
    //alias Pattern.Match!(Input) Result;
    //writeln(Result.successful,": ", Result.Types.stringof, " / ", Result.Rest.stringof);
    
    writeln(Pattern(`. {. first}`));
}