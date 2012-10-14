/**
This module is an attempt at introducing pattern-matching in D.

Pattern matching is a type- or value-matching present in functional languages like ML or Haskell.

The goal here is to obtain code like:

----
Pattern!"[ a, b, ... ]" p1; // match any range with at least two elements
                            // the rest being discarded.

auto m1 = p1([0,1,2,3,4]); // match and associate 'a' and 'b' with O and 1.
assert(m1.a == 0 && m1.b == 1);
auto m2 = p1("abc");
assert(m2.a = 'a' && m2.b == 'b');
auto m3 = p1(tuple("abc",0)); // not a range, the pattern fails
assert(m3 == matchFailure; // predefined constant

Pattern!"Tuple!(.,.) t" p2; // match any std.typecons.Tuple with two elements, of any type
auto m4 = p2(tuple("abc",0));
assert(m4.t = tuple("abc",0); // only the global pattern is named, and hence captured
assert(p2(tuple("abc")) == matchFailure); // only one member -> failure
assert(p2(tuple("abc",0,1)) == matchFailure); // three members -> failure (the pattern has no trailing ...

Pattern!" . { int, double, double }" p3; // match any aggregate (struct, class)
                                         // with three members being int,double and double, in that order.
----

Don't salivate, it's not implemented yet.
*/
module pegged.examples.pattern;

import std.range;
import std.traits;
import std.typecons;
import std.typetuple;

import pegged.grammar;

mixin(grammar(`
Pattern:
    Global     <  Choice :eoi
    Choice     <  Sequence (:'/' Sequence)*
    Sequence   <  Primary (:',' Primary)*
    Primary    <  (Aggregate / Range / Identifier / Literal / REST / ANY) Name?

    Name       <  identifier
    Aggregate  <  (Identifier / ANY) (Tuple / Record)
    Tuple      <  '{' (Choice (:',' Choice)*)? '}'
    Record     <  '{' Field (','Field)* '}'
    Field      <  Identifier :'=' Choice
    Range      <  '[' (Choice (:',' Choice)*)? ']'
    Identifier <  identifier
    Literal    <  Number / Char / String/ Bool
    Number     <~ Digit+ ('.' Digit*)?
    Digit      <  [0-9]
    Char       <~ quote . quote # TODO: adding escape chars and Unicode chars
    String     <~ doublequote (!doublequote .)* doublequote
    Bool       <  "true" / "false"

    REST       <  "..."
    ANY        <  "."
    Spacing    <: spacing
`));

template TypeFailure(T...)
{
    enum successful = false;
    alias TypeTuple!() Types;
	enum begin = 0;
	enum end = 0;
    alias T Rest;
}

struct TypeAny
{
    template Match(T...)
    {
        static if (T.length == 0)
            mixin TypeFailure!(T);
        else
        {
            enum successful = true;
            alias T[0..1] Types;
			enum begin = 0;
			enum end = 1;
            alias T[1..$] Rest;
        }
    }
}

struct TypeEnd
{
    template Match(T...)
    {
        static if (T.length == 0)
        {
            enum successful = true;
            alias TypeTuple!() Types;
			enum begin = 0;
			enum end = 0;
            alias T Rest;
        }
        else
            mixin TypeFailure!(T);
    }
}

struct TypeEps
{
    template Match(T...)
    {
        enum successful = true;
        alias TypeTuple!() Types;
		enum begin = 0;
		enum end = 0;
        alias T Rest;
    }
}

struct TypeLiteral(U...)
{
    template Match(T...)
    {
        static if (T.length < U.length || !is(T[0..U.length] == U))
            mixin TypeFailure!(T);
        else
        {
            enum successful = true;
            alias T[0..U.length] Types;
			enum begin = 0;
			enum end = U.length;
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

struct TypeSubType(U...)
{
    template Match(T...)
    {
        static if (T.length < U.length || !isSubtype!(T[0..U.length],U))
            mixin TypeFailure!(T);
        else
        {
            enum successful = true;
            alias T[0..U.length] Types;
			enum begin = 0;
			enum end = U.length;
            alias T[U.length..$] Rest;
        }
    }
}

struct TypeOr(alias Pattern1, alias Pattern2)
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

template TypeOr(Patterns...) if (Patterns.length > 2)
{
    alias TypeOr!(Patterns[0], TypeOr!(Patterns[1..$])) TypeOr;
}

template TypeOr(Patterns...) if (Patterns.length == 1)
{
    alias Patterns[0] TypeOr;
}

struct TypeAnd(alias Pattern1, alias Pattern2)
{
    template Match(T...)
    {
        alias Pattern1.Match!(T) P1;
        alias Pattern2.Match!(Pattern1.Match!(T).Rest) P2;
        static if (P1.successful && P2.successful)
        {
            enum successful = true;
            alias TypeTuple!(P1.Types, P2.Types) Types;
			enum begin = P1.begin;
			enum end = P2.end;
            alias P2.Rest Rest;
        }
        else
            mixin TypeFailure!(T);
    }
}

template TypeAnd(Patterns...) if (Patterns.length > 2)
{
    alias TypeAnd!(Patterns[0], TypeAnd!(Patterns[1..$])) And;
}

template TypeAnd(Patterns...) if (Patterns.length == 1)
{
    alias Patterns[0] TypeAnd;
}

struct TypeOption(alias Pattern)
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
			enum begin = 0;
			enum end = 0;
            alias T Rest;
        }
    }
}

struct TypeZeroOrMore(alias Pattern)
{
    template Match(T...)
    {
        alias Pattern.Match!(T) P;
        static if (P.successful)
        {
            enum successful = true;
            alias TypeTuple!(P.Types, TypeZeroOrMore!(Pattern).Match!(P.Rest).Types) Types;
			enum begin = P.begin;
			alias TypeZeroOrMore!(Pattern).Match!(P.Rest) More;
			enum end = P.end + More.end;
            alias TypeZeroOrMore!(Pattern).Match!(P.Rest).Rest Rest;
        }
        else
        {
            enum successful = true;
            alias TypeTuple!() Types;
			enum begin = 0;
			enum end = 0;
            alias T Rest;
        }
    }
}

struct TypeOneOrMore(alias Pattern)
{
    template Match(T...)
    {
        alias Pattern.Match!(T) P;
        static if (P.successful)
        {
            enum successful = true;
            alias TypeTuple!(P.Types, TypeZeroOrMore!(Pattern).Match!(P.Rest).Types) Types;
			enum begin = P.begin;
			alias TypeZeroOrMore!(Pattern).Match!(P.Rest) More;
			enum end = P.end + More.end;
            alias TypeZeroOrMore!(Pattern).Match!(P.Rest).Rest Rest;
        }
        else
            mixin TypeFailure!(T);
    }
}

/**
Discards the matched types, but propagate the indices.
*/
struct TypeDiscardMatch(alias Pattern)
{
	template Match(T...)
	{
		alias Pattern.Match!(T) P;
		static if (P.successful)
		{
			enum successful = true;
			alias TypeTuple!() Types; // Forget the match,
			enum begin = P.begin;     // but propagate indices
			enum end = P.end;         //
			alias P.Rest Rest;        // and the input
		}
		else
			mixin TypeFailure!(T);
	}
}

struct TypePosLookAhead(alias Pattern)
{
	template Match(T...)
	{
		alias Pattern.Match!(T) P;
		static if (P.successful)
		{
			enum successful = true;
			alias TypeTuple!() Types;
			enum begin = 0;
			enum end = 0;
			alias T Rest;
		}
		else
			mixin TypeFailure!(T);
	}
}

struct TypeNegLookAhead(alias Pattern)
{
	template Match(T...)
	{
		alias Pattern.Match!(T) P;
		static if (P.successful)
			mixin TypeFailure!(T);
		else
		{
			enum successful = true;
			alias TypeTuple!() Types;
			enum begin = 0;
			enum end = 0;
			alias T Rest;
		}
	}
}

struct isRange
{
	template Match(T...)
	{
		static if (isInputRange!(T[0]))
		{
			enum successful = true;
			alias T[0] Types;
			enum begin = 0;
			enum end = 1;
			alias T[1..$] Rest;
		}
		else
			mixin TypeFailure!(T);
	}
}

struct TypeSatisfy(alias test)
{
	template Match(T...)
	{
		static if (test!(T[0]))
		{
			enum successful = true;
			alias T[0] Types;
			enum begin = 0;
			enum end = 1;
			alias T[1..$] Rest;
		}
		else
			mixin TypeFailure!(T);
	}
}

struct MatchResult(size_t junction, T...)
{
	bool successful;
	T[0..junction] match;
	size_t begin;
	size_t end;
	T[junction..$] rest;
}

struct Success(M...)
{
	M match;
	size_t begin;
	size_t end;

	auto rest(R...)(R rest)
	{
		return MatchResult!(M.length, M, R)(true, match, begin, end, rest);
	}
}

Success!(M) success(M...)(M match, size_t begin, size_t end)
{
	return Success!(M)(match, begin, end);
}

auto failure(R...)(R rest)
{
	return MatchResult!(0,R)(false, 0,0, rest);
}

auto anyValue(Args...)(Args args)
{
	static if (Args.length > 0)
		return success(args[0],0,1).rest(args[1..$]);
	else
		return failure(args);
}

auto endValue(Args...)(Args args)
{
	static if (Args.length == 0)
		return success(0,0).rest(args);
	else
		return failure(args);
}

/+
template literalValue(values...)
{
	auto literalValue(Args...)(Args args)
	{
		if (
			return success(0,values.length).rest(args[values.length..$]);
		else
			return failure(args);
	}
}
+/

Tuple!(ElementType!R, R) rangeAny(R)(R r) if (isInputRange!R)
{
	if (r.empty)
		throw new Exception("Empty range, could not pattern-match");
	else
	{
		auto head = r.front();
		r.popFront();
		return tuple(head, r);
	}
}

Tuple!(R,R) rangeRest(R)(R r) if (isInputRange!R)
{
	if (r.empty)
		throw new Exception("Empty range, could not pattern-match");
	else
		return tuple(r, r); // second r will not be used, but is needed to be compatible with other range patterns: Tuple!(result,rest)
}

template RangePatternResult(R) if (isInputRange!R)
{
	template RangePatternResult(alias T)
	{
		alias ReturnType!(T!R) RangePatternResult;
	}
}

template onRange(RangePatterns...)
{
	auto onRange(R)(R r) if (isInputRange!(R))
	{
		alias RangePatternResult!R GetType;

		staticMap!(GetType, RangePatterns) _temp;

		static if (RangePatterns.length > 0)
		{
			_temp[0] = RangePatterns[0](r);
			foreach(i,pat; RangePatterns[1..$])
				_temp[i+1] = RangePatterns[i+1](_temp[i][1]);
		}

		struct MatchResult
		{
			GetType!(RangePatterns[0]).Types[0] a;
			GetType!(RangePatterns[1]).Types[0] b;
			GetType!(RangePatterns[2]).Types[0] c;
		}

		return MatchResult(_temp[0][0],_temp[1][0],_temp[2][0]);
	}
}

