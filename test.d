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

/+
mixin( grammar!(Memoization.yes)( `
Test:
  Div <- Block( 'div' )
  Block( Tag ) <- Open2( Tag )
                  ( Block( Tag ) / ( !(HtmlTag( "/" Tag )) .))*
                  HtmlTag( "/" Tag )

  HtmlTag( Tag ) <- "<" Tag ">"

  Open1( Tag )  <- "<" Tag ">"
  Open2( Tag )  <- HtmlTag( Tag )
`));
+/

struct GenericTest(TParseTree)
{
    struct Test
    {
    enum name = "Test";
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static bool isRule(string s)
    {
        switch(s)
        {
            default:
                if (s.length >= 10 && s[0..10] == "Test.Div!(") return true;
                if (s.length >= 12 && s[0..12] == "Test.Block!(") return true;
                if (s.length >= 14 && s[0..14] == "Test.HtmlTag!(") return true;
                if (s.length >= 12 && s[0..12] == "Test.Open1!(") return true;
                if (s.length >= 12 && s[0..12] == "Test.Open2!(") return true;
                return false;
        }
    }
    mixin decimateTree;
    alias spacing Spacing;

    static TParseTree Div(TParseTree p)
    {
        if(auto m = tuple("Div",p.end) in memo)
        {
            //writeln("Found Div at pos ",p.end);
            return *m;
        }
        else
        {
            TParseTree result = pegged.peg.named!(Block!(pegged.peg.literal!("div")), name ~ ".Div")(p);
            if (result.successful)
                    memo[tuple("Div",result.begin)] = result;
            return result;
        }
    }

    static TParseTree Div(string s)
    {
        memo = null;
        return pegged.peg.named!(Block!(pegged.peg.literal!("div")), name ~ ".Div")(TParseTree("", false,[], s));
    }

    template Block(alias Tag)
    {
        static TParseTree Block(TParseTree p)
        {
            if(auto m = tuple("Block("~getName!Tag~")",p.end) in memo)
            {
                //writeln("Found Block("~__traits(identifier,Tag)~") at pos ",p.end);
                return *m;
            }
            else
            {
                TParseTree result = pegged.peg.named!(
                    pegged.peg.and!(Open2!(Tag), 
                                    pegged.peg.zeroOrMore!(pegged.peg.or!(Block!(Tag), 
                                                                          pegged.peg.and!(pegged.peg.negLookahead!(HtmlTag!(pegged.peg.and!(pegged.peg.literal!("/"), 
                                                                                                                                            Tag))), 
                                                                                          pegged.peg.any))), 
                                    HtmlTag!(pegged.peg.and!(pegged.peg.literal!("/"), 
                                                             Tag)))
                    , name ~ ".Block")(p);
                if (result.successful)
                    memo[tuple("Block("~getName!(Tag)~")",result.begin)] = result;
                return result;
            }
        }

        static TParseTree Block(string s)
        {
            memo = null;
            return pegged.peg.named!(
                pegged.peg.and!(Open2!(Tag), 
                                pegged.peg.zeroOrMore!(pegged.peg.or!(Block!(Tag), 
                                                                      pegged.peg.and!(pegged.peg.negLookahead!(HtmlTag!(pegged.peg.and!(pegged.peg.literal!("/"), 
                                                                                                                                        Tag))), 
                                                                                      pegged.peg.any))), 
                                HtmlTag!(pegged.peg.and!(pegged.peg.literal!("/"), 
                                                         Tag))), 
                                     name ~ ".Block")(TParseTree("", false,[], s));
        }
    }
    
    template HtmlTag(alias Tag)
    {
        static TParseTree HtmlTag(TParseTree p)
        {
            string innerName = "HtmlTag!(" ~ getName!(Tag) ~ ")";
            //writeln("HtmlTag called with Tag =", __traits(identifier,Tag), " or ", getName!Tag);
            if(auto m = tuple(innerName,p.end) in memo)
            {
                //writeln("Found HtmlTag("~__traits(identifier,Tag)~") at pos ",p.end);
                return *m;
            }
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<"), Tag, pegged.peg.literal!(">")), name ~ ".HtmlTag!(" ~ getName!(Tag) ~ ")")(p);
                if (result.successful)
                    memo[tuple(innerName,result.begin)] = result;
                return result;
            }
        }

        static TParseTree HtmlTag(string s)
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<"), Tag, pegged.peg.literal!(">")), name ~ ".HtmlTag")(TParseTree("", false,[], s));
        }
    }
    
    template Open1(alias Tag)
    {
        static TParseTree Open1(TParseTree p)
        {
            if(auto m = tuple("Open1("~getName!(Tag)~")",p.end) in memo)
            {
                //writeln("Found Open1("~__traits(identifier,Tag)~") at pos ",p.end);
                return *m;
            }
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<"), Tag, pegged.peg.literal!(">")), name ~ ".Open1")(p);
                if (result.successful)
                    memo[tuple("Open1("~getName!(Tag)~")",result.begin)] = result;
                return result;
            }
        }

        static TParseTree Open1(string s)
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<"), Tag, pegged.peg.literal!(">")), name ~ ".Open1")(TParseTree("", false,[], s));
        }
    }
    
    template Open2(alias Tag)
    {
        static TParseTree Open2(TParseTree p)
        {
            if(auto m = tuple("Open2("~getName!(Tag)~")",p.end) in memo)
            {
                //writeln("Found Open2("~__traits(identifier,Tag)~") at pos ",p.end);
                return *m;
            }
            else
            {
                TParseTree result = pegged.peg.named!(HtmlTag!(Tag), name ~ ".Open2")(p);
                if (result.successful)
                    memo[tuple("Open2("~getName!(Tag)~")",result.begin)] = result;
                return result;
            }
        }

        static TParseTree Open2(string s)
        {
            memo = null;
            return pegged.peg.named!(HtmlTag!(Tag), name ~ ".Open2")(TParseTree("", false,[], s));
        }
    }
     
    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Div(p));
        result.children = [result];
        result.name = "Test";
        return result;
    }

    static TParseTree opCall(string input)
    {
        memo = null;
        return Test(TParseTree(``, false, [], input, 0, 0));
    }
}
}

alias GenericTest!(ParseTree).Test Test;

mixin(grammar(`
Recursive:
    A <- "a" B / eps
    B <- A

`));

void main() 
{
    writeln(Recursive("aaa"));
    writeln(Test("<div></div>"));
    
    
    //writeln(getName!(a)());
/+
    writeln( grammar!(Memoization.yes)( `
Test:
  Div <- Block( 'div' )
  Block( Tag ) <- Open2( Tag )
                  ( Block( Tag ) / ( !(HtmlTag( "/" Tag )) .))*
                  HtmlTag( "/" Tag )

  HtmlTag( Tag ) <- "<" Tag ">"

  Open1( Tag )  <- "<" Tag ">"
  Open2( Tag )  <- HtmlTag( Tag )
    `));
+/

}