/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.typecons;

import pegged.grammar;


struct Parameterized
{
    import std.typecons:Tuple, tuple;
    static ParseTree[Tuple!(string, uint)] memo;
    enum names = [`Nested`:true];
    mixin decimateTree;
    alias spacing Spacing;

    static ParseTree Nested(alias Elem, T)(T p) if (is(T == ParseTree))
    {
        if(auto m = tuple("Nested",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(discard!(literal!("(")), 
                                               Nested!(Elem,ParseTree), 
                                               discard!(literal!(")"))),
                                          identifier
                                         ), "Nested")(p);
            memo[tuple("Nested",p.end)] = result;
            return result;
        }
    }

    static ParseTree Nested(alias Elem, T)(T p) if (is(T:string))
    {
        memo = null;        
        return Nested!(Elem)(ParseTree("", false,[], p));
    }

}

struct Gram(alias a)
{
    import std.typecons:Tuple, tuple;
    static ParseTree[Tuple!(string, uint)] memo;
    enum names = [`A`:true, `rule`:true];
    mixin decimateTree;
    alias spacing Spacing;

    static ParseTree A(ParseTree p)
    {
        if(auto m = tuple("A",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("a")), "A")(p);
            memo[tuple("A",p.end)] = result;
            return result;
        }
    }

    static ParseTree A(string s)
    {
        memo = null;        
        return A(ParseTree("", false,[], s));
    }

    static ParseTree rule(alias b, T : ParseTree)(T p)
    {
        if(auto m = tuple("rule",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(a, b), "rule")(p);
            memo[tuple("rule",p.end)] = result;
            return result;
        }
    }

    static ParseTree rule(alias b, T : string)(T p)
    {
        memo = null;        
        ParseTree result = named!(and!(a, b), "rule")(ParseTree("",false,[],p));
        memo[tuple("rule",0)] = result;
    }

    static ParseTree opCall(T)(T p) // !
    {
        memo = null; // !
        ParseTree result = decimateTree(A(p));
        result.children = [result];
        result.name = "Gram";
        return result;
    }

}

/**
 * TODO: when a rule is templated, both the string and the parse string version must be templated
 *       Add a ,T : ParseTree)(T p) or ,T : string)(T p) at the end of the name 
 *       For the string case: 
 *     static ParseTree rule(alias b, T : string)(T p)
    {
        memo = null;        
        ParseTree result = named!(code, "rule")(ParseTree("",false,[],p));
        memo[tuple("rule",0)] = result;
    }

    TODO: The same for parameterized grammars. opCall must be modified.
    TODO: updates doc to explain spacing can be user-defined (spaceAnd will call it)
    TODO: add an enum inside ParseTree's, containing the rules's name, to enable final switch selection
    TODO: qualified names for rules (grammarName.ruleName)
    TODO: parameterize the grammars on a ParseTree type
    TODO: inlining
    TODO: modify the gen_grammar to regenerate correctly the new engine
    TODO: modify the makefile
    TODO: fuse with the master branch
    TODO: grammar introspection: grammar name, rule names, call graph, 
**/

struct Arithmetic
{
    import std.typecons:Tuple, tuple;
    static ParseTree[Tuple!(string, uint)] memo;
    static bool[string] names;
    static this()
    {
        names = [`Term`:true, `Add`:true, `Sub`:true, `Factor`:true, `Mul`:true, `Div`:true, `Primary`:true, `Parens`:true, `Neg`:true, `Number`:true];
    }
    mixin decimateTree;
    alias spacing Spacing;

    static ParseTree Term(ParseTree p)
    {
        if(auto m = tuple("Term",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(spaceAnd!(Spacing, Factor, zeroOrMore!(or!(spaceAnd!(Spacing, Add), spaceAnd!(Spacing, Sub)))), "Term")(p);
            memo[tuple("Term",p.end)] = result;
            return result;
        }
    }

    static ParseTree Term(string s)
    {
        memo = null;
        return Term(ParseTree("", false,[], s));
    }

    static ParseTree Add(ParseTree p)
    {
        if(auto m = tuple("Add",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(spaceAnd!(Spacing, literal!("+"), Factor), "Add")(p);
            memo[tuple("Add",p.end)] = result;
            return result;
        }
    }

    static ParseTree Add(string s)
    {
        memo = null;
        return Add(ParseTree("", false,[], s));
    }

    static ParseTree Sub(ParseTree p)
    {
        if(auto m = tuple("Sub",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(spaceAnd!(Spacing, literal!("-"), Factor), "Sub")(p);
            memo[tuple("Sub",p.end)] = result;
            return result;
        }
    }

    static ParseTree Sub(string s)
    {
        memo = null;
        return Sub(ParseTree("", false,[], s));
    }

    static ParseTree Factor(ParseTree p)
    {
        if(auto m = tuple("Factor",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(spaceAnd!(Spacing, Primary, zeroOrMore!(or!(spaceAnd!(Spacing, Mul), spaceAnd!(Spacing, Div)))), "Factor")(p);
            memo[tuple("Factor",p.end)] = result;
            return result;
        }
    }

    static ParseTree Factor(string s)
    {
        memo = null;
        return Factor(ParseTree("", false,[], s));
    }

    static ParseTree Mul(ParseTree p)
    {
        if(auto m = tuple("Mul",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(spaceAnd!(Spacing, literal!("*"), Primary), "Mul")(p);
            memo[tuple("Mul",p.end)] = result;
            return result;
        }
    }

    static ParseTree Mul(string s)
    {
        memo = null;
        return Mul(ParseTree("", false,[], s));
    }

    static ParseTree Div(ParseTree p)
    {
        if(auto m = tuple("Div",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(spaceAnd!(Spacing, literal!("/"), Primary), "Div")(p);
            memo[tuple("Div",p.end)] = result;
            return result;
        }
    }

    static ParseTree Div(string s)
    {
        memo = null;
        return Div(ParseTree("", false,[], s));
    }

    static ParseTree Primary(ParseTree p)
    {
        if(auto m = tuple("Primary",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(spaceAnd!(Spacing, Parens), spaceAnd!(Spacing, Neg), spaceAnd!(Spacing, Number)), "Primary")(p);
            memo[tuple("Primary",p.end)] = result;
            return result;
        }
    }

    static ParseTree Primary(string s)
    {
        memo = null;
        return Primary(ParseTree("", false,[], s));
    }

    static ParseTree Parens(ParseTree p)
    {
        if(auto m = tuple("Parens",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(spaceAnd!(Spacing, discard!(literal!("(")), Term, discard!(literal!(")"))), "Parens")(p);
            memo[tuple("Parens",p.end)] = result;
            return result;
        }
    }

    static ParseTree Parens(string s)
    {
        memo = null;
        return Parens(ParseTree("", false,[], s));
    }

    static ParseTree Neg(ParseTree p)
    {
        if(auto m = tuple("Neg",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(spaceAnd!(Spacing, literal!("-"), Primary), "Neg")(p);
            memo[tuple("Neg",p.end)] = result;
            return result;
        }
    }

    static ParseTree Neg(string s)
    {
        memo = null;
        return Neg(ParseTree("", false,[], s));
    }

    static ParseTree Number(ParseTree p)
    {
        if(auto m = tuple("Number",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(spaceAnd!(Spacing, fuse!(spaceAnd!(Spacing, oneOrMore!(charRange!('0', '9'))))), "Number")(p);
            memo[tuple("Number",p.end)] = result;
            return result;
        }
    }

    static ParseTree Number(string s)
    {
        memo = null;
        return Number(ParseTree("", false,[], s));
    }

    static ParseTree opCall(ParseTree p)
    {
        ParseTree result = decimateTree(Term(p));
        result.children = [result];
        result.name = "Arithmetic";
        return result;
    }

    static ParseTree opCall(string input)
    {
        memo = null;
        return Arithmetic(ParseTree(``, false, [], input, 0, 0));
    }
}

    mixin(grammar(`
    Arithmetic2:
    Term    < Factor (Add / Sub)*
    Add     < "+" Factor
    Sub     < "-" Factor
    Factor  < Primary (Mul / Div)*
    Mul     < "*" Primary
    Div     < "/" Primary
    Primary < Parens / Neg / Number
    Parens  < :"(" Term :")"
    Neg     < "-" Primary
    Number  < ~([0-9]+)
`));

    
void main()
{
    auto input = "1+(1*7898/(9-8+4/1*44-23/45))";
writeln(grammar(`
    Arithmetic2:
    Term    < Factor (Add / Sub)*
    Add     < "+" Factor
    Sub     < "-" Factor
    Factor  < Primary (Mul / Div)*
    Mul     < "*" Primary
    Div     < "/" Primary
    Primary < Parens / Neg / Number
    Parens  < :"(" Term :")"
    Neg     < "-" Primary
    Number  < ~([0-9]+)
`));
    int N = 256;
    foreach(i; 0..4)
    {
        auto b = benchmark!(()=> Arithmetic2(input), ()=> Arithmetic(input))(N);
        auto first = b[0].to!("msecs",float)/N;
        auto second = b[1].to!("msecs",float)/N;
        writeln(first, " ms/call  ", second, " ms/call, ", (first/second-1)*100, "% speedup");
        input = input ~ "+" ~ input ~ "-" ~ input;
    }

}