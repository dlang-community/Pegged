/**
This module contains the engine behind Pegged, the expression templates building blocks to create a top-down
recursive-descent parser.

The terminals and non-terminals described here are meant to be used inside a Pegged grammar. As such, they are a bit less
user-friendly than what's output by pegged.grammar. For example they take a ParseTree as input, not a string.

See the /docs directory for the full documentation as markdown files.
*/
module pegged.peg;

import std.conv;
import std.range: equal;
import std.typetuple;

/**
The basic parse tree, as used throughout the project. 
You can defined your own parse tree node, but respect the basic layout.
*/
struct ParseTree
{
    string name; /// The node name
    bool successful; /// Indicates whether a parsing was successful or not
    string[] matches; /// The matched input's parts. Some expressions match at more than one place, hence matches is an array.
    
    string input; /// The input string that generated the parse tree. Stored here for the parse tree to be passed to other expressions, as input.
    size_t begin, end; /// Indices for the matched part (from the very beginning of the first match to the last char of the last match.
    
    ParseTree[] children; /// The sub-trees created by sub-rules parsing.
    
    /**
    Basic toString for easy pretty-printing.
    */
    string toString(string tabs = "") 
    {
        string result = name ~ (successful ? " " : "(failure) ") ~ " " ~ to!string([begin, end]) ~ to!string(matches) ~ "\n";
        foreach(i,child; children)
            result ~= tabs ~ " +-" ~ child.toString(tabs ~ ((i < children.length -1 ) ? " | " : "   "));
        //result ~= "\n";
        
        return result;
    }
    
        
    /**
    Comparing ParseTree's.
    */
    bool opEquals(ParseTree p)
    {
        return ( p.name       == name
              && p.successful == successful
              && p.matches    == matches
              && p.input      == input
              && p.begin      == begin
              && p.end        == end
              && equal(p.children, children));
    }
}


unittest // ParseTree testing
{
    ParseTree p;
    assert(p == p, "Self-identity on null tree.");
    
    p = ParseTree("Name", true, ["abc", "", "def"], "input", 0, 1, null);
    assert(p == p, "Self identity on non-null tree.");
    
    ParseTree q = p;
    assert(p == q, "Copying makes equal trees.");
    
    q.children = [p,p];
    assert(p != q, "Tree with different children are not equal.");
    
    p.children = [p,p];
    assert(p == q, "Adding equivalent children is OK.");
    
    p.matches = null;
    assert(p != q, "Nulling matches makes trees unequal.");
    
    p.matches = q.matches;
    assert(p == q, "Copying matches makes equal trees.");
}


/**
Basic rule, that always fail without consuming.
*/
ParseTree fail(ParseTree p)
{
    return ParseTree("fail", false, [], p.input, p.end, p.end, null);
}


/// ditto
ParseTree fail(string input)
{
    return fail(ParseTree("", false, [], input));
}

unittest // 'fail' unit test
{
    ParseTree input = ParseTree("input", true, [], "This is the input string.", 0,0, null);
    ParseTree result = fail(input);
    assert(!result.successful, "'fail' fails.");
    assert(result.matches is null, "'fail' makes no match.");
    assert(result.input == input.input, "'fail' does not change the input.");
    assert(result.end == input.end, "'fail' puts the index after the previous parse.");
    assert(result.children is null, "'fail' has no children.");
    
    result = fail("This is the input string.");
    assert(!result.successful, "'fail' fails.");
}

/**
Matches the end of input. Fails if there is any character left.
*/
ParseTree eoi(ParseTree p)
{
    return ParseTree("eoi", p.end == p.input.length, [], p.input, p.end, p.end);
}

/// ditto
ParseTree eoi(string input)
{
    return eoi(ParseTree("", false, [], input));
}

alias eoi endOfInput; /// helper alias.

unittest // 'eoi' unit test
{
    ParseTree input = ParseTree("input", true, [], "This is the input string.", 0,0, null);
    ParseTree result = eoi(input);
    assert(!result.successful, "'eoi' fails on non-null string.");
    assert(result.matches is null, "'eoi' makes no match.");
    assert(result.input == input.input, "'eoi' does not change the input.");
    assert(result.end == input.end, "'eoi' puts the index after the previous parse.");
    assert(result.children is null, "'eoi' has no children.");
    
    input = ParseTree("input", true, [], "", 0,0, null);
    result = eoi(input);
    assert(result.successful, "'eoi' succeeds on strings of length 0.");
    result = eoi("");
    assert(result.successful, "'eoi' succeeds on strings of length 0.");
    result = eoi(null);
    assert(result.successful, "'eoi' succeeds on null strings");
}   

/**
Match any character. As long as there is at least a character left in the input, it succeeds. 
Conversely, it fails only if called at the end of the input.
*/
ParseTree any(ParseTree p)
{
    if (p.end < p.input.length)
        return ParseTree("any", true, [p.input[p.end..p.end+1]], p.input, p.end, p.end+1);
    else
        return ParseTree("any", false, [], p.input, p.end, p.end);
}

/// ditto
ParseTree any(string input)
{
    return any(ParseTree("", false, [], input));
}

unittest // 'any' unit test
{
    ParseTree input = ParseTree("input", true, [], "This is the input string.", 0,0, null);
    ParseTree result = any(input);
    
    assert(result.successful, "'any' succeeds on non-null strings.");
    assert(result.matches  == ["T"], "'any' matches the first char in an input.");
    assert(result.input == input.input, "'any' does not change the input.");
    assert(result.end == input.end+1, "'any' advances the index by one position.");
    assert(result.children is null, "'any' has no children.");
    
    result = any("a");
    assert(result.successful, "'any' matches on strings of length one.");
    assert(result.matches == ["a"], "'any' matches the first char in an input.");
    assert(result.input == "a", "'any' does not change the input.");
    assert(result.end == 1, "'any' advances the index by one position.");
    assert(result.children is null, "'any' has no children.");
        
    input = ParseTree("input", true, [], "", 0,0, null);
    
    result = any(input);
    assert(!result.successful, "'any' fails on strings of length 0.");
    assert(result.matches is null, "'any' does not match on strings of length 0.");
    assert(result.end == 0, "'any' does not advance the index.");
    
    result = any("");
    assert(!result.successful, "'any' fails on strings of length 0.");
    assert(result.matches is null, "'any' does not match on strings of length 0.");
    assert(result.end == 0, "'any' does not advance the index.");
    
    result = any(null);
    assert(!result.successful, "'any' fails on null strings.");
    assert(result.matches is null, "'any' does not match on strings of length 0.");
    assert(result.end == 0, "'any' does not advance the index.");
}

/**
Represents a literal in a PEG, like "abc" or 'abc' (or even '').
It succeeds if a prefix of the input is equal to its template parameter and fails otherwise.
*/
template literal(string s)
{
    ParseTree literal(ParseTree p)
    {
        if (p.end+s.length <= p.input.length && p.input[p.end..p.end+s.length] == s)
            return ParseTree("literal("~s~")", true, [s], p.input, p.end, p.end+s.length);
        else
            return ParseTree("literal("~s~")", false, [], p.input, p.end, p.end);
    }

    ParseTree literal(string input)
    {
        return .literal!(s)(ParseTree("", false, [], input));
    }
}

unittest // 'literal' unit test
{
    ParseTree input = ParseTree("input", true, [], "abcdef", 0,0, null);
    
    alias literal!"a" a;
    alias literal!"abc" abc;
    alias literal!"" empty;
    
    ParseTree result = a(input);
    
    assert(result.successful, "'a' succeeds on inputs beginning with 'a'.");
    assert(result.matches  == ["a"], "'a' matches the 'a' at the beginning.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end+1, "'a' advances the index by one position.");
    assert(result.children is null, "'a' has no children.");
    
    result = a("abcdef");
    
    assert(result.successful, "'a' succeeds on inputs beginning with 'a'.");
    assert(result.matches  == ["a"], "'a' matches the 'a' at the beginning.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end+1, "'a' advances the index by one position.");
    assert(result.children is null, "'a' has no children.");
    
    result = abc(input);
    
    assert(result.successful, "'abc' succeeds on inputs beginning with 'abc'.");
    assert(result.matches  == ["abc"], "'abc' matches 'abc' at the beginning.");
    assert(result.input == input.input, "'abc' does not change the input.");
    assert(result.end == input.end+3, "'abc' advances the index by 3 positions.");
    assert(result.children is null, "'abc' has no children.");
    
    result = abc("abcdef");
    
    assert(result.successful, "'abc' succeeds on inputs beginning with 'abc'.");
    assert(result.matches  == ["abc"], "'abc' matches 'abc' at the beginning.");
    assert(result.input == input.input, "'abc' does not change the input.");
    assert(result.end == input.end+3, "'abc' advances the index by 3 positions.");
    assert(result.children is null, "'abc' has no children.");
    
    result = empty(input);
    
    assert(result.successful, "'' succeeds on non-null inputs.");
    assert(result.matches  == [""], "'' matches '' at the beginning.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");
    
    result = empty("abcdef");
    
    assert(result.successful, "'' succeeds on non-null inputs.");
    assert(result.matches  == [""], "'' matches '' at the beginning.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");
    
    input.input = "bcdef";
    
    result = a(input);
    
    assert(!result.successful, "'a' fails on inputs not beginning with 'a'.");
    assert(result.matches is null, "'a' makes no match on 'bcdef'.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end, "'a' does not advances the index on 'bcdef'.");
    assert(result.children is null, "'a' has no children.");
    
    result = abc(input);
    
    assert(!result.successful, "'abc' fails on inputs not beginning with 'abc'.");
    assert(result.matches is null, "'abc' does no match on 'bcdef'.");
    assert(result.input == input.input, "'abc' does not change the input.");
    assert(result.end == input.end, "'abc' does not advance the index on 'bcdef'.");
    assert(result.children is null, "'abc' has no children.");
    
    result = empty(input);
    
    assert(result.successful, "'' succeeds on non-null inputs.");
    assert(result.matches == [""], "'' matches '' at the beginning.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");
    
    input.input = "";
    
    result = a(input);
    
    assert(!result.successful, "'a' fails on empty strings.");
    assert(result.matches is null, "'a' does not match ''.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end, "'a' does not advance the index on 'bcdef'.");
    assert(result.children is null, "'a' has no children.");
    
    result = abc(input);
    
    assert(!result.successful, "'abc' fails on empty strings.");
    assert(result.matches is null, "'abc' does not match ''.");
    assert(result.input == input.input, "'abc' does not change the input.");
    assert(result.end == input.end, "'abc' does not advance the index on 'bcdef'.");
    assert(result.children is null, "'abc' has no children.");
    
    result = empty(input);
    
    assert(result.successful, "'' succeeds on empty strings.");
    assert(result.matches  == [""], "'' matches '' at the beginning, even on empty strings.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");
}

/**
Represents a range of chars, from begin to end, included. So charRange!('a','z') matches
all English lowercase letters. If fails if the input is empty or does not begin with a character
between begin and end.

If begin == end, it will match one char (begin... or end).

begin > end is non-legal.
*/
template charRange(char begin, char end) if (begin <= end)
{
    ParseTree charRange(ParseTree p)
    {
        if (p.end < p.input.length && p.input[p.end] >= begin && p.input[p.end] <= end)
            return ParseTree("charRange("~begin~"," ~ end ~ ")", true, [p.input[p.end..p.end+1]], p.input, p.end, p.end+1);
        else
            return ParseTree("charRange("~begin~"," ~ end ~ ")", false, [], p.input, p.end, p.end);
    }
    
    ParseTree charRange(string input)
    {
        return .charRange!(begin,end)(ParseTree("",false,[],input));
    }
}

unittest // 'charRange' unit test
{
    ParseTree input = ParseTree("input", true, [], "abcdef", 0,0, null);
    
    alias charRange!('a','a') aa;
    alias charRange!('a','b') ab;
    alias charRange!('a','z') az;
    
    static assert(!__traits(compiles, {alias charRange!('z','a') za;}));
    
    ParseTree result = aa(input);
    
    assert(result.successful, "'a-a' succeeds on inputs beginning with 'a'.");
    assert(result.matches  == ["a"], "'a-a' matches the 'a' at the beginning.");
    assert(result.input == input.input, "'a-a' does not change the input.");
    assert(result.end == input.end+1, "'a-a' advances the index by one position.");
    assert(result.children is null, "'a-a' has no children.");
    
    result = ab("abcdef");
    
    assert(result.successful, "'a-b' succeeds on inputs beginning with 'a'.");
    assert(result.matches  == ["a"], "'a-b' matches the 'a' at the beginning.");
    assert(result.input == input.input, "'a-b' does not change the input.");
    assert(result.end == input.end+1, "'a-b' advances the index by one position.");
    assert(result.children is null, "'a-b' has no children.");
    
    result = az(input);
    
    assert(result.successful, "'a-z' succeeds on inputs beginning with 'abc'.");
    assert(result.matches  == ["a"], "'a-z' matches 'a' at the beginning.");
    assert(result.input == input.input, "'a-z' does not change the input.");
    assert(result.end == input.end+1, "'a-z' advances the index by one position.");
    assert(result.children is null, "'a-z' has no children.");
    
    input.input = "bcdef";
    
    result = aa(input);
    
    assert(!result.successful, "'a-a' fails on inputs not beginning with 'a'.");
    assert(result.matches is null, "'a-a' makes no match on 'bcdef'.");
    assert(result.input == input.input, "'a-a' does not change the input.");
    assert(result.end == input.end, "'a-a' does not advances the index on 'bcdef'.");
    assert(result.children is null, "'a-a' has no children.");
    
    result = ab(input);
    
    assert(result.successful, "'a-b' succeeds on inputs beginning with 'b'.");
    assert(result.matches == ["b"], "'a-b' matches on 'bcdef'.");
    assert(result.input == input.input, "'a-b' does not change the input.");
    assert(result.end == input.end+1, "'a-b' advances the index by one position'.");
    assert(result.children is null, "'a-b' has no children.");
    
    result = az(input);
    
    assert(result.successful, "'a-z' succeeds on 'bcdef'.");
    assert(result.matches == ["b"], "'a-z' matches 'b' at the beginning of 'bcdef'.");
    assert(result.input == input.input, "'a-z' does not change the input.");
    assert(result.end == input.end+1, "'a-z' advances the index by one position.");
    assert(result.children is null, "'a-z' has no children.");
    
    input.input = "";
    
    result = aa(input);
    
    assert(!result.successful, "'a-a' fails on empty strings.");
    assert(result.matches is null, "'a-a' does not match ''.");
    assert(result.input == input.input, "'a-a' does not change the input.");
    assert(result.end == input.end, "'a-a' does not advance the index on ''.");
    assert(result.children is null, "'a-a' has no children.");
    
    result = ab(input);
    
    assert(!result.successful, "'a-b' fails on empty strings.");
    assert(result.matches is null, "'a-b' does not match ''.");
    assert(result.input == input.input, "'a-b' does not change the input.");
    assert(result.end == input.end, "'a-b' does not advance the index on ''.");
    assert(result.children is null, "'a-b' has no children.");
    
    result = az(input);
    
    assert(!result.successful, "'a-z' fails on empty strings.");
    assert(result.matches is null, "'a-z' does not match ''.");
    assert(result.input == input.input, "'a-z' does not change the input.");
    assert(result.end == input.end, "'a-z' does not advance the index on ''.");
    assert(result.children is null, "'a-z' has no children.");
    
    input.input = "123";
    
    result = aa(input);
    
    assert(!result.successful, "'a-a' fails on '123'.");
    assert(result.matches is null, "'a-a' does not match '123'.");
    assert(result.input == input.input, "'a-a' does not change the input.");
    assert(result.end == input.end, "'a-a' does not advance the index on '123'.");
    assert(result.children is null, "'a-a' has no children.");
    
    result = ab(input);
    
    assert(!result.successful, "'a-b' fails on '123'.");
    assert(result.matches is null, "'a-b' does not match '123'.");
    assert(result.input == input.input, "'a-b' does not change the input.");
    assert(result.end == input.end, "'a-b' does not advance the index on '123'.");
    assert(result.children is null, "'a-b' has no children.");
    
    result = az(input);
    
    assert(!result.successful, "'a-z' fails on '123'.");
    assert(result.matches is null, "'a-z' does not match '123'.");
    assert(result.input == input.input, "'a-z' does not change the input.");
    assert(result.end == input.end, "'a-z' does not advance the index on '123'.");
    assert(result.children is null, "'a-z' has no children.");
    
    alias charRange!(char.min,char.max) allChars;
        
    assert(allChars("\0").successful);
    assert(allChars("" ~ char.min).successful);
    assert(allChars("" ~ char.max).successful);
    assert(!allChars("").successful);
    assert(allChars("\xFF").successful);
}

/**
eps matches the empty string (usually denoted by the Greek letter 'epsilon') and always succeeds.
It's equivalent to literal!"".
*/
ParseTree eps(ParseTree p)
{
    return ParseTree("eps", true, [""], p.input, p.end, p.end);
}

/**
Basic operator: it matches if all its subrules (stored in the rules template parameter tuple) match
the input successively. Its subrules parse trees are stored as its children and its matches field
will contain all its subrules matches, in order.

----
alias and!(literal!"abc", charRange!('a','z')) rule; // abc followed by any letter between a and z.
ParseTree input = ParseTree("",false,[],"abcd"); // low-level plumbing, the rules described here act on ParseTree's not strings.
                                                 // It's equivalent to "abcd" as input 
auto result = rule(input);

assert(result.successful); // OK, 'abc' followed by 'd'
assert(result.matches == ["abc", "d"]); // stores the matches
assert(result.children.length == 2); // two children, the result of "abc" on "abcd" and the result of [a-z] on "d"

input.input = "abc"; // changing the input string;
assert(!rule(input)).successful); // NOK, abc alone
input.input = "ab";
assert(!rule(input)).successful); // NOK, does not begin by abc
----

If it fails, the last children will contain the failed node. That way, when printing, as sort of diagnostic is given:

----
alias and!(literal!"abc", charRange!('a','z')) rule; // 'abc[a-z]', aka 'abc' followed by any letter between 'a' and 'z'.
ParseTree input = ParseTree("",false,[],"abc1"); // equivalent to "abc1"

auto failure = rule(input);
writeln(failure);
/+
writes:
and(failure)  [0, 3]["abc"]
 +-literal(abc)  [0, 3]["abc"]
 +-charRange(a,z)(failure)  [3, 3][]
+/
----

So we know the global 'and' failed, that the first sub-rule ('abc') succeeded on input[0..3] with "abc" 
and that the second subrule ('[a-z]') failed at position 3 (so, on '1').
*/
ParseTree and(rules...)(ParseTree p) if (rules.length > 0)
{
    bool isNullNode(ParseTree node)
    {
        return (  node.name == "discard" || node.matches is null 
        //|| node.begin == node.end
        ) && node.name != "keep";
    }
    
    ParseTree result = ParseTree("and", false, [], p.input, p.end, p.end, []);
    
    foreach(i,r; rules)
    {
        ParseTree temp = r(result);
        result.end = temp.end;
        if (temp.successful) 
        {
            if (!isNullNode(temp)) // discard empty nodes
            {
                result.matches ~= temp.matches;
                if (temp.name != "drop")
                    result.children ~= temp;
            }
        }
        else
        {
            result.children ~= temp;// add the failed node, to indicate which failed
            return result; // and end the parsing attempt right there
        }
    }        
    result.successful = true;
    return result;
}

/**
Basic operator: it matches if one of its subrules (stored in the rules template parameter tuple) match
the input. The subrules are tested in order, from rules[0] to rules[$-1]. 

The matching subrule parse trees is stored as its only child and its matches field
will contain all the subrule matches, in order.

----
alias or!(literal!"abc", charRange!('a','z')) rule; // abc or, failing that, any letter between a and z.
ParseTree input = ParseTree("",false,[],"defg"); // low-level plumbing, the rules described here act on ParseTree's not strings.
                                                 // It's equivalent to "defg" as input 
auto result = rule(input);

assert(result.successful); // OK
assert(result.matches == ["d"]); // stores the (in this case) only match
assert(result.children.length == 1); // one child, the result of "abc" or [a-z], depending on which rule succeeded.

input.input = "abc"; // changing the input string;
assert(rule(input)).successful); // Still OK
input.input = "1abc";
assert(!rule(input)).successful); // NOK, does not begin by abc nor by [a-z]
----

If it fails, the last children will contain the failed node that matched furthest (longes match). That way, when printing, as sort of diagnostic is given:

----
alias or!(literal!"abc", and!(literal!"ab", charRange!('0','9'))) rule; // 'abc' or 'ab[0-9]'
ParseTree input = ParseTree("",false,[],"abd"); // equivalent to "abd"

auto failure = rule(input);
writeln(failure);
/+
or(failure)  [0, 2]["ab"]
 +-and(failure)  [0, 2]["ab"]
    +-literal(ab)  [0, 2]["ab"]
    +-charRange(0,9)(failure)  [2, 2][]
+/
----

So we know 'or' failed, that the 'and' sub-rule had the longest match, matching 'ab' and failing for [0-9] on index 2.
*/
ParseTree or(rules...)(ParseTree p) if (rules.length > 0)
{
    ParseTree longestFail;
    foreach(i,r; rules)
    {
        ParseTree temp = r(p);
        if (temp.successful)
        {
            temp.children = [temp];
            temp.name = "or";
            return temp;
        }
        else
            longestFail = (temp.end > longestFail.end) ? temp : longestFail;  
    }            
    
    /* all subrules failed, we will take the longest match */
    longestFail.children = [longestFail];
    longestFail.name = "or";
    return longestFail;
}

/**
or special case for literal list ("abstract"/"alias"/...)
*/
ParseTree keywords(kws...)(ParseTree p) if (kws.length > 0)
{
    string keywordCode(string[] keywords)
    {
        string result;
        foreach(kw; keywords)
            result ~= "if (p.end+"~to!string(kw.length) ~ " <= p.input.length "
                ~" && p.input[p.end..p.end+"~to!string(kw.length)~"]==\""~kw~"\") return ParseTree(``,true,[\""~kw~"\"],p.input,p.end,p.end+"~to!string(kw.length)~");\n";
        result ~= "return ParseTree(``,false,[],p.input,p.end,p.end);";
        return result;
    }
    
    mixin(keywordCode([kws]));
}

/**
Tries to match subrule 'r' zero or more times. It always succeeds, since if 'r' fails
from the very beginning, it matched 'r' zero times...

Its matches are those of its subrules (they might be different for each match) and its
children are all the parse trees returned by the successive application of 'r'.

----
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
zeroOrMore  [0, 7]["abc", "d", "abc"]
 +-or  [0, 3]["abc"]
 |  +-literal(abc)  [0, 3]["abc"]
 +-or  [3, 4]["d"]
 |  +-literal(d)  [3, 4]["d"]
 +-or  [4, 7]["abc"]
    +-literal(abc)  [4, 7]["abc"]
+/
----

So we know the first child used the 'literal!"abc"' sub-rule and matched input[0..3]. 
The second matched input[3..4] and the third input[4..7].

----
input = ParseTree("",false,[], "efgh");
result = rule(input);
assert(result.successful); // succeed, even though all patterns failed.
assert(result.children.length == 0);
----
*/
ParseTree zeroOrMore(alias r)(ParseTree p)
{
    auto result = ParseTree("zeroOrMore", true, [], p.input, p.end, p.end);
    auto temp = r(result);
    while(temp.successful)
    {
        result.matches ~= temp.matches;
        result.children ~= temp;
        result.end = temp.end;
        temp = r(result);
    }
    result.successful = true;
    return result;
}

/**
Tries to match subrule 'r' one or more times. If 'r' fails
from the very beginning, it fails and else succeeds.

Its matches are those of its subrules (they might be different for each match) and its
children are all the parse trees returned by the successive application of 'r'.

----
alias oneOrMore!(or!(literal!"abc", literal!"d")) rule; // in PEG-speak:  '("abc" / "d")*'
ParseTree input = ParseTree("",false,[], "abcdabce");

ParseTree result = rule(input);

assert(result.successful);
assert(result.matches == ["abc", "d", "abc"]);
assert(result.end == 7); // matched "abcdabce"[0..7] => "abcdabc". "e" at the end is not part of the parse tree.
assert(result.children.length == 3);
writeln(result);
/+
writes:
oneOrMore  [0, 7]["abc", "d", "abc"]
 +-or  [0, 3]["abc"]
 |  +-literal(abc)  [0, 3]["abc"]
 +-or  [3, 4]["d"]
 |  +-literal(d)  [3, 4]["d"]
 +-or  [4, 7]["abc"]
    +-literal(abc)  [4, 7]["abc"]
+/
----

So we know the first child used the 'literal!"abc"' sub-rule and matched input[0..3]. 
The second matched input[3..4] and the third input[4..7].

----
input = ParseTree("",false,[], "efgh");
result = rule(input);
assert(!result.successful); // fails, since it failed on the first try.
----
*/
ParseTree oneOrMore(alias r)(ParseTree p)
{
    auto result = ParseTree("oneOrMore", false, [], p.input, p.end, p.end);
    auto temp = r(result);
    if (!temp.successful)
    {
        result.end = temp.end;
    }
    else
    {
        while(temp.successful)
        {
            result.matches ~= temp.matches;
            result.children ~= temp;
            result.end = temp.end;
            temp = r(result);
        }
        result.successful = true;
    }
    return result;
}

/**
Given a subrule 'r', represents the expression 'r?'. It tries to match 'r' and if this matches
successfully, it returns this match. If 'r' failed, 'r?' is still a success, but without any child nor match.

----
alias option!(literal!"abc") rule; // Aka '"abc"?'
ParseTree input = ParseTree("",false,[],"abcd");

ParseTree result = rule(input);
assert(result.successful);
assert(result.matches == ["abc"]);
assert(result.children.length == 1);
assert(result.children[0] == literal!"abc"(input));
----
*/
ParseTree option(alias r)(ParseTree p)
{
    auto result = r(p);
    if (result.successful)
        return ParseTree("option", true, result.matches, result.input, result.begin, result.end, [result]);
    else
        return ParseTree("option", true, [], p.input, p.end, p.end, null);
}

/**
Internal helper template, to get a parse tree node with a name. For example, given:
----
alias or!(literal!"abc", charRange!('0','9')) myRule;
----

myRule gives nodes named "or", since its the parent rule. If you want nodes to be named "myRule":

----
alias named!(
             or!(literal!"abc", charRange!('0','9')),
             "myRule"
            ) myRule;
----
*/
template named(alias r, string name)
{
    ParseTree named(ParseTree p)
    {
        /+static if (is(r))
        {
            r temp;
            ParseTree result = temp(p);
        }
        else+/
            ParseTree result = r(p);
        
        result.name = name;
        return result;
    }
}

/**
Low-level representation for the expression 'r {act}'. That is, it applies rule 'r'
on the input and then calls 'act' on the resulting ParseTree.
*/
template action(alias r, alias act)
{
    ParseTree action(ParseTree p)
    {
        return act(r(p));
    }
}

/**
Concatenates a ParseTree's matches as one match and discards its children. Equivalent to the expression '~r'.
*/
ParseTree fuse(alias r)(ParseTree p)
{
    p = r(p);
    string fused;
    foreach(match; p.matches)
        fused ~= match;
    p.matches = [fused];
    p.children = null; // also discard children
    return p;
}

/**
Calls 'r' on the input and then discards its children nodes.
*/
ParseTree discardChildren(alias r)(ParseTree p)
{
    p = r(p);
    p.children = null;
    return p;
}

/**
Calls 'r' on the input and then discards its matches.
*/
ParseTree discardMatches(alias r)(ParseTree p)
{
    p = r(p);
    p.matches = null;
    return p;
}

ParseTree leavesGetter(ParseTree p)
{
    ParseTree[] leaves(ParseTree pt)
    {
        ParseTree[] leavesList;
        if (pt.children.length == 0)
            return [pt];
        else
            foreach(child; pt.children)
                leavesList ~= leaves(child);
        return leavesList;
    }
    
    p.children = leaves(p).dup;
    return p;
}

template getLeaves(alias r)
{
    alias action!(r, leavesGetter) getLeaves;
}

ParseTree ruleFilter(string name)(ParseTree p)
{
    ParseTree[] filter(ParseTree pt)
    {
        ParseTree[] list;
        if (pt.name == name)
            return [pt];
        else
            foreach(child; pt.children)
                list ~= filter(child);
        return list;
    }
    
    p.children = filter(p).dup;
    return p;
}

template filterRule(alias r, string target)
{
    alias action!(r, ruleFilter!target) filterRule;
}

/**
Calls 'r' on the input and then discard everything 'r' returned: no children, no match and index
put at the end of the match. It's the low-level engine behind ':r'.
*/
ParseTree discard(alias r)(ParseTree p)
{
    ParseTree result = r(p);
    result.matches = null;
    result.begin = result.end;
    result.name = "discard";
    return result;
}

/**
Calls 'r' on the input and then discards everything 'r' did, except its matches (so that
they propagate upwards). Equivalent to ';r'.
*/
ParseTree drop(alias r)(ParseTree p)
{
    ParseTree result = r(p);
    result.name = "drop";
    return result;    
}

/**
Makes 'r's result be kept when it would be discarded by the tree-decimation done by a grammar.
Equivalent to '^r'.
*/
ParseTree keep(alias r)(ParseTree p)
{
    auto result = r(p);
    result.children = [result];
    result.name = "keep";
    return result;
}

/**
Tries 'r' on the input. If it succeeds, the rule also succeeds, without consuming any input.
If 'r' fails, then posLookahead!r also fails. Low-level implementation of '&r'.
*/
template posLookahead(alias r)
{
    ParseTree posLookahead(ParseTree p)
    {
        auto temp = r(p);
        return ParseTree("posLookahead", temp.successful, [], p.input, p.end, p.end);
    }
}

/**
Tries 'r' on the input. If it fails, the rule succeeds, without consuming any input.
If 'r' succeeds, then negLookahead!r fails. Low-level implementation of '!r'.
*/
template negLookahead(alias r)
{
    ParseTree negLookahead(ParseTree p)
    {
        auto temp = r(p);
        return ParseTree("negLookahead", !temp.successful, [], p.input, p.end, p.end);
    }
}

/* pre-defined rules */

alias named!(or!(literal!"\r\n", literal!"\n", literal!"\r"), "endOfLine") endOfLine; /// predefined end-of-line parser
alias endOfLine eol; /// helper alias.

alias or!(literal!(" "), literal!("\t")) space; /// predefined space-recognizing parser (space or tabulation).
alias named!(fuse!(discardChildren!(zeroOrMore!space)), "spaces") spaces; /// aka '~space*'
alias or!(space, endOfLine) blank; /// Any blank char (spaces or end of line).
alias named!(fuse!(discardChildren!(zeroOrMore!blank)), "spacing") spacing; /// The basic space-management parser: fuse zero or more blank spaces.

alias charRange!('0', '9') digit; /// Decimal digit: [0-9]
alias named!(fuse!(discardChildren!(oneOrMore!digit)), "digits") digits; /// [0-9]+

alias or!(charRange!('0','9'), charRange!('a','f'), charRange!('A', 'F')) hexDigit; /// Hexadecimal digit: [0-9a-fA-F]

alias charRange!('a', 'z') alpha; /// [a-z]
alias charRange!('A', 'Z') Alpha; /// [A-Z]

alias and!(oneOrMore!(or!(alpha, Alpha, literal!("_"))), zeroOrMore!(or!(digit, alpha, Alpha, literal!("_")))) ident; 
alias named!(fuse!(discardChildren!ident), "identifier")  identifier; /// [a-zA-Z_][a-zA-Z_0-9]*, the basic C-family identifier
alias named!(fuse!(discardChildren!(and!(identifier, zeroOrMore!(and!(literal!".", identifier))))), "qualifiedIdentifier") qualifiedIdentifier; /// qualified identifiers (identifers separated by dots: abd.def.g).

alias named!(literal!"/", "slash") slash; /// A parser recognizing '/'
alias named!(literal!"\\", "backslash") backslash; /// A parser recognizing '\'
alias named!(literal!"'", "quote") quote; /// A parser recognizing ' (single quote)
alias named!(literal!"\"", "doublequote") doublequote; /// A parser recognizing " (double quote)
alias named!(literal!"`", "backquote") backquote; /// A parser recognizing ` (backquote)

/// A list of elem's separated by sep's. One element minimum.
template list(alias elem, alias sep)
{
    alias named!(spaceAnd!(elem, (zeroOrMore!(spaceAnd!(discardMatches!(sep), elem)))), "list") list;
}

/// A list of elem's separated by sep's. The empty list (no elem, no sep) is OK.
template list0(alias elem, alias sep)
{
    alias named!(option!(spaceAnd!(elem, zeroOrMore!(spaceAnd!(discardMatches!(sep), elem)))), "list0") list0;
}

template AddSpace(alias sp)
{
    template AddSpace(alias r)
    {
        alias TypeTuple!(r, discard!sp) AddSpace;
    }
}

/**
The engine behind the '< ' Pegged rule: all sequence subelements of a rule are interspersed
with a space-consuming rule, given as the first template parameter.

----
alias and!(literal!"abc", literal!"def") rule1; // "abc" "def", equivalent to "abcdef"
alias spaceAnd!(spacing, literal!"abc", literal!"def") rule2; // "abc" "def", but with spaces in-between.

auto input1 = ParseTree("",false,[], "abcdef");
auto input2 = ParseTree("",false,[], "abc       

def"); // with spaces and end of line markers.

assert(rule1(input1).successful); // OK
assert(!rule1(input2).successful); // NOK, couldn't find "def" after "abc"

assert(rule2(input1).successful); // OK
assert(rule2(input2).successful); // Still OK
assert(rule2(input2).matches == ["abc","def"]);
----

As you can see on the previous line, spaceAnd discards the matched spaces and returns matches only for the 'real' subrules.
*/
template spaceAnd(alias sp, rules...)
{
    alias and!(discard!(option!sp), staticMap!(AddSpace!(option!sp), rules)) spaceAnd;
}

/// Mixin to simplify a parse tree inside a grammar
mixin template decimateTree()
{
    static ParseTree decimateTree(ParseTree p)
    {
        if(p.children.length == 0) return p;
        
        ParseTree[] filterChildren(ParseTree pt)
        {
            ParseTree[] result;
            foreach(child; pt.children)
            {
                if (isRule(child.name)) // keep nodes that belongs to the current grammar
                {
                    child.children = filterChildren(child);
                    result ~= child;
                }
                else if (child.name == "keep") // 'keep' node are never discarded. They have only one child, the node to keep
                {
                    result ~= child.children[0];
                }
                else // discard this node, but see if its children contain nodes to keep
                {
                    result ~= filterChildren(child);
                }
            }
            return result;
        }
        p.children = filterChildren(p);
        return p;
    }
}

/**
Discard one-child nodes and replace them with their children. 
Most grammar tend to produce long one-child/one-child branches, 
simplifyTree compacts these.
*/
ParseTree simplifyTree(ParseTree p) 
{ 
    foreach(ref child; p.children) 
        child = simplifyTree(child); 
    
    if (p.children.length != 1) 
        return p; 
    else // linear tree 
        return p.children[0]; 
} 

/**
Returns: the number of nodes in a parse tree.
*/
size_t size(ParseTree p) 
{ 
        size_t result = 1; 
        foreach(child; p.children) 
                result += size(child); 
        return result; 
} 