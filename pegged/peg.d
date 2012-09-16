/**
This module contains the engine behind Pegged, the expression templates building blocks to create a top-down
recursive-descent parser.

The terminals and non-terminals described here are meant to be used inside a Pegged grammar. As such, they are a bit less
user-friendly than what's output by pegged.grammar. For example they take a ParseTree as input, not a string.

See the /docs directory for the full documentation as markdown files.
*/
module pegged.peg;

import std.conv;
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
}


/**
Basic rule, that always fail without consuming.
*/
ParseTree fail(ParseTree p)
{
    return ParseTree("fail", false, [], p.input, p.end, p.end, null);
}

/**
Matches the end of input. Fails if there is any character left.
*/
ParseTree eoi(ParseTree p)
{
    return ParseTree("eoi", p.end == p.input.length, [], p.input, p.end, p.end);
}

alias eoi endOfInput; /// helper alias.

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

/**
Represents a literal in a PEG, like "abc" or 'abc' (or even '').
It succeeds if a prefix of the input is equal to its template parameter and fails otherwise.
*/
ParseTree literal(string s)(ParseTree p)
{
    if (p.end+s.length <= p.input.length && p.input[p.end..p.end+s.length] == s)
        return ParseTree("literal("~s~")", true, [s], p.input, p.end, p.end+s.length);
    else
        return ParseTree("literal("~s~")", false, [], p.input, p.end, p.end);
}

/**
Represents a range of chars, from begin to end, included. So charRange!('a','z') matches
all English lowercase letters. If fails if the input is empty or does not begin with a character
between begin and end.
*/
ParseTree charRange(char begin, char end)(ParseTree p)
{
    if (p.end < p.input.length && p.input[p.end] >= begin && p.input[p.end] <= end)
        return ParseTree("charRange("~begin~"," ~ end ~ ")", true, [p.input[p.end..p.end+1]], p.input, p.end, p.end+1);
    else
        return ParseTree("charRange("~begin~"," ~ end ~ ")", false, [], p.input, p.end, p.end);
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
        return result;
    else
        return ParseTree("option", true, [], p.input, p.end, p.end, null);
}

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

template action(alias r, alias act)
{
    ParseTree action(ParseTree p)
    {
        return act(r(p));
    }
}

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

ParseTree discardChildren(alias r)(ParseTree p)
{
    p = r(p);
    p.children = null;
    return p;
}

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

ParseTree discard(alias r)(ParseTree p)
{
    ParseTree result = r(p);
    result.matches = null;
    result.begin = result.end;
    result.name = "discard";
    return result;
}

ParseTree drop(alias r)(ParseTree p)
{
    ParseTree result = r(p);
    result.name = "drop";
    return result;    
}

ParseTree keep(alias r)(ParseTree p)
{
    auto result = r(p);
    result.children = [result];
    result.name = "keep";
    return result;
}

template posLookahead(alias r)
{
    ParseTree posLookahead(ParseTree p)
    {
        auto temp = r(p);
        return ParseTree("posLookahead", temp.successful, [], p.input, p.end, p.end);
    }
}

template negLookahead(alias r)
{
    ParseTree negLookahead(ParseTree p)
    {
        auto temp = r(p);
        return ParseTree("negLookahead", !temp.successful, [], p.input, p.end, p.end);
    }
}

/* pre-defined rules */
alias named!(or!(literal!"\r\n", literal!"\n", literal!"\r"), "endOfLine") endOfLine;
alias endOfLine eol;

alias or!(literal!(" "), literal!("\t")) space;
alias named!(fuse!(discardChildren!(zeroOrMore!space)), "spaces") spaces;
alias or!(space, endOfLine) blank;
alias named!(fuse!(discardChildren!(zeroOrMore!blank)), "spacing") spacing;

alias charRange!('0', '9') digit;
alias named!(fuse!(discardChildren!(oneOrMore!digit)), "digits") digits;

alias or!(charRange!('0','9'), charRange!('a','f'), charRange!('A', 'F')) hexDigit;

alias charRange!('a', 'z') alpha;
alias charRange!('A', 'Z') Alpha;

alias and!(oneOrMore!(or!(alpha, Alpha, literal!("_"))), zeroOrMore!(or!(digit, alpha, Alpha, literal!("_")))) ident;
alias named!(fuse!(discardChildren!ident), "identifier")  identifier;
alias named!(fuse!(discardChildren!(and!(identifier, zeroOrMore!(and!(literal!".", identifier))))), "qualifiedIdentifier") qualifiedIdentifier;

alias named!(literal!"/", "slash") slash;
alias named!(literal!"\\", "backslash") backslash;
alias named!(literal!"'", "quote") quote;
alias named!(literal!"\"", "doublequote") doublequote;
alias named!(literal!"`", "backquote") backquote;

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
