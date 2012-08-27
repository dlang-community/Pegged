module pegged.peg;

import std.conv;
import std.typetuple;

struct ParseTree
{
    string name;
    bool successful;
    string[] matches;
    
    string input;
    size_t begin, end;
    
    ParseTree[] children;
    
    string toString(string tabs = "") 
    {
        string result = name ~ (successful ? " " : "(failure) ") ~ " " ~ to!string([begin, end]) ~ to!string(matches) ~ "\n";
        foreach(i,child; children)
            result ~= tabs ~ " +-" ~ child.toString(tabs ~ ((i < children.length -1 ) ? " | " : "   "));
        //result ~= "\n";
        
        return result;
    }
}


/* Standard rules */
ParseTree fail(ParseTree p)
{
    return ParseTree("fail", false, [], p.input, p.end, p.end, null);
}

ParseTree eoi(ParseTree p)
{
    return ParseTree("eoi", p.end == p.input.length, [], p.input, p.end, p.end);
}

ParseTree any(ParseTree p)
{
    if (p.end < p.input.length)
        return ParseTree("any", true, [p.input[p.end..p.end+1]], p.input, p.end, p.end+1);
    else
        return ParseTree("any", false, [], p.input, p.end, p.end);
}

ParseTree literal(string s)(ParseTree p)
{
    if (p.end+s.length <= p.input.length && p.input[p.end..p.end+s.length] == s)
        return ParseTree("literal("~s~")", true, [s], p.input, p.end, p.end+s.length);
    else
        return ParseTree("literal("~s~")", false, [], p.input, p.end, p.end);
}

ParseTree charRange(char begin, char end)(ParseTree p)
{
    if (p.end < p.input.length && p.input[p.end] >= begin && p.input[p.end] <= end)
        return ParseTree("charRange("~begin~"," ~ end ~ ")", true, [p.input[p.end..p.end+1]], p.input, p.end, p.end+1);
    else
        return ParseTree("charRange("~begin~"," ~ end ~ ")", false, [], p.input, p.end, p.end);
}

ParseTree eps(ParseTree p)
{
    return ParseTree("eps", true, [""], p.input, p.end, p.end);
}

ParseTree and(rules...)(ParseTree p)
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

ParseTree or(rules...)(ParseTree p)
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
        /* both failed, we will take the longest match */
    longestFail.children = [longestFail];
    longestFail.name = "or";
    return longestFail;
}

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
        static if (is(r))
        {
            r temp;
            ParseTree result = temp(p);
        }
        else
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
alias or!(literal!(" "), literal!("\t")) space;
alias named!(fuse!(discardChildren!(zeroOrMore!space)), "spaces") spaces;
alias named!(or!(literal!"\r\n", literal!"\n", literal!"\r"), "endOfLine") endOfLine;
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
                if (child.name in names) // keep nodes that belongs to the current grammar
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
