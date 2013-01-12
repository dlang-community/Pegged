module pegged.dynamic;

import std.conv: to;

import pegged.peg;


ParseTree delegate(ParseTree) dynamicEoi()
{
    return (ParseTree p)
    {
        if (p.end == p.input.length)
            return ParseTree("eoi", true, [], p.input, p.end, p.end);
        else
            return ParseTree("eoi", false, ["end of input"], p.input, p.end, p.end);
    };
}


ParseTree delegate(ParseTree) dynamicAny()
{
    return(ParseTree p)
    {
        if (p.end < p.input.length)
            return ParseTree("any", true, [p.input[p.end..p.end+1]], p.input, p.end, p.end+1);
        else
            return ParseTree("any", false, ["any char"], p.input, p.end, p.end);
    };
}

ParseTree delegate(ParseTree) dynamicLiteral(string s)
{
    return (ParseTree p)
    {
        if (p.end+s.length <= p.input.length && p.input[p.end..p.end+s.length] == s)
            return ParseTree("literal(\"" ~ s ~ "\")", true, [s], p.input, p.end, p.end+s.length);
        else
            return ParseTree("literal(\"" ~ s ~ "\")", false, [`"` ~ s ~ `"`], p.input, p.end, p.end);

    };
}

ParseTree delegate(ParseTree) dynamicCharRange(char begin, char end)
{
    return (ParseTree p)
    {
        string longName = "a char between '"~to!string(begin)~"' and '"~to!string(end)~"'";
        if (p.end < p.input.length && p.input[p.end] >= begin && p.input[p.end] <= end)
           return ParseTree("charRange", true, [p.input[p.end..p.end+1]], p.input, p.end, p.end+1);
        else
            return ParseTree("charRange", false, [longName], p.input, p.end, p.end);
    };

}

ParseTree delegate(ParseTree) dynamicEps()
{
    return (ParseTree p)
    {
        return ParseTree("eps", true, [""], p.input, p.end, p.end);
    };
}

ParseTree delegate(ParseTree) dynamicWrapAround( ParseTree delegate(ParseTree) before
                                               , ParseTree delegate(ParseTree) middle
                                               , ParseTree delegate(ParseTree) after)
{
    return(ParseTree p)
    {
        ParseTree temp = before(p);
        if (!temp.successful)
            return temp;

        ParseTree result = middle(temp);
        if (!result.successful)
            return result;
        result.begin = temp.begin;

        temp = after(result);
        if (!temp.successful)
            return temp;

        result.end = temp.end;
        return result;
    };
}

ParseTree delegate(ParseTree) dynamicOneOrMore(ParseTree delegate(ParseTree) r)
{
    return (ParseTree p)
    {
        auto result = ParseTree("oneOrMore", true, [], p.input, p.end, p.end);
        auto temp = r(result);
        while(temp.successful
            && (temp.begin < temp.end // To avoid infinite loops on epsilon-matching rules
            || temp.name.startsWith("discard!(")))
        {
            result.matches ~= temp.matches;
            result.children ~= temp;
            result.end = temp.end;
            temp = r(result);
        }
        result.successful = true;
        return result;
    };
}

ParseTree delegate(ParseTree) dynamicAnd(ParseTree delegate(ParseTree)[] rules...)
{
    return (ParseTree p)
    {
        ParseTree result = ParseTree("and", false, [], p.input, p.end, p.end, []);

        foreach(i,r; rules)
        {
            ParseTree temp = r(result);
            result.end = temp.end;
            if (temp.successful)
            {
                result.matches ~= temp.matches;
                result.children ~= temp;
            }
            else
            {
                result.children ~= temp;// add the failed node, to indicate which failed
                if (temp.matches.length > 0)
                    result.matches ~= temp.matches[$-1];
                return result; // and end the parsing attempt right there
            }
        }
        result.successful = true;
        return result;
    };
}

ParseTree delegate(ParseTree) dynamicOr(ParseTree delegate(ParseTree)[] rules...)
{
    return (ParseTree p)
    {
        // error-management
        ParseTree longestFail = ParseTree("or", false, [], p.input, p.end, 0);
        string[] errorStrings;
        size_t errorStringChars;
        string orErrorString;

        ParseTree[] results;
        string[] names;
        size_t[] failedLength;
        size_t maxFailedLength;

        // Real 'or' loop
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
            {
                string errName = " (dynamic or: longest error)";
                failedLength ~= temp.end;
                if (temp.end >= longestFail.end)
                {
                    maxFailedLength = temp.end;
                    longestFail = temp;
                    names ~= errName;
                    results ~= temp;

                    if (temp.end == longestFail.end)
                        errorStringChars += temp.matches[$-1].length + errName.length + 4;
                    else
                        errorStringChars = temp.matches[$-1].length + errName.length + 4;
                }
                // Else, this error parsed less input than another one: we discard it.
            }
        }

        // All subrules failed, we will take the longest match as the result
        // If more than one node failed at the same (farthest) position, we concatenate their error messages

        char[] errString;// = new char[](errorStringChars);
        errString.length = errorStringChars;
        uint start = 0;
        foreach(i; 0..rules.length)
        {
            if (failedLength[i] == maxFailedLength)
            {
                auto temp = results[i];
                auto len = temp.matches[$-1].length;
                auto nlen = names[i].length;
                errString[start .. start+len] = temp.matches[$-1];
                errString[start+len .. start+len+names[i].length] = names[i];
                errString[start+len+nlen .. start+len+nlen+4] = " or ";
                start += len + names[i].length + 4;
            }
        }
        orErrorString = cast(string)(errString[0..$-4]);

        longestFail.matches = longestFail.matches[0..$-1]  // discarding longestFail error message
                            ~ [orErrorString];             // and replacing it by the new, concatenated one.
        longestFail.name = "or";
        longestFail.begin = p.end;
        return longestFail;
    };
}
