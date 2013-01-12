module pegged.dynamic;

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

ParseTree delegate(ParseTree) dynamicAnd(ParseTree delegate(ParseTree)[] rules...)
{
    return (ParseTree p)
    {
        ParseTree result = ParseTree("and   ", false, [], p.input, p.end, p.end, []);

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
        ParseTree longestFail = ParseTree("dynamicOr", false, [], p.input, p.end, 0);
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
                temp.name = "dynamicOr";
                return temp;
            }
            else
            {
                enum errName = " (" ~ getName!(r)() ~")";
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
        longestFail.name = "dynamicOr";
        longestFail.begin = p.end;
        return longestFail;
    };
}
