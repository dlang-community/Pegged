module pegged.dynamicgrammar;

import pegged.peg;
import pegged.dynamicpeg;


struct ParameterizedRule
{
    size_t numArgs;
    Dynamic delegate(Dynamic[]) code;

    Dynamic opCall(D...)(D rules)
    {
        Dynamic[] args;
        foreach(i,rule; rules)
        {
            static if (is(typeof(rule) == Dynamic))
                args ~= rule;
            else
                args ~= rule(); // Dynamic delegate()
        }

        if(rules.length == numArgs)
        {
            switch(numArgs)
            {
                case 1:
                    return code([args[0]]);
                case 2:
                    return code([args[0], args[1]]);
                case 3:
                    return code([args[0], args[1], args[2]]);
                case 4:
                    return code([args[0], args[1], args[2], args[3]]);
                case 5:
                    return code([args[0], args[1], args[2], args[3], args[4]]);
                case 6:
                    return code([args[0], args[1], args[2], args[3], args[4], args[5]]);
                default:
                    throw new Exception("Unimplemented parameterized rule.");
            }
        }
        else
            throw new Exception( "ParameterizedRule called with "
                               ~ to!string(rules.length)
                               ~ " args. Waited for "
                               ~ to!string(numArgs) ~ ".");
        return code(args);
    }
}

ParameterizedRule parameterizedRule(size_t n, Dynamic delegate(Dynamic[] d) code)
{
    ParameterizedRule pr;
    pr.numArgs = n;
    pr.code = code;
    return pr;
}

struct DynamicGrammar
{
    Dynamic[string] rules;
    string startingRule;
    ParameterizedRule[string] paramRules;
    string grammarName;

    ParseTree decimateTree(ParseTree p)
    {
        if(p.children.length == 0) return p;

        ParseTree[] filterChildren(ParseTree pt)
        {
            ParseTree[] result;
            foreach(child; pt.children)
            {
                if (  (child.name.startsWith(grammarName) && child.matches.length != 0)
                   || !child.successful && child.children.length == 0)
                {
                    child.children = filterChildren(child);
                    result ~= child;
                }
                else if (child.name.startsWith("keep!(")) // 'keep' node are never discarded.
                                               // They have only one child, the node to keep
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

    ParseTree opCall(ParseTree p)
    {
        ParseTree result = decimateTree(rules[startingRule](p));
        result.children = [result];
        result.name = grammarName;
        return result;
    }

    ParseTree opCall(string input)
    {
        ParseTree result = decimateTree(rules[startingRule](ParseTree(``, false, [], input, 0, 0)));
        result.children = [result];
        result.name = grammarName;
        return result;
    }

    void opIndexAssign(D)(D code, string s)
    {
        rules[s]= named(code, "Test." ~ s);
    }
    Dynamic opIndex(string s)
    {
        return rules[s];
    }
}
