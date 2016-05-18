/**
Dynamic grammars:
    - no CT parsing (this one will stay)
    - no memoization (could be added)
    - slightly less handy parameterized rules (could be better)
    - no semantic actions (drat, this one is the worse)
    - no calling from other grammars?

Advantages:
    - fully runtime configurable: change rules, add rules, delete rules.
*/
module pegged.dynamic.grammar;

import std.algorithm : startsWith;
import std.array;
import std.stdio;

import pegged.peg;
import pegged.parser;
import pegged.dynamic.peg;

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
                    throw new Exception("Unimplemented parameterized rule with more than 6 arguments.");
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
    string grammarName;
    string startingRule;
    Dynamic[string] rules;
    ParameterizedRule[string] paramRules;

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
        return decimateTree(rules[startingRule](p));
        /+
        result.children = [result];
        result.name = grammarName;
        return result;
        +/
    }

    ParseTree opCall(string input)
    {
        return decimateTree(rules[startingRule](ParseTree(``, false, [], input, 0, 0)));
        /+
        result.children = [result];
        result.name = grammarName;
        return result;
        +/
    }

    void opIndexAssign(D)(D code, string s)
    {
        rules[s]= named(code, grammarName ~ "." ~ s);
    }

    Dynamic opIndex(string s)
    {
        return rules[s];
    }
}

// Helper to insert 'Spacing' before and after Primaries
ParseTree spaceArrow(ParseTree input)
{
    ParseTree wrapInSpaces(ParseTree p)
    {
        ParseTree spacer =
        ParseTree("Pegged.Prefix", true, null, null, 0,0, [
            ParseTree("Pegged.Suffix", true, null, null, 0, 0, [
                ParseTree("Pegged.Primary", true, null, null, 0, 0, [
                    ParseTree("Pegged.RhsName", true, null, null, 0,0, [
                        ParseTree("Pegged.Identifier", true, ["Spacing"])
                    ])
                ])
            ])
        ]);
        ParseTree result = ParseTree("Pegged.WrapAround", true, p.matches, p.input, p.begin, p.end, p.children);
        result.children = spacer ~ result.children ~ spacer;
        return result;
    }
    return modify!( p => p.name == "Pegged.Primary",
                    wrapInSpaces)(input);
}

Dynamic makeRule(string def, Dynamic[string] context)
{
    ParseTree p = Pegged.decimateTree(Pegged.Definition(def));
    return makeRule(p, context);
}

Dynamic makeRule(ParseTree def, Dynamic[string] context)
{
    Dynamic code;

    Dynamic getDyn(string name)
    {
        if (name in context)
            return context[name];
        else
            throw new Exception("Unknown name: " ~ name);
    }

    Dynamic ruleFromTree(ParseTree p)
    {
        //writeln("rfT: ", p.name, " ", p.matches);
        //Dynamic result;
        switch(p.name)
        {
            case "Pegged.Expression":
                //if (p.children.length > 1) // OR expression
                //{
                    Dynamic[] children;
                    foreach(seq; p.children)
                        children ~= ruleFromTree(seq);
                    return distribute!(or)(children);
                //}
                //else // One child -> just a sequence, no need for a or!( , )
                //{
                //    return ruleFromTree(p.children[0]);
                //}
                //break;
            case "Pegged.Sequence":
                //if (p.children.length > 1) // real sequence
                //{
                    Dynamic[] children;
                    foreach(seq; p.children)
                        children ~= ruleFromTree(seq);
                    return distribute!(pegged.dynamic.peg.and)(children);
                /+}
                else // One child -> just a Suffix, no need for a and!( , )
                {
                    return ruleFromTree(p.children[0]);
                }
                +/
            case "Pegged.Prefix":
                ParseTree temp;
                foreach_reverse(i,child; p.children[0..$-1]) // transforming a list into a linear tree
                {
                    temp  = p.children[$-1];
                    p.children[$-1] = child;
                    p.children[$-1].children = [temp];
                }
                return ruleFromTree(p.children[$-1]);
            case "Pegged.Suffix":
                foreach(child; p.children[1..$])
                {
                    ParseTree temp = p.children[0];
                    p.children[0] = child;
                    p.children[0].children = [temp];
                }
                return ruleFromTree(p.children[0]);
            case "Pegged.Primary":
                return ruleFromTree(p.children[0]);
            case "Pegged.RhsName":
                return ruleFromTree(p.children[0]);
            case "Pegged.Identifier":
                //writeln("Identifier: ", p.matches[0], " ");//, getDyn(p.matches[0])()(ParseTree()).name);
                return getDyn(p.matches[0]);
            case "Pegged.Literal":
                //writeln("Literal: ", p.matches);
                if(p.matches.length == 3) // standard case
                    return literal(p.matches[1]);
                else // only two children -> empty literal
                    return eps();
            case "Pegged.CharClass":
                if (p.children.length > 1)
                {
                    Dynamic[] children;
                    foreach(seq; p.children)
                        children ~= ruleFromTree(seq);
                    return distribute!(pegged.dynamic.peg.or)(children);
                }
                else // One child -> just a sequence, no need for a or!( , )
                {
                    return ruleFromTree(p.children[0]);
                }
                //break;
            case "Pegged.CharRange":
                /// Make the generation at the Char level: directly what is needed, be it `` or "" or whatever
                if (p.children.length > 1) // a-b range
                {
                    return charRange(p.matches[0].front, p.matches[2].front);
                }
                else // lone char
                {
                    //result = "pegged.peg.literal!(";
                    string ch = p.matches[0];
                    switch (ch)
                    {
                        case "\\[":
                        case "\\]":
                        case "\\-":
                            return literal(ch[0..1]);
                        case "\\\'":
                            return literal("\'");
                        case "\\`":
                            return literal("`");
                        case "\\":
                        case "\\\\":
                            return literal("\\");
                        case "\"":
                        case "\\\"":
                            return literal("\"");
                        case "\n":
                        case "\r":
                        case "\t":
                            return literal(ch);
                        default:
                            return literal(ch);
                    }
                }
            case "Pegged.Char":
                string ch = p.matches[0];
                switch (ch)
                {
                    case "\\[":
                    case "\\]":
                    case "\\-":

                    case "\\\'":
                    case "\\\"":
                    case "\\`":
                    case "\\\\":
                        return literal(ch[1..$]);
                    case "\n":
                    case "\r":
                    case "\t":
                        return literal(ch);
                    default:
                        return literal(ch);
                }
                //break;
            case "Pegged.POS":
                return posLookahead(ruleFromTree(p.children[0]));
            case "Pegged.NEG":
                return negLookahead(ruleFromTree(p.children[0]));
            case "Pegged.FUSE":
                return fuse(ruleFromTree(p.children[0]));
            case "Pegged.DISCARD":
                return discard(ruleFromTree(p.children[0]));
            case "Pegged.KEEP":
                return keep(ruleFromTree(p.children[0]));
            case "Pegged.DROP":
                return drop(ruleFromTree(p.children[0]));
            case "Pegged.PROPAGATE":
                return propagate(ruleFromTree(p.children[0]));
            case "Pegged.OPTION":
                return option(ruleFromTree(p.children[0]));
            case "Pegged.ZEROORMORE":
                return zeroOrMore(ruleFromTree(p.children[0]));
            case "Pegged.ONEORMORE":
                return oneOrMore(ruleFromTree(p.children[0]));
            case "Pegged.Action":
                Dynamic result = ruleFromTree(p.children[0]);
                foreach(act; p.matches[1..$])
                    result = action(result, getDyn(act));
                return result;
            case "Pegged.ANY":
                return any();
            case "Pegged.WrapAround":
                return wrapAround( ruleFromTree(p.children[0])
                                        , ruleFromTree(p.children[1])
                                        , ruleFromTree(p.children[2]));
            default:
                throw new Exception("Bad tree: " ~ p.toString());
        }
    }

    switch(def.children[1].children[0].name)
    {
        case "Pegged.LEFTARROW":
            code = ruleFromTree(def.children[2]);
            break;
        case "Pegged.FUSEARROW":
            code = fuse(ruleFromTree(def.children[2]));
            break;
        case "Pegged.DISCARDARROW":
            code = discard(ruleFromTree(def.children[2]));
            break;
        case "Pegged.KEEPARROW":
            code = keep(ruleFromTree(def.children[2]));
            break;
        case "Pegged.DROPARROW":
            code = drop(ruleFromTree(def.children[2]));
            break;
        case "Pegged.PROPAGATEARROW":
            code = propagate(ruleFromTree(def.children[2]));
            break;
        case "Pegged.SPACEARROW":
            ParseTree modified = spaceArrow(def.children[2]);
            code = ruleFromTree(modified);
            break;
        case "Pegged.ACTIONARROW":
            Dynamic actionResult = ruleFromTree(def.children[2]);
            foreach(act; def.children[1].matches[1..$])
                actionResult = action(actionResult, getDyn(act));
            code = actionResult;
            break;
        default:
            throw new Exception("Unknow arrow: " ~ def.children[1].children[0].name);
            //break;
    }

    return named(code, def.matches[0]);
}

DynamicGrammar grammar(string definition, Dynamic[string] context = null)
{
    //writeln("Entering dyn gram");
    ParseTree defAsParseTree = Pegged(definition);
    //writeln(defAsParseTree);
    if (!defAsParseTree.successful)
    {
        // To work around a strange bug with ParseTree printing at compile time
        throw new Exception("Bad grammar input: " ~ defAsParseTree.toString(""));
    }

    DynamicGrammar gram;
    foreach(name, rule; context)
    {
        gram.rules[name] = rule;
    }

    ParseTree p = defAsParseTree.children[0];

    string grammarName = p.children[0].matches[0];
    string shortGrammarName = p.children[0].matches[0];
    gram.grammarName = shortGrammarName;

    // Predefined spacing
    gram.rules["Spacing"] = discard(zeroOrMore(or(literal(" "), literal("\t"), literal("\n"), literal("\r"))));

    ParseTree[] definitions = p.children[1 .. $];

    foreach(i,def; definitions)
    {
        gram[def.matches[0]] = fail();
    }

    foreach(i,def; definitions)
    {
        string shortName = def.matches[0];

        if (i == 0)
            gram.startingRule = shortName;
        // prepending the global grammar name, to get a qualified-name rule 'Gram.Rule'
        def.matches[0] = shortGrammarName ~ "." ~ def.matches[0];
        gram.rules[shortName] = makeRule(def, gram.rules);
    }

    return gram;
}

Dynamic distribute(alias fun)(Dynamic[] args)
{
    //mixin(makeSwitch(40));
    switch(args.length)
    {
        case 1:
            return fun(args[0]);
        case 2:
            return fun(args[0], args[1]);
        case 3:
            return fun(args[0], args[1], args[2]);
        case 4:
            return fun(args[0], args[1], args[2], args[3]);
        case 5:
            return fun(args[0], args[1], args[2], args[3], args[4]);
        case 6:
            return fun(args[0], args[1], args[2], args[3], args[4], args[5]);
        default:
            return fun(fun(args[0], args[1], args[2], args[3], args[4], args[5]), distribute!fun(args[6..$]));
    }
}
