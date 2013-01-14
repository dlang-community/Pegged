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

DynamicGrammar grammar(string definition)
{
    ParseTree defAsParseTree = Pegged(definition);

    if (!defAsParseTree.successful)
    {
        // To work around a strange bug with ParseTree printing at compile time
        throw new Exception("Bad grammar input: " ~ defAsParseTree.toString(""));
    }

    DynamicGrammar gram;

        case "Pegged.Grammar":
            string grammarName = generateCode(p.children[0]);
            string shortGrammarName = p.children[0].matches[0];
            //string invokedGrammarName = generateCode(transformName(p.children[0]));
            string firstRuleName = generateCode(p.children[1].children[0]);

            result =  "struct Generic" ~ shortGrammarName ~ "(TParseTree)\n"
                    ~ "{\n"
                    ~ "    struct " ~ grammarName ~ "\n    {\n"
                    ~ "    enum name = \"" ~ shortGrammarName ~ "\";\n";

            if (withMemo == Memoization.yes)
                result ~= "    import std.typecons:Tuple, tuple;\n"
                        ~ "    static TParseTree[Tuple!(string, size_t)] memo;\n";

            ParseTree[] definitions = p.children[1 .. $];

            foreach(i,def; definitions)
            {
                string numParam = (def.children[0].children.length > 1) ? ("_" ~ to!string(def.children[0].children[1].children.length)) : "";
                result ~= "    static ParseTree delegate(ParseTree) before" ~ def.matches[0] ~ numParam ~ ";\n"
                        ~ "    static ParseTree delegate(ParseTree) after"  ~ def.matches[0] ~ numParam ~ ";\n";
            }

            result ~= "    static bool isRule(string s)\n"
                    ~ "    {\n"
                    ~ "        switch(s)\n"
                    ~ "        {\n";

            bool[string] ruleNames; // to avoid duplicates, when using parameterized rules
            string parameterizedRulesSpecialCode; // because param rules need to be put in the 'default' part of the switch
            bool userDefinedSpacing = false;

            string paramRuleHandler(string target)
            {
                return "if (s.length >= "~to!string(shortGrammarName.length + target.length + 3)
                        ~" && s[0.."~to!string(shortGrammarName.length + target.length + 3)~"] == \""
                        ~shortGrammarName ~ "." ~ target~"!(\") return true;";
            }

            foreach(i,def; definitions)
            {
                if (def.matches[0] !in ruleNames)
                {
                    ruleNames[def.matches[0]] = true;

                    if (def.children[0].children.length > 1) // Parameterized rule
                        parameterizedRulesSpecialCode ~= "                " ~ paramRuleHandler(def.matches[0])~ "\n";
                    else
                        result ~= "            case \"" ~ shortGrammarName ~ "." ~ def.matches[0] ~ "\":\n";
                }
                if (def.matches[0] == "Spacing") // user-defined spacing
                    userDefinedSpacing = true;
            }
            result ~= "                return true;\n"
                    ~ "            default:\n"
                    ~ parameterizedRulesSpecialCode
                    ~ "                return false;\n        }\n    }\n";

            result ~= "    mixin decimateTree;\n";

            // If the grammar provides a Spacing rule, then this will be used.
            // else, the predefined 'spacing' rule is used.
            result ~= userDefinedSpacing ? "" : "    alias spacing Spacing;\n\n";

            // Creating the inner functions, each corresponding to a grammar rule
            foreach(def; definitions)
                result ~= generateCode(def, shortGrammarName);

            // if the first rule is parameterized (a template), it's impossible to get an opCall
            // because we don't know with which template arguments it should be called.
            // So no opCall is generated in this case.
            if (p.children[1].children[0].children.length == 1)
            {
                // General calling interface
                result ~= "    static TParseTree opCall(TParseTree p)\n"
                        ~  "    {\n"
                        ~  "        TParseTree result = decimateTree(" ~ firstRuleName ~ "(p));\n"
                        ~  "        result.children = [result];\n"
                        ~  "        result.name = \"" ~ shortGrammarName ~ "\";\n"
                        ~  "        return result;\n"
                        ~  "    }\n\n"
                        ~  "    static TParseTree opCall(string input)\n"
                        ~  "    {\n";

                if (withMemo == Memoization.no)
                    result ~= "        return " ~ shortGrammarName ~ "(TParseTree(``, false, [], input, 0, 0));\n"
                            ~  "}\n";
                else
                    result ~= "        if(__ctfe)\n"
                            ~  "        {\n"
                            ~  "            return " ~ shortGrammarName ~ "(TParseTree(``, false, [], input, 0, 0));\n"
                            ~  "        }\n"
                            ~  "        else\n"
                            ~  "        {\n"
                            ~  "            memo = null;\n"
                            ~  "            return " ~ shortGrammarName ~ "(TParseTree(``, false, [], input, 0, 0));\n"
                            ~  "        }\n"
                            ~  "    }\n";

                result ~= "    static string opCall(GetName g)\n"
                        ~ "    {\n"
                        ~ "        return \"" ~ shortGrammarName ~ "\";\n"
                        ~ "    }\n\n";
            }
            result ~= "    }\n" // end of grammar struct definition
                    ~ "}\n\n" // end of template definition
                    ~ "alias Generic" ~ shortGrammarName ~ "!(ParseTree)."
                    ~ shortGrammarName ~ " " ~ shortGrammarName ~ ";\n\n";
            break;
        case "Pegged.Definition":
            // children[0]: name
            // children[1]: arrow (arrow type as first child)
            // children[2]: description

            string code;

            switch(p.children[1].children[0].name)
            {
                case "Pegged.LEFTARROW":
                    code ~= generateCode(p.children[2]);
                    break;
                case "Pegged.FUSEARROW":
                    code ~= "pegged.peg.fuse!(" ~ generateCode(p.children[2]) ~ ")";
                    break;
                case "Pegged.DISCARDARROW":
                    code ~= "pegged.peg.discard!(" ~ generateCode(p.children[2]) ~ ")";
                    break;
                case "Pegged.KEEPARROW":
                    code ~= "pegged.peg.keep!("~ generateCode(p.children[2]) ~ ")";
                    break;
                case "Pegged.DROPARROW":
                    code ~= "pegged.peg.drop!("~ generateCode(p.children[2]) ~ ")";
                    break;
                case "Pegged.PROPAGATEARROW":
                    code ~= "pegged.peg.propagate!("~ generateCode(p.children[2]) ~ ")";
                    break;
                case "Pegged.SPACEARROW":
                    ParseTree modified = spaceArrow(p.children[2]);
                    code ~= generateCode(modified);
                    break;
                case "Pegged.ACTIONARROW":
                    auto actionResult = generateCode(p.children[2]);
                    foreach(action; p.children[1].matches[1..$])
                        actionResult = "pegged.peg.action!(" ~ actionResult ~ ", " ~ action ~ ")";
                    code ~= actionResult;
                    break;
                default:
                    break;
            }

            bool parameterizedRule = p.children[0].children.length > 1;
            string completeName = generateCode(p.children[0]);
            string shortName = p.matches[0];
            string innerName;
            string hookedName = p.matches[0];

            if (parameterizedRule)
            {
                result =  "    template " ~ completeName ~ "\n"
                        ~ "    {\n";
                innerName ~= "\"" ~ shortName ~ "!(\" ~ ";
                hookedName ~= "_" ~ to!string(p.children[0].children[1].children.length);
                foreach(i,param; p.children[0].children[1].children)
                    innerName ~= "pegged.peg.getName!("~ param.children[0].matches[0]
                                ~ (i<p.children[0].children[1].children.length-1 ? ")() ~ \", \" ~ "
                                                                                    : ")");
                innerName ~= " ~ \")\"";
            }
            else
            {
                innerName ~= "`" ~ completeName ~ "`";
            }

            string ctfeCode = "pegged.peg.named!("         ~ code                          ~ ", \"" ~ propagatedName ~ "." ~ innerName[1..$-1] ~ "\")";
            code =            "pegged.peg.named!(hooked!(" ~ code ~ ", \"" ~ hookedName ~ "\"), \"" ~ propagatedName ~ "." ~ innerName[1..$-1] ~ "\")";

            if (withMemo == Memoization.no)
                result ~= "    static TParseTree " ~ shortName ~ "(TParseTree p)\n"
                        ~  "    {\n"
                        ~  "        if(__ctfe)\n"
                        ~  "            return " ~ ctfeCode ~ "(p);\n"
                        ~  "        else\n"
                        ~  "             return " ~ code ~ "(p);\n"
                        ~  "    }\n"
                        ~  "    static TParseTree " ~ shortName ~ "(string s)\n"
                        ~  "    {\n"
                        ~  "        if(__ctfe)\n"
                        ~  "            return " ~ ctfeCode ~ "(TParseTree(\"\", false,[], s));\n"
                        ~  "        else\n"
                        ~  "            return " ~ code ~ "(TParseTree(\"\", false,[], s));\n"
                        ~  "    }\n";
            else
                result ~= "    static TParseTree " ~ shortName ~ "(TParseTree p)\n"
                        ~  "    {\n"
                        ~  "        if(__ctfe)\n"
                        ~  "        {\n"
                        ~  "            return " ~ ctfeCode ~ "(p);\n"
                        ~  "        }\n"
                        ~  "        else\n"
                        ~  "        {\n"
                        ~  "            if(auto m = tuple("~innerName~",p.end) in memo)\n"
                        ~  "                return *m;\n"
                        ~  "            else\n"
                        ~  "            {\n"
                        ~  "                TParseTree result = " ~ code ~ "(p);\n"
                        ~  "                memo[tuple("~innerName~",p.end)] = result;\n"
                        ~  "                return result;\n"
                        ~  "            }\n"
                        ~  "        }\n"
                        ~  "    }\n\n"
                        ~  "    static TParseTree " ~ shortName ~ "(string s)\n"
                        ~  "    {\n"
                        ~  "        if(__ctfe)\n"
                        ~  "        {\n"
                        ~  "            return " ~ ctfeCode ~ "(TParseTree(\"\", false,[], s));\n"
                        ~  "        }\n"
                        ~  "        else\n"
                        ~  "        {\n"
                        ~  "            memo = null;\n"
                        ~  "            return " ~ code ~ "(TParseTree(\"\", false,[], s));\n"
                        ~  "        }\n"
                        ~  "    }\n";

                result ~= "    static string " ~ shortName ~ "(GetName g)\n"
                        ~  "    {\n"
                        ~  "        return \"" ~ propagatedName ~ "." ~ innerName[1..$-1] ~ "\";\n"
                        ~  "    }\n\n";

            if (parameterizedRule)
                result ~= "    }\n";

            break;
        case "Pegged.GrammarName":
            result = generateCode(p.children[0]);
            if (p.children.length == 2)
                result ~= generateCode(p.children[1]);
            break;
        case "Pegged.LhsName":
            result = generateCode(p.children[0]);
            if (p.children.length == 2)
                result ~= generateCode(p.children[1]);
            break;
        case "Pegged.ParamList":
            result = "(";
            foreach(i,child; p.children)
                result ~= generateCode(child) ~ ", ";
            result = result[0..$-2] ~ ")";
            break;
        case "Pegged.Param":
            result = "alias " ~ generateCode(p.children[0]);
            break;
        case "Pegged.SingleParam":
            result = p.matches[0];
            break;
        case "Pegged.DefaultParam":
            result = p.matches[0] ~ " = " ~ generateCode(p.children[1]);
            break;
    }

    Dynamic ruleFromTree(ParseTree p)
    {
        Dynamic result;
        switch(p.name)
        {
            case "Pegged.Expression":
                if (p.children.length > 1) // OR expression
                {
                    Dynamic[] children;
                    foreach(seq; p.children)
                        children ~= ruleFromTree(seq);
                    return distribute!(or)(children)
                }
                else // One child -> just a sequence, no need for a or!( , )
                {
                    return ruleFromTree(p.children[0]);
                }
                break;
            case "Pegged.Sequence":
                if (p.children.length > 1) // real sequence
                {
                    Dynamic[] children;
                    foreach(seq; p.children)
                        children ~= ruleFromTree(seq);
                    return distribute!(and)(children)
                }
                else // One child -> just a Suffix, no need for a and!( , )
                {
                    return ruleFromTree(p.children[0]);
                }
                break;
            case "Pegged.Prefix":
                foreach(i,ref child; p.children[0..$-1]) // transforming a list into a linear tree
                    child.children[0] = p.children[i+1];
                return ruleFromTree(p.children[0]);
            case "Pegged.Suffix":
                result = ruleFromTree(p.children[0]);
                foreach(child; p.children[1..$])
                {
                    switch (child.name)
                    {
                        case "Pegged.OPTION":
                            result = option(result);
                        case "Pegged.ZEROORMORE":
                            result =zeroOrMore(result);
                        case "Pegged.ONEORMORE":
                            result= oneOrMore(result);
                        /+ Action is not implemented
                        case "Pegged.Action":
                            foreach(action; child.matches)
                                result = "pegged.peg.action!(" ~ result ~ ", " ~ action ~ ")";
                            break;
                        +/
                        default:
                            throw new Exception("Bad suffix");
                    }
                }
                return result;
            case "Pegged.Primary":
                return ruleFromTree(p.children[0]);
            ////////////////////////////////////////////////////////////////////////////// To be continued
            case "Pegged.RhsName":
                result = "";
                foreach(i,child; p.children)
                    result ~= generateCode(child);
                break;
            case "Pegged.ArgList":
                result = "!(";
                foreach(child; p.children)
                    result ~= generateCode(child) ~ ", "; // Allow  A <- List('A'*,',')
                result = result[0..$-2] ~ ")";
                break;
            case "Pegged.Identifier":
                result = p.matches[0];
                break;
            case "Pegged.NAMESEP":
                result = ".";
                break;
            case "Pegged.Literal":
                if(p.matches.length == 3) // standard case
                    result = "pegged.peg.literal!(\"" ~ p.matches[1] ~ "\")";
                else // only two children -> empty literal
                    result = "pegged.peg.literal!(``)";
                break;
            case "Pegged.CharClass":
                if (p.children.length > 1)
                {
                    result = "pegged.peg.or!(";
                    foreach(seq; p.children)
                        result ~= generateCode(seq) ~ ", ";
                    result = result[0..$-2] ~ ")";
                }
                else // One child -> just a sequence, no need for a or!( , )
                {
                    result = generateCode(p.children[0]);
                }
                break;
            case "Pegged.CharRange":
                /// Make the generation at the Char level: directly what is needed, be it `` or "" or whatever
                if (p.children.length > 1) // a-b range
                {
                    result = "pegged.peg.charRange!('" ~ generateCode(p.children[0])
                                                       ~ "', '"
                                                       ~ generateCode(p.children[1])
                                                       ~ "')";
                }
                else // lone char
                {
                    result = "pegged.peg.literal!(";
                    string ch = p.matches[0];
                    switch (ch)
                    {
                        case "\\[":
                        case "\\]":
                        case "\\-":
                            result ~= "\""  ~ ch[1..$] ~ "\")";
                            break;
                        case "\\\'":
                            result ~= "\"'\")";
                            break;
                        case "\\`":
                            result ~= q{"`")};
                            break;
                        case "\\":
                        case "\\\\":
                            result ~= "`\\`)";
                            break;
                        case "\"":
                        case "\\\"":
                            result ~= "`\"`)";
                            break;
                        case "\n":
                        case "\r":
                        case "\t":
                            result ~= "\"" ~ to!string(to!dchar(ch)) ~ "\")";
                            break;
                        default:
                            result ~= "\"" ~ ch ~ "\")";
                    }
                }
                break;
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
                        result = ch[1..$];
                        break;
                    case "\n":
                    case "\r":
                    case "\t":
                        result = to!string(to!dchar(ch));
                        break;
                    default:
                        result = ch;
                }
                break;
            case "Pegged.POS":
                return posLookahead(p.children[0]);
            case "Pegged.NEG":
                return negLookahead(p.children[0]);
            case "Pegged.FUSE":
                return fuse(p.children[0]);
            case "Pegged.DISCARD":
                return discard(p.children[0]);
            case "Pegged.KEEP":
                return keep(p.children[0]);
            case "Pegged.DROP":
                return drop(p.children[0]);
            case "Pegged.PROPAGATE":
                return propagate(p.children[0]);
            case "Pegged.OPTION":
                result = "pegged.peg.option!(";
                break;
            case "Pegged.ZEROORMORE":
                result = "pegged.peg.zeroOrMore!(";
                break;
            case "Pegged.ONEORMORE":
                result = "pegged.peg.oneOrMore!(";
                break;
            case "Pegged.Action":
                result = generateCode(p.children[0]);
                foreach(action; p.matches[1..$])
                    result = "pegged.peg.action!(" ~ result ~ ", " ~ action ~ ")";
                break;
            case "Pegged.ANY":
                result = "pegged.peg.any";
                break;
            case "Pegged.WrapAround":
                result = "pegged.peg.wrapAround!(" ~ generateCode(p.children[0]) ~ ", "
                                                   ~ generateCode(p.children[1]) ~ ", "
                                                   ~ generateCode(p.children[2]) ~ ")";
                break;
            default:
                result = "Bad tree: " ~ p.toString();
                break;
        }
        return result;
    }



    return gr;
}

Dynamic distribute(alias fun)(Dynamic[] args)
{
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
        case 7:
            return fun(args[0], args[1], args[2], args[3], args[4], args[5], args[6];
        case 8:
            return fun(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
        case 9:
            return fun(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]);
        default:
            throw new Exception("Unimplemented distribute for more than 7 arguments.");
    }
}