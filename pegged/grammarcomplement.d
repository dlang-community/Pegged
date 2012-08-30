/**
 * This module contains the complementary functions used after the parser generated
 * from pegged/examples/peggedgrammar.d
 */
module grammarcomplement;


void asModule(Memoization withMemo = Memoization.yes)(string moduleName, string grammarString)
{
    asModule!(withMemo)(moduleName, moduleName~".d", grammarString);
}

void asModule(Memoization withMemo = Memoization.yes)(string moduleName, string fileName, string grammarString)
{
    import std.stdio;
    auto f = File(fileName,"w");
    
    f.write("/**\nThis module was automatically generated from the following grammar:\n\n");
    f.write(grammarString);
    f.write("\n\n*/\n");
    
    f.write("module " ~ moduleName ~ ";\n\n");
    f.write("public import pegged.peg;\n");
    f.write(grammar!(withMemo)(grammarString));
}

enum Memoization { no, yes }


string grammar(Memoization withMemo = Memoization.yes)(string definition)
{
    ParseTree defAsParseTree = Pegged(definition);
    
    if (!defAsParseTree.successful)
        return "static assert(false, `" ~ defAsParseTree.toString() ~ "`);";
    
    string generateCode(ParseTree p)
    {
        string result;
        
        switch (p.name)
        {
            case "Pegged":
                result = generateCode(p.children[0]);
                break;
            case "Grammar":
                string grammarName = generateCode(p.children[0]);
                string shortGrammarName = p.children[0].matches[0];
                //string invokedGrammarName = generateCode(transformName(p.children[0]));
                string firstRuleName = generateCode(p.children[1].children[0]);
                
                result =  "struct " ~ grammarName ~ "\n{\n";
                static if (withMemo == Memoization.yes)
                {
                    result ~= "    import std.typecons:Tuple, tuple;\n";
                    result ~= "    static ParseTree[Tuple!(string, uint)] memo;\n";
                }
                
                result ~= "    enum names = [";
                
                ParseTree[] definitions = p.children[1 .. $];
                bool userDefinedSpacing = false;
                foreach(def; definitions)
                {
                    result ~= "`" ~ def.matches[0] ~ "`:true, ";
                    if (def.matches[0] == "Spacing") // user-defined spacing
                        userDefinedSpacing = true;
                }
                result = result[0..$-2] ~ "];\n";
                
                result ~= "    mixin decimateTree;\n";
                
                
                // If the grammar provides a Spacing rule, then this will be used.
                // else, the predefined 'spacing' rule is used.
                result ~= userDefinedSpacing ? "" : "    alias spacing Spacing;\n\n";
                    
                // Creating the inner functions, each corresponding to a grammar rule
                foreach(def; definitions)
                    result ~= generateCode(def);
                
                // if the first rule is parameterized (a template), it's impossible to get an opCall
                // because we don't know with which template arguments it should be called.
                // So no opCall is generated in this case.
                if (p.children[1].children[0].children.length == 1) 
                {
                    // General calling interface
                    result ~= "    static ParseTree opCall(ParseTree p)\n"
                           ~  "    {\n"
                           ~  "        ParseTree result = decimateTree(" ~ firstRuleName ~ "(p));\n"
                           ~  "        result.children = [result];\n"
                           ~  "        result.name = \"" ~ shortGrammarName ~ "\";\n"
                           ~  "        return result;\n"
                           ~  "    }\n\n"
                           ~  "    static ParseTree opCall(string input)\n"
                           ~  "    {\n";
                    static if (withMemo == Memoization.yes)
                        result ~= "        memo = null;\n";
                        
                    result ~= "        return " ~ shortGrammarName ~ "(ParseTree(``, false, [], input, 0, 0));\n"
                           ~  "    }\n";
                
                    //result ~= "    ParseTree opDispatch(string rule)(string input)\n{\n";
                    //result ~= "        mixin(\"return \" ~ rule ~ \"(ParseTree(``, false, [], input, 0, 0))\");\n}\n";
                }
                result ~= "}\n\n"; // end of grammar struct definition
                break;
            case "Definition":
                // children[0]: name
                // children[1]: arrow (arrow type as first child)
                // children[2]: description
                result =  "    static ParseTree " ~ generateCode(p.children[0]) ~ "(ParseTree p)\n    {\n";
                static if (withMemo == Memoization.yes)
                    result ~= "        if(auto m = tuple(\""~p.matches[0]~"\",p.end) in memo)\n"
                            ~ "            return *m;\n"
                            ~ "        else\n"
                            ~ "        {\n"
                            ~ "            ParseTree result = named!(";
                else
                    result ~= "        return named!(";
                    
                switch(p.children[1].children[0].name)
                {
                    case "LEFTARROW":
                        result ~= generateCode(p.children[2]);
                        break;
                    case "FUSEARROW":
                        result ~= "fuse!(" ~ generateCode(p.children[2]) ~ ")";
                        break;
                    case "DISCARDARROW":
                        result ~= "discard!(" ~ generateCode(p.children[2]) ~ ")";
                        break;
                    case "SPACEARROW":
                        string temp = generateCode(p.children[2]);
                        size_t i = 0;
                        while(i < temp.length-4) // a while loop to make it work at compile-time.
                        {
                            if (temp[i..i+5] == "and!(")
                            {
                                result ~= "spaceAnd!(Spacing, ";
                                i = i + 5;
                            }
                            else
                            {
                                result ~= temp[i];
                                i = i + 1; // ++i doesn't work at CT
                            }
                        }
                        result ~= temp[$-4..$];
                        break;
                    default:
                        break;
                }
                
                result ~= ", \"" ~ p.matches[0] ~ "\")(p);\n";
                static if (withMemo == Memoization.yes)
                    result ~= "            memo[tuple(\"" ~ p.matches[0] ~ "\",p.end)] = result;\n"
                           ~  "            return result;\n"
                           ~  "        }\n";
                
                result ~= "    }\n\n"
                       ~  "    static ParseTree " ~ generateCode(p.children[0]) ~ "(string s)\n    {\n";
                static if (withMemo == Memoization.yes)
                    result ~=  "        memo = null;";
                    
                result ~= "        return " ~ generateCode(p.children[0]) ~ "(ParseTree(\"\", false,[], s));\n    }\n\n";

                break;
            case "GrammarName":
                result = generateCode(p.children[0]);
                if (p.children.length == 2)
                    result ~= generateCode(p.children[1]);
                break;
            case "LhsName":
                result = generateCode(p.children[0]);
                if (p.children.length == 2)
                    result ~= generateCode(p.children[1]);
                break;
            case "ParamList":
                result = "(";
                foreach(i,child; p.children)
                    result ~= generateCode(child) ~ ", ";
                result = result[0..$-2] ~ ")";
                break;
            case "Param":
                result = "alias " ~ generateCode(p.children[0]);
                break;
            case "SingleParam":
                result = p.matches[0];
                break;
            case "DefaultParam":
                result = p.matches[0] ~ " = " ~ generateCode(p.children[1]);
                break;
            case "Expression":
                if (p.children.length > 1) // OR expression
                {
                    result = "or!(";
                    foreach(seq; p.children)
                        result ~= generateCode(seq) ~ ", ";
                    result = result[0..$-2] ~ ")";
                }
                else // One child -> just a sequence, no need for a or!( , )
                {
                    result = generateCode(p.children[0]);
                }
                break;
            case "Sequence":
                //if (p.children.length > 1) // real sequence
                //{
                    result = "and!(";
                    foreach(seq; p.children)
                        result ~= generateCode(seq) ~ ", ";
                    result = result[0..$-2] ~ ")";
                //}
                //else // One child -> just a Suffix, no need for a and!( , )
                //{
                //    result = generateCode(p.children[0]);
                //}
                break;
            case "Prefix":
                result = generateCode(p.children[$-1]);
                foreach(child; p.children[0..$-1])
                    result = generateCode(child) ~ result ~ ")";
                break;
            case "Suffix":
                result = generateCode(p.children[0]);
                foreach(child; p.children[1..$])
                {
                    switch (child.name)
                    {
                        case "OPTION":
                            result = "option!(" ~ result ~ ")";
                            break;
                        case "ZEROORMORE":
                            result = "zeroOrMore!(" ~ result ~ ")";
                            break;
                        case "ONEORMORE":
                            result = "oneOrMore!(" ~ result ~ ")";
                            break;
                        case "Action":
                            foreach(action; child.matches)
                                result = "action!(" ~ result ~ ", " ~ action ~ ")";
                            break;
                        default:
                            break;
                    }
                }
                break;
            case "Primary":
                result = generateCode(p.children[0]);
                break;
            case "RhsName":
                result = "";
                foreach(i,child; p.children)
                    result ~= generateCode(child);
                break;
            case "ArgList":
                result = "!(";
                foreach(child; p.children)
                    result ~= generateCode(child) ~ ", "; // Allow  A <- List('A'*,',') 
                result = result[0..$-2] ~ ")";
                break;
            case "Identifier":
                result = p.matches[0];
                break;
            case "NAMESEP":
                result = ".";
                break;
            case "Literal":
                result = "literal!(\"" ~ p.matches[0] ~ "\")";
                break;
            case "CharClass":
                if (p.children.length > 1)
                {
                    result = "or!(";
                    foreach(seq; p.children)
                        result ~= generateCode(seq) ~ ", ";
                    result = result[0..$-2] ~ ")";
                }
                else // One child -> just a sequence, no need for a or!( , )
                {
                    result = generateCode(p.children[0]);
                }
                break;
            case "CharRange":
                if (p.children.length > 1) // a-b range
                {
                    result = "charRange!('" ~ generateCode(p.children[0]) ~ "', '" ~ generateCode(p.children[1]) ~ "')";
                }
                else // lone char
                {
                    result = "literal!(\"" ~ generateCode(p.children[0]) ~ "\")";
                }
                break;
            case "Char":
                string ch = p.matches[0];
                if(ch == "\\[" || ch == "\\]" || ch == "\\-")
                    result = ch[1..$];
                else
                    result = ch;
                break;
            case "POS":
                result = "posLookahead!(";
                break;
            case "NEG":
                result = "negLookahead!(";
                break;
            case "FUSE":
                result = "fuse!(";
                break;
            case "DISCARD":
                result = "discard!(";
                break;
            //case "CUT":
            //    result = "discardChildren!(";
            //    break;
            case "KEEP":
                result = "keep!(";
                break;
            case "DROP":
                result = "drop!(";
                break;
            case "OPTION":
                result = "option!(";
                break;
            case "ZEROORMORE":
                result = "zeroOrMore!(";
                break;
            case "ONEORMORE":
                result = "oneOrMore!(";
                break;
            case "Action":
                result = generateCode(p.children[0]);
                foreach(action; p.matches[1..$])
                    result = "action!(" ~ result ~ ", " ~ action ~ ")";
                break;
            case "ANY":
                result = "pegged.peg.any";
                break;
            default:
                result = "Bad tree: " ~ p.toString();
                break;
        }
        return result;
    }
    return generateCode(defAsParseTree);
}
