/**
 * Parser generation module for Pegged.
 * The Pegged parser itself is in pegged.parser, generated from pegged.examples.peggedgrammar.
 */
module pegged.grammar;

import std.conv: to;

public import pegged.peg;
import pegged.introspection;
import pegged.parser;

enum Memoization { no, yes }

void asModule(Memoization withMemo = Memoization.no)(string moduleName, string grammarString)
{
    asModule!(withMemo)(moduleName, moduleName, grammarString);
}

void asModule(Memoization withMemo = Memoization.no)(string moduleName, string fileName, string grammarString)
{
    import std.stdio;
    auto f = File(fileName ~ ".d","w");
    
    f.write("/**\nThis module was automatically generated from the following grammar:\n\n");
    f.write(grammarString);
    f.write("\n\n*/\n");
    
    f.write("module " ~ moduleName ~ ";\n\n");
    f.write("public import pegged.peg;\n");
    f.write(grammar!(withMemo)(grammarString));
}

string grammar(Memoization withMemo = Memoization.no)(string definition)
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
            case "Pegged.Grammar":
                string grammarName = generateCode(p.children[0]);
                string shortGrammarName = p.children[0].matches[0];
                //string invokedGrammarName = generateCode(transformName(p.children[0]));
                string firstRuleName = generateCode(p.children[1].children[0]);
                
                result =  "struct Generic" ~ shortGrammarName ~ "(TParseTree)\n"
                        ~ "{\n"
                        ~ "    struct " ~ grammarName ~ "\n    {\n"
                        ~ "    enum name = \"" ~ shortGrammarName ~ "\";\n";
                        
                static if (withMemo == Memoization.yes)
                {
                    result ~= "    import std.typecons:Tuple, tuple;\n";
                    result ~= "    static TParseTree[Tuple!(string, size_t)] memo;\n";
                }
                
                result ~= "    static bool isRule(string s)\n"
                        ~ "    {\n"
                        ~ "        switch(s)\n"
                        ~ "        {\n";
                
                ParseTree[] definitions = p.children[1 .. $];
				bool[string] ruleNames; // to avoid duplicates, when using parameterized rules
                bool userDefinedSpacing = false;
                foreach(i,def; definitions)
                {
                    if (def.matches[0] !in ruleNames)
					{
						ruleNames[def.matches[0]] = true;
						result ~= "            case \"" ~ shortGrammarName ~ "." ~ def.matches[0] ~ "\":\n";
					}
                    if (def.matches[0] == "Spacing") // user-defined spacing
                        userDefinedSpacing = true;
                }
                result ~= "                return true;\n"
                        ~ "            default:\n"
                        ~ "                return false;\n        }\n    }\n";
                
                result ~= "    mixin decimateTree;\n";
                
				// Introspection information
				result ~= "    import pegged.introspection;\n"
				        ~ "    static RuleInfo[string] info;\n"
						~ "    static string[] ruleNames;\n" 
						~ "    static this()\n"
						~ "    {\n"
						~ "         info = ruleInfo(q{" ~ definition ~"});\n"
						~ "         ruleNames = info.keys;\n"
						~ "    }\n";
                
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
                    result ~= "    static TParseTree opCall(TParseTree p)\n"
                           ~  "    {\n"
                           ~  "        TParseTree result = decimateTree(" ~ firstRuleName ~ "(p));\n"
                           ~  "        result.children = [result];\n"
                           ~  "        result.name = \"" ~ shortGrammarName ~ "\";\n"
                           ~  "        return result;\n"
                           ~  "    }\n\n"
                           ~  "    static TParseTree opCall(string input)\n"
                           ~  "    {\n";
                    static if (withMemo == Memoization.yes)
                        result ~= "        memo = null;\n";
                        
                    result ~= "        return " ~ shortGrammarName ~ "(TParseTree(``, false, [], input, 0, 0));\n"
                           ~  "    }\n";
                }
                result ~= "    }\n" // end of grammar struct definition
                        ~ "}\n\n" // end of template definition
                        ~ "alias Generic" ~ shortGrammarName ~ "!(ParseTree)." ~ shortGrammarName ~ " " ~ shortGrammarName ~ ";\n\n";
                break;
            case "Pegged.Definition":
                // children[0]: name
                // children[1]: arrow (arrow type as first child)
                // children[2]: description
                
                string code = "named!(";
                    
                switch(p.children[1].children[0].name)
                {
                    case "Pegged.LEFTARROW":
                        code ~= generateCode(p.children[2]);
                        break;
                    case "Pegged.FUSEARROW":
                        code ~= "fuse!(" ~ generateCode(p.children[2]) ~ ")";
                        break;
                    case "Pegged.DISCARDARROW":
                        code ~= "discard!(" ~ generateCode(p.children[2]) ~ ")";
                        break;
                    case "Pegged.SPACEARROW":
                        string temp = generateCode(p.children[2]);
                        size_t i = 0;
                        while(i < temp.length-4) // a while loop to make it work at compile-time.
                        {
                            if (temp[i..i+5] == "and!(")
                            {
                                code ~= "spaceAnd!(Spacing, ";
                                i = i + 5;
                            }
                            else
                            {
                                code ~= temp[i];
                                i = i + 1; // ++i doesn't work at CT?
                            }
                        }
                        code ~= temp[$-4..$];
                        break;
                    default:
                        break;
                }
                
                code ~= ", name ~ \"." ~ p.matches[0] ~ "\")";

                //bool parameterizedRule = p.children[0].children.length > 1;
                
                //if (parameterizedRule)
                //{
                //    result =  "    template " ~ generateCode(p.children[0]) ~ "\n"
                //            ~ "    {\n";
                //}
                
                result ~=  "    static TParseTree " ~ generateCode(p.children[0]) ~ "(TParseTree p)\n    {\n";
                 
                static if (withMemo == Memoization.yes)
                    result ~= "        if(auto m = tuple(\""~generateCode(p.children[0])~"\",p.end) in memo)\n"
                            ~ "            return *m;\n"
                            ~ "        else\n"
                            ~ "        {\n"
                            ~ "            TParseTree result = ";
                else
                    result ~= "        return ";
                
                result ~= code ~ "(p);\n";
                
                static if (withMemo == Memoization.yes)
                    result ~= "            memo[tuple(\"" ~ generateCode(p.children[0]) ~ "\",p.end)] = result;\n"
                           ~  "            return result;\n"
                           ~  "        }\n";
                
                result ~= "    }\n\n";
                result ~= "    static TParseTree " ~ generateCode(p.children[0]) ~ "(string s)\n    {\n";
                                
                static if (withMemo == Memoization.yes)
                    result ~=  "        memo = null;\n";
                
                result ~= "        return " ~ code ~ "(TParseTree(\"\", false,[], s));\n    }\n\n";
                
                //if (parameterizedRule)
                //    result ~= "     }\n";
                
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
            case "Pegged.Expression":
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
            case "Pegged.Sequence":
                if (p.children.length > 1) // real sequence
                {
                    result = "and!(";
                    foreach(seq; p.children)
					{
                        string elementCode = generateCode(seq);
						if (elementCode.length > 6 && elementCode[0..5] == "and!(") // flattening inner sequences
							elementCode = elementCode[5..$-1]; // cutting 'and!(' and ')'
						result ~= elementCode ~ ", ";
					}
                    result = result[0..$-2] ~ ")";
                }
                else // One child -> just a Suffix, no need for a and!( , )
                {
                    result = generateCode(p.children[0]);
                }
                break;
            case "Pegged.Prefix":
                result = generateCode(p.children[$-1]);
                foreach(child; p.children[0..$-1])
                    result = generateCode(child) ~ result ~ ")";
                break;
            case "Pegged.Suffix":
                result = generateCode(p.children[0]);
                foreach(child; p.children[1..$])
                {
                    switch (child.name)
                    {
                        case "Pegged.OPTION":
                            result = "option!(" ~ result ~ ")";
                            break;
                        case "Pegged.ZEROORMORE":
                            result = "zeroOrMore!(" ~ result ~ ")";
                            break;
                        case "Pegged.ONEORMORE":
                            result = "oneOrMore!(" ~ result ~ ")";
                            break;
                        case "Pegged.Action":
                            foreach(action; child.matches)
                                result = "action!(" ~ result ~ ", " ~ action ~ ")";
                            break;
                        default:
                            break;
                    }
                }
                break;
            case "Pegged.Primary":
                result = generateCode(p.children[0]);
                break;
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
                result = "literal!(\"" ~ p.matches[0] ~ "\")";
                break;
            case "Pegged.CharClass":
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
            case "Pegged.CharRange":
                if (p.children.length > 1) // a-b range
                {
                    result = "charRange!('" ~ generateCode(p.children[0]) ~ "', '" ~ generateCode(p.children[1]) ~ "')";
                }
                else // lone char
                {
                    result = "literal!(\"" ~ generateCode(p.children[0]) ~ "\")";
                }
                break;
            case "Pegged.Char":
                string ch = p.matches[0];
                if(ch == "\\[" || ch == "\\]" || ch == "\\-")
                    result = ch[1..$];
                else
                    result = ch;
                break;
            case "Pegged.POS":
                result = "posLookahead!(";
                break;
            case "Pegged.NEG":
                result = "negLookahead!(";
                break;
            case "Pegged.FUSE":
                result = "fuse!(";
                break;
            case "Pegged.DISCARD":
                result = "discard!(";
                break;
            //case "Pegged.CUT":
            //    result = "discardChildren!(";
            //    break;
            case "Pegged.KEEP":
                result = "keep!(";
                break;
            case "Pegged.DROP":
                result = "drop!(";
                break;
            case "Pegged.OPTION":
                result = "option!(";
                break;
            case "Pegged.ZEROORMORE":
                result = "zeroOrMore!(";
                break;
            case "Pegged.ONEORMORE":
                result = "oneOrMore!(";
                break;
            case "Pegged.Action":
                result = generateCode(p.children[0]);
                foreach(action; p.matches[1..$])
                    result = "action!(" ~ result ~ ", " ~ action ~ ")";
                break;
            case "Pegged.ANY":
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
