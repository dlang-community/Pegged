module pegged.grammar;

import std.conv:to;

public import pegged.peg;

struct Peg
{
    enum names = ["Grammar":true, 
                  "Definition":true, "Expression":true, "Sequence":true, "Prefix":true, "Suffix":true, "Primary":true, 
                  "Identifier":true, "GrammarName":true, "LhsName":true, "RhsName":true,
                  "ParamList":true, "Param":true, "DefaultParam":true, "SingleParam":true, "ArgList":true,
                  "Literal":true, "CharClass":true, "CharRange":true, "Char":true,
                  "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DISCARDARROW":true, "SPACEARROW":true,
                  "OR":true, 
                  "POS":true, "NEG":true, "FUSE":true,"DISCARD":true, "CUT":true, "KEEP":true, "DROP":true,
                  "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "Action":true, "ACTIONOPEN": true, "ACTIONCLOSE":true, "SEPARATOR": true, 
                  "OPEN":true, "CLOSE":true, "ASSIGN":true, "NAMESEP":true,
                  "ANY":true, 
                  "Spacing":true, "Comment":true, "Space":true,"EndOfLine":true, "EndOfInput": true];
      
    mixin decimateTree;
    
    //# Hierarchical syntax

    // Grammar <- Spacing GrammarName Definition+ EndOfInput
    static ParseTree Grammar(ParseTree p)
    {
        return named!(and!(discard!Spacing, GrammarName, oneOrMore!Definition, EndOfInput), "Grammar")(p);
    }
    
    // Definition <- LhsName Arrow Expression
    static ParseTree Definition(ParseTree p)
    {
        return named!(and!(LhsName, Arrow, Expression), "Definition")(p);
    }
    
    // Expression <- Sequence (OR Sequence)*
    static ParseTree Expression(ParseTree p)
    {
        return named!(and!(Sequence, zeroOrMore!(and!(discard!OR, Sequence))), "Expression")(p);
    }
    
    // Sequence <- Prefix+
    static ParseTree Sequence(ParseTree p)
    {
        return named!(oneOrMore!(Prefix), "Sequence")(p);
    }
    
    // Prefix <- (POS / NEG / FUSE / DISCARD / CUT / KEEP / DROP)* Suffix
    static ParseTree Prefix(ParseTree p)
    {
        return named!(and!(zeroOrMore!(or!(POS, NEG, FUSE, DISCARD, CUT, KEEP, DROP)), Suffix), "Prefix")(p);
    }
    
    // Suffix <- Primary (OPTION / ZEROORMORE / ONEORMORE / Action)*
    static ParseTree Suffix(ParseTree p)
    {
        return named!(and!(Primary, zeroOrMore!(or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Suffix")(p);
    }
    
    
    // Primary <- RhsName !Arrow 
    //          / OPEN Expression CLOSE 
    //          / Literal 
    //          / Class 
    //          / ANY
    static ParseTree Primary(ParseTree p)
    {
        return named!(or!( and!(RhsName, negLookahead!(Arrow)),
                           and!(discard!OPEN, Expression, discard!CLOSE),
                           Literal,
                           CharClass,
                           ANY)       
                    , "Primary")(p);
    }
    
    //# Lexical syntax
    // Identifier <- identifier
    static ParseTree Identifier(ParseTree p)
    {
        return named!(identifier, "Identifier")(p);
    }
    
    // GrammarName <- Identifier ParamList? Spacing :':' Spacing
    static ParseTree GrammarName(ParseTree p)
    {
        return named!(and!(Identifier, option!ParamList, discard!Spacing, discard!(literal!":"), discard!Spacing), "GrammarName")(p);
    }
    
    // LhsName <- Identifier ParamList? Spacing
    static ParseTree LhsName(ParseTree p)
    {
        return named!(and!(Identifier, option!ParamList, discard!Spacing), "LhsName")(p);
    }
    
    // RhsName <- Identifier ArgList? (NAMESEP Identifier ArgList?)* Spacing         # NAMESEP is *not* discarded
    static ParseTree RhsName(ParseTree p)
    {
        return named!(and!(Identifier, option!ArgList, zeroOrMore!(and!(NAMESEP, Identifier, option!ArgList)), discard!Spacing), "RhsName")(p);
    }
    
    // ParamList   <- :OPEN Param (:SEPARATOR Param)*  :CLOSE
    static ParseTree ParamList(ParseTree p)
    {
        return named!(and!(discard!OPEN, Param, zeroOrMore!(and!(discard!SEPARATOR, Param)), discard!CLOSE), "ParamList")(p);
    }

    // Param <- DefaultParam / SingleParam
    static ParseTree Param(ParseTree p)
    {
        return named!(or!(DefaultParam, SingleParam), "Param")(p);
    }

    // DefaultParam <- Identifier Spacing ASSIGN Expression
    static ParseTree DefaultParam(ParseTree p)
    {
        return named!(and!(Identifier, discard!Spacing, discard!(ASSIGN), Expression), "DefaultParam")(p);
    }

    // SingleParam <- Identifier Spacing
    static ParseTree SingleParam(ParseTree p)
    {
        return named!(and!(Identifier, discard!Spacing), "SingleParam")(p);
    }

    // ArgList     <- :OPEN Expression (:SEPARATOR Expression)* :CLOSE
    static ParseTree ArgList(ParseTree p)
    {
        return named!(and!(discard!OPEN, Expression, zeroOrMore!(and!(discard!SEPARATOR, Expression)), discard!CLOSE), "ArgList")(p);
    }

    // Literal <- [’] (![’] Char)* [’] Spacing 
    //          / ["] (!["] Char)* ["] Spacing
    static ParseTree Literal(ParseTree p)
    {
        return named!(fuse!(discardChildren!(
                      or!( and!(discard!quote      , zeroOrMore!(and!(negLookahead!quote      , Char)), discard!quote      , discard!Spacing),
                           and!(discard!doublequote, zeroOrMore!(and!(negLookahead!doublequote, Char)), discard!doublequote, discard!Spacing)))),
                           "Literal")(p);
    }
    
    // CharClass <- ’[’ (!’]’ CharRange)* ’]’ Spacing
    static ParseTree CharClass(ParseTree p)
    {
        return named!(and!(discard!(literal!"["), zeroOrMore!(and!(negLookahead!(literal!"]"), CharRange)), discard!(literal!"]"), discard!Spacing), "CharClass")(p);
    }
    
    // CharRange <- Char ’-’ Char / Char
    static ParseTree CharRange(ParseTree p)
    {
        return named!(or!( and!(Char, discard!(literal!"-"), Char), Char), "CharRange")(p);
    }
    
    // Char        <~ BackSlash ( Quote
    //                          / DoubleQuote
    //                          / BackQuote
    //                          / BackSlash 
    //                          / '-'                
    //                          / '[' 
    //                          / ']' 
    //                          / [nrt]
    //                          / [0-2][0-7][0-7]
    //                          / [0-7][0-7]?
    //                          / 'x' Hex Hex
    //                          / 'u' Hex Hex Hex Hex
    //                          / 'U' Hex Hex Hex Hex Hex Hex Hex Hex)
    //              / . # or anything else
    static ParseTree Char(ParseTree p)
    {
        return named!(fuse!(discardChildren!(
                      or!( and!(backslash, or!(quote, doublequote, backquote, backslash,
                                               literal!"-", literal!"]",literal!"[",
                                               literal!"n", literal!"r", literal!"t",
                                               and!(charRange!('0', '2'), charRange!('0', '7'), charRange!('0', '7')),
                                               and!(charRange!('0', '7'), option!(charRange!('0', '7'))),
                                               and!(literal!"x", hexDigit, hexDigit),
                                               and!(literal!"u", hexDigit, hexDigit, hexDigit, hexDigit),
                                               and!(literal!"U", hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))),
                           pegged.peg.any)
                                         )), "Char")(p);
    }
    
    // Arrow <- LEFTARROW / FUSEARROW / DISCARDARROW / SPACEARROW
    static ParseTree Arrow(ParseTree p)
    {
        return named!(or!(LEFTARROW, FUSEARROW, DISCARDARROW, SPACEARROW), "Arrow")(p);
    }
    
    // LEFTARROW <- ’<-’ Spacing
    static ParseTree LEFTARROW(ParseTree p)
    {
        return named!(and!(literal!"<-", discard!Spacing), "LEFTARROW")(p);
    }
    
    // FUSEARROW <- ’<~’ Spacing
    static ParseTree FUSEARROW(ParseTree p)
    {
        return named!(and!(literal!"<~", discard!Spacing), "FUSEARROW")(p);
    }
    
    // DISCARDARROW <- ’<:’ Spacing
    static ParseTree DISCARDARROW(ParseTree p)
    {
        return named!(and!(literal!"<:", discard!Spacing), "DISCARDARROW")(p);
    }
    
    // SPACEARROW <- ’< ’ Spacing
    static ParseTree SPACEARROW(ParseTree p)
    {
        return named!(and!(literal!"< ", discard!Spacing), "SPACEARROW")(p);
    }
    
    // OR <- ’/’ Spacing
    static ParseTree OR(ParseTree p)
    {
        return named!(and!(literal!"/", discard!Spacing), "OR")(p);
    }
    
    // POS <- ’&’ Spacing
    static ParseTree POS(ParseTree p)
    {
        return named!(and!(literal!"&", discard!Spacing), "POS")(p);
    }
    
    // NEG <- ’!’ Spacing
    static ParseTree NEG(ParseTree p)
    {
        return named!(and!(literal!"!", discard!Spacing), "NEG")(p);
    }
    
    // FUSE <- ’~’ Spacing
    static ParseTree FUSE(ParseTree p)
    {
        return named!(and!(literal!"~", discard!Spacing), "FUSE")(p);
    }
    
    // DISCARD <- ’:’ Spacing
    static ParseTree DISCARD(ParseTree p)
    {
        return named!(and!(literal!":", discard!Spacing), "DISCARD")(p);
    }
    
    // CUT <- ’\’ Spacing
    static ParseTree CUT(ParseTree p)
    {
        return named!(and!(backslash, discard!Spacing), "CUT")(p);
    }
    
    // KEEP <- ’^’ Spacing
    static ParseTree KEEP(ParseTree p)
    {
        return named!(and!(literal!"^", discard!Spacing), "KEEP")(p);
    }
    
    // DROP <- ’;’ Spacing
    static ParseTree DROP(ParseTree p)
    {
        return named!(and!(literal!";", discard!Spacing), "DROP")(p);
    }
    
    // OPTION <- ’?’ Spacing
    static ParseTree OPTION(ParseTree p)
    {
        return named!(and!(literal!"?", discard!Spacing), "OPTION")(p);
    }
    
    // ZEROORMORE <- ’*’ Spacing
    static ParseTree ZEROORMORE(ParseTree p)
    {
        return named!(and!(literal!"*", discard!Spacing), "ZEROORMORE")(p);
    }
    
    // ONEORMORE <- ’+’ Spacing
    static ParseTree ONEORMORE(ParseTree p)
    {
        return named!(and!(literal!"+", discard!Spacing), "ONEORMORE")(p);
    }
    
    // Action <- ACTIONOPEN qualifiedIdentifier (SEPARATOR qualifiedIdentifier)* ACTIONCLOSE
    static ParseTree Action(ParseTree p)
    {
        return named!(and!(discard!ACTIONOPEN, qualifiedIdentifier, zeroOrMore!(and!(discard!SEPARATOR, qualifiedIdentifier)), discard!ACTIONCLOSE), "Action")(p);
    }
    
    // ACTIONOPEN <- ’{’ Spacing
    static ParseTree ACTIONOPEN(ParseTree p)
    {
        return named!(and!(literal!"{", discard!Spacing), "ACTIONOPEN")(p);
    }
    
    // ACTIONCLOSE <- ’}’ Spacing
    static ParseTree ACTIONCLOSE(ParseTree p)
    {
        return named!(and!(literal!"}", discard!Spacing), "ACTIONCLOSE")(p);
    }
    
    // SEPARATOR <- ’,’ Spacing
    static ParseTree SEPARATOR(ParseTree p)
    {
        return named!(and!(literal!",", discard!Spacing), "SEPARATOR")(p);
    }
   
    // ASSIGN <- ’=' Spacing
    static ParseTree ASSIGN(ParseTree p)
    {
        return named!(and!(literal!"=", discard!Spacing), "ASSIGN")(p);
    }
    
    // NAMESEP <- '.'   # No Spacing
    static ParseTree NAMESEP(ParseTree p)
    {
        return named!(literal!".", "NAMESEP")(p);
    }
      
    // OPEN <- ’(’ Spacing
    static ParseTree OPEN(ParseTree p)
    {
        return named!(and!(literal!"(", discard!Spacing), "OPEN")(p);
    }
    
    // CLOSE <- ’)’ Spacing
    static ParseTree CLOSE(ParseTree p)
    {
        return named!(and!(literal!")", discard!Spacing), "CLOSE")(p);
    }
    
    // ANY <- ’.’ Spacing
    static ParseTree ANY(ParseTree p)
    {
        return named!(and!(literal!".", discard!Spacing), "ANY")(p);
    }
    
    // Spacing <- (Space / Comment)*
    static ParseTree Spacing(ParseTree p)
    {
        return named!(zeroOrMore!(or!(Space, Comment)), "Spacing")(p);
    }
    
    // Comment <- ’#’ (!EndOfLine .)* EndOfLine
    static ParseTree Comment(ParseTree p)
    {
        return named!(and!(literal!"#", zeroOrMore!(and!(negLookahead!EndOfLine, pegged.peg.any)), EndOfLine), "Comment")(p);
    }
    
    // Space <- ’ ’ / ’\t’ / EndOfLine
    static ParseTree Space(ParseTree p)
    {
        return named!(or!(literal!" ", literal!"\t", EndOfLine), "Space")(p);
    }
    
    // EndOfLine <- ’\r\n’ / ’\n’ / ’\r’
    static ParseTree EndOfLine(ParseTree p)
    {
        return named!(or!(literal!"\r\n", literal!"\n", literal!"\r"), "EndOfLine")(p);
    }
    
    // EndOfInput <- !.
    static ParseTree EndOfInput(ParseTree p)
    {
        return named!(eoi, "EndOfInput")(p);
    }

    static ParseTree opCall(ParseTree p)
    {
        return decimateTree(Grammar(p));
    }

    static ParseTree opCall(string input)
    {
        return Peg(ParseTree("Peg", false, [], input, 0, 0));
    }
}

string grammar(string definition)
{
    ParseTree defAsParseTree = Peg(definition);
    
    if (!defAsParseTree.successful)
        return "static assert(false, `" ~ defAsParseTree.toString() ~ "`);";
    
    string generateCode(ParseTree p)
    {
        string result;
        
        switch (p.name)
        {
            case "Grammar":
                string grammarName = generateCode(p.children[0]);
                string shortGrammarName = p.children[0].matches[0];
                //string invokedGrammarName = generateCode(transformName(p.children[0]));
                string firstRuleName = generateCode(p.children[1].children[0]);
                
                result =  "struct " ~ grammarName ~ "\n{\n";
                result ~= "    enum names = [";
                
                ParseTree[] definitions = p.children[1 .. $];
                bool userDefinedSpacing = false;
                foreach(def; definitions)
                {
                    result ~= "`" ~ def.matches[0] ~ "`:true, ";
                    if (def.matches[0] == "Spacing") // user-defined spacing
                        userDefinedSpacing = true;
                }
                result = result[0..$-2] ~ "];\n\n";
                
                result ~= "    mixin decimateTree;\n\n";
                
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
                    result ~= "    static ParseTree opCall(ParseTree p)\n";
                    result ~= "    {\n";
                    result ~= "        ParseTree result = decimateTree(" ~ firstRuleName ~ "(p));\n";
                    result ~= "        result.children = [result];\n";
                    result ~= "        result.name = \"" ~ shortGrammarName ~ "\";\n";
                    result ~= "        return result;\n";
                    result ~= "    }\n\n";
                    result ~= "    static ParseTree opCall(string input)\n";
                    result ~= "    {\n";
                    result ~= "        return " ~ shortGrammarName ~ "(ParseTree(``, false, [], input, 0, 0));\n";
                    result ~= "    }\n";
                
                    //result ~= "    ParseTree opDispatch(string rule)(string input)\n{\n";
                    //result ~= "        mixin(\"return \" ~ rule ~ \"(ParseTree(``, false, [], input, 0, 0))\");\n}\n";
                }
                result ~= "}\n\n"; // end of grammar struct definition
                break;
            case "Definition":
                // children[0]: name
                // children[1]: arrow (arrow type as first child)
                // children[2]: description
                result = "    static ParseTree " ~ generateCode(p.children[0]) ~ "(ParseTree p)\n    {\n";
                result ~="        return named!(";
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
                        int i = 0;
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
                result ~= "    }\n\n";
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
            case "CUT":
                result = "discardChildren!(";
                break;
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
