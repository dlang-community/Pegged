/**
This module was automatically generated from the following grammar:


# This is the PEG extended grammar used by Pegged
Pegged:

# Syntactic rules:
Grammar      <- Spacing GrammarName Definition+ :eoi
Definition   <- LhsName Arrow Expression
Expression   <- Sequence (OR Sequence)*
Sequence     <- Prefix+
Prefix       <- (POS / NEG / FUSE / DISCARD / KEEP / DROP)* Suffix
Suffix       <- Primary (OPTION / ZEROORMORE / ONEORMORE / Action)*
Primary      <- RhsName !Arrow 
              / :OPEN Expression :CLOSE 
              / Literal 
              / CharClass 
              / ANY
# Lexical syntax
Identifier   <- identifier
GrammarName  <- Identifier ParamList? Spacing :':' Spacing
LhsName      <- Identifier ParamList? Spacing
RhsName      <- Identifier ArgList? (NAMESEP Identifier ArgList?)* Spacing         # NAMESEP is *not* discarded
ParamList    <- :OPEN Param (:SEPARATOR Param)*  :CLOSE
Param        <- DefaultParam / SingleParam
DefaultParam <- Identifier Spacing ASSIGN Expression
SingleParam  <- Identifier Spacing
ArgList      <- :OPEN Expression (:SEPARATOR Expression)* :CLOSE
Action       <- ACTIONOPEN qualifiedIdentifier (SEPARATOR qualifiedIdentifier)* ACTIONCLOSE

Literal      <~ :quote       (!quote Char)*       :quote       Spacing 
              / :doublequote (!doublequote Char)* :doublequote Spacing
CharClass    <- :'[' (!']' CharRange)* :']' Spacing
CharRange    <- Char '-' Char / Char         

# Terminals
Char         <~ backslash ( quote
                          / doublequote
                          / backquote
                          / backslash 
                          / '-'                
                          / '[' 
                          / ']' 
                          / [nrt]
                          / [0-2][0-7][0-7]
                          / [0-7][0-7]?
                          / 'x' hexDigit hexDigit
                          / 'u' hexDigit hexDigit hexDigit hexDigit
                          / 'U' hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit hexDigit
                          )
              / . # or anything else
Arrow        <- LEFTARROW / FUSEARROW / DISCARDARROW / SPACEARROW
LEFTARROW    <- '<-' Spacing
FUSEARROW    <- '<~' Spacing
DISCARDARROW <- '<:' Spacing
SPACEARROW   <- '<' Spacing
OR           <- '/' Spacing
POS          <- '&' Spacing
NEG          <- '!' Spacing
FUSE         <- '~' Spacing
DISCARD      <- ':' Spacing
KEEP         <- '^' Spacing
DROP         <- ';' Spacing
OPTION       <- '?' Spacing
ZEROORMORE   <- '*' Spacing
ONEORMORE    <- '+' Spacing
ACTIONOPEN   <- '{' Spacing
ACTIONCLOSE  <- '}' Spacing
SEPARATOR    <- ',' Spacing
ASSIGN       <- '=' Spacing
NAMESEP      <- '.'   # No Spacing
OPEN         <- '(' Spacing
CLOSE        <- ')' Spacing
ANY          <- '.' Spacing
Spacing      <: (Space / Comment)*
Comment      <- '#' (!EndOfLine .)* EndOfLine
Space        <- ' ' / '\t' / EndOfLine
EndOfLine    <- '\r\n' / '\n' / '\r'
EndOfInput   <- !.


*/
module pegged.grammar;

public import pegged.peg;
struct Pegged
{
    enum names = [`Grammar`:true, `Definition`:true, `Expression`:true, `Sequence`:true, `Prefix`:true, `Suffix`:true, `Primary`:true, `Identifier`:true, `GrammarName`:true, `LhsName`:true, `RhsName`:true, `ParamList`:true, `Param`:true, `DefaultParam`:true, `SingleParam`:true, `ArgList`:true, `Action`:true, `Literal`:true, `CharClass`:true, `CharRange`:true, `Char`:true, `Arrow`:true, `LEFTARROW`:true, `FUSEARROW`:true, `DISCARDARROW`:true, `SPACEARROW`:true, `OR`:true, `POS`:true, `NEG`:true, `FUSE`:true, `DISCARD`:true, `KEEP`:true, `DROP`:true, `OPTION`:true, `ZEROORMORE`:true, `ONEORMORE`:true, `ACTIONOPEN`:true, `ACTIONCLOSE`:true, `SEPARATOR`:true, `ASSIGN`:true, `NAMESEP`:true, `OPEN`:true, `CLOSE`:true, `ANY`:true, `Spacing`:true, `Comment`:true, `Space`:true, `EndOfLine`:true, `EndOfInput`:true];

    mixin decimateTree;

    static ParseTree Grammar(ParseTree p)
    {
        return named!(and!(Spacing, GrammarName, oneOrMore!(Definition), discard!(eoi)), "Grammar")(p);
    }

    static ParseTree Definition(ParseTree p)
    {
        return named!(and!(LhsName, Arrow, Expression), "Definition")(p);
    }

    static ParseTree Expression(ParseTree p)
    {
        return named!(and!(Sequence, zeroOrMore!(and!(discard!OR, Sequence))), "Expression")(p);
    }

    static ParseTree Sequence(ParseTree p)
    {
        return named!(and!(oneOrMore!(Prefix)), "Sequence")(p);
    }

    static ParseTree Prefix(ParseTree p)
    {
        return named!(and!(zeroOrMore!(or!(and!(POS), and!(NEG), and!(FUSE), and!(DISCARD), and!(KEEP), and!(DROP))), Suffix), "Prefix")(p);
    }

    static ParseTree Suffix(ParseTree p)
    {
        return named!(and!(Primary, zeroOrMore!(or!(and!(OPTION), and!(ZEROORMORE), and!(ONEORMORE), and!(Action)))), "Suffix")(p);
    }

    static ParseTree Primary(ParseTree p)
    {
        return named!(or!(and!(RhsName, negLookahead!(Arrow)), and!(discard!OPEN, Expression, discard!CLOSE), and!(Literal), and!(CharClass), and!(ANY)), "Primary")(p);
    }

    static ParseTree Identifier(ParseTree p)
    {
        return named!(and!(identifier), "Identifier")(p);
    }

    static ParseTree GrammarName(ParseTree p)
    {
        return named!(and!(Identifier, option!(ParamList), Spacing, discard!(literal!(":")), Spacing), "GrammarName")(p);
    }

    static ParseTree LhsName(ParseTree p)
    {
        return named!(and!(Identifier, option!(ParamList), Spacing), "LhsName")(p);
    }

    static ParseTree RhsName(ParseTree p)
    {
        return named!(and!(Identifier, option!(ArgList), zeroOrMore!(and!(NAMESEP, Identifier, option!(ArgList))), Spacing), "RhsName")(p);
    }

    static ParseTree ParamList(ParseTree p)
    {
        return named!(and!(discard!(OPEN), Param, zeroOrMore!(and!(discard!(SEPARATOR), Param)), discard!(CLOSE)), "ParamList")(p);
    }

    static ParseTree Param(ParseTree p)
    {
        return named!(or!(and!(DefaultParam), and!(SingleParam)), "Param")(p);
    }

    static ParseTree DefaultParam(ParseTree p)
    {
        return named!(and!(Identifier, Spacing, discard!ASSIGN, Expression), "DefaultParam")(p);
    }

    static ParseTree SingleParam(ParseTree p)
    {
        return named!(and!(Identifier, Spacing), "SingleParam")(p);
    }

    static ParseTree ArgList(ParseTree p)
    {
        return named!(and!(discard!(OPEN), Expression, zeroOrMore!(and!(discard!(SEPARATOR), Expression)), discard!(CLOSE)), "ArgList")(p);
    }

    static ParseTree Action(ParseTree p)
    {
        return named!(and!(ACTIONOPEN, qualifiedIdentifier, zeroOrMore!(and!(SEPARATOR, qualifiedIdentifier)), ACTIONCLOSE), "Action")(p);
    }

    static ParseTree Literal(ParseTree p)
    {
        return named!(fuse!(or!(and!(discard!(quote), zeroOrMore!(and!(negLookahead!(quote), Char)), discard!(quote), Spacing), and!(discard!(doublequote), zeroOrMore!(and!(negLookahead!(doublequote), Char)), discard!(doublequote), Spacing))), "Literal")(p);
    }

    static ParseTree CharClass(ParseTree p)
    {
        return named!(and!(discard!(literal!("[")), zeroOrMore!(and!(negLookahead!(literal!("]")), CharRange)), discard!(literal!("]")), Spacing), "CharClass")(p);
    }

    static ParseTree CharRange(ParseTree p)
    {
        return named!(or!(and!(Char, literal!("-"), Char), and!(Char)), "CharRange")(p);
    }

    static ParseTree Char(ParseTree p)
    {
        return named!(fuse!(or!(and!(backslash, or!(and!(quote), and!(doublequote), and!(backquote), and!(backslash), and!(literal!("-")), and!(literal!("[")), and!(literal!("]")), and!(or!(literal!("n"), literal!("r"), literal!("t"))), and!(charRange!('0', '2'), charRange!('0', '7'), charRange!('0', '7')), and!(charRange!('0', '7'), option!(charRange!('0', '7'))), and!(literal!("x"), hexDigit, hexDigit), and!(literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), and!(literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), and!(pegged.peg.any))), "Char")(p);
    }

    static ParseTree Arrow(ParseTree p)
    {
        return named!(or!(and!(LEFTARROW), and!(FUSEARROW), and!(DISCARDARROW), and!(SPACEARROW)), "Arrow")(p);
    }

    static ParseTree LEFTARROW(ParseTree p)
    {
        return named!(and!(literal!("<-"), Spacing), "LEFTARROW")(p);
    }

    static ParseTree FUSEARROW(ParseTree p)
    {
        return named!(and!(literal!("<~"), Spacing), "FUSEARROW")(p);
    }

    static ParseTree DISCARDARROW(ParseTree p)
    {
        return named!(and!(literal!("<:"), Spacing), "DISCARDARROW")(p);
    }

    static ParseTree SPACEARROW(ParseTree p)
    {
        return named!(and!(literal!("<"), Spacing), "SPACEARROW")(p);
    }

    static ParseTree OR(ParseTree p)
    {
        return named!(and!(literal!("/"), Spacing), "OR")(p);
    }

    static ParseTree POS(ParseTree p)
    {
        return named!(and!(literal!("&"), Spacing), "POS")(p);
    }

    static ParseTree NEG(ParseTree p)
    {
        return named!(and!(literal!("!"), Spacing), "NEG")(p);
    }

    static ParseTree FUSE(ParseTree p)
    {
        return named!(and!(literal!("~"), Spacing), "FUSE")(p);
    }

    static ParseTree DISCARD(ParseTree p)
    {
        return named!(and!(literal!(":"), Spacing), "DISCARD")(p);
    }

    static ParseTree KEEP(ParseTree p)
    {
        return named!(and!(literal!("^"), Spacing), "KEEP")(p);
    }

    static ParseTree DROP(ParseTree p)
    {
        return named!(and!(literal!(";"), Spacing), "DROP")(p);
    }

    static ParseTree OPTION(ParseTree p)
    {
        return named!(and!(literal!("?"), Spacing), "OPTION")(p);
    }

    static ParseTree ZEROORMORE(ParseTree p)
    {
        return named!(and!(literal!("*"), Spacing), "ZEROORMORE")(p);
    }

    static ParseTree ONEORMORE(ParseTree p)
    {
        return named!(and!(literal!("+"), Spacing), "ONEORMORE")(p);
    }

    static ParseTree ACTIONOPEN(ParseTree p)
    {
        return named!(and!(literal!("{"), Spacing), "ACTIONOPEN")(p);
    }

    static ParseTree ACTIONCLOSE(ParseTree p)
    {
        return named!(and!(literal!("}"), Spacing), "ACTIONCLOSE")(p);
    }

    static ParseTree SEPARATOR(ParseTree p)
    {
        return named!(and!(literal!(","), Spacing), "SEPARATOR")(p);
    }

    static ParseTree ASSIGN(ParseTree p)
    {
        return named!(and!(literal!("="), Spacing), "ASSIGN")(p);
    }

    static ParseTree NAMESEP(ParseTree p)
    {
        return named!(and!(literal!(".")), "NAMESEP")(p);
    }

    static ParseTree OPEN(ParseTree p)
    {
        return named!(and!(literal!("("), Spacing), "OPEN")(p);
    }

    static ParseTree CLOSE(ParseTree p)
    {
        return named!(and!(literal!(")"), Spacing), "CLOSE")(p);
    }

    static ParseTree ANY(ParseTree p)
    {
        return named!(and!(literal!("."), Spacing), "ANY")(p);
    }

    static ParseTree Spacing(ParseTree p)
    {
        return named!(discard!(and!(zeroOrMore!(or!(and!(Space), and!(Comment))))), "Spacing")(p);
    }

    static ParseTree Comment(ParseTree p)
    {
        return named!(and!(literal!("#"), zeroOrMore!(and!(negLookahead!(EndOfLine), pegged.peg.any)), EndOfLine), "Comment")(p);
    }

    static ParseTree Space(ParseTree p)
    {
        return named!(or!(and!(literal!(" ")), and!(literal!("\t")), and!(EndOfLine)), "Space")(p);
    }

    static ParseTree EndOfLine(ParseTree p)
    {
        return named!(or!(and!(literal!("\r\n")), and!(literal!("\n")), and!(literal!("\r"))), "EndOfLine")(p);
    }

    static ParseTree EndOfInput(ParseTree p)
    {
        return named!(and!(negLookahead!(pegged.peg.any)), "EndOfInput")(p);
    }

    static ParseTree opCall(ParseTree p)
    {
        ParseTree result = decimateTree(Grammar(p));
        result.children = [result];
        result.name = "Pegged";
        return result;
    }

    static ParseTree opCall(string input)
    {
        return Pegged(ParseTree(``, false, [], input, 0, 0));
    }
}


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
