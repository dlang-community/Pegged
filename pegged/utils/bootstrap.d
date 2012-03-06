module pegged.bootstrap;

import std.algorithm: startsWith;
import std.conv;

import pegged.grammar; 

mixin Grammar!(
     "PEGrammar  <- S Definition+ EOI"
    ,"Definition <- Ident LEFTARROW Expression"
    ,"Expression <- Sequence (OR Sequence)*"
    ,"Sequence   <- Prefix*"
    ,"Prefix     <- (LOOKAHEAD / NOT)? Suffix"
    ,"Suffix     <- Primary (OPTION / ONEORMORE / ZEROORMORE)?"
    ,"Primary    <- Ident !LEFTARROW
                  / GroupExpr
                  / Literal / Class / ANY"
    ,"GroupExpr  <- :OPEN Expression :CLOSE"
    ,"Ident      <- Identifier S"
    ,"Literal    <~ :Quote (!Quote Char)* :Quote S
                  / :DoubleQuote (!DoubleQuote Char)* :DoubleQuote S"
    ,"Class      <- :'[' (!']' Range)* :']' S"
    ,"Range      <- Char :'-' Char / Char"
    ,"Char       <~ BackSlash [nrt]
                  / !BackSlash _"
    ,`LEFTARROW  <- "<-" S`
    ,`OR         <- '/' S`
    ,`LOOKAHEAD  <- '&' S`
    ,`NOT        <- '!' S`
    ,`OPTION     <- '?' S`
    ,`ZEROORMORE <- '*' S`
    ,`ONEORMORE  <- '+' S`
    ,`OPEN       <- '(' S`
    ,`CLOSE      <- ')' S`
    ,`ANY        <- '_' S`
    ,`S          <: (Blank / Comment)*`
    ,`Blank      <- ' ' / "\t" / EOL`
    ,`Comment    <~ '#'>(!EOL>_)*>(EOL/EOI)`
);

string PEGtoCode(ParseResult p, string[] names = [""])
{
    string result;
    auto ch = p.children;
    
    void recurse() 
    {
        foreach(child; ch) result ~= PEGtoCode(child);
    }
    
    switch (p.name)
    {
        case "PEGrammar":
            
            foreach(child; ch) result ~= PEGtoCode(child, names);
            break;
        case "Definition":
            if (ch.length < 3)
                return "ERROR, Bad Definition";
            string ruleNames = "    enum ruleNames = [";
            foreach(name; names)
                ruleNames ~= "\"" ~ name ~ "\":true,";
            ruleNames = ruleNames[0..$-1] ~ "];\n";
            auto code = ruleNames ~
"   static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                        p.namedCaptures,
                        ParseResult(\""~ch[0].capture[0]~"\", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                        p.namedCaptures,
                        ParseResult(\""~ch[0].capture[0]~"\", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}\n";

            result = "class " ~ ch[0].capture[0] ~ " : " ~ PEGtoCode(ch[2]) 
                   ~ " {\n" ~ code;
            break;
        case "Expression":
            if (ch.length > 1) // OR present
            {
                result ~= "Or!(";
                foreach(i,child; ch)
                    if (i%2 == 0) result ~= PEGtoCode(child) ~ ",";
                result = result[0..$-1] ~ ")";
            }
            else
                result ~= PEGtoCode(ch[0]);
            break;
        case "Sequence":
            if (ch.length > 1)
            {
                result ~= "Seq!(";
                foreach(child; ch) 
                {
                    auto temp = PEGtoCode(child);
                    if (temp.startsWith("Seq!("))
                        temp = temp[5..$-1];
                    result ~= temp ~ ",";
                }
                result = result[0..$-1] ~ ")";
            }
            else
                result ~= PEGtoCode(ch[0]);
            break;
        case "Prefix":
            if (ch.length > 1)
                switch (ch[0].name)
                {
                    case "NOT":
                        result ~= "NegLookAhead!(" ~ PEGtoCode(ch[1]) ~ ")";
                        break;
                    case "LOOKAHEAD":
                        result ~= "PosLookAhead!(" ~ PEGtoCode(ch[1]) ~ ")";
                        break;
                    default:
                        break;
                }
            else
                result ~= PEGtoCode(ch[0]);
            break;
        case "Suffix":
            if (ch.length > 1)
                switch (ch[1].name)
                {
                    case "OPTION":
                        result ~= "Option!(" ~ PEGtoCode(ch[0]) ~ ")";
                        break;
                    case "ZEROORMORE":
                        result ~= "ZeroOrMore!(" ~ PEGtoCode(ch[0]) ~ ")";
                        break;
                    case "ONEORMORE":
                        result ~= "OneOrMore!(" ~ PEGtoCode(ch[0]) ~ ")";
                        break;
                    default:
                        break;
                }
            else
                result ~= PEGtoCode(ch[0]);
            break;
        case "Primary":
            recurse();
            break;
        case "GroupExpr":
            if (ch.length == 0) return "ERROR: Empty group ()";
            auto temp = PEGtoCode(ch[0]);
            if (temp.startsWith("Seq!(")) return temp;
            result ~= "Seq!(" ~ temp ~ ")";
            break;
        case "Ident":
            result ~= p.capture[0];
            break;
        case "Literal":
            if (p.capture[0].length == 0)
                return "ERROR: empty literal";
            result ~= "Lit!(\"" ~ p.capture[0] ~ "\")";
            break;
        case "Class":
            result ~= "Or!(";
            if (ch.length == 0)
                return "ERROR: Empty Class of chars []";
            else 
            {
                foreach(child; ch)
                {
                    auto temp = PEGtoCode(child);
                    if (temp.startsWith("Or!("))
                        temp = temp[4..$-1];
                    result ~= temp ~ ",";
                }
                result = result[0..$-1] ~ ")";
            }
            break;
        case "Range":
            if (ch.length == 2)
            {
                result ~= "Range!('" ~ ch[0].capture[0] 
                            ~ "','" 
                            ~ ch[1].capture[0] ~ "')";
            }
            else
                result ~= "Char!('" ~ ch[0].capture[0] ~ "')"; 
            break;
        case "Char":
            result ~= ch[0].capture[0];
            break;
        case "LEFTARROW":
            break;
        case "OR":
            recurse();
            break;
        case "LOOKAHEAD":
            break;
        case "NOT":
            break;
        case "OPTION":
            break;
        case "ZEROORMORE":
            break;
        case "ONEORMORE":
            break;
        case "OPEN":
            break;
        case "CLOSE":
            break;
        case "ANY":
            result ~= "Any";
            break;
        case "S":
            break;
        case "Comment":
            break;
        default:
            result ~= "ERROR: Unknown name: " ~ p.name;
    }
    return result;
}

string toCode(Output grammarAsOutput)
{    
    string[] names;
    foreach(definition; grammarAsOutput.parseTree.children)
        names ~= definition.children[0].capture[0];
    return PEGtoCode(grammarAsOutput.parseTree, names);
}