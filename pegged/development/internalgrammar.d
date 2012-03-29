module pegged.internalgrammar;

import std.algorithm : startsWith;
import std.array;
import std.conv;

public import pegged.peg;
import pegged.internalpeg;


void asModule(string moduleName, string grammarString)
{
    import std.stdio;
    auto f = File(moduleName~".d","w");
    
    f.write("/**\nThis module was automatically generated from the following grammar:\n");
    f.write(grammarString);
    f.write("*/\n");
    
    f.write("module " ~ moduleName ~ ";\n\n");
    f.write("import pegged.peg, pegged.internalpeg;\nimport std.array;\nimport std.conv;\n\n");
    f.write(grammar(grammarString));
}

string grammar(string g)
{    
    auto grammarAsOutput = PEGGED.parse(g);
    string[] names;
    foreach(definition; grammarAsOutput.children[0].children)
        if (definition.name == "Definition") 
            names ~= definition.capture[0];
    string ruleNames = "    enum ruleNames = [";
    foreach(name; names)
        ruleNames ~= "\"" ~ name ~ "\":true,";
    ruleNames = ruleNames[0..$-1] ~ "];\n";
    
    string PEGtoCode(ParseTree p)
    {
        string result;
        auto ch = p.children;
        
        switch (p.name)
        {
            case "PEGGED":
                return PEGtoCode(ch[0]);
            case "Grammar":
                bool named = ch[0].name == "GrammarName";
                string grammarName = named ? ch[0].capture[0] 
                                           : names.front;
                
                result = 
"class " ~ grammarName ~ " : Parser
{
    enum name = `"~ grammarName ~ "`;\n"
    ~ ruleNames ~ "
    mixin internalPeg!();
    Input input;
    
    static Output parse(Input _input)
    {
        return (new ."~grammarName~"()).parse_(_input);
    }
    
    Output parse_(Input _input)
    {
        input = _input;
        auto p = (new "~names.front~"()).parse();
        return Output(input.text, p.end, input.namedCaptures,
                      ParseTree(name, p.success, p.capture, p.begin, p.end, [p]));
    }
    
        
    static Output parse(string input)
    {
        return (new ."~grammarName~"()).parse_(Input(input, Pos(0,0,0), NamedCaptures.init));
    }

    static Output parse(Output input)
    {
        return (new ."~grammarName~"()).parse_(Input(input.text, input.pos, input.namedCaptures));
    }

    ParseTree[] filterChildren(ParseTree p)
    {
        ParseTree[] filteredChildren;
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
    
";
                foreach(child; named ? ch[1..$] : ch)
                    result ~= PEGtoCode(child);
                return result ~ "}\n";
            case "Definition":
                string code = "    enum name = `" ~ch[0].capture[0]~ "`;

    override ParseTree parse()
    {
        auto p = (new typeof(super)()).parse();
        return ParseTree(`"~ch[0].capture[0]~"`, p.success, p.capture, p.begin, p.end, 
                                (p.name in ruleNames) ? [p] : filterChildren(p));
    }
    
    //mixin(stringToInputMixin());
    ";

                string inheritance;
                switch(ch[1].children[0].name)
                {
                    case "LEFTARROW":
                        inheritance = PEGtoCode(ch[2]);
                        break;
                    case "FUSEARROW":
                        inheritance = "Fuse!(" ~ PEGtoCode(ch[2]) ~ ")";
                        break;
                    case "DROPARROW":
                        inheritance = "Drop!(" ~ PEGtoCode(ch[2]) ~ ")";
                        break;
                    case "ACTIONARROW":
                        inheritance = "Action!(" ~ PEGtoCode(ch[2]) ~ ", " ~ ch[1].capture[1] ~ ")";
                        break;
                    case "SPACEARROW":
                        if (ch[2].children[0].name == "Sequence")
                            inheritance = "Space" ~ PEGtoCode(ch[2]);
                        else
                            inheritance = PEGtoCode(ch[2]);
                        break;
                    default:
                        inheritance ="ERROR: Bad arrow: " ~ ch[1].name;
                        break;
                }

                return "class " 
                    ~ ch[0].capture[0] // name 
                    ~ (ch[0].capture.length == 2 ? ch[0].capture[1] : "") // parameter list
                    ~ " : " ~ inheritance // inheritance code
                    ~ "\n{\n" 
                    ~ code // inner code
                    ~ "\n}\n\n";
            case "Expression":
                if (ch.length > 1) // OR present
                {
                    result = "Or!(";
                    foreach(i,child; ch)
                        if (i%2 == 0) result ~= PEGtoCode(child) ~ ",";
                    result = result[0..$-1] ~ ")";
                }
                else // one-element Or -> dropping the Or!( )
                    result = PEGtoCode(ch[0]);
                return result;
            case "Sequence":
                if (ch.length > 1)
                {
                    result = "Seq!(";
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
                    result = PEGtoCode(ch[0]);
                return result;
            case "Prefix":
                if (ch.length > 1)
                    switch (ch[0].name)
                    {
                        case "NOT":
                            result = "NegLookAhead!(" ~ PEGtoCode(ch[1]) ~ ")";
                            break;
                        case "LOOKAHEAD":
                            result = "PosLookAhead!(" ~ PEGtoCode(ch[1]) ~ ")";
                            break;
                        case "DROP":
                            result = "Drop!(" ~ PEGtoCode(ch[1]) ~ ")";
                            break;
                        case "FUSE":
                            result = "Fuse!(" ~ PEGtoCode(ch[1]) ~ ")";
                            break;
                        default:
                            break;
                    }
                else
                    result = PEGtoCode(ch[0]);
                return result;
            case "Suffix":
                if (ch.length > 1)
                    switch (ch[1].name)
                    {
                        case "OPTION":
                            result = "Option!(" ~ PEGtoCode(ch[0]) ~ ")";
                            break;
                        case "ZEROORMORE":
                            result = "ZeroOrMore!(" ~ PEGtoCode(ch[0]) ~ ")";
                            break;
                        case "ONEORMORE":
                            result = "OneOrMore!(" ~ PEGtoCode(ch[0]) ~ ")";
                            break;
                        case "NamedExpr":
                            if (ch[1].capture.length == 2)
                                result = "Named!(" ~ PEGtoCode(ch[0]) ~ ", \"" ~ ch[1].capture[1] ~ "\")";
                            else
                                result = "PushName!(" ~ PEGtoCode(ch[0]) ~ ")";
                            break;
                        case "WithAction":
                            result = "Action!(" ~ PEGtoCode(ch[0]) ~ ", " ~ ch[1].capture[0] ~ ")";
                            break;
                        default:
                            break;
                    }
                else
                    result = PEGtoCode(ch[0]);
                return result;
            case "Primary":
                foreach(child; ch) result ~= PEGtoCode(child);
                return result;
            case "Name":
                result = p.capture[0];
                if (ch.length == 1) result ~= PEGtoCode(ch[0]);
                return result;
            case "ArgList":
                result = "!(";
                foreach(child; ch)
                    result ~= PEGtoCode(child) ~ ","; // Wow! Allow  A <- List('A'*,',') 
                result = result[0..$-1] ~ ")";
                return result;
            case "GroupExpr":
                if (ch.length == 0) return "ERROR: Empty group ()";
                auto temp = PEGtoCode(ch[0]);
                if (ch.length == 1 || temp.startsWith("Seq!(")) return temp;
                result = "Seq!(" ~ temp ~ ")";
                return result;
            case "Ident":
                return p.capture[0];
            case "Literal":
                if (p.capture[0].length == 0)
                    return "ERROR: empty literal";
                return "Lit!(\"" ~ p.capture[0] ~ "\")";
            case "Class":
                if (ch.length == 0)
                    return "ERROR: Empty Class of chars []";
                else 
                {
                    if (ch.length > 1)
                    {
                        result = "Or!(";
                        foreach(child; ch)
                        {
                            auto temp = PEGtoCode(child);
                            if (temp.startsWith("Or!("))
                                temp = temp[4..$-1];
                            result ~= temp ~ ",";
                        }
                        result = result[0..$-1] ~ ")";
                    }
                    else
                        result = PEGtoCode(ch[0]);
                }
                return result;
            case "CharRange":
                if (ch.length == 2)
                    return "Range!('" ~ PEGtoCode(ch[0]) ~ "','" ~ PEGtoCode(ch[1]) ~ "')";
                else
                    return "Lit!(\"" ~ PEGtoCode(ch[0]) ~ "\")"; 
            case "Char":
                if (p.capture.length == 2) // escape sequence \-, \[, \] 
                    return p.capture[1];
                else
                    return p.capture[0];
            case "OR":
                foreach(child; ch) result ~= PEGtoCode(child);
                return result;
            case "ANY":
                return "Any";
            default:
                return "";
        }
    }

    return PEGtoCode(grammarAsOutput.parseTree);
}

/**
This module was automatically generated from the following grammar:

PEGGED:
Grammar     <- S GrammarName? Definition+ EOI
GrammarName <- Identifier S :":" S
Definition  <- RuleName Arrow Expression S
RuleName    <- Identifier (ParamList?) S
Expression  <- Sequence (OR Sequence)*
Sequence    <- Prefix+
Prefix      <- (LOOKAHEAD / NOT / DROP / FUSE)? Suffix
Suffix      <- Primary ( OPTION 
                       / ONEORMORE 
                       / ZEROORMORE 
                       / NamedExpr 
                       / WithAction)? S
Primary     <- Name !Arrow
             / GroupExpr
             / Literal 
             / Class 
             / ANY

Name        <- QualifiedIdentifier ArgList? S
GroupExpr   <- :OPEN Expression :CLOSE S
Literal     <~ :Quote (!Quote Char)* :Quote S
             / :DoubleQuote (!DoubleQuote Char)* :DoubleQuote S
Class       <- :'[' (!']' CharRange)* :']' S
CharRange   <- Char :'-' Char / Char
Char        <- BackSlash ('-' / BackSlash / '[' / ']') # Escape sequences
             / !BackSlash .
ParamList   <~ OPEN Identifier (',' Identifier)* CLOSE S
ArgList     <- :OPEN Expression (:',' Expression)* :CLOSE S
NamedExpr   <- NAME Identifier? S
WithAction  <~ :ACTIONOPEN Identifier :ACTIONCLOSE S

Arrow       <- LEFTARROW / FUSEARROW / DROPARROW / ACTIONARROW / SPACEARROW
LEFTARROW   <- "<-" S
FUSEARROW   <- "<~" S
DROPARROW   <- "<:" S
ACTIONARROW <- "<" WithAction S
SPACEARROW  <- "<" S
  
OR          <- '/' S
    
LOOKAHEAD   <- '&' S
NOT         <- '!' S

DROP        <- ':' S
FUSE        <- '~' S
  
#SPACEMUNCH <- '>' S
    
NAME        <- '=' S
ACTIONOPEN  <- '{' S
ACTIONCLOSE <- '}' S
    
OPTION     <- '?' S
ZEROORMORE <- '*' S
ONEORMORE  <- '+' S
    
OPEN       <- '(' S
CLOSE      <- ')' S
    
ANY        <- '.' S
    
S          <: ~(Blank / EOL / Comment)*
Comment    <- "#" (!EOL .)* (EOL/EOI)
*/
class PEGGED : Parser
{
    enum name = `PEGGED`;
    enum ruleNames = ["Grammar":true,"GrammarName":true,"Definition":true,"RuleName":true,"Expression":true,"Sequence":true,"Prefix":true,"Suffix":true,"Primary":true,"Name":true,"GroupExpr":true,"Literal":true,"Class":true,"CharRange":true,"Char":true,"ParamList":true,"ArgList":true,"NamedExpr":true,"WithAction":true,"Arrow":true,"LEFTARROW":true,"FUSEARROW":true,"DROPARROW":true,"ACTIONARROW":true,"SPACEARROW":true,"OR":true,"LOOKAHEAD":true,"NOT":true,"DROP":true,"FUSE":true,"NAME":true,"ACTIONOPEN":true,"ACTIONCLOSE":true,"OPTION":true,"ZEROORMORE":true,"ONEORMORE":true,"OPEN":true,"CLOSE":true,"ANY":true,"S":true,"Comment":true];

    static Output parse(Input input)
    {
        auto p = Grammar.parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(name, p.success, p.capture, input.pos, p.pos, [p.parseTree]));
    }
    
    mixin(stringToInputMixin());

    static ParseTree[] filterChildren(ParseTree p)
    {
        ParseTree[] filteredChildren;
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
    
class Grammar : Seq!(S,Option!(GrammarName),OneOrMore!(Definition),EOI)
{
    enum name = `Grammar`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Grammar`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class GrammarName : Seq!(Identifier,S,Drop!(Lit!(":")),S)
{
    enum name = `GrammarName`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`GrammarName`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Definition : Seq!(RuleName,Arrow,Expression,S)
{
    enum name = `Definition`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Definition`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class RuleName : Seq!(Identifier,Option!(ParamList),S)
{
    enum name = `RuleName`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`RuleName`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Expression : Seq!(Sequence,ZeroOrMore!(Seq!(OR,Sequence)))
{
    enum name = `Expression`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Expression`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Sequence : OneOrMore!(Prefix)
{
    enum name = `Sequence`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Sequence`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Prefix : Seq!(Option!(Or!(LOOKAHEAD,NOT,DROP,FUSE)),Suffix)
{
    enum name = `Prefix`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Prefix`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Suffix : Seq!(Primary,Option!(Or!(OPTION,ONEORMORE,ZEROORMORE,NamedExpr,WithAction)),S)
{
    enum name = `Suffix`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Suffix`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Primary : Or!(Seq!(Name,NegLookAhead!(Arrow)),GroupExpr,Literal,Class,ANY)
{
    enum name = `Primary`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Primary`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Name : Seq!(QualifiedIdentifier,Option!(ArgList),S)
{
    enum name = `Name`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Name`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class GroupExpr : Seq!(Drop!(OPEN),Expression,Drop!(CLOSE),S)
{
    enum name = `GroupExpr`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`GroupExpr`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Literal : Fuse!(Or!(Seq!(Drop!(Quote),ZeroOrMore!(Seq!(NegLookAhead!(Quote),Char)),Drop!(Quote),S),Seq!(Drop!(DoubleQuote),ZeroOrMore!(Seq!(NegLookAhead!(DoubleQuote),Char)),Drop!(DoubleQuote),S)))
{
    enum name = `Literal`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Literal`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Class : Seq!(Drop!(Lit!("[")),ZeroOrMore!(Seq!(NegLookAhead!(Lit!("]")),CharRange)),Drop!(Lit!("]")),S)
{
    enum name = `Class`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Class`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class CharRange : Or!(Seq!(Char,Drop!(Lit!("-")),Char),Char)
{
    enum name = `CharRange`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`CharRange`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Char : Or!(Seq!(BackSlash,Or!(Lit!("-"),BackSlash,Lit!("["),Lit!("]"))),Seq!(NegLookAhead!(BackSlash),Any))
{
    enum name = `Char`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Char`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class ParamList : Fuse!(Seq!(OPEN,Identifier,ZeroOrMore!(Seq!(Lit!(","),Identifier)),CLOSE,S))
{
    enum name = `ParamList`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`ParamList`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class ArgList : Seq!(Drop!(OPEN),Expression,ZeroOrMore!(Seq!(Drop!(Lit!(",")),Expression)),Drop!(CLOSE),S)
{
    enum name = `ArgList`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`ArgList`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class NamedExpr : Seq!(NAME,Option!(Identifier),S)
{
    enum name = `NamedExpr`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`NamedExpr`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class WithAction : Fuse!(Seq!(Drop!(ACTIONOPEN),Identifier,Drop!(ACTIONCLOSE),S))
{
    enum name = `WithAction`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`WithAction`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Arrow : Or!(LEFTARROW,FUSEARROW,DROPARROW,ACTIONARROW,SPACEARROW)
{
    enum name = `Arrow`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Arrow`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class LEFTARROW : Seq!(Lit!("<-"),S)
{
    enum name = `LEFTARROW`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`LEFTARROW`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class FUSEARROW : Seq!(Lit!("<~"),S)
{
    enum name = `FUSEARROW`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`FUSEARROW`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class DROPARROW : Seq!(Lit!("<:"),S)
{
    enum name = `DROPARROW`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`DROPARROW`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class ACTIONARROW : Seq!(Lit!("<"),WithAction,S)
{
    enum name = `ACTIONARROW`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`ACTIONARROW`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class SPACEARROW : Seq!(Lit!("<"),S)
{
    enum name = `SPACEARROW`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`SPACEARROW`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class OR : Seq!(Lit!("/"),S)
{
    enum name = `OR`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`OR`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class LOOKAHEAD : Seq!(Lit!("&"),S)
{
    enum name = `LOOKAHEAD`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`LOOKAHEAD`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class NOT : Seq!(Lit!("!"),S)
{
    enum name = `NOT`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`NOT`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class DROP : Seq!(Lit!(":"),S)
{
    enum name = `DROP`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`DROP`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class FUSE : Seq!(Lit!("~"),S)
{
    enum name = `FUSE`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`FUSE`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class NAME : Seq!(Lit!("="),S)
{
    enum name = `NAME`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`NAME`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class ACTIONOPEN : Seq!(Lit!("{"),S)
{
    enum name = `ACTIONOPEN`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`ACTIONOPEN`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class ACTIONCLOSE : Seq!(Lit!("}"),S)
{
    enum name = `ACTIONCLOSE`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`ACTIONCLOSE`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class OPTION : Seq!(Lit!("?"),S)
{
    enum name = `OPTION`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`OPTION`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class ZEROORMORE : Seq!(Lit!("*"),S)
{
    enum name = `ZEROORMORE`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`ZEROORMORE`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class ONEORMORE : Seq!(Lit!("+"),S)
{
    enum name = `ONEORMORE`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`ONEORMORE`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class OPEN : Seq!(Lit!("("),S)
{
    enum name = `OPEN`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`OPEN`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class CLOSE : Seq!(Lit!(")"),S)
{
    enum name = `CLOSE`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`CLOSE`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class ANY : Seq!(Lit!("."),S)
{
    enum name = `ANY`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`ANY`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class S : Drop!(Fuse!(ZeroOrMore!(Or!(Blank,EOL,Comment))))
{
    enum name = `S`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`S`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

class Comment : Seq!(Lit!("#"),ZeroOrMore!(Seq!(NegLookAhead!(EOL),Any)),Or!(EOL,EOI))
{
    enum name = `Comment`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`Comment`, p.success, p.capture, input.pos, p.pos, 
                                (p.name in ruleNames) ? [p.parseTree] : filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
    
}

}
