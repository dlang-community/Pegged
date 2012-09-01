/**
This module was automatically generated from the following grammar:


# This is the PEG extended grammar used by Pegged
Pegged:

# Syntactic rules:
Grammar      <- Spacing GrammarName Definition+ :eoi
Definition   <- LhsName Arrow Expression
Expression   <- Sequence (:OR Sequence)*
Sequence     <- Prefix+
Prefix       <- (POS / NEG / FUSE / DISCARD / KEEP / DROP)* Suffix
Suffix       <- Primary (OPTION / ZEROORMORE / ONEORMORE / Action)*
Primary      <- !(LhsName Arrow) 
                ( RhsName 
                / :OPEN Expression :CLOSE 
                / Literal 
                / CharClass 
                / ANY)
# Lexical syntax
Identifier   <- identifier
GrammarName  <- Identifier ParamList? Spacing :':' Spacing
LhsName      <- Identifier ParamList? Spacing
RhsName      <- Identifier ArgList? (NAMESEP Identifier ArgList?)* Spacing         # NAMESEP is *not* discarded
ParamList    <- :OPEN Param (:SEPARATOR Param)*  :CLOSE
Param        <- DefaultParam / SingleParam
DefaultParam <- Identifier Spacing :ASSIGN Expression
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
module pegged.parser;

public import pegged.peg;
struct Pegged
{
    import std.typecons:Tuple, tuple;
    static ParseTree[Tuple!(string, uint)] memo;
    static bool[string] names;
    static this()
    {
        names = [`Grammar`:true, `Definition`:true, `Expression`:true, `Sequence`:true, 
                 `Prefix`:true, `Suffix`:true, `Primary`:true, `Identifier`:true, 
                 `GrammarName`:true, `LhsName`:true, `RhsName`:true, `ParamList`:true, 
                 `Param`:true, `DefaultParam`:true, `SingleParam`:true, `ArgList`:true, 
                 `Action`:true, `Literal`:true, `CharClass`:true, `CharRange`:true, 
                 `Char`:true, `Arrow`:true, `LEFTARROW`:true, `FUSEARROW`:true, 
                 `DISCARDARROW`:true, `SPACEARROW`:true, `OR`:true, `POS`:true, 
                 `NEG`:true, `FUSE`:true, `DISCARD`:true, `KEEP`:true, 
                 `DROP`:true, `OPTION`:true, `ZEROORMORE`:true, `ONEORMORE`:true, 
                 `ACTIONOPEN`:true, `ACTIONCLOSE`:true, `SEPARATOR`:true, `ASSIGN`:true, 
                 `NAMESEP`:true, `OPEN`:true, `CLOSE`:true, `ANY`:true, 
                 `Spacing`:true, `Comment`:true, `Space`:true, `EndOfLine`:true, 
                 `EndOfInput`:true];
    }
    mixin decimateTree;
    static ParseTree Grammar(ParseTree p)
    {
        if(auto m = tuple("Grammar",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Spacing, GrammarName, oneOrMore!(Definition), discard!(eoi)), "Grammar")(p);
            memo[tuple("Grammar",p.end)] = result;
            return result;
        }
    }

    static ParseTree Grammar(string s)
    {
        memo = null;
        return Grammar(ParseTree("", false,[], s));
    }

    static ParseTree Definition(ParseTree p)
    {
        if(auto m = tuple("Definition",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(LhsName, Arrow, Expression), "Definition")(p);
            memo[tuple("Definition",p.end)] = result;
            return result;
        }
    }

    static ParseTree Definition(string s)
    {
        memo = null;
        return Definition(ParseTree("", false,[], s));
    }

    static ParseTree Expression(ParseTree p)
    {
        if(auto m = tuple("Expression",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Sequence, zeroOrMore!(and!(discard!(OR), Sequence))), "Expression")(p);
            memo[tuple("Expression",p.end)] = result;
            return result;
        }
    }

    static ParseTree Expression(string s)
    {
        memo = null;
        return Expression(ParseTree("", false,[], s));
    }

    static ParseTree Sequence(ParseTree p)
    {
        if(auto m = tuple("Sequence",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(oneOrMore!(Prefix)), "Sequence")(p);
            memo[tuple("Sequence",p.end)] = result;
            return result;
        }
    }

    static ParseTree Sequence(string s)
    {
        memo = null;
        return Sequence(ParseTree("", false,[], s));
    }

    static ParseTree Prefix(ParseTree p)
    {
        if(auto m = tuple("Prefix",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(zeroOrMore!(or!(and!(POS), and!(NEG), and!(FUSE), and!(DISCARD), and!(KEEP), and!(DROP))), Suffix), "Prefix")(p);
            memo[tuple("Prefix",p.end)] = result;
            return result;
        }
    }

    static ParseTree Prefix(string s)
    {
        memo = null;
        return Prefix(ParseTree("", false,[], s));
    }

    static ParseTree Suffix(ParseTree p)
    {
        if(auto m = tuple("Suffix",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Primary, zeroOrMore!(or!(and!(OPTION), and!(ZEROORMORE), and!(ONEORMORE), and!(Action)))), "Suffix")(p);
            memo[tuple("Suffix",p.end)] = result;
            return result;
        }
    }

    static ParseTree Suffix(string s)
    {
        memo = null;
        return Suffix(ParseTree("", false,[], s));
    }

    static ParseTree Primary(ParseTree p)
    {
        if(auto m = tuple("Primary",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(negLookahead!(and!(LhsName, Arrow)), or!(and!(RhsName), and!(discard!(OPEN), Expression, discard!(CLOSE)), and!(Literal), and!(CharClass), and!(ANY))), "Primary")(p);
            memo[tuple("Primary",p.end)] = result;
            return result;
        }
    }

    static ParseTree Primary(string s)
    {
        memo = null;
        return Primary(ParseTree("", false,[], s));
    }

    static ParseTree Identifier(ParseTree p)
    {
        if(auto m = tuple("Identifier",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(identifier), "Identifier")(p);
            memo[tuple("Identifier",p.end)] = result;
            return result;
        }
    }

    static ParseTree Identifier(string s)
    {
        memo = null;
        return Identifier(ParseTree("", false,[], s));
    }

    static ParseTree GrammarName(ParseTree p)
    {
        if(auto m = tuple("GrammarName",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Identifier, option!(ParamList), Spacing, discard!(literal!(":")), Spacing), "GrammarName")(p);
            memo[tuple("GrammarName",p.end)] = result;
            return result;
        }
    }

    static ParseTree GrammarName(string s)
    {
        memo = null;
        return GrammarName(ParseTree("", false,[], s));
    }

    static ParseTree LhsName(ParseTree p)
    {
        if(auto m = tuple("LhsName",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Identifier, option!(ParamList), Spacing), "LhsName")(p);
            memo[tuple("LhsName",p.end)] = result;
            return result;
        }
    }

    static ParseTree LhsName(string s)
    {
        memo = null;
        return LhsName(ParseTree("", false,[], s));
    }

    static ParseTree RhsName(ParseTree p)
    {
        if(auto m = tuple("RhsName",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Identifier, option!(ArgList), zeroOrMore!(and!(NAMESEP, Identifier, option!(ArgList))), Spacing), "RhsName")(p);
            memo[tuple("RhsName",p.end)] = result;
            return result;
        }
    }

    static ParseTree RhsName(string s)
    {
        memo = null;
        return RhsName(ParseTree("", false,[], s));
    }

    static ParseTree ParamList(ParseTree p)
    {
        if(auto m = tuple("ParamList",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(discard!(OPEN), Param, zeroOrMore!(and!(discard!(SEPARATOR), Param)), discard!(CLOSE)), "ParamList")(p);
            memo[tuple("ParamList",p.end)] = result;
            return result;
        }
    }

    static ParseTree ParamList(string s)
    {
        memo = null;
        return ParamList(ParseTree("", false,[], s));
    }

    static ParseTree Param(ParseTree p)
    {
        if(auto m = tuple("Param",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(DefaultParam), and!(SingleParam)), "Param")(p);
            memo[tuple("Param",p.end)] = result;
            return result;
        }
    }

    static ParseTree Param(string s)
    {
        memo = null;
        return Param(ParseTree("", false,[], s));
    }

    static ParseTree DefaultParam(ParseTree p)
    {
        if(auto m = tuple("DefaultParam",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Identifier, Spacing, discard!(ASSIGN), Expression), "DefaultParam")(p);
            memo[tuple("DefaultParam",p.end)] = result;
            return result;
        }
    }

    static ParseTree DefaultParam(string s)
    {
        memo = null;
        return DefaultParam(ParseTree("", false,[], s));
    }

    static ParseTree SingleParam(ParseTree p)
    {
        if(auto m = tuple("SingleParam",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Identifier, Spacing), "SingleParam")(p);
            memo[tuple("SingleParam",p.end)] = result;
            return result;
        }
    }

    static ParseTree SingleParam(string s)
    {
        memo = null;
        return SingleParam(ParseTree("", false,[], s));
    }

    static ParseTree ArgList(ParseTree p)
    {
        if(auto m = tuple("ArgList",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(discard!(OPEN), Expression, zeroOrMore!(and!(discard!(SEPARATOR), Expression)), discard!(CLOSE)), "ArgList")(p);
            memo[tuple("ArgList",p.end)] = result;
            return result;
        }
    }

    static ParseTree ArgList(string s)
    {
        memo = null;
        return ArgList(ParseTree("", false,[], s));
    }

    static ParseTree Action(ParseTree p)
    {
        if(auto m = tuple("Action",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(ACTIONOPEN, qualifiedIdentifier, zeroOrMore!(and!(SEPARATOR, qualifiedIdentifier)), ACTIONCLOSE), "Action")(p);
            memo[tuple("Action",p.end)] = result;
            return result;
        }
    }

    static ParseTree Action(string s)
    {
        memo = null;
        return Action(ParseTree("", false,[], s));
    }

    static ParseTree Literal(ParseTree p)
    {
        if(auto m = tuple("Literal",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(fuse!(or!(and!(discard!(quote), zeroOrMore!(and!(negLookahead!(quote), Char)), discard!(quote), Spacing), and!(discard!(doublequote), zeroOrMore!(and!(negLookahead!(doublequote), Char)), discard!(doublequote), Spacing))), "Literal")(p);
            memo[tuple("Literal",p.end)] = result;
            return result;
        }
    }

    static ParseTree Literal(string s)
    {
        memo = null;
        return Literal(ParseTree("", false,[], s));
    }

    static ParseTree CharClass(ParseTree p)
    {
        if(auto m = tuple("CharClass",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(discard!(literal!("[")), zeroOrMore!(and!(negLookahead!(literal!("]")), CharRange)), discard!(literal!("]")), Spacing), "CharClass")(p);
            memo[tuple("CharClass",p.end)] = result;
            return result;
        }
    }

    static ParseTree CharClass(string s)
    {
        memo = null;
        return CharClass(ParseTree("", false,[], s));
    }

    static ParseTree CharRange(ParseTree p)
    {
        if(auto m = tuple("CharRange",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(Char, literal!("-"), Char), and!(Char)), "CharRange")(p);
            memo[tuple("CharRange",p.end)] = result;
            return result;
        }
    }

    static ParseTree CharRange(string s)
    {
        memo = null;
        return CharRange(ParseTree("", false,[], s));
    }

    static ParseTree Char(ParseTree p)
    {
        if(auto m = tuple("Char",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(fuse!(or!(and!(backslash, or!(and!(quote), and!(doublequote), and!(backquote), and!(backslash), and!(literal!("-")), and!(literal!("[")), and!(literal!("]")), and!(or!(literal!("n"), literal!("r"), literal!("t"))), and!(charRange!('0', '2'), charRange!('0', '7'), charRange!('0', '7')), and!(charRange!('0', '7'), option!(charRange!('0', '7'))), and!(literal!("x"), hexDigit, hexDigit), and!(literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), and!(literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), and!(pegged.peg.any))), "Char")(p);
            memo[tuple("Char",p.end)] = result;
            return result;
        }
    }

    static ParseTree Char(string s)
    {
        memo = null;
        return Char(ParseTree("", false,[], s));
    }

    static ParseTree Arrow(ParseTree p)
    {
        if(auto m = tuple("Arrow",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(LEFTARROW), and!(FUSEARROW), and!(DISCARDARROW), and!(SPACEARROW)), "Arrow")(p);
            memo[tuple("Arrow",p.end)] = result;
            return result;
        }
    }

    static ParseTree Arrow(string s)
    {
        memo = null;
        return Arrow(ParseTree("", false,[], s));
    }

    static ParseTree LEFTARROW(ParseTree p)
    {
        if(auto m = tuple("LEFTARROW",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("<-"), Spacing), "LEFTARROW")(p);
            memo[tuple("LEFTARROW",p.end)] = result;
            return result;
        }
    }

    static ParseTree LEFTARROW(string s)
    {
        memo = null;
        return LEFTARROW(ParseTree("", false,[], s));
    }

    static ParseTree FUSEARROW(ParseTree p)
    {
        if(auto m = tuple("FUSEARROW",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("<~"), Spacing), "FUSEARROW")(p);
            memo[tuple("FUSEARROW",p.end)] = result;
            return result;
        }
    }

    static ParseTree FUSEARROW(string s)
    {
        memo = null;
        return FUSEARROW(ParseTree("", false,[], s));
    }

    static ParseTree DISCARDARROW(ParseTree p)
    {
        if(auto m = tuple("DISCARDARROW",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("<:"), Spacing), "DISCARDARROW")(p);
            memo[tuple("DISCARDARROW",p.end)] = result;
            return result;
        }
    }

    static ParseTree DISCARDARROW(string s)
    {
        memo = null;
        return DISCARDARROW(ParseTree("", false,[], s));
    }

    static ParseTree SPACEARROW(ParseTree p)
    {
        if(auto m = tuple("SPACEARROW",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("<"), Spacing), "SPACEARROW")(p);
            memo[tuple("SPACEARROW",p.end)] = result;
            return result;
        }
    }

    static ParseTree SPACEARROW(string s)
    {
        memo = null;
        return SPACEARROW(ParseTree("", false,[], s));
    }

    static ParseTree OR(ParseTree p)
    {
        if(auto m = tuple("OR",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("/"), Spacing), "OR")(p);
            memo[tuple("OR",p.end)] = result;
            return result;
        }
    }

    static ParseTree OR(string s)
    {
        memo = null;
        return OR(ParseTree("", false,[], s));
    }

    static ParseTree POS(ParseTree p)
    {
        if(auto m = tuple("POS",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("&"), Spacing), "POS")(p);
            memo[tuple("POS",p.end)] = result;
            return result;
        }
    }

    static ParseTree POS(string s)
    {
        memo = null;
        return POS(ParseTree("", false,[], s));
    }

    static ParseTree NEG(ParseTree p)
    {
        if(auto m = tuple("NEG",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("!"), Spacing), "NEG")(p);
            memo[tuple("NEG",p.end)] = result;
            return result;
        }
    }

    static ParseTree NEG(string s)
    {
        memo = null;
        return NEG(ParseTree("", false,[], s));
    }

    static ParseTree FUSE(ParseTree p)
    {
        if(auto m = tuple("FUSE",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("~"), Spacing), "FUSE")(p);
            memo[tuple("FUSE",p.end)] = result;
            return result;
        }
    }

    static ParseTree FUSE(string s)
    {
        memo = null;
        return FUSE(ParseTree("", false,[], s));
    }

    static ParseTree DISCARD(ParseTree p)
    {
        if(auto m = tuple("DISCARD",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!(":"), Spacing), "DISCARD")(p);
            memo[tuple("DISCARD",p.end)] = result;
            return result;
        }
    }

    static ParseTree DISCARD(string s)
    {
        memo = null;
        return DISCARD(ParseTree("", false,[], s));
    }

    static ParseTree KEEP(ParseTree p)
    {
        if(auto m = tuple("KEEP",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("^"), Spacing), "KEEP")(p);
            memo[tuple("KEEP",p.end)] = result;
            return result;
        }
    }

    static ParseTree KEEP(string s)
    {
        memo = null;
        return KEEP(ParseTree("", false,[], s));
    }

    static ParseTree DROP(ParseTree p)
    {
        if(auto m = tuple("DROP",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!(";"), Spacing), "DROP")(p);
            memo[tuple("DROP",p.end)] = result;
            return result;
        }
    }

    static ParseTree DROP(string s)
    {
        memo = null;
        return DROP(ParseTree("", false,[], s));
    }

    static ParseTree OPTION(ParseTree p)
    {
        if(auto m = tuple("OPTION",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("?"), Spacing), "OPTION")(p);
            memo[tuple("OPTION",p.end)] = result;
            return result;
        }
    }

    static ParseTree OPTION(string s)
    {
        memo = null;
        return OPTION(ParseTree("", false,[], s));
    }

    static ParseTree ZEROORMORE(ParseTree p)
    {
        if(auto m = tuple("ZEROORMORE",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("*"), Spacing), "ZEROORMORE")(p);
            memo[tuple("ZEROORMORE",p.end)] = result;
            return result;
        }
    }

    static ParseTree ZEROORMORE(string s)
    {
        memo = null;
        return ZEROORMORE(ParseTree("", false,[], s));
    }

    static ParseTree ONEORMORE(ParseTree p)
    {
        if(auto m = tuple("ONEORMORE",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("+"), Spacing), "ONEORMORE")(p);
            memo[tuple("ONEORMORE",p.end)] = result;
            return result;
        }
    }

    static ParseTree ONEORMORE(string s)
    {
        memo = null;
        return ONEORMORE(ParseTree("", false,[], s));
    }

    static ParseTree ACTIONOPEN(ParseTree p)
    {
        if(auto m = tuple("ACTIONOPEN",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("{"), Spacing), "ACTIONOPEN")(p);
            memo[tuple("ACTIONOPEN",p.end)] = result;
            return result;
        }
    }

    static ParseTree ACTIONOPEN(string s)
    {
        memo = null;
        return ACTIONOPEN(ParseTree("", false,[], s));
    }

    static ParseTree ACTIONCLOSE(ParseTree p)
    {
        if(auto m = tuple("ACTIONCLOSE",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("}"), Spacing), "ACTIONCLOSE")(p);
            memo[tuple("ACTIONCLOSE",p.end)] = result;
            return result;
        }
    }

    static ParseTree ACTIONCLOSE(string s)
    {
        memo = null;
        return ACTIONCLOSE(ParseTree("", false,[], s));
    }

    static ParseTree SEPARATOR(ParseTree p)
    {
        if(auto m = tuple("SEPARATOR",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!(","), Spacing), "SEPARATOR")(p);
            memo[tuple("SEPARATOR",p.end)] = result;
            return result;
        }
    }

    static ParseTree SEPARATOR(string s)
    {
        memo = null;
        return SEPARATOR(ParseTree("", false,[], s));
    }

    static ParseTree ASSIGN(ParseTree p)
    {
        if(auto m = tuple("ASSIGN",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("="), Spacing), "ASSIGN")(p);
            memo[tuple("ASSIGN",p.end)] = result;
            return result;
        }
    }

    static ParseTree ASSIGN(string s)
    {
        memo = null;
        return ASSIGN(ParseTree("", false,[], s));
    }

    static ParseTree NAMESEP(ParseTree p)
    {
        if(auto m = tuple("NAMESEP",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!(".")), "NAMESEP")(p);
            memo[tuple("NAMESEP",p.end)] = result;
            return result;
        }
    }

    static ParseTree NAMESEP(string s)
    {
        memo = null;
        return NAMESEP(ParseTree("", false,[], s));
    }

    static ParseTree OPEN(ParseTree p)
    {
        if(auto m = tuple("OPEN",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("("), Spacing), "OPEN")(p);
            memo[tuple("OPEN",p.end)] = result;
            return result;
        }
    }

    static ParseTree OPEN(string s)
    {
        memo = null;
        return OPEN(ParseTree("", false,[], s));
    }

    static ParseTree CLOSE(ParseTree p)
    {
        if(auto m = tuple("CLOSE",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!(")"), Spacing), "CLOSE")(p);
            memo[tuple("CLOSE",p.end)] = result;
            return result;
        }
    }

    static ParseTree CLOSE(string s)
    {
        memo = null;
        return CLOSE(ParseTree("", false,[], s));
    }

    static ParseTree ANY(ParseTree p)
    {
        if(auto m = tuple("ANY",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("."), Spacing), "ANY")(p);
            memo[tuple("ANY",p.end)] = result;
            return result;
        }
    }

    static ParseTree ANY(string s)
    {
        memo = null;
        return ANY(ParseTree("", false,[], s));
    }

    static ParseTree Spacing(ParseTree p)
    {
        if(auto m = tuple("Spacing",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(discard!(and!(zeroOrMore!(or!(and!(Space), and!(Comment))))), "Spacing")(p);
            memo[tuple("Spacing",p.end)] = result;
            return result;
        }
    }

    static ParseTree Spacing(string s)
    {
        memo = null;
        return Spacing(ParseTree("", false,[], s));
    }

    static ParseTree Comment(ParseTree p)
    {
        if(auto m = tuple("Comment",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("#"), zeroOrMore!(and!(negLookahead!(EndOfLine), pegged.peg.any)), EndOfLine), "Comment")(p);
            memo[tuple("Comment",p.end)] = result;
            return result;
        }
    }

    static ParseTree Comment(string s)
    {
        memo = null;
        return Comment(ParseTree("", false,[], s));
    }

    static ParseTree Space(ParseTree p)
    {
        if(auto m = tuple("Space",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!(" ")), and!(literal!("\t")), and!(EndOfLine)), "Space")(p);
            memo[tuple("Space",p.end)] = result;
            return result;
        }
    }

    static ParseTree Space(string s)
    {
        memo = null;
        return Space(ParseTree("", false,[], s));
    }

    static ParseTree EndOfLine(ParseTree p)
    {
        if(auto m = tuple("EndOfLine",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("\r\n")), and!(literal!("\n")), and!(literal!("\r"))), "EndOfLine")(p);
            memo[tuple("EndOfLine",p.end)] = result;
            return result;
        }
    }

    static ParseTree EndOfLine(string s)
    {
        memo = null;
        return EndOfLine(ParseTree("", false,[], s));
    }

    static ParseTree EndOfInput(ParseTree p)
    {
        if(auto m = tuple("EndOfInput",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(negLookahead!(pegged.peg.any)), "EndOfInput")(p);
            memo[tuple("EndOfInput",p.end)] = result;
            return result;
        }
    }

    static ParseTree EndOfInput(string s)
    {
        memo = null;
        return EndOfInput(ParseTree("", false,[], s));
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
        memo = null;
        return Pegged(ParseTree(``, false, [], input, 0, 0));
    }
}
