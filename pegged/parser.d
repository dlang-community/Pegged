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
struct GenericPegged(TParseTree)
{
    struct Pegged
{
    static bool isRule(string s)
    {
        switch(s)
        {
            case "Grammar":
            case "Definition":
            case "Expression":
            case "Sequence":
            case "Prefix":
            case "Suffix":
            case "Primary":
            case "Identifier":
            case "GrammarName":
            case "LhsName":
            case "RhsName":
            case "ParamList":
            case "Param":
            case "DefaultParam":
            case "SingleParam":
            case "ArgList":
            case "Action":
            case "Literal":
            case "CharClass":
            case "CharRange":
            case "Char":
            case "Arrow":
            case "LEFTARROW":
            case "FUSEARROW":
            case "DISCARDARROW":
            case "SPACEARROW":
            case "OR":
            case "POS":
            case "NEG":
            case "FUSE":
            case "DISCARD":
            case "KEEP":
            case "DROP":
            case "OPTION":
            case "ZEROORMORE":
            case "ONEORMORE":
            case "ACTIONOPEN":
            case "ACTIONCLOSE":
            case "SEPARATOR":
            case "ASSIGN":
            case "NAMESEP":
            case "OPEN":
            case "CLOSE":
            case "ANY":
            case "Spacing":
            case "Comment":
            case "Space":
            case "EndOfLine":
            case "EndOfInput":
                return true;
            default:
                return false;
        }
    }
    mixin decimateTree;
    static TParseTree Grammar(TParseTree p)
    {
        return named!(and!(Spacing, GrammarName, oneOrMore!(Definition), discard!(eoi)), "Grammar")(p);
    }

    static TParseTree Grammar(string s)
    {
        return Grammar(TParseTree("", false,[], s));
    }

    static TParseTree Definition(TParseTree p)
    {
        return named!(and!(LhsName, Arrow, Expression), "Definition")(p);
    }

    static TParseTree Definition(string s)
    {
        return Definition(TParseTree("", false,[], s));
    }

    static TParseTree Expression(TParseTree p)
    {
        return named!(and!(Sequence, zeroOrMore!(and!(discard!(OR), Sequence))), "Expression")(p);
    }

    static TParseTree Expression(string s)
    {
        return Expression(TParseTree("", false,[], s));
    }

    static TParseTree Sequence(TParseTree p)
    {
        return named!(and!(oneOrMore!(Prefix)), "Sequence")(p);
    }

    static TParseTree Sequence(string s)
    {
        return Sequence(TParseTree("", false,[], s));
    }

    static TParseTree Prefix(TParseTree p)
    {
        return named!(and!(zeroOrMore!(or!(and!(POS), and!(NEG), and!(FUSE), and!(DISCARD), and!(KEEP), and!(DROP))), Suffix), "Prefix")(p);
    }

    static TParseTree Prefix(string s)
    {
        return Prefix(TParseTree("", false,[], s));
    }

    static TParseTree Suffix(TParseTree p)
    {
        return named!(and!(Primary, zeroOrMore!(or!(and!(OPTION), and!(ZEROORMORE), and!(ONEORMORE), and!(Action)))), "Suffix")(p);
    }

    static TParseTree Suffix(string s)
    {
        return Suffix(TParseTree("", false,[], s));
    }

    static TParseTree Primary(TParseTree p)
    {
        return named!(and!(negLookahead!(and!(LhsName, Arrow)), or!(and!(RhsName), and!(discard!(OPEN), Expression, discard!(CLOSE)), and!(Literal), and!(CharClass), and!(ANY))), "Primary")(p);
    }

    static TParseTree Primary(string s)
    {
        return Primary(TParseTree("", false,[], s));
    }

    static TParseTree Identifier(TParseTree p)
    {
        return named!(and!(identifier), "Identifier")(p);
    }

    static TParseTree Identifier(string s)
    {
        return Identifier(TParseTree("", false,[], s));
    }

    static TParseTree GrammarName(TParseTree p)
    {
        return named!(and!(Identifier, option!(ParamList), Spacing, discard!(literal!(":")), Spacing), "GrammarName")(p);
    }

    static TParseTree GrammarName(string s)
    {
        return GrammarName(TParseTree("", false,[], s));
    }

    static TParseTree LhsName(TParseTree p)
    {
        return named!(and!(Identifier, option!(ParamList), Spacing), "LhsName")(p);
    }

    static TParseTree LhsName(string s)
    {
        return LhsName(TParseTree("", false,[], s));
    }

    static TParseTree RhsName(TParseTree p)
    {
        return named!(and!(Identifier, option!(ArgList), zeroOrMore!(and!(NAMESEP, Identifier, option!(ArgList))), Spacing), "RhsName")(p);
    }

    static TParseTree RhsName(string s)
    {
        return RhsName(TParseTree("", false,[], s));
    }

    static TParseTree ParamList(TParseTree p)
    {
        return named!(and!(discard!(OPEN), Param, zeroOrMore!(and!(discard!(SEPARATOR), Param)), discard!(CLOSE)), "ParamList")(p);
    }

    static TParseTree ParamList(string s)
    {
        return ParamList(TParseTree("", false,[], s));
    }

    static TParseTree Param(TParseTree p)
    {
        return named!(or!(and!(DefaultParam), and!(SingleParam)), "Param")(p);
    }

    static TParseTree Param(string s)
    {
        return Param(TParseTree("", false,[], s));
    }

    static TParseTree DefaultParam(TParseTree p)
    {
        return named!(and!(Identifier, Spacing, discard!(ASSIGN), Expression), "DefaultParam")(p);
    }

    static TParseTree DefaultParam(string s)
    {
        return DefaultParam(TParseTree("", false,[], s));
    }

    static TParseTree SingleParam(TParseTree p)
    {
        return named!(and!(Identifier, Spacing), "SingleParam")(p);
    }

    static TParseTree SingleParam(string s)
    {
        return SingleParam(TParseTree("", false,[], s));
    }

    static TParseTree ArgList(TParseTree p)
    {
        return named!(and!(discard!(OPEN), Expression, zeroOrMore!(and!(discard!(SEPARATOR), Expression)), discard!(CLOSE)), "ArgList")(p);
    }

    static TParseTree ArgList(string s)
    {
        return ArgList(TParseTree("", false,[], s));
    }

    static TParseTree Action(TParseTree p)
    {
        return named!(and!(ACTIONOPEN, qualifiedIdentifier, zeroOrMore!(and!(SEPARATOR, qualifiedIdentifier)), ACTIONCLOSE), "Action")(p);
    }

    static TParseTree Action(string s)
    {
        return Action(TParseTree("", false,[], s));
    }

    static TParseTree Literal(TParseTree p)
    {
        return named!(fuse!(or!(and!(discard!(quote), zeroOrMore!(and!(negLookahead!(quote), Char)), discard!(quote), Spacing), and!(discard!(doublequote), zeroOrMore!(and!(negLookahead!(doublequote), Char)), discard!(doublequote), Spacing))), "Literal")(p);
    }

    static TParseTree Literal(string s)
    {
        return Literal(TParseTree("", false,[], s));
    }

    static TParseTree CharClass(TParseTree p)
    {
        return named!(and!(discard!(literal!("[")), zeroOrMore!(and!(negLookahead!(literal!("]")), CharRange)), discard!(literal!("]")), Spacing), "CharClass")(p);
    }

    static TParseTree CharClass(string s)
    {
        return CharClass(TParseTree("", false,[], s));
    }

    static TParseTree CharRange(TParseTree p)
    {
        return named!(or!(and!(Char, literal!("-"), Char), and!(Char)), "CharRange")(p);
    }

    static TParseTree CharRange(string s)
    {
        return CharRange(TParseTree("", false,[], s));
    }

    static TParseTree Char(TParseTree p)
    {
        return named!(fuse!(or!(and!(backslash, or!(and!(quote), and!(doublequote), and!(backquote), and!(backslash), and!(literal!("-")), and!(literal!("[")), and!(literal!("]")), and!(or!(literal!("n"), literal!("r"), literal!("t"))), and!(charRange!('0', '2'), charRange!('0', '7'), charRange!('0', '7')), and!(charRange!('0', '7'), option!(charRange!('0', '7'))), and!(literal!("x"), hexDigit, hexDigit), and!(literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), and!(literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), and!(pegged.peg.any))), "Char")(p);
    }

    static TParseTree Char(string s)
    {
        return Char(TParseTree("", false,[], s));
    }

    static TParseTree Arrow(TParseTree p)
    {
        return named!(or!(and!(LEFTARROW), and!(FUSEARROW), and!(DISCARDARROW), and!(SPACEARROW)), "Arrow")(p);
    }

    static TParseTree Arrow(string s)
    {
        return Arrow(TParseTree("", false,[], s));
    }

    static TParseTree LEFTARROW(TParseTree p)
    {
        return named!(and!(literal!("<-"), Spacing), "LEFTARROW")(p);
    }

    static TParseTree LEFTARROW(string s)
    {
        return LEFTARROW(TParseTree("", false,[], s));
    }

    static TParseTree FUSEARROW(TParseTree p)
    {
        return named!(and!(literal!("<~"), Spacing), "FUSEARROW")(p);
    }

    static TParseTree FUSEARROW(string s)
    {
        return FUSEARROW(TParseTree("", false,[], s));
    }

    static TParseTree DISCARDARROW(TParseTree p)
    {
        return named!(and!(literal!("<:"), Spacing), "DISCARDARROW")(p);
    }

    static TParseTree DISCARDARROW(string s)
    {
        return DISCARDARROW(TParseTree("", false,[], s));
    }

    static TParseTree SPACEARROW(TParseTree p)
    {
        return named!(and!(literal!("<"), Spacing), "SPACEARROW")(p);
    }

    static TParseTree SPACEARROW(string s)
    {
        return SPACEARROW(TParseTree("", false,[], s));
    }

    static TParseTree OR(TParseTree p)
    {
        return named!(and!(literal!("/"), Spacing), "OR")(p);
    }

    static TParseTree OR(string s)
    {
        return OR(TParseTree("", false,[], s));
    }

    static TParseTree POS(TParseTree p)
    {
        return named!(and!(literal!("&"), Spacing), "POS")(p);
    }

    static TParseTree POS(string s)
    {
        return POS(TParseTree("", false,[], s));
    }

    static TParseTree NEG(TParseTree p)
    {
        return named!(and!(literal!("!"), Spacing), "NEG")(p);
    }

    static TParseTree NEG(string s)
    {
        return NEG(TParseTree("", false,[], s));
    }

    static TParseTree FUSE(TParseTree p)
    {
        return named!(and!(literal!("~"), Spacing), "FUSE")(p);
    }

    static TParseTree FUSE(string s)
    {
        return FUSE(TParseTree("", false,[], s));
    }

    static TParseTree DISCARD(TParseTree p)
    {
        return named!(and!(literal!(":"), Spacing), "DISCARD")(p);
    }

    static TParseTree DISCARD(string s)
    {
        return DISCARD(TParseTree("", false,[], s));
    }

    static TParseTree KEEP(TParseTree p)
    {
        return named!(and!(literal!("^"), Spacing), "KEEP")(p);
    }

    static TParseTree KEEP(string s)
    {
        return KEEP(TParseTree("", false,[], s));
    }

    static TParseTree DROP(TParseTree p)
    {
        return named!(and!(literal!(";"), Spacing), "DROP")(p);
    }

    static TParseTree DROP(string s)
    {
        return DROP(TParseTree("", false,[], s));
    }

    static TParseTree OPTION(TParseTree p)
    {
        return named!(and!(literal!("?"), Spacing), "OPTION")(p);
    }

    static TParseTree OPTION(string s)
    {
        return OPTION(TParseTree("", false,[], s));
    }

    static TParseTree ZEROORMORE(TParseTree p)
    {
        return named!(and!(literal!("*"), Spacing), "ZEROORMORE")(p);
    }

    static TParseTree ZEROORMORE(string s)
    {
        return ZEROORMORE(TParseTree("", false,[], s));
    }

    static TParseTree ONEORMORE(TParseTree p)
    {
        return named!(and!(literal!("+"), Spacing), "ONEORMORE")(p);
    }

    static TParseTree ONEORMORE(string s)
    {
        return ONEORMORE(TParseTree("", false,[], s));
    }

    static TParseTree ACTIONOPEN(TParseTree p)
    {
        return named!(and!(literal!("{"), Spacing), "ACTIONOPEN")(p);
    }

    static TParseTree ACTIONOPEN(string s)
    {
        return ACTIONOPEN(TParseTree("", false,[], s));
    }

    static TParseTree ACTIONCLOSE(TParseTree p)
    {
        return named!(and!(literal!("}"), Spacing), "ACTIONCLOSE")(p);
    }

    static TParseTree ACTIONCLOSE(string s)
    {
        return ACTIONCLOSE(TParseTree("", false,[], s));
    }

    static TParseTree SEPARATOR(TParseTree p)
    {
        return named!(and!(literal!(","), Spacing), "SEPARATOR")(p);
    }

    static TParseTree SEPARATOR(string s)
    {
        return SEPARATOR(TParseTree("", false,[], s));
    }

    static TParseTree ASSIGN(TParseTree p)
    {
        return named!(and!(literal!("="), Spacing), "ASSIGN")(p);
    }

    static TParseTree ASSIGN(string s)
    {
        return ASSIGN(TParseTree("", false,[], s));
    }

    static TParseTree NAMESEP(TParseTree p)
    {
        return named!(and!(literal!(".")), "NAMESEP")(p);
    }

    static TParseTree NAMESEP(string s)
    {
        return NAMESEP(TParseTree("", false,[], s));
    }

    static TParseTree OPEN(TParseTree p)
    {
        return named!(and!(literal!("("), Spacing), "OPEN")(p);
    }

    static TParseTree OPEN(string s)
    {
        return OPEN(TParseTree("", false,[], s));
    }

    static TParseTree CLOSE(TParseTree p)
    {
        return named!(and!(literal!(")"), Spacing), "CLOSE")(p);
    }

    static TParseTree CLOSE(string s)
    {
        return CLOSE(TParseTree("", false,[], s));
    }

    static TParseTree ANY(TParseTree p)
    {
        return named!(and!(literal!("."), Spacing), "ANY")(p);
    }

    static TParseTree ANY(string s)
    {
        return ANY(TParseTree("", false,[], s));
    }

    static TParseTree Spacing(TParseTree p)
    {
        return named!(discard!(and!(zeroOrMore!(or!(and!(Space), and!(Comment))))), "Spacing")(p);
    }

    static TParseTree Spacing(string s)
    {
        return Spacing(TParseTree("", false,[], s));
    }

    static TParseTree Comment(TParseTree p)
    {
        return named!(and!(literal!("#"), zeroOrMore!(and!(negLookahead!(EndOfLine), pegged.peg.any)), EndOfLine), "Comment")(p);
    }

    static TParseTree Comment(string s)
    {
        return Comment(TParseTree("", false,[], s));
    }

    static TParseTree Space(TParseTree p)
    {
        return named!(or!(and!(literal!(" ")), and!(literal!("\t")), and!(EndOfLine)), "Space")(p);
    }

    static TParseTree Space(string s)
    {
        return Space(TParseTree("", false,[], s));
    }

    static TParseTree EndOfLine(TParseTree p)
    {
        return named!(or!(and!(literal!("\r\n")), and!(literal!("\n")), and!(literal!("\r"))), "EndOfLine")(p);
    }

    static TParseTree EndOfLine(string s)
    {
        return EndOfLine(TParseTree("", false,[], s));
    }

    static TParseTree EndOfInput(TParseTree p)
    {
        return named!(and!(negLookahead!(pegged.peg.any)), "EndOfInput")(p);
    }

    static TParseTree EndOfInput(string s)
    {
        return EndOfInput(TParseTree("", false,[], s));
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Grammar(p));
        result.children = [result];
        result.name = "Pegged";
        return result;
    }

    static TParseTree opCall(string input)
    {
        return Pegged(TParseTree(``, false, [], input, 0, 0));
    }
    }
}

alias GenericPegged!(ParseTree).Pegged Pegged;

