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
Arrow        <- LEFTARROW / FUSEARROW / DISCARDARROW / KEEPARROW / SPACEARROW
LEFTARROW    <- '<-' Spacing
FUSEARROW    <- '<~' Spacing
DISCARDARROW <- '<:' Spacing
KEEPARROW    <- '<^' Spacing
SPACEARROW   <- '< ' Spacing
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
    enum name = "Pegged";
    static bool isRule(string s)
    {
        switch(s)
        {
            case "Pegged.Grammar":
            case "Pegged.Definition":
            case "Pegged.Expression":
            case "Pegged.Sequence":
            case "Pegged.Prefix":
            case "Pegged.Suffix":
            case "Pegged.Primary":
            case "Pegged.Identifier":
            case "Pegged.GrammarName":
            case "Pegged.LhsName":
            case "Pegged.RhsName":
            case "Pegged.ParamList":
            case "Pegged.Param":
            case "Pegged.DefaultParam":
            case "Pegged.SingleParam":
            case "Pegged.ArgList":
            case "Pegged.Action":
            case "Pegged.Literal":
            case "Pegged.CharClass":
            case "Pegged.CharRange":
            case "Pegged.Char":
            case "Pegged.Arrow":
            case "Pegged.LEFTARROW":
            case "Pegged.FUSEARROW":
            case "Pegged.DISCARDARROW":
            case "Pegged.KEEPARROW":
            case "Pegged.SPACEARROW":
            case "Pegged.OR":
            case "Pegged.POS":
            case "Pegged.NEG":
            case "Pegged.FUSE":
            case "Pegged.DISCARD":
            case "Pegged.KEEP":
            case "Pegged.DROP":
            case "Pegged.OPTION":
            case "Pegged.ZEROORMORE":
            case "Pegged.ONEORMORE":
            case "Pegged.ACTIONOPEN":
            case "Pegged.ACTIONCLOSE":
            case "Pegged.SEPARATOR":
            case "Pegged.ASSIGN":
            case "Pegged.NAMESEP":
            case "Pegged.OPEN":
            case "Pegged.CLOSE":
            case "Pegged.ANY":
            case "Pegged.Spacing":
            case "Pegged.Comment":
            case "Pegged.Space":
            case "Pegged.EndOfLine":
            case "Pegged.EndOfInput":
                return true;
            default:
                return false;
        }
    }
    mixin decimateTree;
    static TParseTree Grammar(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), name ~ `.`~ `Grammar`)(p);
    }

    static TParseTree Grammar(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), name ~ `.`~ `Grammar`)(TParseTree("", false,[], s));
    }

    static string Grammar(GetName g)
    {
        return name ~ `.`~ `Grammar`;
    }

    static TParseTree Definition(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(LhsName, Arrow, Expression), name ~ `.`~ `Definition`)(p);
    }

    static TParseTree Definition(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(LhsName, Arrow, Expression), name ~ `.`~ `Definition`)(TParseTree("", false,[], s));
    }

    static string Definition(GetName g)
    {
        return name ~ `.`~ `Definition`;
    }

    static TParseTree Expression(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), name ~ `.`~ `Expression`)(p);
    }

    static TParseTree Expression(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), name ~ `.`~ `Expression`)(TParseTree("", false,[], s));
    }

    static string Expression(GetName g)
    {
        return name ~ `.`~ `Expression`;
    }

    static TParseTree Sequence(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.oneOrMore!(Prefix), name ~ `.`~ `Sequence`)(p);
    }

    static TParseTree Sequence(string s)
    {
        return pegged.peg.named!(pegged.peg.oneOrMore!(Prefix), name ~ `.`~ `Sequence`)(TParseTree("", false,[], s));
    }

    static string Sequence(GetName g)
    {
        return name ~ `.`~ `Sequence`;
    }

    static TParseTree Prefix(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP)), Suffix), name ~ `.`~ `Prefix`)(p);
    }

    static TParseTree Prefix(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP)), Suffix), name ~ `.`~ `Prefix`)(TParseTree("", false,[], s));
    }

    static string Prefix(GetName g)
    {
        return name ~ `.`~ `Prefix`;
    }

    static TParseTree Suffix(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), name ~ `.`~ `Suffix`)(p);
    }

    static TParseTree Suffix(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), name ~ `.`~ `Suffix`)(TParseTree("", false,[], s));
    }

    static string Suffix(GetName g)
    {
        return name ~ `.`~ `Suffix`;
    }

    static TParseTree Primary(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CharClass, ANY)), name ~ `.`~ `Primary`)(p);
    }

    static TParseTree Primary(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CharClass, ANY)), name ~ `.`~ `Primary`)(TParseTree("", false,[], s));
    }

    static string Primary(GetName g)
    {
        return name ~ `.`~ `Primary`;
    }

    static TParseTree Identifier(TParseTree p)
    {
        return pegged.peg.named!(identifier, name ~ `.`~ `Identifier`)(p);
    }

    static TParseTree Identifier(string s)
    {
        return pegged.peg.named!(identifier, name ~ `.`~ `Identifier`)(TParseTree("", false,[], s));
    }

    static string Identifier(GetName g)
    {
        return name ~ `.`~ `Identifier`;
    }

    static TParseTree GrammarName(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(":")), Spacing), name ~ `.`~ `GrammarName`)(p);
    }

    static TParseTree GrammarName(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(":")), Spacing), name ~ `.`~ `GrammarName`)(TParseTree("", false,[], s));
    }

    static string GrammarName(GetName g)
    {
        return name ~ `.`~ `GrammarName`;
    }

    static TParseTree LhsName(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), name ~ `.`~ `LhsName`)(p);
    }

    static TParseTree LhsName(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), name ~ `.`~ `LhsName`)(TParseTree("", false,[], s));
    }

    static string LhsName(GetName g)
    {
        return name ~ `.`~ `LhsName`;
    }

    static TParseTree RhsName(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), name ~ `.`~ `RhsName`)(p);
    }

    static TParseTree RhsName(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), name ~ `.`~ `RhsName`)(TParseTree("", false,[], s));
    }

    static string RhsName(GetName g)
    {
        return name ~ `.`~ `RhsName`;
    }

    static TParseTree ParamList(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ParamList`)(p);
    }

    static TParseTree ParamList(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ParamList`)(TParseTree("", false,[], s));
    }

    static string ParamList(GetName g)
    {
        return name ~ `.`~ `ParamList`;
    }

    static TParseTree Param(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.or!(DefaultParam, SingleParam), name ~ `.`~ `Param`)(p);
    }

    static TParseTree Param(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(DefaultParam, SingleParam), name ~ `.`~ `Param`)(TParseTree("", false,[], s));
    }

    static string Param(GetName g)
    {
        return name ~ `.`~ `Param`;
    }

    static TParseTree DefaultParam(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), name ~ `.`~ `DefaultParam`)(p);
    }

    static TParseTree DefaultParam(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), name ~ `.`~ `DefaultParam`)(TParseTree("", false,[], s));
    }

    static string DefaultParam(GetName g)
    {
        return name ~ `.`~ `DefaultParam`;
    }

    static TParseTree SingleParam(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing), name ~ `.`~ `SingleParam`)(p);
    }

    static TParseTree SingleParam(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing), name ~ `.`~ `SingleParam`)(TParseTree("", false,[], s));
    }

    static string SingleParam(GetName g)
    {
        return name ~ `.`~ `SingleParam`;
    }

    static TParseTree ArgList(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ArgList`)(p);
    }

    static TParseTree ArgList(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ArgList`)(TParseTree("", false,[], s));
    }

    static string ArgList(GetName g)
    {
        return name ~ `.`~ `ArgList`;
    }

    static TParseTree Action(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(ACTIONOPEN, qualifiedIdentifier, pegged.peg.zeroOrMore!(pegged.peg.and!(SEPARATOR, qualifiedIdentifier)), ACTIONCLOSE), name ~ `.`~ `Action`)(p);
    }

    static TParseTree Action(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(ACTIONOPEN, qualifiedIdentifier, pegged.peg.zeroOrMore!(pegged.peg.and!(SEPARATOR, qualifiedIdentifier)), ACTIONCLOSE), name ~ `.`~ `Action`)(TParseTree("", false,[], s));
    }

    static string Action(GetName g)
    {
        return name ~ `.`~ `Action`;
    }

    static TParseTree Literal(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(quote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char)), pegged.peg.discard!(quote), Spacing), pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), pegged.peg.discard!(doublequote), Spacing))), name ~ `.`~ `Literal`)(p);
    }

    static TParseTree Literal(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(quote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char)), pegged.peg.discard!(quote), Spacing), pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), pegged.peg.discard!(doublequote), Spacing))), name ~ `.`~ `Literal`)(TParseTree("", false,[], s));
    }

    static string Literal(GetName g)
    {
        return name ~ `.`~ `Literal`;
    }

    static TParseTree CharClass(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), CharRange)), pegged.peg.discard!(pegged.peg.literal!("]")), Spacing), name ~ `.`~ `CharClass`)(p);
    }

    static TParseTree CharClass(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), CharRange)), pegged.peg.discard!(pegged.peg.literal!("]")), Spacing), name ~ `.`~ `CharClass`)(TParseTree("", false,[], s));
    }

    static string CharClass(GetName g)
    {
        return name ~ `.`~ `CharClass`;
    }

    static TParseTree CharRange(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!("-"), Char), Char), name ~ `.`~ `CharRange`)(p);
    }

    static TParseTree CharRange(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!("-"), Char), Char), name ~ `.`~ `CharRange`)(TParseTree("", false,[], s));
    }

    static string CharRange(GetName g)
    {
        return name ~ `.`~ `CharRange`;
    }

    static TParseTree Char(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), name ~ `.`~ `Char`)(p);
    }

    static TParseTree Char(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), name ~ `.`~ `Char`)(TParseTree("", false,[], s));
    }

    static string Char(GetName g)
    {
        return name ~ `.`~ `Char`;
    }

    static TParseTree Arrow(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, SPACEARROW), name ~ `.`~ `Arrow`)(p);
    }

    static TParseTree Arrow(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, SPACEARROW), name ~ `.`~ `Arrow`)(TParseTree("", false,[], s));
    }

    static string Arrow(GetName g)
    {
        return name ~ `.`~ `Arrow`;
    }

    static TParseTree LEFTARROW(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<-"), Spacing), name ~ `.`~ `LEFTARROW`)(p);
    }

    static TParseTree LEFTARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<-"), Spacing), name ~ `.`~ `LEFTARROW`)(TParseTree("", false,[], s));
    }

    static string LEFTARROW(GetName g)
    {
        return name ~ `.`~ `LEFTARROW`;
    }

    static TParseTree FUSEARROW(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<~"), Spacing), name ~ `.`~ `FUSEARROW`)(p);
    }

    static TParseTree FUSEARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<~"), Spacing), name ~ `.`~ `FUSEARROW`)(TParseTree("", false,[], s));
    }

    static string FUSEARROW(GetName g)
    {
        return name ~ `.`~ `FUSEARROW`;
    }

    static TParseTree DISCARDARROW(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<:"), Spacing), name ~ `.`~ `DISCARDARROW`)(p);
    }

    static TParseTree DISCARDARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<:"), Spacing), name ~ `.`~ `DISCARDARROW`)(TParseTree("", false,[], s));
    }

    static string DISCARDARROW(GetName g)
    {
        return name ~ `.`~ `DISCARDARROW`;
    }

    static TParseTree KEEPARROW(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<^"), Spacing), name ~ `.`~ `KEEPARROW`)(p);
    }

    static TParseTree KEEPARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("<^"), Spacing), name ~ `.`~ `KEEPARROW`)(TParseTree("", false,[], s));
    }

    static string KEEPARROW(GetName g)
    {
        return name ~ `.`~ `KEEPARROW`;
    }

    static TParseTree SPACEARROW(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("< "), Spacing), name ~ `.`~ `SPACEARROW`)(p);
    }

    static TParseTree SPACEARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("< "), Spacing), name ~ `.`~ `SPACEARROW`)(TParseTree("", false,[], s));
    }

    static string SPACEARROW(GetName g)
    {
        return name ~ `.`~ `SPACEARROW`;
    }

    static TParseTree OR(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/"), Spacing), name ~ `.`~ `OR`)(p);
    }

    static TParseTree OR(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/"), Spacing), name ~ `.`~ `OR`)(TParseTree("", false,[], s));
    }

    static string OR(GetName g)
    {
        return name ~ `.`~ `OR`;
    }

    static TParseTree POS(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("&"), Spacing), name ~ `.`~ `POS`)(p);
    }

    static TParseTree POS(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("&"), Spacing), name ~ `.`~ `POS`)(TParseTree("", false,[], s));
    }

    static string POS(GetName g)
    {
        return name ~ `.`~ `POS`;
    }

    static TParseTree NEG(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("!"), Spacing), name ~ `.`~ `NEG`)(p);
    }

    static TParseTree NEG(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("!"), Spacing), name ~ `.`~ `NEG`)(TParseTree("", false,[], s));
    }

    static string NEG(GetName g)
    {
        return name ~ `.`~ `NEG`;
    }

    static TParseTree FUSE(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("~"), Spacing), name ~ `.`~ `FUSE`)(p);
    }

    static TParseTree FUSE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("~"), Spacing), name ~ `.`~ `FUSE`)(TParseTree("", false,[], s));
    }

    static string FUSE(GetName g)
    {
        return name ~ `.`~ `FUSE`;
    }

    static TParseTree DISCARD(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(":"), Spacing), name ~ `.`~ `DISCARD`)(p);
    }

    static TParseTree DISCARD(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(":"), Spacing), name ~ `.`~ `DISCARD`)(TParseTree("", false,[], s));
    }

    static string DISCARD(GetName g)
    {
        return name ~ `.`~ `DISCARD`;
    }

    static TParseTree KEEP(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("^"), Spacing), name ~ `.`~ `KEEP`)(p);
    }

    static TParseTree KEEP(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("^"), Spacing), name ~ `.`~ `KEEP`)(TParseTree("", false,[], s));
    }

    static string KEEP(GetName g)
    {
        return name ~ `.`~ `KEEP`;
    }

    static TParseTree DROP(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(";"), Spacing), name ~ `.`~ `DROP`)(p);
    }

    static TParseTree DROP(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(";"), Spacing), name ~ `.`~ `DROP`)(TParseTree("", false,[], s));
    }

    static string DROP(GetName g)
    {
        return name ~ `.`~ `DROP`;
    }

    static TParseTree OPTION(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("?"), Spacing), name ~ `.`~ `OPTION`)(p);
    }

    static TParseTree OPTION(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("?"), Spacing), name ~ `.`~ `OPTION`)(TParseTree("", false,[], s));
    }

    static string OPTION(GetName g)
    {
        return name ~ `.`~ `OPTION`;
    }

    static TParseTree ZEROORMORE(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("*"), Spacing), name ~ `.`~ `ZEROORMORE`)(p);
    }

    static TParseTree ZEROORMORE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("*"), Spacing), name ~ `.`~ `ZEROORMORE`)(TParseTree("", false,[], s));
    }

    static string ZEROORMORE(GetName g)
    {
        return name ~ `.`~ `ZEROORMORE`;
    }

    static TParseTree ONEORMORE(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("+"), Spacing), name ~ `.`~ `ONEORMORE`)(p);
    }

    static TParseTree ONEORMORE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("+"), Spacing), name ~ `.`~ `ONEORMORE`)(TParseTree("", false,[], s));
    }

    static string ONEORMORE(GetName g)
    {
        return name ~ `.`~ `ONEORMORE`;
    }

    static TParseTree ACTIONOPEN(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("{"), Spacing), name ~ `.`~ `ACTIONOPEN`)(p);
    }

    static TParseTree ACTIONOPEN(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("{"), Spacing), name ~ `.`~ `ACTIONOPEN`)(TParseTree("", false,[], s));
    }

    static string ACTIONOPEN(GetName g)
    {
        return name ~ `.`~ `ACTIONOPEN`;
    }

    static TParseTree ACTIONCLOSE(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("}"), Spacing), name ~ `.`~ `ACTIONCLOSE`)(p);
    }

    static TParseTree ACTIONCLOSE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("}"), Spacing), name ~ `.`~ `ACTIONCLOSE`)(TParseTree("", false,[], s));
    }

    static string ACTIONCLOSE(GetName g)
    {
        return name ~ `.`~ `ACTIONCLOSE`;
    }

    static TParseTree SEPARATOR(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(","), Spacing), name ~ `.`~ `SEPARATOR`)(p);
    }

    static TParseTree SEPARATOR(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(","), Spacing), name ~ `.`~ `SEPARATOR`)(TParseTree("", false,[], s));
    }

    static string SEPARATOR(GetName g)
    {
        return name ~ `.`~ `SEPARATOR`;
    }

    static TParseTree ASSIGN(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("="), Spacing), name ~ `.`~ `ASSIGN`)(p);
    }

    static TParseTree ASSIGN(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("="), Spacing), name ~ `.`~ `ASSIGN`)(TParseTree("", false,[], s));
    }

    static string ASSIGN(GetName g)
    {
        return name ~ `.`~ `ASSIGN`;
    }

    static TParseTree NAMESEP(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.literal!("."), name ~ `.`~ `NAMESEP`)(p);
    }

    static TParseTree NAMESEP(string s)
    {
        return pegged.peg.named!(pegged.peg.literal!("."), name ~ `.`~ `NAMESEP`)(TParseTree("", false,[], s));
    }

    static string NAMESEP(GetName g)
    {
        return name ~ `.`~ `NAMESEP`;
    }

    static TParseTree OPEN(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("("), Spacing), name ~ `.`~ `OPEN`)(p);
    }

    static TParseTree OPEN(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("("), Spacing), name ~ `.`~ `OPEN`)(TParseTree("", false,[], s));
    }

    static string OPEN(GetName g)
    {
        return name ~ `.`~ `OPEN`;
    }

    static TParseTree CLOSE(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(")"), Spacing), name ~ `.`~ `CLOSE`)(p);
    }

    static TParseTree CLOSE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(")"), Spacing), name ~ `.`~ `CLOSE`)(TParseTree("", false,[], s));
    }

    static string CLOSE(GetName g)
    {
        return name ~ `.`~ `CLOSE`;
    }

    static TParseTree ANY(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("."), Spacing), name ~ `.`~ `ANY`)(p);
    }

    static TParseTree ANY(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("."), Spacing), name ~ `.`~ `ANY`)(TParseTree("", false,[], s));
    }

    static string ANY(GetName g)
    {
        return name ~ `.`~ `ANY`;
    }

    static TParseTree Spacing(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(Space, Comment))), name ~ `.`~ `Spacing`)(p);
    }

    static TParseTree Spacing(string s)
    {
        return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(Space, Comment))), name ~ `.`~ `Spacing`)(TParseTree("", false,[], s));
    }

    static string Spacing(GetName g)
    {
        return name ~ `.`~ `Spacing`;
    }

    static TParseTree Comment(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("#"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(EndOfLine), pegged.peg.any)), EndOfLine), name ~ `.`~ `Comment`)(p);
    }

    static TParseTree Comment(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("#"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(EndOfLine), pegged.peg.any)), EndOfLine), name ~ `.`~ `Comment`)(TParseTree("", false,[], s));
    }

    static string Comment(GetName g)
    {
        return name ~ `.`~ `Comment`;
    }

    static TParseTree Space(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), EndOfLine), name ~ `.`~ `Space`)(p);
    }

    static TParseTree Space(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), EndOfLine), name ~ `.`~ `Space`)(TParseTree("", false,[], s));
    }

    static string Space(GetName g)
    {
        return name ~ `.`~ `Space`;
    }

    static TParseTree EndOfLine(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.keywords!("\r\n", "\n", "\r"), name ~ `.`~ `EndOfLine`)(p);
    }

    static TParseTree EndOfLine(string s)
    {
        return pegged.peg.named!(pegged.peg.keywords!("\r\n", "\n", "\r"), name ~ `.`~ `EndOfLine`)(TParseTree("", false,[], s));
    }

    static string EndOfLine(GetName g)
    {
        return name ~ `.`~ `EndOfLine`;
    }

    static TParseTree EndOfInput(TParseTree p)
    {
        return pegged.peg.named!(pegged.peg.negLookahead!(pegged.peg.any), name ~ `.`~ `EndOfInput`)(p);
    }

    static TParseTree EndOfInput(string s)
    {
        return pegged.peg.named!(pegged.peg.negLookahead!(pegged.peg.any), name ~ `.`~ `EndOfInput`)(TParseTree("", false,[], s));
    }

    static string EndOfInput(GetName g)
    {
        return name ~ `.`~ `EndOfInput`;
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
    static string opCall(GetName g)
    {
        return "Pegged";
    }

    }
}

alias GenericPegged!(ParseTree).Pegged Pegged;

