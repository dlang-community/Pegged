/++
This module was automatically generated from the following grammar:


# This is the PEG extended grammar used by Pegged
Pegged:

# Syntactic rules:
Grammar      <- Spacing GrammarName Definition+ :eoi
Definition   <- LhsName Arrow Expression
Expression   <- :OR? Sequence (:OR Sequence)*
Sequence     <- Prefix+
Prefix       <- (POS / NEG / FUSE / DISCARD / KEEP / DROP / PROPAGATE)* Suffix
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
Action       <- :ACTIONOPEN qualifiedIdentifier (:SEPARATOR qualifiedIdentifier)* :ACTIONCLOSE

Literal      <- quote       ~(!quote Char)*       quote       Spacing
              / doublequote ~(!doublequote Char)* doublequote Spacing
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

Arrow        <- LEFTARROW / FUSEARROW / DISCARDARROW / KEEPARROW / DROPARROW / PROPAGATEARROW / SPACEARROW
LEFTARROW    <- '<-' Spacing
FUSEARROW    <- '<~' Spacing
DISCARDARROW <- '<:' Spacing
KEEPARROW    <- '<^' Spacing
DROPARROW    <- '<;' Spacing
PROPAGATEARROW <- '<%' Spacing
SPACEARROW   <- '<' Spacing

OR           <- '/' Spacing

POS          <- '&' Spacing
NEG          <- '!' Spacing
FUSE         <- '~' Spacing
DISCARD      <- ':' Spacing
KEEP         <- '^' Spacing
DROP         <- ';' Spacing
PROPAGATE    <- '%' Spacing

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
Comment      <- '#' (!eol .)* :eol
Space        <- spacing / "\\t" / "\\n" / "\\r"


+/
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
            case "Pegged.DROPARROW":
            case "Pegged.PROPAGATEARROW":
            case "Pegged.SPACEARROW":
            case "Pegged.OR":
            case "Pegged.POS":
            case "Pegged.NEG":
            case "Pegged.FUSE":
            case "Pegged.DISCARD":
            case "Pegged.KEEP":
            case "Pegged.DROP":
            case "Pegged.PROPAGATE":
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
                return true;
            default:
                return false;
        }
    }
    mixin decimateTree;
    static TParseTree Grammar(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), "Pegged.Grammar")(p);
    }
    static TParseTree Grammar(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), "Pegged.Grammar")(TParseTree("", false,[], s));
    }
    static string Grammar(GetName g)
    {
        return "Pegged.Grammar";
    }

    static TParseTree Definition(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(LhsName, Arrow, Expression), "Pegged.Definition")(p);
    }
    static TParseTree Definition(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(LhsName, Arrow, Expression), "Pegged.Definition")(TParseTree("", false,[], s));
    }
    static string Definition(GetName g)
    {
        return "Pegged.Definition";
    }

    static TParseTree Expression(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), "Pegged.Expression")(p);
    }
    static TParseTree Expression(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), "Pegged.Expression")(TParseTree("", false,[], s));
    }
    static string Expression(GetName g)
    {
        return "Pegged.Expression";
    }

    static TParseTree Sequence(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.oneOrMore!(Prefix), "Pegged.Sequence")(p);
    }
    static TParseTree Sequence(string s)
    {
        return pegged.peg.named!(pegged.peg.oneOrMore!(Prefix), "Pegged.Sequence")(TParseTree("", false,[], s));
    }
    static string Sequence(GetName g)
    {
        return "Pegged.Sequence";
    }

    static TParseTree Prefix(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), "Pegged.Prefix")(p);
    }
    static TParseTree Prefix(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), "Pegged.Prefix")(TParseTree("", false,[], s));
    }
    static string Prefix(GetName g)
    {
        return "Pegged.Prefix";
    }

    static TParseTree Suffix(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Pegged.Suffix")(p);
    }
    static TParseTree Suffix(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Pegged.Suffix")(TParseTree("", false,[], s));
    }
    static string Suffix(GetName g)
    {
        return "Pegged.Suffix";
    }

    static TParseTree Primary(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CharClass, ANY)), "Pegged.Primary")(p);
    }
    static TParseTree Primary(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CharClass, ANY)), "Pegged.Primary")(TParseTree("", false,[], s));
    }
    static string Primary(GetName g)
    {
        return "Pegged.Primary";
    }

    static TParseTree Identifier(TParseTree p)
    {
         return pegged.peg.named!(identifier, "Pegged.Identifier")(p);
    }
    static TParseTree Identifier(string s)
    {
        return pegged.peg.named!(identifier, "Pegged.Identifier")(TParseTree("", false,[], s));
    }
    static string Identifier(GetName g)
    {
        return "Pegged.Identifier";
    }

    static TParseTree GrammarName(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(`:`)), Spacing), "Pegged.GrammarName")(p);
    }
    static TParseTree GrammarName(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(`:`)), Spacing), "Pegged.GrammarName")(TParseTree("", false,[], s));
    }
    static string GrammarName(GetName g)
    {
        return "Pegged.GrammarName";
    }

    static TParseTree LhsName(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), "Pegged.LhsName")(p);
    }
    static TParseTree LhsName(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), "Pegged.LhsName")(TParseTree("", false,[], s));
    }
    static string LhsName(GetName g)
    {
        return "Pegged.LhsName";
    }

    static TParseTree RhsName(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), "Pegged.RhsName")(p);
    }
    static TParseTree RhsName(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), "Pegged.RhsName")(TParseTree("", false,[], s));
    }
    static string RhsName(GetName g)
    {
        return "Pegged.RhsName";
    }

    static TParseTree ParamList(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), "Pegged.ParamList")(p);
    }
    static TParseTree ParamList(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), "Pegged.ParamList")(TParseTree("", false,[], s));
    }
    static string ParamList(GetName g)
    {
        return "Pegged.ParamList";
    }

    static TParseTree Param(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(DefaultParam, SingleParam), "Pegged.Param")(p);
    }
    static TParseTree Param(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(DefaultParam, SingleParam), "Pegged.Param")(TParseTree("", false,[], s));
    }
    static string Param(GetName g)
    {
        return "Pegged.Param";
    }

    static TParseTree DefaultParam(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), "Pegged.DefaultParam")(p);
    }
    static TParseTree DefaultParam(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), "Pegged.DefaultParam")(TParseTree("", false,[], s));
    }
    static string DefaultParam(GetName g)
    {
        return "Pegged.DefaultParam";
    }

    static TParseTree SingleParam(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing), "Pegged.SingleParam")(p);
    }
    static TParseTree SingleParam(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing), "Pegged.SingleParam")(TParseTree("", false,[], s));
    }
    static string SingleParam(GetName g)
    {
        return "Pegged.SingleParam";
    }

    static TParseTree ArgList(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), "Pegged.ArgList")(p);
    }
    static TParseTree ArgList(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), "Pegged.ArgList")(TParseTree("", false,[], s));
    }
    static string ArgList(GetName g)
    {
        return "Pegged.ArgList";
    }

    static TParseTree Action(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), qualifiedIdentifier, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), qualifiedIdentifier)), pegged.peg.discard!(ACTIONCLOSE)), "Pegged.Action")(p);
    }
    static TParseTree Action(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), qualifiedIdentifier, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), qualifiedIdentifier)), pegged.peg.discard!(ACTIONCLOSE)), "Pegged.Action")(TParseTree("", false,[], s));
    }
    static string Action(GetName g)
    {
        return "Pegged.Action";
    }

    static TParseTree Literal(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, Spacing)), "Pegged.Literal")(p);
    }
    static TParseTree Literal(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, Spacing)), "Pegged.Literal")(TParseTree("", false,[], s));
    }
    static string Literal(GetName g)
    {
        return "Pegged.Literal";
    }

    static TParseTree CharClass(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(`[`)), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(`]`)), CharRange)), pegged.peg.discard!(pegged.peg.literal!(`]`)), Spacing), "Pegged.CharClass")(p);
    }
    static TParseTree CharClass(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(`[`)), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(`]`)), CharRange)), pegged.peg.discard!(pegged.peg.literal!(`]`)), Spacing), "Pegged.CharClass")(TParseTree("", false,[], s));
    }
    static string CharClass(GetName g)
    {
        return "Pegged.CharClass";
    }

    static TParseTree CharRange(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!(`-`), Char), Char), "Pegged.CharRange")(p);
    }
    static TParseTree CharRange(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!(`-`), Char), Char), "Pegged.CharRange")(TParseTree("", false,[], s));
    }
    static string CharRange(GetName g)
    {
        return "Pegged.CharRange";
    }

    static TParseTree Char(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!(`-`), pegged.peg.literal!(`[`), pegged.peg.literal!(`]`), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!(`x`), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`u`), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`U`), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "Pegged.Char")(p);
    }
    static TParseTree Char(string s)
    {
        return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!(`-`), pegged.peg.literal!(`[`), pegged.peg.literal!(`]`), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!(`x`), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`u`), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`U`), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "Pegged.Char")(TParseTree("", false,[], s));
    }
    static string Char(GetName g)
    {
        return "Pegged.Char";
    }

    static TParseTree Arrow(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, SPACEARROW), "Pegged.Arrow")(p);
    }
    static TParseTree Arrow(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, SPACEARROW), "Pegged.Arrow")(TParseTree("", false,[], s));
    }
    static string Arrow(GetName g)
    {
        return "Pegged.Arrow";
    }

    static TParseTree LEFTARROW(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<-`), Spacing), "Pegged.LEFTARROW")(p);
    }
    static TParseTree LEFTARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<-`), Spacing), "Pegged.LEFTARROW")(TParseTree("", false,[], s));
    }
    static string LEFTARROW(GetName g)
    {
        return "Pegged.LEFTARROW";
    }

    static TParseTree FUSEARROW(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<~`), Spacing), "Pegged.FUSEARROW")(p);
    }
    static TParseTree FUSEARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<~`), Spacing), "Pegged.FUSEARROW")(TParseTree("", false,[], s));
    }
    static string FUSEARROW(GetName g)
    {
        return "Pegged.FUSEARROW";
    }

    static TParseTree DISCARDARROW(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<:`), Spacing), "Pegged.DISCARDARROW")(p);
    }
    static TParseTree DISCARDARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<:`), Spacing), "Pegged.DISCARDARROW")(TParseTree("", false,[], s));
    }
    static string DISCARDARROW(GetName g)
    {
        return "Pegged.DISCARDARROW";
    }

    static TParseTree KEEPARROW(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<^`), Spacing), "Pegged.KEEPARROW")(p);
    }
    static TParseTree KEEPARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<^`), Spacing), "Pegged.KEEPARROW")(TParseTree("", false,[], s));
    }
    static string KEEPARROW(GetName g)
    {
        return "Pegged.KEEPARROW";
    }

    static TParseTree DROPARROW(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<;`), Spacing), "Pegged.DROPARROW")(p);
    }
    static TParseTree DROPARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<;`), Spacing), "Pegged.DROPARROW")(TParseTree("", false,[], s));
    }
    static string DROPARROW(GetName g)
    {
        return "Pegged.DROPARROW";
    }

    static TParseTree PROPAGATEARROW(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<%`), Spacing), "Pegged.PROPAGATEARROW")(p);
    }
    static TParseTree PROPAGATEARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<%`), Spacing), "Pegged.PROPAGATEARROW")(TParseTree("", false,[], s));
    }
    static string PROPAGATEARROW(GetName g)
    {
        return "Pegged.PROPAGATEARROW";
    }

    static TParseTree SPACEARROW(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<`), Spacing), "Pegged.SPACEARROW")(p);
    }
    static TParseTree SPACEARROW(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<`), Spacing), "Pegged.SPACEARROW")(TParseTree("", false,[], s));
    }
    static string SPACEARROW(GetName g)
    {
        return "Pegged.SPACEARROW";
    }

    static TParseTree OR(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`/`), Spacing), "Pegged.OR")(p);
    }
    static TParseTree OR(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`/`), Spacing), "Pegged.OR")(TParseTree("", false,[], s));
    }
    static string OR(GetName g)
    {
        return "Pegged.OR";
    }

    static TParseTree POS(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`&`), Spacing), "Pegged.POS")(p);
    }
    static TParseTree POS(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`&`), Spacing), "Pegged.POS")(TParseTree("", false,[], s));
    }
    static string POS(GetName g)
    {
        return "Pegged.POS";
    }

    static TParseTree NEG(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`!`), Spacing), "Pegged.NEG")(p);
    }
    static TParseTree NEG(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`!`), Spacing), "Pegged.NEG")(TParseTree("", false,[], s));
    }
    static string NEG(GetName g)
    {
        return "Pegged.NEG";
    }

    static TParseTree FUSE(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`~`), Spacing), "Pegged.FUSE")(p);
    }
    static TParseTree FUSE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`~`), Spacing), "Pegged.FUSE")(TParseTree("", false,[], s));
    }
    static string FUSE(GetName g)
    {
        return "Pegged.FUSE";
    }

    static TParseTree DISCARD(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`:`), Spacing), "Pegged.DISCARD")(p);
    }
    static TParseTree DISCARD(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`:`), Spacing), "Pegged.DISCARD")(TParseTree("", false,[], s));
    }
    static string DISCARD(GetName g)
    {
        return "Pegged.DISCARD";
    }

    static TParseTree KEEP(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`^`), Spacing), "Pegged.KEEP")(p);
    }
    static TParseTree KEEP(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`^`), Spacing), "Pegged.KEEP")(TParseTree("", false,[], s));
    }
    static string KEEP(GetName g)
    {
        return "Pegged.KEEP";
    }

    static TParseTree DROP(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`;`), Spacing), "Pegged.DROP")(p);
    }
    static TParseTree DROP(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`;`), Spacing), "Pegged.DROP")(TParseTree("", false,[], s));
    }
    static string DROP(GetName g)
    {
        return "Pegged.DROP";
    }

    static TParseTree PROPAGATE(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`%`), Spacing), "Pegged.PROPAGATE")(p);
    }
    static TParseTree PROPAGATE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`%`), Spacing), "Pegged.PROPAGATE")(TParseTree("", false,[], s));
    }
    static string PROPAGATE(GetName g)
    {
        return "Pegged.PROPAGATE";
    }

    static TParseTree OPTION(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`?`), Spacing), "Pegged.OPTION")(p);
    }
    static TParseTree OPTION(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`?`), Spacing), "Pegged.OPTION")(TParseTree("", false,[], s));
    }
    static string OPTION(GetName g)
    {
        return "Pegged.OPTION";
    }

    static TParseTree ZEROORMORE(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`*`), Spacing), "Pegged.ZEROORMORE")(p);
    }
    static TParseTree ZEROORMORE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`*`), Spacing), "Pegged.ZEROORMORE")(TParseTree("", false,[], s));
    }
    static string ZEROORMORE(GetName g)
    {
        return "Pegged.ZEROORMORE";
    }

    static TParseTree ONEORMORE(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`+`), Spacing), "Pegged.ONEORMORE")(p);
    }
    static TParseTree ONEORMORE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`+`), Spacing), "Pegged.ONEORMORE")(TParseTree("", false,[], s));
    }
    static string ONEORMORE(GetName g)
    {
        return "Pegged.ONEORMORE";
    }

    static TParseTree ACTIONOPEN(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`{`), Spacing), "Pegged.ACTIONOPEN")(p);
    }
    static TParseTree ACTIONOPEN(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`{`), Spacing), "Pegged.ACTIONOPEN")(TParseTree("", false,[], s));
    }
    static string ACTIONOPEN(GetName g)
    {
        return "Pegged.ACTIONOPEN";
    }

    static TParseTree ACTIONCLOSE(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`}`), Spacing), "Pegged.ACTIONCLOSE")(p);
    }
    static TParseTree ACTIONCLOSE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`}`), Spacing), "Pegged.ACTIONCLOSE")(TParseTree("", false,[], s));
    }
    static string ACTIONCLOSE(GetName g)
    {
        return "Pegged.ACTIONCLOSE";
    }

    static TParseTree SEPARATOR(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`,`), Spacing), "Pegged.SEPARATOR")(p);
    }
    static TParseTree SEPARATOR(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`,`), Spacing), "Pegged.SEPARATOR")(TParseTree("", false,[], s));
    }
    static string SEPARATOR(GetName g)
    {
        return "Pegged.SEPARATOR";
    }

    static TParseTree ASSIGN(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`=`), Spacing), "Pegged.ASSIGN")(p);
    }
    static TParseTree ASSIGN(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`=`), Spacing), "Pegged.ASSIGN")(TParseTree("", false,[], s));
    }
    static string ASSIGN(GetName g)
    {
        return "Pegged.ASSIGN";
    }

    static TParseTree NAMESEP(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.literal!(`.`), "Pegged.NAMESEP")(p);
    }
    static TParseTree NAMESEP(string s)
    {
        return pegged.peg.named!(pegged.peg.literal!(`.`), "Pegged.NAMESEP")(TParseTree("", false,[], s));
    }
    static string NAMESEP(GetName g)
    {
        return "Pegged.NAMESEP";
    }

    static TParseTree OPEN(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`(`), Spacing), "Pegged.OPEN")(p);
    }
    static TParseTree OPEN(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`(`), Spacing), "Pegged.OPEN")(TParseTree("", false,[], s));
    }
    static string OPEN(GetName g)
    {
        return "Pegged.OPEN";
    }

    static TParseTree CLOSE(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`)`), Spacing), "Pegged.CLOSE")(p);
    }
    static TParseTree CLOSE(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`)`), Spacing), "Pegged.CLOSE")(TParseTree("", false,[], s));
    }
    static string CLOSE(GetName g)
    {
        return "Pegged.CLOSE";
    }

    static TParseTree ANY(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`.`), Spacing), "Pegged.ANY")(p);
    }
    static TParseTree ANY(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`.`), Spacing), "Pegged.ANY")(TParseTree("", false,[], s));
    }
    static string ANY(GetName g)
    {
        return "Pegged.ANY";
    }

    static TParseTree Spacing(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(Space, Comment))), "Pegged.Spacing")(p);
    }
    static TParseTree Spacing(string s)
    {
        return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(Space, Comment))), "Pegged.Spacing")(TParseTree("", false,[], s));
    }
    static string Spacing(GetName g)
    {
        return "Pegged.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`#`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), "Pegged.Comment")(p);
    }
    static TParseTree Comment(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`#`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), "Pegged.Comment")(TParseTree("", false,[], s));
    }
    static string Comment(GetName g)
    {
        return "Pegged.Comment";
    }

    static TParseTree Space(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(spacing, pegged.peg.literal!(`\\t`), pegged.peg.literal!(`\\n`), pegged.peg.literal!(`\\r`)), "Pegged.Space")(p);
    }
    static TParseTree Space(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(spacing, pegged.peg.literal!(`\\t`), pegged.peg.literal!(`\\n`), pegged.peg.literal!(`\\r`)), "Pegged.Space")(TParseTree("", false,[], s));
    }
    static string Space(GetName g)
    {
        return "Pegged.Space";
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

