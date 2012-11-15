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
Spacing      <: (blank / Comment)*
Comment      <- '#' (!eol .)* :eol


+/
module pegged.parser;

public import pegged.peg;
struct GenericPegged(TParseTree)
{
    struct Pegged
    {
    enum name = "Pegged";
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
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
                return true;
            default:
                return false;
        }
    }
    mixin decimateTree;
    static TParseTree Grammar(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), name ~ `.`~ `Grammar`)(p);
        }
        else
        {
            if(auto m = tuple(`Grammar`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), name ~ `.`~ `Grammar`)(p);
                memo[tuple(`Grammar`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Grammar(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), name ~ `.`~ `Grammar`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), name ~ `.`~ `Grammar`)(TParseTree("", false,[], s));
        }
    }
    static string Grammar(GetName g)
    {
        return name ~ `.`~ `Grammar`;
    }

    static TParseTree Definition(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(LhsName, Arrow, Expression), name ~ `.`~ `Definition`)(p);
        }
        else
        {
            if(auto m = tuple(`Definition`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(LhsName, Arrow, Expression), name ~ `.`~ `Definition`)(p);
                memo[tuple(`Definition`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Definition(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(LhsName, Arrow, Expression), name ~ `.`~ `Definition`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(LhsName, Arrow, Expression), name ~ `.`~ `Definition`)(TParseTree("", false,[], s));
        }
    }
    static string Definition(GetName g)
    {
        return name ~ `.`~ `Definition`;
    }

    static TParseTree Expression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), name ~ `.`~ `Expression`)(p);
        }
        else
        {
            if(auto m = tuple(`Expression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), name ~ `.`~ `Expression`)(p);
                memo[tuple(`Expression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Expression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), name ~ `.`~ `Expression`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), name ~ `.`~ `Expression`)(TParseTree("", false,[], s));
        }
    }
    static string Expression(GetName g)
    {
        return name ~ `.`~ `Expression`;
    }

    static TParseTree Sequence(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(Prefix), name ~ `.`~ `Sequence`)(p);
        }
        else
        {
            if(auto m = tuple(`Sequence`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.oneOrMore!(Prefix), name ~ `.`~ `Sequence`)(p);
                memo[tuple(`Sequence`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Sequence(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(Prefix), name ~ `.`~ `Sequence`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.oneOrMore!(Prefix), name ~ `.`~ `Sequence`)(TParseTree("", false,[], s));
        }
    }
    static string Sequence(GetName g)
    {
        return name ~ `.`~ `Sequence`;
    }

    static TParseTree Prefix(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), name ~ `.`~ `Prefix`)(p);
        }
        else
        {
            if(auto m = tuple(`Prefix`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), name ~ `.`~ `Prefix`)(p);
                memo[tuple(`Prefix`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Prefix(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), name ~ `.`~ `Prefix`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), name ~ `.`~ `Prefix`)(TParseTree("", false,[], s));
        }
    }
    static string Prefix(GetName g)
    {
        return name ~ `.`~ `Prefix`;
    }

    static TParseTree Suffix(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), name ~ `.`~ `Suffix`)(p);
        }
        else
        {
            if(auto m = tuple(`Suffix`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), name ~ `.`~ `Suffix`)(p);
                memo[tuple(`Suffix`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Suffix(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), name ~ `.`~ `Suffix`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), name ~ `.`~ `Suffix`)(TParseTree("", false,[], s));
        }
    }
    static string Suffix(GetName g)
    {
        return name ~ `.`~ `Suffix`;
    }

    static TParseTree Primary(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CharClass, ANY)), name ~ `.`~ `Primary`)(p);
        }
        else
        {
            if(auto m = tuple(`Primary`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CharClass, ANY)), name ~ `.`~ `Primary`)(p);
                memo[tuple(`Primary`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Primary(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CharClass, ANY)), name ~ `.`~ `Primary`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CharClass, ANY)), name ~ `.`~ `Primary`)(TParseTree("", false,[], s));
        }
    }
    static string Primary(GetName g)
    {
        return name ~ `.`~ `Primary`;
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(identifier, name ~ `.`~ `Identifier`)(p);
        }
        else
        {
            if(auto m = tuple(`Identifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(identifier, name ~ `.`~ `Identifier`)(p);
                memo[tuple(`Identifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(identifier, name ~ `.`~ `Identifier`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(identifier, name ~ `.`~ `Identifier`)(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return name ~ `.`~ `Identifier`;
    }

    static TParseTree GrammarName(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(`:`)), Spacing), name ~ `.`~ `GrammarName`)(p);
        }
        else
        {
            if(auto m = tuple(`GrammarName`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(`:`)), Spacing), name ~ `.`~ `GrammarName`)(p);
                memo[tuple(`GrammarName`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree GrammarName(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(`:`)), Spacing), name ~ `.`~ `GrammarName`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(`:`)), Spacing), name ~ `.`~ `GrammarName`)(TParseTree("", false,[], s));
        }
    }
    static string GrammarName(GetName g)
    {
        return name ~ `.`~ `GrammarName`;
    }

    static TParseTree LhsName(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), name ~ `.`~ `LhsName`)(p);
        }
        else
        {
            if(auto m = tuple(`LhsName`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), name ~ `.`~ `LhsName`)(p);
                memo[tuple(`LhsName`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LhsName(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), name ~ `.`~ `LhsName`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), name ~ `.`~ `LhsName`)(TParseTree("", false,[], s));
        }
    }
    static string LhsName(GetName g)
    {
        return name ~ `.`~ `LhsName`;
    }

    static TParseTree RhsName(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), name ~ `.`~ `RhsName`)(p);
        }
        else
        {
            if(auto m = tuple(`RhsName`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), name ~ `.`~ `RhsName`)(p);
                memo[tuple(`RhsName`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RhsName(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), name ~ `.`~ `RhsName`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), name ~ `.`~ `RhsName`)(TParseTree("", false,[], s));
        }
    }
    static string RhsName(GetName g)
    {
        return name ~ `.`~ `RhsName`;
    }

    static TParseTree ParamList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ParamList`)(p);
        }
        else
        {
            if(auto m = tuple(`ParamList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ParamList`)(p);
                memo[tuple(`ParamList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParamList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ParamList`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ParamList`)(TParseTree("", false,[], s));
        }
    }
    static string ParamList(GetName g)
    {
        return name ~ `.`~ `ParamList`;
    }

    static TParseTree Param(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(DefaultParam, SingleParam), name ~ `.`~ `Param`)(p);
        }
        else
        {
            if(auto m = tuple(`Param`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(DefaultParam, SingleParam), name ~ `.`~ `Param`)(p);
                memo[tuple(`Param`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Param(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(DefaultParam, SingleParam), name ~ `.`~ `Param`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(DefaultParam, SingleParam), name ~ `.`~ `Param`)(TParseTree("", false,[], s));
        }
    }
    static string Param(GetName g)
    {
        return name ~ `.`~ `Param`;
    }

    static TParseTree DefaultParam(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), name ~ `.`~ `DefaultParam`)(p);
        }
        else
        {
            if(auto m = tuple(`DefaultParam`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), name ~ `.`~ `DefaultParam`)(p);
                memo[tuple(`DefaultParam`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DefaultParam(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), name ~ `.`~ `DefaultParam`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), name ~ `.`~ `DefaultParam`)(TParseTree("", false,[], s));
        }
    }
    static string DefaultParam(GetName g)
    {
        return name ~ `.`~ `DefaultParam`;
    }

    static TParseTree SingleParam(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing), name ~ `.`~ `SingleParam`)(p);
        }
        else
        {
            if(auto m = tuple(`SingleParam`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(Identifier, Spacing), name ~ `.`~ `SingleParam`)(p);
                memo[tuple(`SingleParam`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleParam(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing), name ~ `.`~ `SingleParam`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(Identifier, Spacing), name ~ `.`~ `SingleParam`)(TParseTree("", false,[], s));
        }
    }
    static string SingleParam(GetName g)
    {
        return name ~ `.`~ `SingleParam`;
    }

    static TParseTree ArgList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ArgList`)(p);
        }
        else
        {
            if(auto m = tuple(`ArgList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ArgList`)(p);
                memo[tuple(`ArgList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArgList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ArgList`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), name ~ `.`~ `ArgList`)(TParseTree("", false,[], s));
        }
    }
    static string ArgList(GetName g)
    {
        return name ~ `.`~ `ArgList`;
    }

    static TParseTree Action(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), qualifiedIdentifier, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), qualifiedIdentifier)), pegged.peg.discard!(ACTIONCLOSE)), name ~ `.`~ `Action`)(p);
        }
        else
        {
            if(auto m = tuple(`Action`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), qualifiedIdentifier, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), qualifiedIdentifier)), pegged.peg.discard!(ACTIONCLOSE)), name ~ `.`~ `Action`)(p);
                memo[tuple(`Action`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Action(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), qualifiedIdentifier, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), qualifiedIdentifier)), pegged.peg.discard!(ACTIONCLOSE)), name ~ `.`~ `Action`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), qualifiedIdentifier, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), qualifiedIdentifier)), pegged.peg.discard!(ACTIONCLOSE)), name ~ `.`~ `Action`)(TParseTree("", false,[], s));
        }
    }
    static string Action(GetName g)
    {
        return name ~ `.`~ `Action`;
    }

    static TParseTree Literal(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, Spacing)), name ~ `.`~ `Literal`)(p);
        }
        else
        {
            if(auto m = tuple(`Literal`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, Spacing)), name ~ `.`~ `Literal`)(p);
                memo[tuple(`Literal`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Literal(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, Spacing)), name ~ `.`~ `Literal`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, Spacing)), name ~ `.`~ `Literal`)(TParseTree("", false,[], s));
        }
    }
    static string Literal(GetName g)
    {
        return name ~ `.`~ `Literal`;
    }

    static TParseTree CharClass(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(`[`)), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(`]`)), CharRange)), pegged.peg.discard!(pegged.peg.literal!(`]`)), Spacing), name ~ `.`~ `CharClass`)(p);
        }
        else
        {
            if(auto m = tuple(`CharClass`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(`[`)), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(`]`)), CharRange)), pegged.peg.discard!(pegged.peg.literal!(`]`)), Spacing), name ~ `.`~ `CharClass`)(p);
                memo[tuple(`CharClass`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CharClass(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(`[`)), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(`]`)), CharRange)), pegged.peg.discard!(pegged.peg.literal!(`]`)), Spacing), name ~ `.`~ `CharClass`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!(`[`)), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(`]`)), CharRange)), pegged.peg.discard!(pegged.peg.literal!(`]`)), Spacing), name ~ `.`~ `CharClass`)(TParseTree("", false,[], s));
        }
    }
    static string CharClass(GetName g)
    {
        return name ~ `.`~ `CharClass`;
    }

    static TParseTree CharRange(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!(`-`), Char), Char), name ~ `.`~ `CharRange`)(p);
        }
        else
        {
            if(auto m = tuple(`CharRange`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!(`-`), Char), Char), name ~ `.`~ `CharRange`)(p);
                memo[tuple(`CharRange`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CharRange(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!(`-`), Char), Char), name ~ `.`~ `CharRange`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!(`-`), Char), Char), name ~ `.`~ `CharRange`)(TParseTree("", false,[], s));
        }
    }
    static string CharRange(GetName g)
    {
        return name ~ `.`~ `CharRange`;
    }

    static TParseTree Char(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!(`-`), pegged.peg.literal!(`[`), pegged.peg.literal!(`]`), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!(`x`), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`u`), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`U`), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), name ~ `.`~ `Char`)(p);
        }
        else
        {
            if(auto m = tuple(`Char`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!(`-`), pegged.peg.literal!(`[`), pegged.peg.literal!(`]`), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!(`x`), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`u`), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`U`), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), name ~ `.`~ `Char`)(p);
                memo[tuple(`Char`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Char(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!(`-`), pegged.peg.literal!(`[`), pegged.peg.literal!(`]`), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!(`x`), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`u`), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`U`), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), name ~ `.`~ `Char`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!(`-`), pegged.peg.literal!(`[`), pegged.peg.literal!(`]`), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!(`x`), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`u`), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!(`U`), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), name ~ `.`~ `Char`)(TParseTree("", false,[], s));
        }
    }
    static string Char(GetName g)
    {
        return name ~ `.`~ `Char`;
    }

    static TParseTree Arrow(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, SPACEARROW), name ~ `.`~ `Arrow`)(p);
        }
        else
        {
            if(auto m = tuple(`Arrow`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, SPACEARROW), name ~ `.`~ `Arrow`)(p);
                memo[tuple(`Arrow`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Arrow(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, SPACEARROW), name ~ `.`~ `Arrow`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, SPACEARROW), name ~ `.`~ `Arrow`)(TParseTree("", false,[], s));
        }
    }
    static string Arrow(GetName g)
    {
        return name ~ `.`~ `Arrow`;
    }

    static TParseTree LEFTARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<-`), Spacing), name ~ `.`~ `LEFTARROW`)(p);
        }
        else
        {
            if(auto m = tuple(`LEFTARROW`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<-`), Spacing), name ~ `.`~ `LEFTARROW`)(p);
                memo[tuple(`LEFTARROW`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LEFTARROW(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<-`), Spacing), name ~ `.`~ `LEFTARROW`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<-`), Spacing), name ~ `.`~ `LEFTARROW`)(TParseTree("", false,[], s));
        }
    }
    static string LEFTARROW(GetName g)
    {
        return name ~ `.`~ `LEFTARROW`;
    }

    static TParseTree FUSEARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<~`), Spacing), name ~ `.`~ `FUSEARROW`)(p);
        }
        else
        {
            if(auto m = tuple(`FUSEARROW`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<~`), Spacing), name ~ `.`~ `FUSEARROW`)(p);
                memo[tuple(`FUSEARROW`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FUSEARROW(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<~`), Spacing), name ~ `.`~ `FUSEARROW`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<~`), Spacing), name ~ `.`~ `FUSEARROW`)(TParseTree("", false,[], s));
        }
    }
    static string FUSEARROW(GetName g)
    {
        return name ~ `.`~ `FUSEARROW`;
    }

    static TParseTree DISCARDARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<:`), Spacing), name ~ `.`~ `DISCARDARROW`)(p);
        }
        else
        {
            if(auto m = tuple(`DISCARDARROW`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<:`), Spacing), name ~ `.`~ `DISCARDARROW`)(p);
                memo[tuple(`DISCARDARROW`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DISCARDARROW(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<:`), Spacing), name ~ `.`~ `DISCARDARROW`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<:`), Spacing), name ~ `.`~ `DISCARDARROW`)(TParseTree("", false,[], s));
        }
    }
    static string DISCARDARROW(GetName g)
    {
        return name ~ `.`~ `DISCARDARROW`;
    }

    static TParseTree KEEPARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<^`), Spacing), name ~ `.`~ `KEEPARROW`)(p);
        }
        else
        {
            if(auto m = tuple(`KEEPARROW`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<^`), Spacing), name ~ `.`~ `KEEPARROW`)(p);
                memo[tuple(`KEEPARROW`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree KEEPARROW(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<^`), Spacing), name ~ `.`~ `KEEPARROW`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<^`), Spacing), name ~ `.`~ `KEEPARROW`)(TParseTree("", false,[], s));
        }
    }
    static string KEEPARROW(GetName g)
    {
        return name ~ `.`~ `KEEPARROW`;
    }

    static TParseTree DROPARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<;`), Spacing), name ~ `.`~ `DROPARROW`)(p);
        }
        else
        {
            if(auto m = tuple(`DROPARROW`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<;`), Spacing), name ~ `.`~ `DROPARROW`)(p);
                memo[tuple(`DROPARROW`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DROPARROW(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<;`), Spacing), name ~ `.`~ `DROPARROW`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<;`), Spacing), name ~ `.`~ `DROPARROW`)(TParseTree("", false,[], s));
        }
    }
    static string DROPARROW(GetName g)
    {
        return name ~ `.`~ `DROPARROW`;
    }

    static TParseTree PROPAGATEARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<%`), Spacing), name ~ `.`~ `PROPAGATEARROW`)(p);
        }
        else
        {
            if(auto m = tuple(`PROPAGATEARROW`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<%`), Spacing), name ~ `.`~ `PROPAGATEARROW`)(p);
                memo[tuple(`PROPAGATEARROW`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PROPAGATEARROW(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<%`), Spacing), name ~ `.`~ `PROPAGATEARROW`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<%`), Spacing), name ~ `.`~ `PROPAGATEARROW`)(TParseTree("", false,[], s));
        }
    }
    static string PROPAGATEARROW(GetName g)
    {
        return name ~ `.`~ `PROPAGATEARROW`;
    }

    static TParseTree SPACEARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<`), Spacing), name ~ `.`~ `SPACEARROW`)(p);
        }
        else
        {
            if(auto m = tuple(`SPACEARROW`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<`), Spacing), name ~ `.`~ `SPACEARROW`)(p);
                memo[tuple(`SPACEARROW`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SPACEARROW(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<`), Spacing), name ~ `.`~ `SPACEARROW`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`<`), Spacing), name ~ `.`~ `SPACEARROW`)(TParseTree("", false,[], s));
        }
    }
    static string SPACEARROW(GetName g)
    {
        return name ~ `.`~ `SPACEARROW`;
    }

    static TParseTree OR(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`/`), Spacing), name ~ `.`~ `OR`)(p);
        }
        else
        {
            if(auto m = tuple(`OR`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`/`), Spacing), name ~ `.`~ `OR`)(p);
                memo[tuple(`OR`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OR(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`/`), Spacing), name ~ `.`~ `OR`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`/`), Spacing), name ~ `.`~ `OR`)(TParseTree("", false,[], s));
        }
    }
    static string OR(GetName g)
    {
        return name ~ `.`~ `OR`;
    }

    static TParseTree POS(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`&`), Spacing), name ~ `.`~ `POS`)(p);
        }
        else
        {
            if(auto m = tuple(`POS`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`&`), Spacing), name ~ `.`~ `POS`)(p);
                memo[tuple(`POS`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree POS(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`&`), Spacing), name ~ `.`~ `POS`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`&`), Spacing), name ~ `.`~ `POS`)(TParseTree("", false,[], s));
        }
    }
    static string POS(GetName g)
    {
        return name ~ `.`~ `POS`;
    }

    static TParseTree NEG(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`!`), Spacing), name ~ `.`~ `NEG`)(p);
        }
        else
        {
            if(auto m = tuple(`NEG`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`!`), Spacing), name ~ `.`~ `NEG`)(p);
                memo[tuple(`NEG`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NEG(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`!`), Spacing), name ~ `.`~ `NEG`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`!`), Spacing), name ~ `.`~ `NEG`)(TParseTree("", false,[], s));
        }
    }
    static string NEG(GetName g)
    {
        return name ~ `.`~ `NEG`;
    }

    static TParseTree FUSE(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`~`), Spacing), name ~ `.`~ `FUSE`)(p);
        }
        else
        {
            if(auto m = tuple(`FUSE`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`~`), Spacing), name ~ `.`~ `FUSE`)(p);
                memo[tuple(`FUSE`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FUSE(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`~`), Spacing), name ~ `.`~ `FUSE`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`~`), Spacing), name ~ `.`~ `FUSE`)(TParseTree("", false,[], s));
        }
    }
    static string FUSE(GetName g)
    {
        return name ~ `.`~ `FUSE`;
    }

    static TParseTree DISCARD(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`:`), Spacing), name ~ `.`~ `DISCARD`)(p);
        }
        else
        {
            if(auto m = tuple(`DISCARD`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`:`), Spacing), name ~ `.`~ `DISCARD`)(p);
                memo[tuple(`DISCARD`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DISCARD(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`:`), Spacing), name ~ `.`~ `DISCARD`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`:`), Spacing), name ~ `.`~ `DISCARD`)(TParseTree("", false,[], s));
        }
    }
    static string DISCARD(GetName g)
    {
        return name ~ `.`~ `DISCARD`;
    }

    static TParseTree KEEP(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`^`), Spacing), name ~ `.`~ `KEEP`)(p);
        }
        else
        {
            if(auto m = tuple(`KEEP`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`^`), Spacing), name ~ `.`~ `KEEP`)(p);
                memo[tuple(`KEEP`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree KEEP(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`^`), Spacing), name ~ `.`~ `KEEP`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`^`), Spacing), name ~ `.`~ `KEEP`)(TParseTree("", false,[], s));
        }
    }
    static string KEEP(GetName g)
    {
        return name ~ `.`~ `KEEP`;
    }

    static TParseTree DROP(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`;`), Spacing), name ~ `.`~ `DROP`)(p);
        }
        else
        {
            if(auto m = tuple(`DROP`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`;`), Spacing), name ~ `.`~ `DROP`)(p);
                memo[tuple(`DROP`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DROP(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`;`), Spacing), name ~ `.`~ `DROP`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`;`), Spacing), name ~ `.`~ `DROP`)(TParseTree("", false,[], s));
        }
    }
    static string DROP(GetName g)
    {
        return name ~ `.`~ `DROP`;
    }

    static TParseTree PROPAGATE(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`%`), Spacing), name ~ `.`~ `PROPAGATE`)(p);
        }
        else
        {
            if(auto m = tuple(`PROPAGATE`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`%`), Spacing), name ~ `.`~ `PROPAGATE`)(p);
                memo[tuple(`PROPAGATE`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PROPAGATE(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`%`), Spacing), name ~ `.`~ `PROPAGATE`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`%`), Spacing), name ~ `.`~ `PROPAGATE`)(TParseTree("", false,[], s));
        }
    }
    static string PROPAGATE(GetName g)
    {
        return name ~ `.`~ `PROPAGATE`;
    }

    static TParseTree OPTION(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`?`), Spacing), name ~ `.`~ `OPTION`)(p);
        }
        else
        {
            if(auto m = tuple(`OPTION`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`?`), Spacing), name ~ `.`~ `OPTION`)(p);
                memo[tuple(`OPTION`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OPTION(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`?`), Spacing), name ~ `.`~ `OPTION`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`?`), Spacing), name ~ `.`~ `OPTION`)(TParseTree("", false,[], s));
        }
    }
    static string OPTION(GetName g)
    {
        return name ~ `.`~ `OPTION`;
    }

    static TParseTree ZEROORMORE(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`*`), Spacing), name ~ `.`~ `ZEROORMORE`)(p);
        }
        else
        {
            if(auto m = tuple(`ZEROORMORE`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`*`), Spacing), name ~ `.`~ `ZEROORMORE`)(p);
                memo[tuple(`ZEROORMORE`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ZEROORMORE(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`*`), Spacing), name ~ `.`~ `ZEROORMORE`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`*`), Spacing), name ~ `.`~ `ZEROORMORE`)(TParseTree("", false,[], s));
        }
    }
    static string ZEROORMORE(GetName g)
    {
        return name ~ `.`~ `ZEROORMORE`;
    }

    static TParseTree ONEORMORE(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`+`), Spacing), name ~ `.`~ `ONEORMORE`)(p);
        }
        else
        {
            if(auto m = tuple(`ONEORMORE`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`+`), Spacing), name ~ `.`~ `ONEORMORE`)(p);
                memo[tuple(`ONEORMORE`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ONEORMORE(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`+`), Spacing), name ~ `.`~ `ONEORMORE`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`+`), Spacing), name ~ `.`~ `ONEORMORE`)(TParseTree("", false,[], s));
        }
    }
    static string ONEORMORE(GetName g)
    {
        return name ~ `.`~ `ONEORMORE`;
    }

    static TParseTree ACTIONOPEN(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`{`), Spacing), name ~ `.`~ `ACTIONOPEN`)(p);
        }
        else
        {
            if(auto m = tuple(`ACTIONOPEN`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`{`), Spacing), name ~ `.`~ `ACTIONOPEN`)(p);
                memo[tuple(`ACTIONOPEN`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ACTIONOPEN(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`{`), Spacing), name ~ `.`~ `ACTIONOPEN`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`{`), Spacing), name ~ `.`~ `ACTIONOPEN`)(TParseTree("", false,[], s));
        }
    }
    static string ACTIONOPEN(GetName g)
    {
        return name ~ `.`~ `ACTIONOPEN`;
    }

    static TParseTree ACTIONCLOSE(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`}`), Spacing), name ~ `.`~ `ACTIONCLOSE`)(p);
        }
        else
        {
            if(auto m = tuple(`ACTIONCLOSE`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`}`), Spacing), name ~ `.`~ `ACTIONCLOSE`)(p);
                memo[tuple(`ACTIONCLOSE`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ACTIONCLOSE(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`}`), Spacing), name ~ `.`~ `ACTIONCLOSE`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`}`), Spacing), name ~ `.`~ `ACTIONCLOSE`)(TParseTree("", false,[], s));
        }
    }
    static string ACTIONCLOSE(GetName g)
    {
        return name ~ `.`~ `ACTIONCLOSE`;
    }

    static TParseTree SEPARATOR(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`,`), Spacing), name ~ `.`~ `SEPARATOR`)(p);
        }
        else
        {
            if(auto m = tuple(`SEPARATOR`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`,`), Spacing), name ~ `.`~ `SEPARATOR`)(p);
                memo[tuple(`SEPARATOR`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SEPARATOR(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`,`), Spacing), name ~ `.`~ `SEPARATOR`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`,`), Spacing), name ~ `.`~ `SEPARATOR`)(TParseTree("", false,[], s));
        }
    }
    static string SEPARATOR(GetName g)
    {
        return name ~ `.`~ `SEPARATOR`;
    }

    static TParseTree ASSIGN(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`=`), Spacing), name ~ `.`~ `ASSIGN`)(p);
        }
        else
        {
            if(auto m = tuple(`ASSIGN`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`=`), Spacing), name ~ `.`~ `ASSIGN`)(p);
                memo[tuple(`ASSIGN`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ASSIGN(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`=`), Spacing), name ~ `.`~ `ASSIGN`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`=`), Spacing), name ~ `.`~ `ASSIGN`)(TParseTree("", false,[], s));
        }
    }
    static string ASSIGN(GetName g)
    {
        return name ~ `.`~ `ASSIGN`;
    }

    static TParseTree NAMESEP(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!(`.`), name ~ `.`~ `NAMESEP`)(p);
        }
        else
        {
            if(auto m = tuple(`NAMESEP`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.literal!(`.`), name ~ `.`~ `NAMESEP`)(p);
                memo[tuple(`NAMESEP`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NAMESEP(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!(`.`), name ~ `.`~ `NAMESEP`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.literal!(`.`), name ~ `.`~ `NAMESEP`)(TParseTree("", false,[], s));
        }
    }
    static string NAMESEP(GetName g)
    {
        return name ~ `.`~ `NAMESEP`;
    }

    static TParseTree OPEN(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`(`), Spacing), name ~ `.`~ `OPEN`)(p);
        }
        else
        {
            if(auto m = tuple(`OPEN`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`(`), Spacing), name ~ `.`~ `OPEN`)(p);
                memo[tuple(`OPEN`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OPEN(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`(`), Spacing), name ~ `.`~ `OPEN`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`(`), Spacing), name ~ `.`~ `OPEN`)(TParseTree("", false,[], s));
        }
    }
    static string OPEN(GetName g)
    {
        return name ~ `.`~ `OPEN`;
    }

    static TParseTree CLOSE(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`)`), Spacing), name ~ `.`~ `CLOSE`)(p);
        }
        else
        {
            if(auto m = tuple(`CLOSE`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`)`), Spacing), name ~ `.`~ `CLOSE`)(p);
                memo[tuple(`CLOSE`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CLOSE(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`)`), Spacing), name ~ `.`~ `CLOSE`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`)`), Spacing), name ~ `.`~ `CLOSE`)(TParseTree("", false,[], s));
        }
    }
    static string CLOSE(GetName g)
    {
        return name ~ `.`~ `CLOSE`;
    }

    static TParseTree ANY(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`.`), Spacing), name ~ `.`~ `ANY`)(p);
        }
        else
        {
            if(auto m = tuple(`ANY`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`.`), Spacing), name ~ `.`~ `ANY`)(p);
                memo[tuple(`ANY`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ANY(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`.`), Spacing), name ~ `.`~ `ANY`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`.`), Spacing), name ~ `.`~ `ANY`)(TParseTree("", false,[], s));
        }
    }
    static string ANY(GetName g)
    {
        return name ~ `.`~ `ANY`;
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), name ~ `.`~ `Spacing`)(p);
        }
        else
        {
            if(auto m = tuple(`Spacing`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), name ~ `.`~ `Spacing`)(p);
                memo[tuple(`Spacing`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), name ~ `.`~ `Spacing`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), name ~ `.`~ `Spacing`)(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return name ~ `.`~ `Spacing`;
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`#`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), name ~ `.`~ `Comment`)(p);
        }
        else
        {
            if(auto m = tuple(`Comment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`#`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), name ~ `.`~ `Comment`)(p);
                memo[tuple(`Comment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`#`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), name ~ `.`~ `Comment`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`#`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), name ~ `.`~ `Comment`)(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return name ~ `.`~ `Comment`;
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
        if(__ctfe)
        {
            return Pegged(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            memo = null;
            return Pegged(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "Pegged";
    }

    }
}

alias GenericPegged!(ParseTree).Pegged Pegged;

