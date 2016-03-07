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
                / CILiteral
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

Literal      <- quote       ~(!quote Char)*       quote       !'i' Spacing
              / doublequote ~(!doublequote Char)* doublequote !'i' Spacing
CILiteral    <- quote       ~(!quote Char)*       quote       :'i' Spacing
              / doublequote ~(!doublequote Char)* doublequote :'i' Spacing
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

Arrow        <- LEFTARROW / FUSEARROW / DISCARDARROW / KEEPARROW / DROPARROW / PROPAGATEARROW / ACTIONARROW / SPACEARROW
LEFTARROW    <- '<-' Spacing
FUSEARROW    <- '<~' Spacing
DISCARDARROW <- '<:' Spacing
KEEPARROW    <- '<^' Spacing
DROPARROW    <- '<;' Spacing
PROPAGATEARROW <- '<%' Spacing
SPACEARROW   <- '<' Spacing
ACTIONARROW  <- '<' Action Spacing

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
Space        <- spacing / "\\t" / "\\n" / "\\r"

# Action Rule
Action      <- :ACTIONOPEN Spacing ((Lambda / qualifiedIdentifier)
(:SEPARATOR (Lambda / qualifiedIdentifier))*) Spacing :ACTIONCLOSE
Lambda      <~ (!(ACTIONCLOSE/SEPARATOR) (LambdaItems / NestedList('{',LambdaItems,'}') / .))*

LambdaItems <- ~DComment / ~DString / ~DParamList
DString     <- WYSString / DBQString / TKNString / DLMString

WYSString   <- 'r' doublequote (!doublequote .)* doublequote /
               backquote (!backquote .)* backquote

DBQString   <- doublequote (!doublequote Char)* doublequote

TKNString   <- (&'q{' ('q' NestedList('{',DString,'}')))

DLMString   <- ('q' doublequote) ( (&'{' NestedList('{',DString,'}'))
                                 / (&'[' NestedList('[',DString,']'))
                                 / (&'(' NestedList('(',DString,')'))
                                 / (&'<' NestedList('<',DString,'>'))
                                 ) doublequote

DComment             <- DLineComment / DBlockComment / DNestingBlockComment

DLineComment         <- "//" (!endOfLine .)* endOfLine
DBlockComment        <- "/*" (!"*/" .)* "*/"
DNestingBlockComment <- NestedList("/+","+/")

DParamList <- NestedList('(',')')

# Linear nested lists with and without special items
NestedList(L,Items,R)   <- ^L ( !(L/R/Items) . )* ( Items
                                                  / NestedList(L,Items,R)
                                                  / ( !(L/R/Items) . )*
                                                  )* ( !(L/R/Items) . )* ^R

NestedList(L,R) <- ^L ( !(L/R) . )* (NestedList(L,R) / ( !(L/R) . )*)* ( !(L/R) . )* ^R


+/
module pegged.parser;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericPegged(TParseTree)
{
    import pegged.dynamic.grammar;
    struct Pegged
    {
    enum name = "Pegged";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    static this()
    {
        rules["Grammar"] = toDelegate(&Grammar);
        rules["Definition"] = toDelegate(&Definition);
        rules["Expression"] = toDelegate(&Expression);
        rules["Sequence"] = toDelegate(&Sequence);
        rules["Prefix"] = toDelegate(&Prefix);
        rules["Suffix"] = toDelegate(&Suffix);
        rules["Primary"] = toDelegate(&Primary);
        rules["Identifier"] = toDelegate(&Identifier);
        rules["GrammarName"] = toDelegate(&GrammarName);
        rules["LhsName"] = toDelegate(&LhsName);
        rules["RhsName"] = toDelegate(&RhsName);
        rules["ParamList"] = toDelegate(&ParamList);
        rules["Param"] = toDelegate(&Param);
        rules["DefaultParam"] = toDelegate(&DefaultParam);
        rules["SingleParam"] = toDelegate(&SingleParam);
        rules["ArgList"] = toDelegate(&ArgList);
        rules["Literal"] = toDelegate(&Literal);
        rules["CILiteral"] = toDelegate(&CILiteral);
        rules["CharClass"] = toDelegate(&CharClass);
        rules["CharRange"] = toDelegate(&CharRange);
        rules["Char"] = toDelegate(&Char);
        rules["Arrow"] = toDelegate(&Arrow);
        rules["LEFTARROW"] = toDelegate(&LEFTARROW);
        rules["FUSEARROW"] = toDelegate(&FUSEARROW);
        rules["DISCARDARROW"] = toDelegate(&DISCARDARROW);
        rules["KEEPARROW"] = toDelegate(&KEEPARROW);
        rules["DROPARROW"] = toDelegate(&DROPARROW);
        rules["PROPAGATEARROW"] = toDelegate(&PROPAGATEARROW);
        rules["SPACEARROW"] = toDelegate(&SPACEARROW);
        rules["ACTIONARROW"] = toDelegate(&ACTIONARROW);
        rules["OR"] = toDelegate(&OR);
        rules["POS"] = toDelegate(&POS);
        rules["NEG"] = toDelegate(&NEG);
        rules["FUSE"] = toDelegate(&FUSE);
        rules["DISCARD"] = toDelegate(&DISCARD);
        rules["KEEP"] = toDelegate(&KEEP);
        rules["DROP"] = toDelegate(&DROP);
        rules["PROPAGATE"] = toDelegate(&PROPAGATE);
        rules["OPTION"] = toDelegate(&OPTION);
        rules["ZEROORMORE"] = toDelegate(&ZEROORMORE);
        rules["ONEORMORE"] = toDelegate(&ONEORMORE);
        rules["ACTIONOPEN"] = toDelegate(&ACTIONOPEN);
        rules["ACTIONCLOSE"] = toDelegate(&ACTIONCLOSE);
        rules["SEPARATOR"] = toDelegate(&SEPARATOR);
        rules["ASSIGN"] = toDelegate(&ASSIGN);
        rules["NAMESEP"] = toDelegate(&NAMESEP);
        rules["OPEN"] = toDelegate(&OPEN);
        rules["CLOSE"] = toDelegate(&CLOSE);
        rules["ANY"] = toDelegate(&ANY);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(name,rule; dg.rules)
        {
            if (name != "Spacing")
                rules[name] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
        return s.startsWith("Pegged.");
    }
    mixin decimateTree;

    static TParseTree Grammar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), "Pegged.Grammar")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), "Pegged.Grammar"), "Grammar")(p);
        }
    }
    static TParseTree Grammar(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), "Pegged.Grammar")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Spacing, GrammarName, pegged.peg.oneOrMore!(Definition), pegged.peg.discard!(eoi)), "Pegged.Grammar"), "Grammar")(TParseTree("", false,[], s));
        }
    }
    static string Grammar(GetName g)
    {
        return "Pegged.Grammar";
    }

    static TParseTree Definition(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(LhsName, Arrow, Expression), "Pegged.Definition")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(LhsName, Arrow, Expression), "Pegged.Definition"), "Definition")(p);
        }
    }
    static TParseTree Definition(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(LhsName, Arrow, Expression), "Pegged.Definition")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(LhsName, Arrow, Expression), "Pegged.Definition"), "Definition")(TParseTree("", false,[], s));
        }
    }
    static string Definition(GetName g)
    {
        return "Pegged.Definition";
    }

    static TParseTree Expression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), "Pegged.Expression")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), "Pegged.Expression"), "Expression")(p);
        }
    }
    static TParseTree Expression(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), "Pegged.Expression")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.option!(OR)), Sequence, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(OR), Sequence))), "Pegged.Expression"), "Expression")(TParseTree("", false,[], s));
        }
    }
    static string Expression(GetName g)
    {
        return "Pegged.Expression";
    }

    static TParseTree Sequence(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(Prefix), "Pegged.Sequence")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(Prefix), "Pegged.Sequence"), "Sequence")(p);
        }
    }
    static TParseTree Sequence(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(Prefix), "Pegged.Sequence")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(Prefix), "Pegged.Sequence"), "Sequence")(TParseTree("", false,[], s));
        }
    }
    static string Sequence(GetName g)
    {
        return "Pegged.Sequence";
    }

    static TParseTree Prefix(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), "Pegged.Prefix")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), "Pegged.Prefix"), "Prefix")(p);
        }
    }
    static TParseTree Prefix(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), "Pegged.Prefix")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(POS, NEG, FUSE, DISCARD, KEEP, DROP, PROPAGATE)), Suffix), "Pegged.Prefix"), "Prefix")(TParseTree("", false,[], s));
        }
    }
    static string Prefix(GetName g)
    {
        return "Pegged.Prefix";
    }

    static TParseTree Suffix(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Pegged.Suffix")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Pegged.Suffix"), "Suffix")(p);
        }
    }
    static TParseTree Suffix(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Pegged.Suffix")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Primary, pegged.peg.zeroOrMore!(pegged.peg.or!(OPTION, ZEROORMORE, ONEORMORE, Action))), "Pegged.Suffix"), "Suffix")(TParseTree("", false,[], s));
        }
    }
    static string Suffix(GetName g)
    {
        return "Pegged.Suffix";
    }

    static TParseTree Primary(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CILiteral, CharClass, ANY)), "Pegged.Primary")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CILiteral, CharClass, ANY)), "Pegged.Primary"), "Primary")(p);
        }
    }
    static TParseTree Primary(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CILiteral, CharClass, ANY)), "Pegged.Primary")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(LhsName, Arrow)), pegged.peg.or!(RhsName, pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.discard!(CLOSE)), Literal, CILiteral, CharClass, ANY)), "Pegged.Primary"), "Primary")(TParseTree("", false,[], s));
        }
    }
    static string Primary(GetName g)
    {
        return "Pegged.Primary";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(identifier, "Pegged.Identifier")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(identifier, "Pegged.Identifier"), "Identifier")(p);
        }
    }
    static TParseTree Identifier(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(identifier, "Pegged.Identifier")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(identifier, "Pegged.Identifier"), "Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "Pegged.Identifier";
    }

    static TParseTree GrammarName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(":")), Spacing), "Pegged.GrammarName")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(":")), Spacing), "Pegged.GrammarName"), "GrammarName")(p);
        }
    }
    static TParseTree GrammarName(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(":")), Spacing), "Pegged.GrammarName")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing, pegged.peg.discard!(pegged.peg.literal!(":")), Spacing), "Pegged.GrammarName"), "GrammarName")(TParseTree("", false,[], s));
        }
    }
    static string GrammarName(GetName g)
    {
        return "Pegged.GrammarName";
    }

    static TParseTree LhsName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), "Pegged.LhsName")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), "Pegged.LhsName"), "LhsName")(p);
        }
    }
    static TParseTree LhsName(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), "Pegged.LhsName")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ParamList), Spacing), "Pegged.LhsName"), "LhsName")(TParseTree("", false,[], s));
        }
    }
    static string LhsName(GetName g)
    {
        return "Pegged.LhsName";
    }

    static TParseTree RhsName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), "Pegged.RhsName")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), "Pegged.RhsName"), "RhsName")(p);
        }
    }
    static TParseTree RhsName(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), "Pegged.RhsName")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, pegged.peg.option!(ArgList), pegged.peg.zeroOrMore!(pegged.peg.and!(NAMESEP, Identifier, pegged.peg.option!(ArgList))), Spacing), "Pegged.RhsName"), "RhsName")(TParseTree("", false,[], s));
        }
    }
    static string RhsName(GetName g)
    {
        return "Pegged.RhsName";
    }

    static TParseTree ParamList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), "Pegged.ParamList")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), "Pegged.ParamList"), "ParamList")(p);
        }
    }
    static TParseTree ParamList(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), "Pegged.ParamList")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Param, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Param)), pegged.peg.discard!(CLOSE)), "Pegged.ParamList"), "ParamList")(TParseTree("", false,[], s));
        }
    }
    static string ParamList(GetName g)
    {
        return "Pegged.ParamList";
    }

    static TParseTree Param(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DefaultParam, SingleParam), "Pegged.Param")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.or!(DefaultParam, SingleParam), "Pegged.Param"), "Param")(p);
        }
    }
    static TParseTree Param(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.or!(DefaultParam, SingleParam), "Pegged.Param")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(DefaultParam, SingleParam), "Pegged.Param"), "Param")(TParseTree("", false,[], s));
        }
    }
    static string Param(GetName g)
    {
        return "Pegged.Param";
    }

    static TParseTree DefaultParam(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), "Pegged.DefaultParam")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), "Pegged.DefaultParam"), "DefaultParam")(p);
        }
    }
    static TParseTree DefaultParam(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), "Pegged.DefaultParam")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing, pegged.peg.discard!(ASSIGN), Expression), "Pegged.DefaultParam"), "DefaultParam")(TParseTree("", false,[], s));
        }
    }
    static string DefaultParam(GetName g)
    {
        return "Pegged.DefaultParam";
    }

    static TParseTree SingleParam(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing), "Pegged.SingleParam")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing), "Pegged.SingleParam"), "SingleParam")(p);
        }
    }
    static TParseTree SingleParam(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing), "Pegged.SingleParam")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Identifier, Spacing), "Pegged.SingleParam"), "SingleParam")(TParseTree("", false,[], s));
        }
    }
    static string SingleParam(GetName g)
    {
        return "Pegged.SingleParam";
    }

    static TParseTree ArgList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), "Pegged.ArgList")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), "Pegged.ArgList"), "ArgList")(p);
        }
    }
    static TParseTree ArgList(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), "Pegged.ArgList")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(OPEN), Expression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), Expression)), pegged.peg.discard!(CLOSE)), "Pegged.ArgList"), "ArgList")(TParseTree("", false,[], s));
        }
    }
    static string ArgList(GetName g)
    {
        return "Pegged.ArgList";
    }

    static TParseTree Literal(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing)), "Pegged.Literal")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing)), "Pegged.Literal"), "Literal")(p);
        }
    }
    static TParseTree Literal(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing)), "Pegged.Literal")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.negLookahead!(pegged.peg.literal!("i")), Spacing)), "Pegged.Literal"), "Literal")(TParseTree("", false,[], s));
        }
    }
    static string Literal(GetName g)
    {
        return "Pegged.Literal";
    }

    static TParseTree CILiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing)), "Pegged.CILiteral")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing)), "Pegged.CILiteral"), "CILiteral")(p);
        }
    }
    static TParseTree CILiteral(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing)), "Pegged.CILiteral")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), Char))), quote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing), pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char))), doublequote, pegged.peg.discard!(pegged.peg.literal!("i")), Spacing)), "Pegged.CILiteral"), "CILiteral")(TParseTree("", false,[], s));
        }
    }
    static string CILiteral(GetName g)
    {
        return "Pegged.CILiteral";
    }

    static TParseTree CharClass(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), CharRange)), pegged.peg.discard!(pegged.peg.literal!("]")), Spacing), "Pegged.CharClass")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), CharRange)), pegged.peg.discard!(pegged.peg.literal!("]")), Spacing), "Pegged.CharClass"), "CharClass")(p);
        }
    }
    static TParseTree CharClass(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), CharRange)), pegged.peg.discard!(pegged.peg.literal!("]")), Spacing), "Pegged.CharClass")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("]")), CharRange)), pegged.peg.discard!(pegged.peg.literal!("]")), Spacing), "Pegged.CharClass"), "CharClass")(TParseTree("", false,[], s));
        }
    }
    static string CharClass(GetName g)
    {
        return "Pegged.CharClass";
    }

    static TParseTree CharRange(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!("-"), Char), Char), "Pegged.CharRange")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!("-"), Char), Char), "Pegged.CharRange"), "CharRange")(p);
        }
    }
    static TParseTree CharRange(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!("-"), Char), Char), "Pegged.CharRange")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(Char, pegged.peg.literal!("-"), Char), Char), "Pegged.CharRange"), "CharRange")(TParseTree("", false,[], s));
        }
    }
    static string CharRange(GetName g)
    {
        return "Pegged.CharRange";
    }

    static TParseTree Char(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "Pegged.Char")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "Pegged.Char"), "Char")(p);
        }
    }
    static TParseTree Char(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "Pegged.Char")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backquote, backslash, pegged.peg.literal!("-"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.or!(pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t")), pegged.peg.and!(pegged.peg.charRange!('0', '2'), pegged.peg.charRange!('0', '7'), pegged.peg.charRange!('0', '7')), pegged.peg.and!(pegged.peg.charRange!('0', '7'), pegged.peg.option!(pegged.peg.charRange!('0', '7'))), pegged.peg.and!(pegged.peg.literal!("x"), hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("u"), hexDigit, hexDigit, hexDigit, hexDigit), pegged.peg.and!(pegged.peg.literal!("U"), hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit, hexDigit))), pegged.peg.any)), "Pegged.Char"), "Char")(TParseTree("", false,[], s));
        }
    }
    static string Char(GetName g)
    {
        return "Pegged.Char";
    }

    static TParseTree Arrow(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, ACTIONARROW, SPACEARROW), "Pegged.Arrow")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, ACTIONARROW, SPACEARROW), "Pegged.Arrow"), "Arrow")(p);
        }
    }
    static TParseTree Arrow(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, ACTIONARROW, SPACEARROW), "Pegged.Arrow")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(LEFTARROW, FUSEARROW, DISCARDARROW, KEEPARROW, DROPARROW, PROPAGATEARROW, ACTIONARROW, SPACEARROW), "Pegged.Arrow"), "Arrow")(TParseTree("", false,[], s));
        }
    }
    static string Arrow(GetName g)
    {
        return "Pegged.Arrow";
    }

    static TParseTree LEFTARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<-"), Spacing), "Pegged.LEFTARROW")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<-"), Spacing), "Pegged.LEFTARROW"), "LEFTARROW")(p);
        }
    }
    static TParseTree LEFTARROW(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<-"), Spacing), "Pegged.LEFTARROW")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<-"), Spacing), "Pegged.LEFTARROW"), "LEFTARROW")(TParseTree("", false,[], s));
        }
    }
    static string LEFTARROW(GetName g)
    {
        return "Pegged.LEFTARROW";
    }

    static TParseTree FUSEARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<~"), Spacing), "Pegged.FUSEARROW")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<~"), Spacing), "Pegged.FUSEARROW"), "FUSEARROW")(p);
        }
    }
    static TParseTree FUSEARROW(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<~"), Spacing), "Pegged.FUSEARROW")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<~"), Spacing), "Pegged.FUSEARROW"), "FUSEARROW")(TParseTree("", false,[], s));
        }
    }
    static string FUSEARROW(GetName g)
    {
        return "Pegged.FUSEARROW";
    }

    static TParseTree DISCARDARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<:"), Spacing), "Pegged.DISCARDARROW")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<:"), Spacing), "Pegged.DISCARDARROW"), "DISCARDARROW")(p);
        }
    }
    static TParseTree DISCARDARROW(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<:"), Spacing), "Pegged.DISCARDARROW")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<:"), Spacing), "Pegged.DISCARDARROW"), "DISCARDARROW")(TParseTree("", false,[], s));
        }
    }
    static string DISCARDARROW(GetName g)
    {
        return "Pegged.DISCARDARROW";
    }

    static TParseTree KEEPARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<^"), Spacing), "Pegged.KEEPARROW")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<^"), Spacing), "Pegged.KEEPARROW"), "KEEPARROW")(p);
        }
    }
    static TParseTree KEEPARROW(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<^"), Spacing), "Pegged.KEEPARROW")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<^"), Spacing), "Pegged.KEEPARROW"), "KEEPARROW")(TParseTree("", false,[], s));
        }
    }
    static string KEEPARROW(GetName g)
    {
        return "Pegged.KEEPARROW";
    }

    static TParseTree DROPARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<;"), Spacing), "Pegged.DROPARROW")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<;"), Spacing), "Pegged.DROPARROW"), "DROPARROW")(p);
        }
    }
    static TParseTree DROPARROW(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<;"), Spacing), "Pegged.DROPARROW")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<;"), Spacing), "Pegged.DROPARROW"), "DROPARROW")(TParseTree("", false,[], s));
        }
    }
    static string DROPARROW(GetName g)
    {
        return "Pegged.DROPARROW";
    }

    static TParseTree PROPAGATEARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<%"), Spacing), "Pegged.PROPAGATEARROW")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<%"), Spacing), "Pegged.PROPAGATEARROW"), "PROPAGATEARROW")(p);
        }
    }
    static TParseTree PROPAGATEARROW(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<%"), Spacing), "Pegged.PROPAGATEARROW")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<%"), Spacing), "Pegged.PROPAGATEARROW"), "PROPAGATEARROW")(TParseTree("", false,[], s));
        }
    }
    static string PROPAGATEARROW(GetName g)
    {
        return "Pegged.PROPAGATEARROW";
    }

    static TParseTree SPACEARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spacing), "Pegged.SPACEARROW")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spacing), "Pegged.SPACEARROW"), "SPACEARROW")(p);
        }
    }
    static TParseTree SPACEARROW(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spacing), "Pegged.SPACEARROW")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Spacing), "Pegged.SPACEARROW"), "SPACEARROW")(TParseTree("", false,[], s));
        }
    }
    static string SPACEARROW(GetName g)
    {
        return "Pegged.SPACEARROW";
    }

    static TParseTree ACTIONARROW(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Action, Spacing), "Pegged.ACTIONARROW")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Action, Spacing), "Pegged.ACTIONARROW"), "ACTIONARROW")(p);
        }
    }
    static TParseTree ACTIONARROW(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Action, Spacing), "Pegged.ACTIONARROW")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("<"), Action, Spacing), "Pegged.ACTIONARROW"), "ACTIONARROW")(TParseTree("", false,[], s));
        }
    }
    static string ACTIONARROW(GetName g)
    {
        return "Pegged.ACTIONARROW";
    }

    static TParseTree OR(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/"), Spacing), "Pegged.OR")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/"), Spacing), "Pegged.OR"), "OR")(p);
        }
    }
    static TParseTree OR(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/"), Spacing), "Pegged.OR")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/"), Spacing), "Pegged.OR"), "OR")(TParseTree("", false,[], s));
        }
    }
    static string OR(GetName g)
    {
        return "Pegged.OR";
    }

    static TParseTree POS(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Spacing), "Pegged.POS")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Spacing), "Pegged.POS"), "POS")(p);
        }
    }
    static TParseTree POS(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Spacing), "Pegged.POS")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("&"), Spacing), "Pegged.POS"), "POS")(TParseTree("", false,[], s));
        }
    }
    static string POS(GetName g)
    {
        return "Pegged.POS";
    }

    static TParseTree NEG(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), Spacing), "Pegged.NEG")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), Spacing), "Pegged.NEG"), "NEG")(p);
        }
    }
    static TParseTree NEG(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), Spacing), "Pegged.NEG")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("!"), Spacing), "Pegged.NEG"), "NEG")(TParseTree("", false,[], s));
        }
    }
    static string NEG(GetName g)
    {
        return "Pegged.NEG";
    }

    static TParseTree FUSE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~"), Spacing), "Pegged.FUSE")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~"), Spacing), "Pegged.FUSE"), "FUSE")(p);
        }
    }
    static TParseTree FUSE(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~"), Spacing), "Pegged.FUSE")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("~"), Spacing), "Pegged.FUSE"), "FUSE")(TParseTree("", false,[], s));
        }
    }
    static string FUSE(GetName g)
    {
        return "Pegged.FUSE";
    }

    static TParseTree DISCARD(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(":"), Spacing), "Pegged.DISCARD")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(":"), Spacing), "Pegged.DISCARD"), "DISCARD")(p);
        }
    }
    static TParseTree DISCARD(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(":"), Spacing), "Pegged.DISCARD")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(":"), Spacing), "Pegged.DISCARD"), "DISCARD")(TParseTree("", false,[], s));
        }
    }
    static string DISCARD(GetName g)
    {
        return "Pegged.DISCARD";
    }

    static TParseTree KEEP(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("^"), Spacing), "Pegged.KEEP")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("^"), Spacing), "Pegged.KEEP"), "KEEP")(p);
        }
    }
    static TParseTree KEEP(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("^"), Spacing), "Pegged.KEEP")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("^"), Spacing), "Pegged.KEEP"), "KEEP")(TParseTree("", false,[], s));
        }
    }
    static string KEEP(GetName g)
    {
        return "Pegged.KEEP";
    }

    static TParseTree DROP(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(";"), Spacing), "Pegged.DROP")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(";"), Spacing), "Pegged.DROP"), "DROP")(p);
        }
    }
    static TParseTree DROP(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(";"), Spacing), "Pegged.DROP")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(";"), Spacing), "Pegged.DROP"), "DROP")(TParseTree("", false,[], s));
        }
    }
    static string DROP(GetName g)
    {
        return "Pegged.DROP";
    }

    static TParseTree PROPAGATE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Spacing), "Pegged.PROPAGATE")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Spacing), "Pegged.PROPAGATE"), "PROPAGATE")(p);
        }
    }
    static TParseTree PROPAGATE(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Spacing), "Pegged.PROPAGATE")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("%"), Spacing), "Pegged.PROPAGATE"), "PROPAGATE")(TParseTree("", false,[], s));
        }
    }
    static string PROPAGATE(GetName g)
    {
        return "Pegged.PROPAGATE";
    }

    static TParseTree OPTION(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("?"), Spacing), "Pegged.OPTION")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("?"), Spacing), "Pegged.OPTION"), "OPTION")(p);
        }
    }
    static TParseTree OPTION(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("?"), Spacing), "Pegged.OPTION")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("?"), Spacing), "Pegged.OPTION"), "OPTION")(TParseTree("", false,[], s));
        }
    }
    static string OPTION(GetName g)
    {
        return "Pegged.OPTION";
    }

    static TParseTree ZEROORMORE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("*"), Spacing), "Pegged.ZEROORMORE")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("*"), Spacing), "Pegged.ZEROORMORE"), "ZEROORMORE")(p);
        }
    }
    static TParseTree ZEROORMORE(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("*"), Spacing), "Pegged.ZEROORMORE")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("*"), Spacing), "Pegged.ZEROORMORE"), "ZEROORMORE")(TParseTree("", false,[], s));
        }
    }
    static string ZEROORMORE(GetName g)
    {
        return "Pegged.ZEROORMORE";
    }

    static TParseTree ONEORMORE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("+"), Spacing), "Pegged.ONEORMORE")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("+"), Spacing), "Pegged.ONEORMORE"), "ONEORMORE")(p);
        }
    }
    static TParseTree ONEORMORE(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("+"), Spacing), "Pegged.ONEORMORE")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("+"), Spacing), "Pegged.ONEORMORE"), "ONEORMORE")(TParseTree("", false,[], s));
        }
    }
    static string ONEORMORE(GetName g)
    {
        return "Pegged.ONEORMORE";
    }

    static TParseTree ACTIONOPEN(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("{"), Spacing), "Pegged.ACTIONOPEN")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("{"), Spacing), "Pegged.ACTIONOPEN"), "ACTIONOPEN")(p);
        }
    }
    static TParseTree ACTIONOPEN(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("{"), Spacing), "Pegged.ACTIONOPEN")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("{"), Spacing), "Pegged.ACTIONOPEN"), "ACTIONOPEN")(TParseTree("", false,[], s));
        }
    }
    static string ACTIONOPEN(GetName g)
    {
        return "Pegged.ACTIONOPEN";
    }

    static TParseTree ACTIONCLOSE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("}"), Spacing), "Pegged.ACTIONCLOSE")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("}"), Spacing), "Pegged.ACTIONCLOSE"), "ACTIONCLOSE")(p);
        }
    }
    static TParseTree ACTIONCLOSE(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("}"), Spacing), "Pegged.ACTIONCLOSE")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("}"), Spacing), "Pegged.ACTIONCLOSE"), "ACTIONCLOSE")(TParseTree("", false,[], s));
        }
    }
    static string ACTIONCLOSE(GetName g)
    {
        return "Pegged.ACTIONCLOSE";
    }

    static TParseTree SEPARATOR(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(","), Spacing), "Pegged.SEPARATOR")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(","), Spacing), "Pegged.SEPARATOR"), "SEPARATOR")(p);
        }
    }
    static TParseTree SEPARATOR(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(","), Spacing), "Pegged.SEPARATOR")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(","), Spacing), "Pegged.SEPARATOR"), "SEPARATOR")(TParseTree("", false,[], s));
        }
    }
    static string SEPARATOR(GetName g)
    {
        return "Pegged.SEPARATOR";
    }

    static TParseTree ASSIGN(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("="), Spacing), "Pegged.ASSIGN")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("="), Spacing), "Pegged.ASSIGN"), "ASSIGN")(p);
        }
    }
    static TParseTree ASSIGN(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("="), Spacing), "Pegged.ASSIGN")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("="), Spacing), "Pegged.ASSIGN"), "ASSIGN")(TParseTree("", false,[], s));
        }
    }
    static string ASSIGN(GetName g)
    {
        return "Pegged.ASSIGN";
    }

    static TParseTree NAMESEP(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("."), "Pegged.NAMESEP")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("."), "Pegged.NAMESEP"), "NAMESEP")(p);
        }
    }
    static TParseTree NAMESEP(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.literal!("."), "Pegged.NAMESEP")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("."), "Pegged.NAMESEP"), "NAMESEP")(TParseTree("", false,[], s));
        }
    }
    static string NAMESEP(GetName g)
    {
        return "Pegged.NAMESEP";
    }

    static TParseTree OPEN(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), Spacing), "Pegged.OPEN")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), Spacing), "Pegged.OPEN"), "OPEN")(p);
        }
    }
    static TParseTree OPEN(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), Spacing), "Pegged.OPEN")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), Spacing), "Pegged.OPEN"), "OPEN")(TParseTree("", false,[], s));
        }
    }
    static string OPEN(GetName g)
    {
        return "Pegged.OPEN";
    }

    static TParseTree CLOSE(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), Spacing), "Pegged.CLOSE")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), Spacing), "Pegged.CLOSE"), "CLOSE")(p);
        }
    }
    static TParseTree CLOSE(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), Spacing), "Pegged.CLOSE")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), Spacing), "Pegged.CLOSE"), "CLOSE")(TParseTree("", false,[], s));
        }
    }
    static string CLOSE(GetName g)
    {
        return "Pegged.CLOSE";
    }

    static TParseTree ANY(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("."), Spacing), "Pegged.ANY")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("."), Spacing), "Pegged.ANY"), "ANY")(p);
        }
    }
    static TParseTree ANY(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("."), Spacing), "Pegged.ANY")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("."), Spacing), "Pegged.ANY"), "ANY")(TParseTree("", false,[], s));
        }
    }
    static string ANY(GetName g)
    {
        return "Pegged.ANY";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Pegged.Spacing")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Pegged.Spacing"), "Spacing")(p);
        }
    }
    static TParseTree Spacing(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Pegged.Spacing")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "Pegged.Spacing"), "Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "Pegged.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("#"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), "Pegged.Comment")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("#"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), "Pegged.Comment"), "Comment")(p);
        }
    }
    static TParseTree Comment(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("#"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), "Pegged.Comment")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("#"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), pegged.peg.discard!(eol)), "Pegged.Comment"), "Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "Pegged.Comment";
    }

    static TParseTree Space(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(spacing, pegged.peg.literal!("\\t"), pegged.peg.literal!("\\n"), pegged.peg.literal!("\\r")), "Pegged.Space")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.or!(spacing, pegged.peg.literal!("\\t"), pegged.peg.literal!("\\n"), pegged.peg.literal!("\\r")), "Pegged.Space"), "Space")(p);
        }
    }
    static TParseTree Space(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.or!(spacing, pegged.peg.literal!("\\t"), pegged.peg.literal!("\\n"), pegged.peg.literal!("\\r")), "Pegged.Space")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(spacing, pegged.peg.literal!("\\t"), pegged.peg.literal!("\\n"), pegged.peg.literal!("\\r")), "Pegged.Space"), "Space")(TParseTree("", false,[], s));
        }
    }
    static string Space(GetName g)
    {
        return "Pegged.Space";
    }

    static TParseTree Action(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), Spacing, pegged.peg.and!(pegged.peg.or!(Lambda, qualifiedIdentifier), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), pegged.peg.or!(Lambda, qualifiedIdentifier)))), Spacing, pegged.peg.discard!(ACTIONCLOSE)), "Pegged.Action")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), Spacing, pegged.peg.and!(pegged.peg.or!(Lambda, qualifiedIdentifier), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), pegged.peg.or!(Lambda, qualifiedIdentifier)))), Spacing, pegged.peg.discard!(ACTIONCLOSE)), "Pegged.Action"), "Action")(p);
        }
    }
    static TParseTree Action(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), Spacing, pegged.peg.and!(pegged.peg.or!(Lambda, qualifiedIdentifier), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), pegged.peg.or!(Lambda, qualifiedIdentifier)))), Spacing, pegged.peg.discard!(ACTIONCLOSE)), "Pegged.Action")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(ACTIONOPEN), Spacing, pegged.peg.and!(pegged.peg.or!(Lambda, qualifiedIdentifier), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(SEPARATOR), pegged.peg.or!(Lambda, qualifiedIdentifier)))), Spacing, pegged.peg.discard!(ACTIONCLOSE)), "Pegged.Action"), "Action")(TParseTree("", false,[], s));
        }
    }
    static string Action(GetName g)
    {
        return "Pegged.Action";
    }

    static TParseTree Lambda(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(ACTIONCLOSE, SEPARATOR)), pegged.peg.or!(LambdaItems, NestedList!(pegged.peg.literal!("{"), LambdaItems, pegged.peg.literal!("}")), pegged.peg.any)))), "Pegged.Lambda")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(ACTIONCLOSE, SEPARATOR)), pegged.peg.or!(LambdaItems, NestedList!(pegged.peg.literal!("{"), LambdaItems, pegged.peg.literal!("}")), pegged.peg.any)))), "Pegged.Lambda"), "Lambda")(p);
        }
    }
    static TParseTree Lambda(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(ACTIONCLOSE, SEPARATOR)), pegged.peg.or!(LambdaItems, NestedList!(pegged.peg.literal!("{"), LambdaItems, pegged.peg.literal!("}")), pegged.peg.any)))), "Pegged.Lambda")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(ACTIONCLOSE, SEPARATOR)), pegged.peg.or!(LambdaItems, NestedList!(pegged.peg.literal!("{"), LambdaItems, pegged.peg.literal!("}")), pegged.peg.any)))), "Pegged.Lambda"), "Lambda")(TParseTree("", false,[], s));
        }
    }
    static string Lambda(GetName g)
    {
        return "Pegged.Lambda";
    }

    static TParseTree LambdaItems(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.fuse!(DComment), pegged.peg.fuse!(DString), pegged.peg.fuse!(DParamList)), "Pegged.LambdaItems")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.fuse!(DComment), pegged.peg.fuse!(DString), pegged.peg.fuse!(DParamList)), "Pegged.LambdaItems"), "LambdaItems")(p);
        }
    }
    static TParseTree LambdaItems(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.fuse!(DComment), pegged.peg.fuse!(DString), pegged.peg.fuse!(DParamList)), "Pegged.LambdaItems")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.fuse!(DComment), pegged.peg.fuse!(DString), pegged.peg.fuse!(DParamList)), "Pegged.LambdaItems"), "LambdaItems")(TParseTree("", false,[], s));
        }
    }
    static string LambdaItems(GetName g)
    {
        return "Pegged.LambdaItems";
    }

    static TParseTree DString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "Pegged.DString")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "Pegged.DString"), "DString")(p);
        }
    }
    static TParseTree DString(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "Pegged.DString")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(WYSString, DBQString, TKNString, DLMString), "Pegged.DString"), "DString")(TParseTree("", false,[], s));
        }
    }
    static string DString(GetName g)
    {
        return "Pegged.DString";
    }

    static TParseTree WYSString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Pegged.WYSString")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Pegged.WYSString"), "WYSString")(p);
        }
    }
    static TParseTree WYSString(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Pegged.WYSString")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote), pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote)), "Pegged.WYSString"), "WYSString")(TParseTree("", false,[], s));
        }
    }
    static string WYSString(GetName g)
    {
        return "Pegged.WYSString";
    }

    static TParseTree DBQString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), "Pegged.DBQString")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), "Pegged.DBQString"), "DBQString")(p);
        }
    }
    static TParseTree DBQString(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), "Pegged.DBQString")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), Char)), doublequote), "Pegged.DBQString"), "DBQString")(TParseTree("", false,[], s));
        }
    }
    static string DBQString(GetName g)
    {
        return "Pegged.DBQString";
    }

    static TParseTree TKNString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}")))), "Pegged.TKNString")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}")))), "Pegged.TKNString"), "TKNString")(p);
        }
    }
    static TParseTree TKNString(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}")))), "Pegged.TKNString")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("q{")), pegged.peg.and!(pegged.peg.literal!("q"), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}")))), "Pegged.TKNString"), "TKNString")(TParseTree("", false,[], s));
        }
    }
    static string TKNString(GetName g)
    {
        return "Pegged.TKNString";
    }

    static TParseTree DLMString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), DString, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), DString, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), DString, pegged.peg.literal!(">")))), doublequote), "Pegged.DLMString")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), DString, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), DString, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), DString, pegged.peg.literal!(">")))), doublequote), "Pegged.DLMString"), "DLMString")(p);
        }
    }
    static TParseTree DLMString(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), DString, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), DString, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), DString, pegged.peg.literal!(">")))), doublequote), "Pegged.DLMString")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("q"), doublequote), pegged.peg.or!(pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("{")), NestedList!(pegged.peg.literal!("{"), DString, pegged.peg.literal!("}"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("[")), NestedList!(pegged.peg.literal!("["), DString, pegged.peg.literal!("]"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("(")), NestedList!(pegged.peg.literal!("("), DString, pegged.peg.literal!(")"))), pegged.peg.and!(pegged.peg.posLookahead!(pegged.peg.literal!("<")), NestedList!(pegged.peg.literal!("<"), DString, pegged.peg.literal!(">")))), doublequote), "Pegged.DLMString"), "DLMString")(TParseTree("", false,[], s));
        }
    }
    static string DLMString(GetName g)
    {
        return "Pegged.DLMString";
    }

    static TParseTree DComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DLineComment, DBlockComment, DNestingBlockComment), "Pegged.DComment")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.or!(DLineComment, DBlockComment, DNestingBlockComment), "Pegged.DComment"), "DComment")(p);
        }
    }
    static TParseTree DComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.or!(DLineComment, DBlockComment, DNestingBlockComment), "Pegged.DComment")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(DLineComment, DBlockComment, DNestingBlockComment), "Pegged.DComment"), "DComment")(TParseTree("", false,[], s));
        }
    }
    static string DComment(GetName g)
    {
        return "Pegged.DComment";
    }

    static TParseTree DLineComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "Pegged.DLineComment")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "Pegged.DLineComment"), "DLineComment")(p);
        }
    }
    static TParseTree DLineComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "Pegged.DLineComment")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "Pegged.DLineComment"), "DLineComment")(TParseTree("", false,[], s));
        }
    }
    static string DLineComment(GetName g)
    {
        return "Pegged.DLineComment";
    }

    static TParseTree DBlockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "Pegged.DBlockComment")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "Pegged.DBlockComment"), "DBlockComment")(p);
        }
    }
    static TParseTree DBlockComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "Pegged.DBlockComment")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "Pegged.DBlockComment"), "DBlockComment")(TParseTree("", false,[], s));
        }
    }
    static string DBlockComment(GetName g)
    {
        return "Pegged.DBlockComment";
    }

    static TParseTree DNestingBlockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "Pegged.DNestingBlockComment")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "Pegged.DNestingBlockComment"), "DNestingBlockComment")(p);
        }
    }
    static TParseTree DNestingBlockComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "Pegged.DNestingBlockComment")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(NestedList!(pegged.peg.literal!("/+"), pegged.peg.literal!("+/")), "Pegged.DNestingBlockComment"), "DNestingBlockComment")(TParseTree("", false,[], s));
        }
    }
    static string DNestingBlockComment(GetName g)
    {
        return "Pegged.DNestingBlockComment";
    }

    static TParseTree DParamList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(NestedList!(pegged.peg.literal!("("), pegged.peg.literal!(")")), "Pegged.DParamList")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(NestedList!(pegged.peg.literal!("("), pegged.peg.literal!(")")), "Pegged.DParamList"), "DParamList")(p);
        }
    }
    static TParseTree DParamList(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(NestedList!(pegged.peg.literal!("("), pegged.peg.literal!(")")), "Pegged.DParamList")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(NestedList!(pegged.peg.literal!("("), pegged.peg.literal!(")")), "Pegged.DParamList"), "DParamList")(TParseTree("", false,[], s));
        }
    }
    static string DParamList(GetName g)
    {
        return "Pegged.DParamList";
    }

    template NestedList(alias L, alias Items, alias R)
    {
    static TParseTree NestedList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_3")(p);
        }
    }
    static TParseTree NestedList(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(Items, NestedList!(L, Items, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R, Items)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_3")(TParseTree("", false,[], s));
        }
    }
    static string NestedList(GetName g)
    {
        return "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(Items)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

    }
    template NestedList(alias L, alias R)
    {
    static TParseTree NestedList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(p);
        }
        else
        {
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_2")(p);
        }
    }
    static TParseTree NestedList(string s)
    {
        if(__ctfe)
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")")(TParseTree("", false,[], s));
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(L), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.zeroOrMore!(pegged.peg.or!(NestedList!(L, R), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(L, R)), pegged.peg.any)), pegged.peg.keep!(R)), "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")"), "NestedList_2")(TParseTree("", false,[], s));
        }
    }
    static string NestedList(GetName g)
    {
        return "Pegged.NestedList!(" ~ pegged.peg.getName!(L)() ~ ", " ~ pegged.peg.getName!(R) ~ ")";
    }

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
        forgetMemo();
        return Pegged(TParseTree(``, false, [], input, 0, 0));
    }
    static string opCall(GetName g)
    {
        return "Pegged";
    }


    static void forgetMemo()
    {
    }
    }
}

alias GenericPegged!(ParseTree).Pegged Pegged;

