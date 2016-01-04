module pegged.examples.peggedgrammar;

import pegged.grammar;

enum PEGGEDgrammar = `
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
`;
