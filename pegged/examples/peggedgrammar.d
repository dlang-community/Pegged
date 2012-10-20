module pegged.examples.peggedgrammar;

import pegged.grammar;

enum PEGGEDgrammar = `
# This is the PEG extended grammar used by Pegged
Pegged:

# Syntactic rules:
Grammar      <- Spacing GrammarName Definition+ :eoi
Definition   <- LhsName Arrow Expression
Expression   <- Sequence (:OR Sequence)*
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
`;
