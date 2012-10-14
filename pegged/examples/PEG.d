module pegged.examples.PEG;

import pegged.grammar;

enum string PEGGgrammar =`

Grammar     <- S Definition+ EOI
Definition  <- Name Arrow Expression S
Expression  <- Sequence (OR Sequence)*
Sequence    <- Prefix+
Prefix      <- (LOOKAHEAD / NOT)? Suffix
Suffix      <- Primary ( OPTION
                       / ONEORMORE
                       / ZEROORMORE )? S
Primary     <- Name !Arrow
             / GroupExpr
             / Literal
             / Class
             / ANY
Name        <- Identifier S
GroupExpr   <- OPEN Expression :CLOSE S
Literal     <- Quote (!Quote Char)* Quote S
             / DoubleQuote (!DoubleQuote Char)* DoubleQuote S
Class       <- '[' (!']' CharRange)* ']' S
CharRange   <- Char '-' Char / Char
Char        <- BackSlash ( Quote
                         / DoubleQuote
                         / BackQuote
                         / BackSlash
                         / [nrt]
                         / [0-2][0-7][0-7]
                         / [0-7][0-7]?)
             / !BackSlash .
# Terminals
Arrow       <- "<-" S
OR          <- '/' S
LOOKAHEAD   <- '&' S
NOT         <- '!' S
OPTION      <- '?' S
ZEROORMORE  <- '*' S
ONEORMORE   <- '+' S
OPEN        <- '(' S
CLOSE       <- ')' S
ANY         <- '.' S
# Blanks
EOL         <- '\r\n' / '\n' / '\r'
Comment     <- "#" (!EOL .)* (EOL/EOI)
S           <- (' ' / '\t' / EOL / Comment)*
EOI         <- !.
`;

// TODO: add unit tests on simple grammars
