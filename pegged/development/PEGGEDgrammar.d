module pegged.development.PEGGEDgrammar;

enum string PEGGEDgrammar =`
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
Char        <- BackSlash ( Quote
                         / DoubleQuote
                         / BackQuote
                         / BackSlash 
                         / '-'
                         / '[' 
                         / ']' 
                         / [nrt]
                         / [0-2][0-7][0-7]
                         / [0-7][0-7]?)
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
`;

