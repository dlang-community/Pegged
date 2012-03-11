module pegged.utils.PEgrammar;


enum string PEG =`
Grammar    <- S Definition+ EOI
Definition <- RuleName Arrow Expression
RuleName   <- Identifier>(ParamList?)
Expression <- Sequence (OR Sequence)*
Sequence   <- Element*
Element    <- Prefix (JOIN Prefix)*
Prefix     <- (LOOKAHEAD / NOT / DROP / FUSE)? Suffix
Suffix     <- Primary 
              (OPTION 
              / ONEORMORE 
              / ZEROORMORE 
              / NamedExpr 
              / WithAction)?
Primary    <- Name !Arrow
            / GroupExpr
            / Literal / Class / ANY
Name       <- QualifiedIdentifier>(ArgList?)
GroupExpr  <- :OPEN Expression :CLOSE

Literal    <~ :Quote (!Quote Char)* :Quote S
            / :DoubleQuote (!DoubleQuote Char)* :DoubleQuote S
Class      <- :'[' (!']' CharRange)* :']' S
CharRange  <- Char :'-' Char / Char
Char       <~ BackSlash [nrt]
            / !BackSlash .
    
ParamList  <~ OPEN Identifier (',' Identifier)* CLOSE
ArgList    <- :OPEN Expression (:',' Expression)* :CLOSE
NamedExpr  <- NAME>Identifier?
    
WithAction <~ :ACTIONOPEN Identifier :ACTIONCLOSE
    
Arrow      <- LEFTARROW / FUSEARROW / DROPARROW / ACTIONARROW
LEFTARROW  <- "<-" S
FUSEARROW  <- "<~" S
DROPARROW  <- "<:" S
ACTIONARROW <- "<">WithAction
  
OR         <- '/' S
    
LOOKAHEAD  <- '&' S
NOT        <- '!' S
DROP       <- ':' S
FUSE       <- '~' S
  
JOIN       <- '>'
    
NAME       <- '=' S
ACTIONOPEN <- '{' S
ACTIONCLOSE <- '}' S
    
OPTION     <- '?' S
ZEROORMORE <- '*' S
ONEORMORE  <- '+' S
    
OPEN       <- '(' S
CLOSE      <- ')' S
    
ANY        <- '.' S
    
S          <: (Blank / Comment)*
Comment    <~ '#'>(!EOL>.)*>(EOL/EOI)
`;

