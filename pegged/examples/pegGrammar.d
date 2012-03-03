module pegged.examples.pegGrammar;

version(none)
{
mixin Grammar!("PEG <- Spacing Definition+ EndOfFile"
              ,"Definition <- Identifier LEFTARROW Expression"
              ,"Expression <- Sequence (OR Sequence)*"
              ,"
Prefix
Suffix
Primary
<-
<-
<-
<-
<-
/
/
Sequence (SLASH Sequence)*
Prefix*
(AND / NOT)? Suffix
Primary (QUESTION / STAR / PLUS)?
Identifier !LEFTARROW
OPEN Expression CLOSE
Literal / Class / DOT
# Lexical syntax
Identifier <- IdentStart IdentCont* Spacing
IdentStart <- [a-zA-Z_]
IdentCont <- IdentStart / [0-9]
Literal
<-
/
<-
<-
<-
/
/
/
[’] (![’] Char)* [’] Spacing
["] (!["] Char)* ["] Spacing
’[’ (!’]’ Range)* ’]’ Spacing
Char ’-’ Char / Char
’\\’ [nrt’"\[\]\\]
’\\’ [0-2][0-7][0-7]
’\\’ [0-7][0-7]?
!’\\’ .
LEFTARROW
SLASH
AND
NOT
QUESTION
STAR
PLUS
OPEN
CLOSE
DOT
<-
<-
<-
<-
<-
<-
<-
<-
<-
<-
’<-’ Spacing
’/’ Spacing
’&’ Spacing
’!’ Spacing
’?’ Spacing
’*’ Spacing
’+’ Spacing
’(’ Spacing
’)’ Spacing
’.’ Spacing
Spacing
Comment
Space
EndOfLine
EndOfFile
<-
<-
<-
<-
<-
(Space / Comment)*
’#’ (!EndOfLine .)* EndOfLine
’ ’ / ’\t’ / EndOfLine
’\r\n’ / ’\n’ / ’\r’
!.
Class
Range
Char
");
}