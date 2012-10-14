/**
 * This module was given by Bjoern Lietz-Spendig
 **/
module pegged.examples.oberon2;


/*
 *
 * PEG Grammar for Oberon-2
 * A detailed language report is available at
 * http://www-vs.informatik.uni-ulm.de:81/projekte/Oberon-2.Report/index.html
 */

/*
The remarkable short syntax of Oberon-2 in --- Wirth EBNF ---

Module  =   MODULE ident ";" [ImportList] DeclSeq [BEGIN StatementSeq] END ident ".".
ImportList   =   IMPORT [ident ":="] ident {"," [ident ":="] ident} ";".
DeclSeq   =   { CONST {ConstDecl ";" } | TYPE {TypeDecl ";"} | VAR {VarDecl ";"}} {ProcDecl ";" | ForwardDecl ";"}.
ConstDecl  =   IdentDef "=" ConstExpr.
TypeDecl  =   IdentDef "=" Type.
VarDecl  =   IdentList ":" Type.
ProcDecl   =   PROCEDURE [Receiver] IdentDef [FormalPars] ";" DeclSeq [BEGIN StatementSeq] END ident.
ForwardDecl  =   PROCEDURE "^" [Receiver] IdentDef [FormalPars].
FormalPars   =   "(" [FPSection {";" FPSection}] ")" [":" Qualident].
FPSection   =   [VAR] ident {"," ident} ":" Type.
Receiver  =   "(" [VAR] ident ":" ident ")".
Type   =   Qualident
  |   ARRAY [ConstExpr {"," ConstExpr}] OF Type
  |   RECORD ["("Qualident")"] FieldList {";" FieldList} END
  |   POINTER TO Type
  |   PROCEDURE [FormalPars].
FieldList   =   [IdentList ":" Type].
StatementSeq  =   Statement {";" Statement}.
Statement   =  [ Designator ":=" Expr
  |   Designator ["(" [ExprList] ")"]
  |   IF Expr THEN StatementSeq {ELSIF Expr THEN StatementSeq} [ELSE StatementSeq] END
  |   CASE Expr OF Case {"|" Case} [ELSE StatementSeq] END
  |   WHILE Expr DO StatementSeq END
  |   REPEAT StatementSeq UNTIL Expr
  |   FOR ident ":=" Expr TO Expr [BY ConstExpr] DO StatementSeq END
  |   LOOP StatementSeq END
  |   WITH Guard DO StatementSeq {"|" Guard DO StatementSeq} [ELSE StatementSeq] END
  |   EXIT
  |   RETURN [Expr]
     ].
Case   =   [CaseLabels {"," CaseLabels} ":" StatementSeq].
CaseLabels   =   ConstExpr [".." ConstExpr].
Guard  =   Qualident ":" Qualident.
ConstExpr  =   Expr.
Expr   =   SimpleExpr [Relation SimpleExpr].
SimpleExpr  =   ["+" | "-"] Term {AddOp Term}.
Term   =   Factor {MulOp Factor}.
Factor   =   Designator ["(" [ExprList] ")"] | number | character | string | NIL | Set | "(" Expr ")" | " ~ " Factor.
Set  =   "{" [Element {"," Element}] "}".
Element   =   Expr [".." Expr].
Relation   =   "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.
AddOp   =   "+" | "-" | OR.
MulOp   =   " * " | "/" | DIV | MOD | "&".
Designator   =   Qualident {"." ident | "[" ExprList "]" | " ^ " | "(" Qualident ")"}.
ExprList   =   Expr {"," Expr}.
IdentList   =   IdentDef {"," IdentDef}.
Qualident   =   [ident "."] ident.
IdentDef   =   ident [" * " | "-"].
*/


/*
FOUR SIMPLE EBNF TO PEGGED TRANFORMATION RULES
Pegged				Wirth EBNF

Sequence
A <- B C			A = B C.

B or C
A <- B / C			A = B|C.

Zero or one B
A <- B?				A = [B].

Zero or more Bs
A <- B*				A = {B}.

List of comma (or otherwise) separated Bs
A <- List(B, ',')   A = B {"," B}.

*/

/// The Oberon-2 PEG grammar (Finally!)
enum string Oberon2Grammar =  `
Oberon2:

Module 			<- "MODULE" Identifier ";" ImportList? DeclSeq ("BEGIN" StatementSeq)? "END" Identifier "."

ImportList 		<- "IMPORT" (Identifier ":=")? Identifier ("," (Identifier ":=")? Identifier)* ";"

DeclSeq 		<- "CONST" (ConstDecl ";")*
                 / "TYPE" (TypeDecl ";")*
                 / "VAR" (VarDecl ";")* (ProcDecl ";" / ForwardDecl ";")*

ConstDecl  		<-  IdentDef "=" ConstExpr
TypeDecl  		<-  IdentDef "=" Type
VarDecl  		<-  IdentList ":" Type
ProcDecl   		<-  "PROCEDURE" Receiver? IdentDef FormalPars? ";" DeclSeq ("BEGIN" StatementSeq)? "END" Identifier
ForwardDecl  	<-  "PROCEDURE" "^" Receiver? IdentDef FormalPars?
FormalPars   	<-  "(" (FPSection (";" FPSection)*)? ")" (":" Qualident)?
FPSection   	<-   "VAR"? Identifier ("," Identifier)? ":" Type
Receiver  		<-   "(" "VAR"? Identifier ":" Identifier ")"

Type   			<-  Qualident
                 / "ARRAY" (ConstExpr ("," ConstExpr)*)? "OF" Type
                 / "RECORD" ("("Qualident")")? FieldList (";" FieldList)* "END"
                 / "POINTER" "TO" Type
                 / "PROCEDURE" (FormalPars)?

FieldList   	<-  (IdentList ":" Type)?

#
StatementSeq  	<-  Statement (";" Statement)* # List( Statement, ';')

Statement   	<-  ( Designator ":=" Expr
  					/ Designator ("(" (ExprList)? ")")
  					/ IfStatement
  					/ CaseStatement
  					/ WhileStatement
  					/ RepeatStatement
  					/ ForStatement
  					/ LoopStatement
  					/ WithStatement
  					/ ExitStatement
  					/ ReturnStatement
  					)?

IfStatement		<-  "IF" Expr "THEN" StatementSeq ("ELSIF" Expr "THEN" StatementSeq)* ("ELSE" StatementSeq)? "END"
CaseStatement	<-	"CASE" Expr "OF" Case ("|" Case)* ("ELSE" StatementSeq) "END"
WhileStatement 	<-	"WHILE" Expr "DO" StatementSeq "END"
RepeatStatement	<-	"REPEAT" StatementSeq "UNTIL" Expr
ForStatement	<-	"FOR" Identifier ":=" Expr "TO" Expr ("BY" ConstExpr)? "DO" StatementSeq "END"
LoopStatement	<-	"LOOP" StatementSeq "END"
WithStatement	<-  "WITH" Guard "DO" StatementSeq ("|" Guard "DO" StatementSeq)* ("ELSE" StatementSeq)? "END"
ExitStatement	<- 	"EXIT"
ReturnStatement <-  "RETURN" Expr?

Case  			<-  (CaseLabels ("," CaseLabels)* ":" StatementSeq)?
CaseLabels   	<-  ConstExpr (".." ConstExpr)?
Guard  			<-  Qualident ":" Qualident
ConstExpr  		<-  Expr
Expr   			<-  SimpleExpr (Relation SimpleExpr)?
SimpleExpr  	<-  ("+" / "-")? Term (AddOp Term)*
Term   			<-  Factor (MulOp Factor)*
Factor   		<- 	Designator ("(" ExprList? ")")?
				 	/ Number
					/ Character
					/ String
					/ "NIL"
					/ Set
					/ "(" Expr ")"
					/ " ~ " Factor

Set  			<-  "{" (Element ("," Element)*)? "}"
Element   		<-   Expr (".." Expr)?

Relation   		<-  "IN" / "IS" / "<=" / ">=" / "=" / "#" / "<" / ">"
AddOp   		<-  "OR" / "+" / "-"
MulOp   		<-  "DIV" / "MOD" / "*" / "/" / "&"
Designator   	<-  Qualident ("." Identifier / "[" ExprList "]" / " ^ " / "(" Qualident ")")*
List(E,S)       <-  E (S E)*
ExprList   		<-  Expr (',' Expr)*
IdentList   	<-  IdentDef (',' IdentDef)*
Qualident   	<-  (Identifier ".")? Identifier
IdentDef   		<-  Identifier (" * " / "-")?

#Numbers
Number			<- Integer / Real
Integer			<- Digit Digit* / Digit HexDigit* "H"
Real			<- Digit Digit* "." Digit* ScaleFactor?
ScaleFactor		<- ("E" / "D") / ("+" / "-")? Digit Digit*
HexDigit		<- [0-9A-F]
Digit			<- [0-9]

#Number Examples
# 1991			INTEGER		1991
# 0DH			SHORTINT	13
# 12.3			REAL		12.3
# 4.567E8		REAL		456700000
# 0.57712566D-6	LONGREAL	0.00000057712566

String              <- DoubleQuotedString / SingleQuotedString
DoubleQuotedString  <- doublequote DQChar* doublequote
SingleQuotedString  <- quote SQChar* quote

DQChar          <- EscapeSequence
                / !doublequote .

SQChar          <- EscapeSequence
                / !quote .

EscapeSequence  <- backslash ( quote
                            / doublequote
                            / backslash
                            / [abfnrtv]
                            / 'x' HexDigit HexDigit
                            / 'u' HexDigit HexDigit HexDigit HexDigit
                            / 'U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
                            )

Character       <- quote (!quote (EscapeSequence / .)) quote

#Oberon-2 comments look like (* Some Text *)
Comment <- '(*' (!'*)' .)* '*)'

# I had to add it. Otherwise, keywords are recognized as identifiers. Note that Oberon does not allow '_'
Identifier 		<~ !Keyword [a-zA-Z] [a-zA-Z0-9]*

Keyword 		<- "ARRAY" / "ABS"/ "ASH" / "BOOLEAN" / "BEGIN" / "BY"
				/  "CASE" / "CHAR" / "CONST" / "COPY" / "CAP"/ "CHR"
				/  "DEC" / "DIV" / "DO" / "ELSIF" / "ELSE" / "ENTIER" / "EXCL" / "EXIT" / "END"
				/  "FALSE" / "FOR" / "HALT" / "INTEGER" / "IMPORT" / "INCL" / "INC" / "IF" / "IN" /"IS"
				/  "LONGREAL" / "LONGINT" / "LONG" / "LOOP" / "LEN"
				/  "MODULE" / "MOD" / "MIN" / "MAX"/ "NIL" /"NEW"
				/  "ODD" / "ORD" / "OF" / "OR" / "PROCEDURE" / "REAL"
				/  "SET" / "SHORTINT" / "SHORT" / "SIZE" / "TRUE"
				/  "POINTER" / "RECORD" / "REPEAT" / "RETURN" / "THEN" / "TYPE" / "TO" / "UNTIL" / "VAR"
				/  "WHILE" / "WITH"

`;
