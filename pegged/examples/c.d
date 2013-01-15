module pegged.examples.c;

import pegged.grammar;

enum Cgrammar = `
C:

TranslationUnit <- ExternalDeclaration (:Spacing ExternalDeclaration)*

ExternalDeclaration < FunctionDefinition / Declaration

FunctionDefinition < DeclarationSpecifiers? Declarator DeclarationList? CompoundStatement

PrimaryExpression < Identifier
                  / CharLiteral
                  / StringLiteral
                  / FloatLiteral
                  / IntegerLiteral
                  / '(' Expression ')'

PostfixExpression < PrimaryExpression ( '[' Expression ']'
                                      / '(' ')'
                                      / '(' ArgumentExpressionList ')'
                                      / '.' Identifier
                                      / "->" Identifier
                                      / "++"
                                      / "--"
                                      )*

ArgumentExpressionList < AssignmentExpression (',' AssignmentExpression)*

UnaryExpression < PostfixExpression
                / IncrementExpression
                / DecrementExpression
                / UnaryOperator CastExpression
                / "sizeof" UnaryExpression
                / "sizeof" '(' TypeName ')'

IncrementExpression < PlusPlus UnaryExpression
PlusPlus <- "++"
DecrementExpression < "--" UnaryExpression

UnaryOperator <- [-&*+~!]

CastExpression < UnaryExpression
               / '(' TypeName ')' CastExpression

MultiplicativeExpression    < CastExpression ([*%/] MultiplicativeExpression)*

AdditiveExpression          < MultiplicativeExpression ([-+] AdditiveExpression)*

ShiftExpression             < AdditiveExpression (("<<" / ">>") ShiftExpression)*

RelationalExpression        < ShiftExpression (("<=" / ">=" / "<" / ">") RelationalExpression)*

EqualityExpression          < RelationalExpression (("==" / "!=") EqualityExpression)*

ANDExpression               < EqualityExpression ('&' ANDExpression)*

ExclusiveORExpression       < ANDExpression ('^' ExclusiveORExpression)*

InclusiveORExpression       < ExclusiveORExpression ('|' InclusiveORExpression)*

LogicalANDExpression        < InclusiveORExpression ("&&" LogicalANDExpression)*

LogicalORExpression         < LogicalANDExpression ("||" LogicalORExpression)*

ConditionalExpression       < LogicalORExpression ('?' Expression ':' ConditionalExpression)?

AssignmentExpression < UnaryExpression AssignmentOperator AssignmentExpression
                     / ConditionalExpression

AssignmentOperator <- "=" / "*=" / "/=" / "%=" / "+=" / "-=" / "<<=" / ">>=" / "&=" / "^=" / "|="

Expression < AssignmentExpression (',' AssignmentExpression)*

ConstantExpression <- ConditionalExpression

#
# C declaration rules
#

Declaration < DeclarationSpecifiers InitDeclaratorList? ';'

DeclarationSpecifiers < ( StorageClassSpecifier
                        / TypeSpecifier
                        / TypeQualifier
                        ) DeclarationSpecifiers?

InitDeclaratorList < InitDeclarator (',' InitDeclarator)*

InitDeclarator < Declarator ('=' Initializer)?

StorageClassSpecifier <- "typedef" / "extern" / "static" / "auto" / "register"

TypeSpecifier <- "void"
               / "char" / "short" / "int" / "long"
               / "float" / "double"
               / "signed" / "unsigned"
               / StructOrUnionSpecifier
               / EnumSpecifier
               #/ TypedefName # To reactivate with an associated semantic action:
               # - keep a list of typedef'd names
               # - and verify that the read identifier is already defined

StructOrUnionSpecifier < ("struct" / "union") ( Identifier ('{' StructDeclarationList '}')?
                                              / '{' StructDeclarationList '}')

StructDeclarationList <- StructDeclaration (:Spacing StructDeclaration)*

StructDeclaration < SpecifierQualifierList StructDeclaratorList ';'

SpecifierQualifierList <- (TypeQualifier / TypeSpecifier) (:Spacing (TypeQualifier / TypeSpecifier))*

StructDeclaratorList < StructDeclarator (',' StructDeclarator)*

StructDeclarator < ( Declarator ConstantExpression?
                   / ConstantExpression)

EnumSpecifier < "enum" ( Identifier ('{' EnumeratorList '}')?
                       / '{' EnumeratorList '}')

EnumeratorList < Enumerator (',' Enumerator)*

Enumerator < EnumerationConstant ('=' ConstantExpression)?

EnumerationConstant <- Identifier

TypeQualifier <- "const" / "volatile"

Declarator < Pointer? DirectDeclarator

DirectDeclarator < (Identifier / '(' Declarator ')') ( '[' ']'
                                                     / '[' ConstantExpression ']'
                                                     / '(' ')'
                                                     / '(' ParameterTypeList ')'
                                                     / '(' IdentifierList ')'
                                                     )*

Pointer < ('*' TypeQualifier*)*

TypeQualifierList <- TypeQualifier (:Spacing TypeQualifier)*

ParameterTypeList < ParameterList (',' "...")?

ParameterList < ParameterDeclaration (',' ParameterDeclaration)*

ParameterDeclaration < DeclarationSpecifiers (Declarator / AbstractDeclarator)?

IdentifierList < Identifier (',' Identifier)*

TypeName < SpecifierQualifierList AbstractDeclarator?

AbstractDeclarator < Pointer DirectAbstractDeclarator
                   / DirectAbstractDeclarator
                   / Pointer

DirectAbstractDeclarator < ('(' AbstractDeclarator ')'
                           / '[' ']'
                           / '[' ConstantExpression ']'
                           / '(' ')'
                           / '(' ParameterTypeList ')'
                           )
                           ( '[' ']'
                           / '[' ConstantExpression ']'
                           / '(' ')'
                           / '(' ParameterTypeList ')'
                           )*

TypedefName <- Identifier

Initializer < AssignmentExpression
            / '{' InitializerList ','? '}'

InitializerList < Initializer (',' Initializer)*

#
# C statement rules
#

Statement < LabeledStatement
          / CompoundStatement
          / ExpressionStatement
          / IfStatement
          / SwitchStatement
          / IterationStatement
          / GotoStatement
          / ContinueStatement
          / BreakStatement
          / ReturnStatement

LabeledStatement < Identifier ':' Statement
                 / 'case' ConstantExpression ':' Statement
                 / 'default' ':' Statement

CompoundStatement < '{' '}'
                  / '{' DeclarationList '}'
                  / '{' StatementList '}'
                  / '{' DeclarationList StatementList '}'

DeclarationList <- Declaration (:Spacing Declaration)*

StatementList <- Statement (:Spacing Statement)*

ExpressionStatement < Expression? ';'

IfStatement < "if" '(' Expression ')' Statement ('else' Statement)?

SwitchStatement < "switch" '(' Expression ')' Statement

IterationStatement < WhileStatement / DoStatement / ForStatement

WhileStatement < "while" '(' Expression ')' Statement

DoStatement < "do" Statement "while" '(' Expression ')' ';'

ForStatement < "for" '(' Expression? ';' Expression? ';' Expression? ')' Statement

GotoStatement < "goto" Identifier ';'

ContinueStatement < "continue" ';'

BreakStatement < "break" ';'

ReturnStatement < Return Expression? :';'

Return <- "return"

# The following comes from me, not an official C grammar

Identifier <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*

Keyword <- "auto" / "break" / "case" / "char" / "const" / "continue"
         / "default" / "double" / "do" / "else" / "enum" / "extern"
         / "float" / "for" / "goto" / "if" / "inline" / "int" / "long"
         / "register" / "restrict" / "return" / "short" / "signed"
         / "sizeof" / "static" / "struct" / "switch" / "typedef" / "union"
         / "unsigned" / "void" / "volatile" / "while"
         / "_Bool" / "_Complex" / "_Imaginary"

Spacing <~ (space / endOfLine / Comment)*

Comment <~ "//" (!endOfLine .)* endOfLine

StringLiteral <~ doublequote (DQChar)* doublequote

DQChar <- EscapeSequence
        / !doublequote .

EscapeSequence <~ backslash ( quote
                            / doublequote
                            / backslash
                            / [abfnrtv]
                            )

CharLiteral <~ quote (!quote (EscapeSequence / .)) quote

IntegerLiteral <~ Sign? Integer IntegerSuffix?

Integer <~ digit+

IntegerSuffix <- "Lu" / "LU" / "uL" / "UL"
               / "L" / "u" / "U"

FloatLiteral <~ Sign? Integer "." Integer? (("e" / "E") Sign? Integer)?

Sign <- "-" / "+"
`;
