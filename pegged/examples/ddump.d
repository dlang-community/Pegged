/**
This module was automatically generated from the following grammar:

# List(E) is predefined in Pegged:
# List(E) <- E ("," E)*
# I should add a space-separated list, and maybe a dot-sept one too.

Module    <- Spacing ModuleDeclaration? DeclDefs?

DeclDefs <- DeclDef DeclDefs?

DeclDef   <- AttributeSpecifier
           / ImportDeclaration
           / EnumDeclaration
           / ClassDeclaration
           / InterfaceDeclaration
           / AggregateDeclaration
           / Declaration
           / Constructor
           / Destructor
           / UnitTest
           / StaticConstructor
           / StaticDestructor
           / SharedStaticConstructor
           / SharedStaticDestructor
           / ConditionalDeclaration
           / DebugSpecification
           / VersionSpecification
           / StaticAssert
           / TemplateDeclaration
           / TemplateMixinDeclaration
           / TemplateMixin
           / MixinDeclaration
           
ModuleDeclaration <- "module" QualifiedIdentifier ";"

ImportDeclaration <- "import" ImportList ";"
                   / "static" "import"  ImportList ";"
                   
ImportList <- Import ("," ImportList)?
            / ImportBindings

Import <- Identifier "=" QualifiedIdentifier
        / QualifiedIdentifier

ImportBindings <- Import ":" List(ImportBind) 

ImportBind <- Identifier ("=" Identifier)?

MixinDeclaration <- "mixin" "(" AssignExpression ")" ";"

# declaration.html
Declaration <- AliasDeclaration
             / AliasThisDeclaration
             / Decl

AliasDeclaration <- "alias" BasicType Declarator

AliasThisDeclaration <- "alias" Identifier "this"  # no ";"?

Decl <- BasicType Declarators ";"
      / BasicType Declarator FunctionBody
      / AutoDeclaration
      / StorageClasses Decl

Declarators <- DeclaratorInitializer ("," List(DeclaratorIdentifier))?

DeclaratorInitializer <- Declarator ("=" Initializer)?

DeclaratorIdentifier <- Identifier ("=" Initializer)?

BasicType <- BasicTypeX
           / "." IdentifierList
           / IdentifierList
           / Typeof "." IdentifierList
           / "const(" Type ")"
           / "immutable(" Type ")"
           / "shared(" Type ")"
           / "inout(" Type ")" 

BasicTypeX <- "bool"
            / "byte" / "ubyte"
            / "short" / "ushort"
            / "int" / "uint"
            / "long" / "ulong"
            / "char" / "wchar" / "dchar"
            / "float" / "double" / "real"   
            / "void"

BasicType2 <- "*"
            / "[" "]"
            / "[" AssignExpression ".." AssignExpression "]"
            / "[" Type "]"
            / "delegate" Parameters FunctionAttributes?
            / "function" Parameters FunctionAttributes?

## Maybe that could factored ##
Declarator <- BasicType2? "(" Declarator ")" DeclaratorSuffixes?
            / BasicType2?     Identifier     DeclaratorSuffixes?

DeclaratorSuffixes <- DeclaratorSuffix DeclaratorSuffixes
            
DeclaratorSuffix <- "[" "]"
                  / "[" AssignExpression "]"
                  / "[" Type "]"
                  / TemplateParameterList? Parameters MemberFunctionAttributes? Constraint?

## Could be written otherwise? #
IdentifierList <- Identifier ("." IdentifierList)?
                / TemplateInstance ("." IdentifierList)?
               
StorageClasses <- StorageClass StorageClasses?

StorageClass <- "abstract"
              / "auto"
              / "const"
              / "deprecated"
              / "enum"
              / "extern"
              / "final"
              / "immutable"
              / "inout"
              / "shared"
              / "nothrow"
              / "override"
              / "pure"
              / "__gshared"
              / Property
              / "scope"
              / "static"
              / "synchronized"

Property <- "@" ( "property"
                / "safe"
                / "trusted"
                / "system"
                / "disable")

Type <- BasicType Declarator2?

Declarator2 <- BasicType2? ("(" Declarator2 ")")? DeclaratorSuffixes?

Parameters <- "(" ParameterList? ")"

ParameterList <- "..."
               / Parameter ParameterList?

Parameter <- InOut? BasicType Declarator ("..." / "=" DefaultInitializerExpression)?
           / InOut? Type "..."?

InOut <- InOutX InOut?

InOutX <- "auto"
        / "const"
        / "final"
        / "immutable"
        / "inout"
        / "in"
        / "lazy"
        / "out"
        / "ref"
        / "scope"
        / "shared"

FunctionAttributes <- FunctionAttribute FunctionAttributes?

FunctionAttribute <- "nothrow"
                   / "pure"
                   / Property
                   
MemberFunctionAttributes <- MemberFunctionAttribute MemberFunctionAttributes?

MemberFunctionAttribute <- "const"
                         / "immutable"
                         / "inout"
                         / "shared"
                         / FunctionAttribute

DefaultInitializerExpression <- AssignExpression
                              / "__FILE__"
                              / "__LINE__"

Initializer <- VoidInitializer / NonVoidInitializer

NonVoidInitializer <- AssignExpression
                    / ArrayInitializer
                    / StructInitializer

ArrayInitializer <- "[" "]"
                  / "[" ArrayMemberInitializations "]"

## Crap
ArrayMemberInitializations <- ArrayMemberInitialization ("," ArrayMemberInitializations?)?

## Verify the order, with PEG
ArrayMemberInitialization <- NonVoidInitializer
                           / AssignExpression ":" NonVoidInitializer

StructInitializer <- "{" "}"
                   / "{" StructMemberInitializers "}"

StructMemberInitializers <- StructMemberInitializer ("," StructMemberInitializers?)?

StructMemberInitializer <- NonVoidInitializer
                         / Identifier : NonVoidInitializer
                         
AutoDeclaration <- StorageClasses AutoDeclarationX ";"

AutoDeclarationX <- List(Identifier "=" Initializer)

Typeof <- "typeof" "(" Expression ")"
        / "typeof" "(" "return" ")"

VoidInitializer <- "void"

## File statement.html

Statement <- ";"
           / NonEmptyStatement
           / ScopeBlockStatement
           
NoScopeNonEmptyStatement <- NonEmptyStatement
                          / BlockStatement

NoScopeStatement <- ";"
                  / NonEmptyStatement
                  / BlockStatement

NonEmptyOrScopeBlockStatement <- NonEmptyStatement
                               / ScopeBlockStatement
                               
NonEmptyStatement <- NonEmptyStatementNoCaseNoDefault
                   / CaseStatement
                   / CaseRangeStatement
                   / DefaultStatement
                   
NonEmptyStatementNoCaseNoDefault <- 
    LabeledStatement
  / ExpressionStatement
  / DeclarationStatement
  / IfStatement
  / WhileStatement
  / DoStatement
  / ForStatement
  / ForeachStatement
  / SwitchStatement
  / FinalSwitchStatement
  / ContinueStatement
  / BreakStatement
  / ReturnStatement
  / GotoStatement
  / WithStatement
  / SynchronizedStatement
  / TryStatement
  / ScopeGuardStatement
  / ThrowStatement
  / AsmStatement
  / PragmaStatement
  / MixinStatement
  / ForeachRangeStatement
  / ConditionalStatement
  / StaticAssert
  / TemplateMixin
  / ImportDeclaration

ScopeStatement <- NonEmptyStatement / BlockStatement

ScopeBlockStatement <- ScopeStatement

LabeledStatement <- Identifier ":" NoScopeStatement

BlockStatement <- "{" StatementList? "}"
                          
StatementList <- Statement StatementList?

ExpressionStatement <- Expression ";"

DeclarationStatement <- Declaration

IfStatement <- "if" "(" IfCondition ")" ThenStatement "else" ElseStatement

IfCondition <- Expression
             / "auto" Identifier "=" Expression
             / BasicType Declarator "=" Expression
             
ThenStatement <- ScopeStatement
 
ElseStatement <- ScopeStatement
 
WhileStatement <- "while" "(" Expression ")" ScopeStatement
 
DoStatement <- "do" ScopeStatement "while" "(" Expression ")" ";"
 
ForStatement <- "for" "(" Initialize Test? ";" Increment? ")" ScopeStatement
 
Initialize <- ";" / NoScopeNonEmptyStatement
 
Test <- Expression
 
Increment <- Expression
 
ForeachStatement <- ("foreach" / "foreach_reverse")
                    "(" List(ForeachType) ";" Aggregate ")"
                     NoScopeNonEmptyStatement
                     
ForeachType <- "ref"? BasicType Declarator
             / "ref"? Identifier
             
Aggregate <- Expression

ForeachRangeStatement <- "(" ForeachType ";" Expression ".." Expression ")"

SwitchStatement <- "switch" "(" Expression ")" ScopeStatement

CaseStatement <- "case" ArgumentList ":" ScopeStatementList

CaseRangeStatement <- "case" AssignExpression ":" 
                      ".." 
                      "case" AssignExpression ":"
                      ScopeStatementList
                      
DefaultStatement <- "default" ":" ScopeStatementList

ScopeStatementList <- StatementListNoCaseNoDefault

StatementListNoCaseNoDefault <- StatementNoCaseNoDefault
                                StatementListNoCaseNoDefault?

StatementNoCaseNoDefault <- ";" 
                          / NonEmptyStatementNoCaseNoDefault
                          / ScopeBlockStatement
                          
FinalSwitchStatement <- "final" "switch" "(" Expression ")"
                        ScopeStatement
                        
ContinueStatement <- "continue" Identifier? ";"

BreakStatement <- "break" Identifier? ";"

ReturnStatement <- "return" Expression? ";"

GotoStatement <- "goto" ( "default" ";"
                        / "case" ";"
                        / "case" Expression ";"
                        / Identifier ";")
               
WithStatement <- "with" 
                 "(" ( Expression / Symbol / TemplateInstance) ")" 
                 ScopeStatement

SynchronizedStatement <- "synchronized" 
                        ( "(" Expression ")" )?
                        ScopeStatement

TryStatement <- "try" ScopeStatement Catches? FinallyStatement?

Catches <- LastCatch / Catch Catches?

LastCatch <- "catch" NoScopeNonEmptyStatement

Catch <- "catch" "(" CatchParameter ")" NoScopeNonEmptyStatement

CatchParameter <- BasicType Identifier

FinallyStatement <- "finally" NoScopeNonEmptyStatement

ThrowStatement <- "throw" Expression ";"  

ScopeGuardStatement <- ( "scope(exit)" 
                       / "scope(success)" 
                       / "scope(failure)")
                       NonEmptyOrScopeBlockStatement

AsmStatement <- "asm" "{" AsmInstructionList? "}"

AsmInstructionList <- AsmInstruction ";" AsmInstructionList?

PragmaStatement <- Pragma NoScopeStatement

MixinStatement <- "mixin" "(" AssignExpression ")" ";"

### File expression.html ###

Expression <- List(AssignExpression)

AssignExpression <- ConditionalExpression (Op AssignExpression)?

Op <- ">>>=" 
    / "^^=" / ">>=" / "<<="
    / "~=" / "+=" / "-=" / "*=" / "^=" / "|=" / "&=" / "/="
    / "="

ConditionalExpression <- OrOrExpression 
                        ("?" Expression ":" ConditionalExpression)?

OrOrExpression <- AndAndExpression ("||" OrOrExpression)?

AndAndExpression <- (OrExpression / CmpExpression) ("&&" AndAndExpression)?
                    
OrExpression <- XorExpression ("|" OrExpression)?

XorExpression <- AndExpression ("^" XorExpression)?

AndExpression <- ShiftExpression ("&" AndExpression)?

CmpExpression <- ShiftExpression
               / EqualExpression
               / IdentityExpression
               / RelExpression
               / InExpression
               
EqualExpression <- ShiftExpression ("==" / "!=") ShiftExpression

IdentityExpression <- ShiftExpression ("!is" / "is") ShiftExpression

RelExpression <- ShiftExpression RelOp ShiftExpression

RelOp <- "!<>="
       / "!<>" / "!<=" / "!>=" / "<>="
       / "<=" / ">=" / "<>" / "!>" / "!<"
       / "<" / ">"

InExpression <- ShiftExpression (("!in" / "in") ShiftExpression)?

ShiftExpression <- AddExpression ((">>>" / ">>" / "<<") AddExpression)?

AddExpression <- (MulExpression / CatExpression)
                 (("+" / "-") MulExpression)?
                 
CatExpression <- AddExpression ("~" MulExpression)?

MulExpression <- UnaryExpression
                 (("*" / "/" / "%") UnaryExpression)?
                 
UnaryExpression <- UnaryOp UnaryExpression
                 / ComplementExpression
                 / "(" Type ")" "." Identifier
                 / NewExpression
                 / DeleteExpression
                 / CastExpression
                 / PowExpression
                 
UnaryOp <- "++" / "--"
         / "+" / "-" / "&" / "*" / "/" / "!"
         
ComplementExpression <- "~" UnaryExpression

NewExpression <- ("new" AllocatorArguments? Type 
                  ("[" AssignExpression "]" / "(" ArgumentList ")" )?)
               / NewAnonClassExpression

AllocatorArguments <- "(" ArgumentList ")"

ArgumentList <- List(AssignExpression)

DeleteExpression <- "delete" UnaryExpression

CastExpression <- "cast" "(" (Type / CastEqual)? ")" UnaryExpression

CastEqual <- "const" "shared"
           / "shared" "const"
           / "inout" "shared"
           / "shared" "inout"
           / "const"
           / "inout"
           / "immutable"
           / "shared"
           
PowExpression <- PostfixExpression ("^^" UnaryExpression)?

PostfixExpression <- PrimaryExpression
                   / IndexExpression
                   / SliceExpression
                   / PostfixExpression ( "." NewExpression
                                       / "." TemplateIdentifier
                                       / "." Identifier
                                       / "++"
                                       / "--"
                                       / "(" ArgumentList? ")"
                                       )
                                       
IndexExpression <- PostfixExpression "[" ArgumentList "]"

SliceExpression <- PostfixExpression ( "[" "]"
                                     / "[" AssignExpression ".." AssignExpression "]"
                                     )
                                     
PrimaryExpression <- "this"
                   / "super"
                   / "null"
                   / "true"
                   / "false"
                   / "$"
                   / "__FILE__"
                   / "__LINE__"
                   / Identifier
                   / ".">Identifier
                   / TemplateInstance
                   / ".">TemplateInstance
                   / IntegerLiteral
                   / FloatLiteral
                   / CharacterLiteral
                   / StringLiterals
                   / ArrayLiteral
                   / AssocArrayLiteral
                   / Lambda
                   / FunctionLiteral
                   / AssertExpression
                   / MixinExpression
                   / ImportExpression
                   / BasicType "." Identifier
                   / Typeof
                   / TypeidExpression
                   / IsExpression
                   / "(" Expression ")"
                   / TraitsExpression

StringLiterals <- StringLiteral StringLiterals?

ArrayLiteral <- "[" ArgumentList "]"

AssocArrayLiteral <- "[" List(KeyValuePair) "]"

KeyValuePair <- AssignExpression ":" AssignExpression

Lambda <- Identifier "=>" AssignExpression
        / ParameterAttributes "=>" AssignExpression

FunctionLiteral <- (("function" / "delegate") Type?)? ParameterAttributes? FunctionBody

ParameterAttributes <- Parameters FunctionAttributes?

AssertExpression <- "assert" "(" AssignExpression ("," AssignExpression)? ")"

MixinExpression <- "mixin" "(" AssignExpression ")"

ImportExpression <- "import" "(" AssignExpression ")"

TypeidExpression <- "typeid" "(" ( Type / Expression ) ")"

IsExpression <- "is" "(" Type
                  ( ":" TypeSpecialization
                  / "==" TypeSpecialization
                  / Identifier ( ":" TypeSpecialization ("," TemplateParameterList)?
                               / "==" TypeSpecialization ("," TemplateParameterList)?
                               )?
                  
                  )?
                ")"

TypeSpecialization <- Type
                    / "struct"
                    / "union"
                    / "class"
                    / "interface"
                    / "enum"
                    / "function"
                    / "delegate"
                    / "super"
                    / "const"
                    / "immutable"
                    / "inout"
                    / "shared"
                    / "return"
                    
## file lex.html

Comment <- BlockComment
         / LineComment
         / NestingBlockComment
###### Manually modified just for the comment in this file ######
# Otherwise the star-slash pattern would stop the current comment 

BlockComment <- '/*' (!'*'>.)* '*'

# The grammar below implements the right comment, of course.
#################################################################

LineComment <- '//' (!EOL>.)* EOL

NestingBlockComment <- '/+' (NestingBlockComment / Text) '+/'

Text <- (!'+/'>.)*

StringLiteral <- WysiwygString
               / AlternateWysiwygString
               / DoubleQuotedString
               # No HexString
               # No DelimitedString
               / TokenString

WysiwygString <- 'r'>DoubleQuote>(!DoubleQuote>.)* DoubleQuote StringPostfix?

AlternateWysiwygString <- BackQuote>(!BackQuote>.)* BackQuote StringPostfix?

DoubleQuotedString <- DoubleQuote>(DQChar)* DoubleQuote StringPostfix?

DQChar <- EscapeSequence 
        / !DoubleQuote>.
        
EscapeSequence <- BackSlash>( Quote
                            / DoubleQuote
                            / BackSlash
                            / [abfnrtv]
                            / 'x'>HexDigit>HexDigit
                            / 'u'>HexDigit>HexDigit>HexDigit>HexDigit
                            / 'U'>HexDigit>HexDigit>HexDigit>HexDigit>HexDigit>HexDigit>HexDigit>HexDigit
                            )

StringPostfix <- "c" / "w" / "d"

TokenString <- "q{">(!"}">.)* "}"

CharacterLiteral <- Quote>(!Quote>(EscapeSequence / .))>Quote

### I'm fed up, I simplify

IntegerLiteral <- DecimalInteger
                / BinaryInteger
                / HexadecimalInteger
                
DecimalInteger <- Integer IntegerSuffix?

Integer <- Digit (Digit/"_")*

IntegerSuffix <- "Lu" / "LU" / "uL" / "UL"
               / "L" / "u" / "U"
               
BinaryInteger <- ("0b" / "0B") [01] ([01] / "_")*

HexadecimalInteger <- ("0x"/"0X") HexDigit (HexDigit / "_")*

HexDigit <- [0-9a-fA-F]

FloatLiteral <- Sign? Integer ("." Integer?)? (("e" / "E") Sign? Integer)?

Sign <- ("-" / "+")?

### file attribute.html

AttributeSpecifier <- Attribute DeclarationBlock
                    / Attribute ":"

Attribute <- LinkageAttribute
           / AlignAttribute
           / Pragma
           / "deprecated"
           / ProtectionAttribute
           / "static"
           / "extern"
           / "final"
           / "synchronized"
           / "override"
           / "abstract"
           / "const"
           / "auto"
           / "scope"
           / "__gshared"
           / "shared"
           / "immutable"
           / "inout"
           / "@disable"
           
DeclarationBlock <- DeclDef
                  / "{" DeclDefs "}"
                    
LinkageAttribute <- "extern" "(" LinkageType ")"

LinkageType <- "C++" / "C" / "D" / "Windows" / "Pascal" / "System"

AlignAttribute <- "align" ("(" IntegerLiteral ")")?

ProtectionAttribute <- "private"
                     / "package"
                     / "protected"
                     / "public"
                     / "export"

### class.html

ClassDeclaration <- "class" Identifier BaseClassList? ClassBody ClassTemplateDeclaration

### I don't why the grammar distinguish SuperClass and Interface
### They cannot be differentiated at this step
BaseClassList <- ":" List(Identifier)

ClassBody <- "{" ClassBodyDeclarations? "}"

ClassBodyDeclarations <- ClassBodyDeclaration ClassBodyDeclarations?

ClassBodyDeclaration <- DeclDef
                      / Invariant
                      / ClassAllocator
                      / ClassDeallocator
                      
Constructor <- "this" Parameters FunctionBody
             / TemplatedConstructor
             
Destructor <- "~" "this" "(" ")" FunctionBody

StaticConstructor <- "static" "this" "(" ")" FunctionBody

StaticDestructor <- "static" "~" "this" "(" ")" FunctionBody

SharedStaticConstructor <- "shared" "static" "this" "(" ")" FunctionBody

SharedStaticDestructor <- "shared" "static" "~" "this" "(" ")" FunctionBody

Invariant <- "invariant" "(" ")" BlockStatement

ClassAllocator <- "new" Parameters FunctionBody

ClassDeallocator <- "delete" Parameters FunctionBody

AliasThis <- "alias" Identifier "this" ";"

NewAnonClassExpression <- "new" AllocatorArguments? "class" ClassArguments? Identifier List(Identifier)? ClassBody 

ClassArguments <- "(" ArgumentList? ")"

### enum.html

EnumDeclaration <- "enum" EnumTag? (":" EnumBaseType)? EnumBody

EnumTag <- Identifier

EnumBaseType <- Type

EnumBody <- ";" / "{" List(EnumMember) "}"

EnumMember <- Type "=" AssignExpression
            / Identifier ("=" AssignExpression)?
            
### function.html

FunctionBody <- BlockStatement
              / BodyStatement
              / InStatement BodyStatement
              / OutStatement BodyStatement
              / InStatement OutStatement BodyStatement
              / OutStatement InStatement BodyStatement
              
InStatement <- "in" BlockStatement

OutStatement <- "out" ("(" Identifier ")" )? BlockStatement

BodyStatement <- "body" BlockStatement

### iasm.html

AsmInstruction <- "align" IntegerExpression
                / "even"
                / "naked"
                / ("db" / "ds" / "di" / "dl" / "df" / "dd" / "de") List(Operand)
                / Identifier ":" AsmInstruction
                / OpCode
                / OpCode List(Operand)
                
IntegerExpression <- IntegerLiteral / Identifier

Operand <- AsmExp

AsmExp <- AsmLogOrExp ("?" AsmExp ":" AsmExp)?

AsmLogOrExp <- AsmLogAndExp ("||" AsmLogAndExp)?

AsmLogAndExp <- AsmOrExp ("&&" AsmOrExp)?

AsmOrExp <- AsmXorExp ("|" AsmXorExp)?

AsmXorExp <- AsmAndExp ("^" AsmAndExp)?

AsmAndExp <- AsmEqualExp ("&" AsmEqualExp)?

AsmEqualExp <- AsmRelExp (("=="/"!=") AsmRelExp)?

AsmRelExp <- AsmShiftExp (("<="/">="/"<"/">") AsmShiftExp)?

AsmShiftExp <- AsmAddExp ((">>>"/"<<"/">>") AsmAddExp)?

AsmAddExp <- AsmMulExp (("+"/"-") AsmMulExp)?

AsmMulExp <- AsmBrExp (("*"/"/"/"%") AsmBrExp)?

AsmBrExp <- AsmUnaExp ("[" AsmExp "]")?

AsmUnaExp <- AsmTypePrefix AsmExp
           / ("offsetof" / "seg") AsmExp
           / ("+" / "-" / "!" / "~") AsmUnaExp
           / AsmPrimaryExp
           
AsmPrimaryExp <- IntegerLiteral
               / FloatLiteral
               / "__LOCAL_SIZE"
               / "$"
               / Register
               / DotIdentifier
               
DotIdentifier <- Identifier ("." DotIdentifier)?

AsmTypePrefix <- ( "near"
                 / "far"
                 / "byte"
                 / "short"
                 / "int"
                 / "word"
                 / "dword"
                 / "qword"
                 / "float"
                 / "double"
                 / "real") "ptr"

### Argh. I cheat. Not complete. ST(0) not there
Register <- Identifier
OpCode <- Identifier                 
                 
### interface.html

InterfaceDeclaration <- "interface" Identifier BaseInterfaceList? InterfaceBody
                      / InterfaceTemplateDeclaration
                      
BaseInterfaceList <- ":" List(Identifier)

InterfaceBody <- "{" DeclDefs? "}"

### pragma.html

Pragma <- "pragma" "(" Identifier ("," ArgumentList)? ")"

### struct.html

AggregateDeclaration <- ("struct" / "union") Identifier (StructBody / ";")
                      / StructTemplateDeclaration
                      / UnionTemplateDeclaration
                      
StructBody <- "{" StructBodyDeclarations? "}"

StructBodyDeclarations <- StructBodyDeclaration StructBodyDeclarations?

StructBodyDeclaration <- DeclDef
                       / StructAllocator 
                       / StructDeallocator
                       / StructPostblit
                       / AliasThis
                       
StructAllocator <- ClassAllocator

StructDeallocator <- ClassDeallocator

StructPostblit <- "this(this)" FunctionBody

### template.html

TemplateDeclaration <- "template" TemplateIdentifier "(" TemplateParameterList ")" Constraint?

TemplateIdentifier <- Identifier

TemplateParameterList <- List(TemplateParameter)

TemplateParameter <- TemplateTypeParameter
                   / TemplateValueParameter
                   / TemplateAliasParameter
                   / TemplateTupleParameter
                   / TemplateThisParameter
                   
TemplateInstance <- TemplateIdentifier ( "!(" List(TemplateArgument) ")"
                                       / "!" TemplateSingleArgument)
                                       
TemplateArgument <- Type
                  / AssignExpression
                  / Symbol
                  
Symbol <- "."? SymbolTail

SymbolTail <- TemplateInstance ("." SymbolTail)?
            / Identifier ("." SymbolTail)? 
            
TemplateSingleArgument <- BasicTypeX
                        / CharacterLiteral
                        / StringLiteral
                        / IntegerLiteral
                        / FloatLiteral
                        / "true"
                        / "false"
                        / "null"
                        / "__LINE__"
                        / "__FILE__"
                        / Identifier
                        
TemplateTypeParameter <- Identifier TTPSpecialization? TTPDefault?

TTPSpecialization <- ":" Type

TTPDefault <- "=" Type

TemplateThisParameter <- "this" TemplateTypeParameter

TemplateValueParameter <- BasicType Declarator TVPSpecialization? TVPDefault?

TVPSpecialization <- ":" ConditionalExpression

TVPDefault <- "=" ("__FILE__" / "__LINE__" / AssignExpression)

TemplateAliasParameter <- "alias" (BasicType Declarator / Identifier) TAPSpecialization? TAPDefault?

TAPSpecialization <- ":" (Type / ConditionalExpression)

TAPDefault <- "=" (Type / ConditionalExpression)

TemplateTupleParameter <- Identifier "..."

TemplatedConstructor <- "this" "(" TemplateParameterList ")" Parameters Constraint? FunctionBody

ClassTemplateDeclaration <- "class" Identifier "(" TemplateParameterList ")" Constraint? BaseClassList? ClassBody

StructTemplateDeclaration <- "struct" Identifier "(" TemplateParameterList ")" Constraint? StructBody

UnionTemplateDeclaration <- "union" Identifier "(" TemplateParameterList ")" Constraint? StructBody

InterfaceTemplateDeclaration <- "interface" Identifier "(" TemplateParameterList ")" Constraint? BaseInterfaceList? InterfaceBody

Constraint <- "if" "(" Expression ")"

### template-mixin.html

TemplateMixinDeclaration <- "mixin" "template" TemplateIdentifier "(" TemplateParameterList ")" Constraint? "{" DeclDefs "}"

TemplateMixin <- "mixin" TemplateIdentifier (("!(" List(TemplateArgument) ")")? MixinIdentifier?) ";"

MixinIdentifier <- Identifier

### traits.html

TraitsExpression <- "__traits" "(" TraitsKeyword "," List(TraitsArgument) ")"

TraitsKeyword <- "isAbstractClass"
               / "isArithmetic"
               / "isAssociativeArray"
               / "isFinalClass"
               / "isFloating"
               / "isIntegral"
               / "isScalar"
               / "isStaticArray"
               / "isUnsigned"
               / "isVitualFunction"
               / "isVirtualMethod"
               / "isAbstractFunction"
               / "isFinalFunction"
               / "isStaticFunction"
               / "isRef"
               / "isOut"
               / "isLazy"
               / "hasMember"
               / "identifier"
               / "getMember"
               / "getOverloads"
               / "getVirtualFunctions"
               / "getVirtualMethods"
               / "parent"
               / "classInstanceSize"
               / "allMembers"
               / "derivedMembers"
               / "isSame"
               / "compiles"

TraitsArgument <- AssignExpression
                / Type
              
### unittest.html

UnitTest <- "unittest" FunctionBody

### version.html

ConditionalDeclaration <- Condition ":" Declarations
                        / Condition CCDeclarationBlock ("else" CCDeclarationBlock)?
                        
CCDeclarationBlock <- Declaration
                    / "{" Declaration? "}"
                    
Declarations <- Declaration Declarations?

ConditionalStatement <- Condition NoScopeNonEmptyStatement ("else" NoScopeNonEmptyStatement)?

Condition <- VersionCondition
           / DebugCondition
           / StaticIfCondition
           
VersionCondition <- "version" "(" (IntegerLiteral / "unittest" / Identifier) ")"

VersionSpecification <- "version" "=" (Identifier/IntegerLiteral) ";"

DebugCondition <- "debug" ("(" (IntegerLiteral / Identifier) ")" )?

DebugSpecification <- "debug" "=" (Identifier / IntegerLiteral) ";"

StaticIfCondition <- "static" "if" "(" AssignExpression ")"

StaticAssert <- "static" "assert" "(" AssignExpression 
                                     ("," AssignExpression)? 
                                   ")" ";"
                                   
# I had to add it. Otherwise, keywords are recognized as identifiers
                                   
Identifier <~ !Keyword>[a-zA-Z_]>[a-zA-Z0-9_]*

Keyword <- "abstract" / "alias" / "align" / "asm" / "assert" / "auto" / "body" / "bool" / "break" / "byte" 
         / "case" / "cast" / "catch" / "cdouble" / "cent" / "cfloat" / "char" / "class" / "const" / "continue" / "creal" / "dchar" 
         / "debug" / "default" / "delegate" / "delete" / "deprecated" / "double" / "do" / "else" / "enum" / "export" / "extern" 
         / "false" / "finally" / "final" / "float" / "foreach_reverse" / "foreach" / "for" / "function" / "goto" / "idouble" / "if" 
         / "ifloat" / "immutable" / "import" / "inout" / "interface" / "invariant" / "int" / "in" / "ireal" / "is" / "lazy" 
         / "long" / "macro" / "mixin" / "module" / "new" / "nothrow" / "null" / "out" / "override" / "package" / "pragma" 
         / "private" / "protected" / "public" / "pure" / "real" / "ref" / "return" / "scope" / "shared" / "short" / "static" 
         / "struct" / "super" / "switch" / "synchronized" / "template" / "this" / "throw" / "true" / "try" / "typedef" / "typeid" 
         / "typeof" / "ubyte" / "ucent" / "uint" / "ulong" / "union" / "unittest" / "ushort" / "version" / "void" / "volatile" 
         / "wchar" / "while" / "with" / "__FILE__" / "__LINE__" / "__gshared" / "__thread" / "__traits"


*/
module pegged.examples.ddump;

import pegged.peg;

class Module : Seq!(Spacing,Option!(ModuleDeclaration),Option!(DeclDefs))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Module", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DeclDefs : Seq!(DeclDef,Option!(DeclDefs))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DeclDefs", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DeclDef : Or!(AttributeSpecifier,ImportDeclaration,EnumDeclaration,ClassDeclaration,InterfaceDeclaration,AggregateDeclaration,Declaration,Constructor,Destructor,UnitTest,StaticConstructor,StaticDestructor,SharedStaticConstructor,SharedStaticDestructor,ConditionalDeclaration,DebugSpecification,VersionSpecification,StaticAssert,TemplateDeclaration,TemplateMixinDeclaration,TemplateMixin,MixinDeclaration)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DeclDef", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ModuleDeclaration : Seq!(Lit!("module"),QualifiedIdentifier,Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ModuleDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ImportDeclaration : Or!(Seq!(Lit!("import"),ImportList,Lit!(";")),Seq!(Lit!("static"),Lit!("import"),ImportList,Lit!(";")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ImportDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ImportList : Or!(Seq!(Import,Option!(Seq!(Lit!(","),ImportList))),ImportBindings)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ImportList", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Import : Or!(Seq!(Identifier,Lit!("="),QualifiedIdentifier),QualifiedIdentifier)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Import", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ImportBindings : Seq!(Import,Lit!(":"),List!(ImportBind))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ImportBindings", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ImportBind : Seq!(Identifier,Option!(Seq!(Lit!("="),Identifier)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ImportBind", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class MixinDeclaration : Seq!(Lit!("mixin"),Lit!("("),AssignExpression,Lit!(")"),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("MixinDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Declaration : Or!(AliasDeclaration,AliasThisDeclaration,Decl)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Declaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AliasDeclaration : Seq!(Lit!("alias"),BasicType,Declarator)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AliasDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AliasThisDeclaration : Seq!(Lit!("alias"),Identifier,Lit!("this"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AliasThisDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Decl : Or!(Seq!(BasicType,Declarators,Lit!(";")),Seq!(BasicType,Declarator,FunctionBody),AutoDeclaration,Seq!(StorageClasses,Decl))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Decl", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Declarators : Seq!(DeclaratorInitializer,Option!(Seq!(Lit!(","),List!(DeclaratorIdentifier))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Declarators", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DeclaratorInitializer : Seq!(Declarator,Option!(Seq!(Lit!("="),Initializer)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DeclaratorInitializer", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DeclaratorIdentifier : Seq!(Identifier,Option!(Seq!(Lit!("="),Initializer)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DeclaratorIdentifier", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class BasicType : Or!(BasicTypeX,Seq!(Lit!("."),IdentifierList),IdentifierList,Seq!(Typeof,Lit!("."),IdentifierList),Seq!(Lit!("const("),Type,Lit!(")")),Seq!(Lit!("immutable("),Type,Lit!(")")),Seq!(Lit!("shared("),Type,Lit!(")")),Seq!(Lit!("inout("),Type,Lit!(")")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("BasicType", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class BasicTypeX : Or!(Lit!("bool"),Lit!("byte"),Lit!("ubyte"),Lit!("short"),Lit!("ushort"),Lit!("int"),Lit!("uint"),Lit!("long"),Lit!("ulong"),Lit!("char"),Lit!("wchar"),Lit!("dchar"),Lit!("float"),Lit!("double"),Lit!("real"),Lit!("void"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("BasicTypeX", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class BasicType2 : Or!(Lit!("*"),Seq!(Lit!("["),Lit!("]")),Seq!(Lit!("["),AssignExpression,Lit!(".."),AssignExpression,Lit!("]")),Seq!(Lit!("["),Type,Lit!("]")),Seq!(Lit!("delegate"),Parameters,Option!(FunctionAttributes)),Seq!(Lit!("function"),Parameters,Option!(FunctionAttributes)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("BasicType2", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Declarator : Or!(Seq!(Option!(BasicType2),Lit!("("),Declarator,Lit!(")"),Option!(DeclaratorSuffixes)),Seq!(Option!(BasicType2),Identifier,Option!(DeclaratorSuffixes)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Declarator", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DeclaratorSuffixes : Seq!(DeclaratorSuffix,DeclaratorSuffixes)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DeclaratorSuffixes", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DeclaratorSuffix : Or!(Seq!(Lit!("["),Lit!("]")),Seq!(Lit!("["),AssignExpression,Lit!("]")),Seq!(Lit!("["),Type,Lit!("]")),Seq!(Option!(TemplateParameterList),Parameters,Option!(MemberFunctionAttributes),Option!(Constraint)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DeclaratorSuffix", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class IdentifierList : Or!(Seq!(Identifier,Option!(Seq!(Lit!("."),IdentifierList))),Seq!(TemplateInstance,Option!(Seq!(Lit!("."),IdentifierList))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("IdentifierList", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StorageClasses : Seq!(StorageClass,Option!(StorageClasses))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StorageClasses", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StorageClass : Or!(Lit!("abstract"),Lit!("auto"),Lit!("const"),Lit!("deprecated"),Lit!("enum"),Lit!("extern"),Lit!("final"),Lit!("immutable"),Lit!("inout"),Lit!("shared"),Lit!("nothrow"),Lit!("override"),Lit!("pure"),Lit!("__gshared"),Property,Lit!("scope"),Lit!("static"),Lit!("synchronized"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StorageClass", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Property : Seq!(Lit!("@"),Or!(Lit!("property"),Lit!("safe"),Lit!("trusted"),Lit!("system"),Lit!("disable")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Property", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Type : Seq!(BasicType,Option!(Declarator2))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Type", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Declarator2 : Seq!(Option!(BasicType2),Option!(Seq!(Lit!("("),Declarator2,Lit!(")"))),Option!(DeclaratorSuffixes))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Declarator2", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Parameters : Seq!(Lit!("("),Option!(ParameterList),Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Parameters", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ParameterList : Or!(Lit!("..."),Seq!(Parameter,Option!(ParameterList)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ParameterList", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Parameter : Or!(Seq!(Option!(InOut),BasicType,Declarator,Option!(Or!(Lit!("..."),Seq!(Lit!("="),DefaultInitializerExpression)))),Seq!(Option!(InOut),Type,Option!(Lit!("..."))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Parameter", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class InOut : Seq!(InOutX,Option!(InOut))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("InOut", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class InOutX : Or!(Lit!("auto"),Lit!("const"),Lit!("final"),Lit!("immutable"),Lit!("inout"),Lit!("in"),Lit!("lazy"),Lit!("out"),Lit!("ref"),Lit!("scope"),Lit!("shared"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("InOutX", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class FunctionAttributes : Seq!(FunctionAttribute,Option!(FunctionAttributes))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("FunctionAttributes", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class FunctionAttribute : Or!(Lit!("nothrow"),Lit!("pure"),Property)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("FunctionAttribute", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class MemberFunctionAttributes : Seq!(MemberFunctionAttribute,Option!(MemberFunctionAttributes))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("MemberFunctionAttributes", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class MemberFunctionAttribute : Or!(Lit!("const"),Lit!("immutable"),Lit!("inout"),Lit!("shared"),FunctionAttribute)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("MemberFunctionAttribute", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DefaultInitializerExpression : Or!(AssignExpression,Lit!("__FILE__"),Lit!("__LINE__"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DefaultInitializerExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Initializer : Or!(VoidInitializer,NonVoidInitializer)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Initializer", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class NonVoidInitializer : Or!(AssignExpression,ArrayInitializer,StructInitializer)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("NonVoidInitializer", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ArrayInitializer : Or!(Seq!(Lit!("["),Lit!("]")),Seq!(Lit!("["),ArrayMemberInitializations,Lit!("]")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ArrayInitializer", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ArrayMemberInitializations : Seq!(ArrayMemberInitialization,Option!(Seq!(Lit!(","),Option!(ArrayMemberInitializations))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ArrayMemberInitializations", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ArrayMemberInitialization : Or!(NonVoidInitializer,Seq!(AssignExpression,Lit!(":"),NonVoidInitializer))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ArrayMemberInitialization", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StructInitializer : Or!(Seq!(Lit!("{"),Lit!("}")),Seq!(Lit!("{"),StructMemberInitializers,Lit!("}")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StructInitializer", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StructMemberInitializers : Seq!(StructMemberInitializer,Option!(Seq!(Lit!(","),Option!(StructMemberInitializers))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StructMemberInitializers", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StructMemberInitializer : Or!(NonVoidInitializer,Seq!(Identifier,Drop!(NonVoidInitializer)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StructMemberInitializer", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AutoDeclaration : Seq!(StorageClasses,AutoDeclarationX,Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AutoDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AutoDeclarationX : List!(Seq!(Identifier,Lit!("="),Initializer))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AutoDeclarationX", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Typeof : Or!(Seq!(Lit!("typeof"),Lit!("("),Expression,Lit!(")")),Seq!(Lit!("typeof"),Lit!("("),Lit!("return"),Lit!(")")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Typeof", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class VoidInitializer : Lit!("void")
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("VoidInitializer", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Statement : Or!(Lit!(";"),NonEmptyStatement,ScopeBlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Statement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class NoScopeNonEmptyStatement : Or!(NonEmptyStatement,BlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("NoScopeNonEmptyStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class NoScopeStatement : Or!(Lit!(";"),NonEmptyStatement,BlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("NoScopeStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class NonEmptyOrScopeBlockStatement : Or!(NonEmptyStatement,ScopeBlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("NonEmptyOrScopeBlockStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class NonEmptyStatement : Or!(NonEmptyStatementNoCaseNoDefault,CaseStatement,CaseRangeStatement,DefaultStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("NonEmptyStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class NonEmptyStatementNoCaseNoDefault : Or!(LabeledStatement,ExpressionStatement,DeclarationStatement,IfStatement,WhileStatement,DoStatement,ForStatement,ForeachStatement,SwitchStatement,FinalSwitchStatement,ContinueStatement,BreakStatement,ReturnStatement,GotoStatement,WithStatement,SynchronizedStatement,TryStatement,ScopeGuardStatement,ThrowStatement,AsmStatement,PragmaStatement,MixinStatement,ForeachRangeStatement,ConditionalStatement,StaticAssert,TemplateMixin,ImportDeclaration)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("NonEmptyStatementNoCaseNoDefault", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ScopeStatement : Or!(NonEmptyStatement,BlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ScopeStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ScopeBlockStatement : ScopeStatement
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ScopeBlockStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class LabeledStatement : Seq!(Identifier,Lit!(":"),NoScopeStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("LabeledStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class BlockStatement : Seq!(Lit!("{"),Option!(StatementList),Lit!("}"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("BlockStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StatementList : Seq!(Statement,Option!(StatementList))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StatementList", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ExpressionStatement : Seq!(Expression,Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ExpressionStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DeclarationStatement : Declaration
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DeclarationStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class IfStatement : Seq!(Lit!("if"),Lit!("("),IfCondition,Lit!(")"),ThenStatement,Lit!("else"),ElseStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("IfStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class IfCondition : Or!(Expression,Seq!(Lit!("auto"),Identifier,Lit!("="),Expression),Seq!(BasicType,Declarator,Lit!("="),Expression))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("IfCondition", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ThenStatement : ScopeStatement
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ThenStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ElseStatement : ScopeStatement
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ElseStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class WhileStatement : Seq!(Lit!("while"),Lit!("("),Expression,Lit!(")"),ScopeStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("WhileStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DoStatement : Seq!(Lit!("do"),ScopeStatement,Lit!("while"),Lit!("("),Expression,Lit!(")"),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DoStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ForStatement : Seq!(Lit!("for"),Lit!("("),Initialize,Option!(Test),Lit!(";"),Option!(Increment),Lit!(")"),ScopeStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ForStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Initialize : Or!(Lit!(";"),NoScopeNonEmptyStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Initialize", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Test : Expression
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Test", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Increment : Expression
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Increment", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ForeachStatement : Seq!(Or!(Lit!("foreach"),Lit!("foreach_reverse")),Lit!("("),List!(ForeachType),Lit!(";"),Aggregate,Lit!(")"),NoScopeNonEmptyStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ForeachStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ForeachType : Or!(Seq!(Option!(Lit!("ref")),BasicType,Declarator),Seq!(Option!(Lit!("ref")),Identifier))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ForeachType", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Aggregate : Expression
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Aggregate", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ForeachRangeStatement : Seq!(Lit!("("),ForeachType,Lit!(";"),Expression,Lit!(".."),Expression,Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ForeachRangeStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class SwitchStatement : Seq!(Lit!("switch"),Lit!("("),Expression,Lit!(")"),ScopeStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("SwitchStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class CaseStatement : Seq!(Lit!("case"),ArgumentList,Lit!(":"),ScopeStatementList)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("CaseStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class CaseRangeStatement : Seq!(Lit!("case"),AssignExpression,Lit!(":"),Lit!(".."),Lit!("case"),AssignExpression,Lit!(":"),ScopeStatementList)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("CaseRangeStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DefaultStatement : Seq!(Lit!("default"),Lit!(":"),ScopeStatementList)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DefaultStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ScopeStatementList : StatementListNoCaseNoDefault
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ScopeStatementList", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StatementListNoCaseNoDefault : Seq!(StatementNoCaseNoDefault,Option!(StatementListNoCaseNoDefault))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StatementListNoCaseNoDefault", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StatementNoCaseNoDefault : Or!(Lit!(";"),NonEmptyStatementNoCaseNoDefault,ScopeBlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StatementNoCaseNoDefault", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class FinalSwitchStatement : Seq!(Lit!("final"),Lit!("switch"),Lit!("("),Expression,Lit!(")"),ScopeStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("FinalSwitchStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ContinueStatement : Seq!(Lit!("continue"),Option!(Identifier),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ContinueStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class BreakStatement : Seq!(Lit!("break"),Option!(Identifier),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("BreakStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ReturnStatement : Seq!(Lit!("return"),Option!(Expression),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ReturnStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class GotoStatement : Seq!(Lit!("goto"),Or!(Seq!(Lit!("default"),Lit!(";")),Seq!(Lit!("case"),Lit!(";")),Seq!(Lit!("case"),Expression,Lit!(";")),Seq!(Identifier,Lit!(";"))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("GotoStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class WithStatement : Seq!(Lit!("with"),Lit!("("),Or!(Expression,Symbol,TemplateInstance),Lit!(")"),ScopeStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("WithStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class SynchronizedStatement : Seq!(Lit!("synchronized"),Option!(Seq!(Lit!("("),Expression,Lit!(")"))),ScopeStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("SynchronizedStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TryStatement : Seq!(Lit!("try"),ScopeStatement,Option!(Catches),Option!(FinallyStatement))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TryStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Catches : Or!(LastCatch,Seq!(Catch,Option!(Catches)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Catches", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class LastCatch : Seq!(Lit!("catch"),NoScopeNonEmptyStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("LastCatch", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Catch : Seq!(Lit!("catch"),Lit!("("),CatchParameter,Lit!(")"),NoScopeNonEmptyStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Catch", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class CatchParameter : Seq!(BasicType,Identifier)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("CatchParameter", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class FinallyStatement : Seq!(Lit!("finally"),NoScopeNonEmptyStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("FinallyStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ThrowStatement : Seq!(Lit!("throw"),Expression,Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ThrowStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ScopeGuardStatement : Seq!(Or!(Lit!("scope(exit)"),Lit!("scope(success)"),Lit!("scope(failure)")),NonEmptyOrScopeBlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ScopeGuardStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmStatement : Seq!(Lit!("asm"),Lit!("{"),Option!(AsmInstructionList),Lit!("}"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmInstructionList : Seq!(AsmInstruction,Lit!(";"),Option!(AsmInstructionList))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmInstructionList", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class PragmaStatement : Seq!(Pragma,NoScopeStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("PragmaStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class MixinStatement : Seq!(Lit!("mixin"),Lit!("("),AssignExpression,Lit!(")"),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("MixinStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Expression : List!(AssignExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Expression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AssignExpression : Seq!(ConditionalExpression,Option!(Seq!(Op,AssignExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AssignExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Op : Or!(Lit!(">>>="),Lit!("^^="),Lit!(">>="),Lit!("<<="),Lit!("~="),Lit!("+="),Lit!("-="),Lit!("*="),Lit!("^="),Lit!("|="),Lit!("&="),Lit!("/="),Lit!("="))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Op", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ConditionalExpression : Seq!(OrOrExpression,Option!(Seq!(Lit!("?"),Expression,Lit!(":"),ConditionalExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ConditionalExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class OrOrExpression : Seq!(AndAndExpression,Option!(Seq!(Lit!("||"),OrOrExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("OrOrExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AndAndExpression : Seq!(Or!(OrExpression,CmpExpression),Option!(Seq!(Lit!("&&"),AndAndExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AndAndExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class OrExpression : Seq!(XorExpression,Option!(Seq!(Lit!("|"),OrExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("OrExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class XorExpression : Seq!(AndExpression,Option!(Seq!(Lit!("^"),XorExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("XorExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AndExpression : Seq!(ShiftExpression,Option!(Seq!(Lit!("&"),AndExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AndExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class CmpExpression : Or!(ShiftExpression,EqualExpression,IdentityExpression,RelExpression,InExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("CmpExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class EqualExpression : Seq!(ShiftExpression,Or!(Lit!("=="),Lit!("!=")),ShiftExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("EqualExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class IdentityExpression : Seq!(ShiftExpression,Or!(Lit!("!is"),Lit!("is")),ShiftExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("IdentityExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class RelExpression : Seq!(ShiftExpression,RelOp,ShiftExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("RelExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class RelOp : Or!(Lit!("!<>="),Lit!("!<>"),Lit!("!<="),Lit!("!>="),Lit!("<>="),Lit!("<="),Lit!(">="),Lit!("<>"),Lit!("!>"),Lit!("!<"),Lit!("<"),Lit!(">"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("RelOp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class InExpression : Seq!(ShiftExpression,Option!(Seq!(Or!(Lit!("!in"),Lit!("in")),ShiftExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("InExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ShiftExpression : Seq!(AddExpression,Option!(Seq!(Or!(Lit!(">>>"),Lit!(">>"),Lit!("<<")),AddExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ShiftExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AddExpression : Seq!(Or!(MulExpression,CatExpression),Option!(Seq!(Or!(Lit!("+"),Lit!("-")),MulExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AddExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class CatExpression : Seq!(AddExpression,Option!(Seq!(Lit!("~"),MulExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("CatExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class MulExpression : Seq!(UnaryExpression,Option!(Seq!(Or!(Lit!("*"),Lit!("/"),Lit!("%")),UnaryExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("MulExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class UnaryExpression : Or!(Seq!(UnaryOp,UnaryExpression),ComplementExpression,Seq!(Lit!("("),Type,Lit!(")"),Lit!("."),Identifier),NewExpression,DeleteExpression,CastExpression,PowExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("UnaryExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class UnaryOp : Or!(Lit!("++"),Lit!("--"),Lit!("+"),Lit!("-"),Lit!("&"),Lit!("*"),Lit!("/"),Lit!("!"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("UnaryOp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ComplementExpression : Seq!(Lit!("~"),UnaryExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ComplementExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class NewExpression : Or!(Seq!(Lit!("new"),Option!(AllocatorArguments),Type,Option!(Or!(Seq!(Lit!("["),AssignExpression,Lit!("]")),Seq!(Lit!("("),ArgumentList,Lit!(")"))))),NewAnonClassExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("NewExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AllocatorArguments : Seq!(Lit!("("),ArgumentList,Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AllocatorArguments", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ArgumentList : List!(AssignExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ArgumentList", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DeleteExpression : Seq!(Lit!("delete"),UnaryExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DeleteExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class CastExpression : Seq!(Lit!("cast"),Lit!("("),Option!(Or!(Type,CastEqual)),Lit!(")"),UnaryExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("CastExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class CastEqual : Or!(Seq!(Lit!("const"),Lit!("shared")),Seq!(Lit!("shared"),Lit!("const")),Seq!(Lit!("inout"),Lit!("shared")),Seq!(Lit!("shared"),Lit!("inout")),Lit!("const"),Lit!("inout"),Lit!("immutable"),Lit!("shared"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("CastEqual", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class PowExpression : Seq!(PostfixExpression,Option!(Seq!(Lit!("^^"),UnaryExpression)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("PowExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class PostfixExpression : Or!(PrimaryExpression,IndexExpression,SliceExpression,Seq!(PostfixExpression,Or!(Seq!(Lit!("."),NewExpression),Seq!(Lit!("."),TemplateIdentifier),Seq!(Lit!("."),Identifier),Lit!("++"),Lit!("--"),Seq!(Lit!("("),Option!(ArgumentList),Lit!(")")))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("PostfixExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class IndexExpression : Seq!(PostfixExpression,Lit!("["),ArgumentList,Lit!("]"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("IndexExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class SliceExpression : Seq!(PostfixExpression,Or!(Seq!(Lit!("["),Lit!("]")),Seq!(Lit!("["),AssignExpression,Lit!(".."),AssignExpression,Lit!("]"))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("SliceExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class PrimaryExpression : Or!(Lit!("this"),Lit!("super"),Lit!("null"),Lit!("true"),Lit!("false"),Lit!("$"),Lit!("__FILE__"),Lit!("__LINE__"),Identifier,Join!(Lit!("."),Identifier),TemplateInstance,Join!(Lit!("."),TemplateInstance),IntegerLiteral,FloatLiteral,CharacterLiteral,StringLiterals,ArrayLiteral,AssocArrayLiteral,Lambda,FunctionLiteral,AssertExpression,MixinExpression,ImportExpression,Seq!(BasicType,Lit!("."),Identifier),Typeof,TypeidExpression,IsExpression,Seq!(Lit!("("),Expression,Lit!(")")),TraitsExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("PrimaryExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StringLiterals : Seq!(StringLiteral,Option!(StringLiterals))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StringLiterals", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ArrayLiteral : Seq!(Lit!("["),ArgumentList,Lit!("]"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ArrayLiteral", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AssocArrayLiteral : Seq!(Lit!("["),List!(KeyValuePair),Lit!("]"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AssocArrayLiteral", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class KeyValuePair : Seq!(AssignExpression,Lit!(":"),AssignExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("KeyValuePair", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Lambda : Or!(Seq!(Identifier,Lit!("=>"),AssignExpression),Seq!(ParameterAttributes,Lit!("=>"),AssignExpression))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Lambda", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class FunctionLiteral : Seq!(Option!(Seq!(Or!(Lit!("function"),Lit!("delegate")),Option!(Type))),Option!(ParameterAttributes),FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("FunctionLiteral", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ParameterAttributes : Seq!(Parameters,Option!(FunctionAttributes))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ParameterAttributes", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AssertExpression : Seq!(Lit!("assert"),Lit!("("),AssignExpression,Option!(Seq!(Lit!(","),AssignExpression)),Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AssertExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class MixinExpression : Seq!(Lit!("mixin"),Lit!("("),AssignExpression,Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("MixinExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ImportExpression : Seq!(Lit!("import"),Lit!("("),AssignExpression,Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ImportExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TypeidExpression : Seq!(Lit!("typeid"),Lit!("("),Or!(Type,Expression),Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TypeidExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class IsExpression : Seq!(Lit!("is"),Lit!("("),Type,Option!(Or!(Seq!(Lit!(":"),TypeSpecialization),Seq!(Lit!("=="),TypeSpecialization),Seq!(Identifier,Option!(Or!(Seq!(Lit!(":"),TypeSpecialization,Option!(Seq!(Lit!(","),TemplateParameterList))),Seq!(Lit!("=="),TypeSpecialization,Option!(Seq!(Lit!(","),TemplateParameterList)))))))),Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("IsExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TypeSpecialization : Or!(Type,Lit!("struct"),Lit!("union"),Lit!("class"),Lit!("interface"),Lit!("enum"),Lit!("function"),Lit!("delegate"),Lit!("super"),Lit!("const"),Lit!("immutable"),Lit!("inout"),Lit!("shared"),Lit!("return"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TypeSpecialization", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Comment : Or!(BlockComment,LineComment,NestingBlockComment)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Comment", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class BlockComment : Seq!(Lit!("/*"),ZeroOrMore!(Join!(NegLookAhead!(Lit!("*/")),Any)),Lit!("*/"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("BlockComment", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class LineComment : Seq!(Lit!("//"),ZeroOrMore!(Join!(NegLookAhead!(EOL),Any)),EOL)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("LineComment", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class NestingBlockComment : Seq!(Lit!("/+"),Or!(NestingBlockComment,Text),Lit!("+/"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("NestingBlockComment", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Text : ZeroOrMore!(Join!(NegLookAhead!(Lit!("+/")),Any))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Text", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StringLiteral : Or!(WysiwygString,AlternateWysiwygString,DoubleQuotedString,TokenString)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StringLiteral", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class WysiwygString : Seq!(Join!(Lit!("r"),DoubleQuote,ZeroOrMore!(Join!(NegLookAhead!(DoubleQuote),Any))),DoubleQuote,Option!(StringPostfix))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("WysiwygString", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AlternateWysiwygString : Seq!(Join!(BackQuote,ZeroOrMore!(Join!(NegLookAhead!(BackQuote),Any))),BackQuote,Option!(StringPostfix))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AlternateWysiwygString", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DoubleQuotedString : Seq!(Join!(DoubleQuote,ZeroOrMore!(DQChar)),DoubleQuote,Option!(StringPostfix))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DoubleQuotedString", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DQChar : Or!(EscapeSequence,Join!(NegLookAhead!(DoubleQuote),Any))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DQChar", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class EscapeSequence : Join!(BackSlash,Or!(Quote,DoubleQuote,BackSlash,Or!(Lit!("a"),Lit!("b"),Lit!("f"),Lit!("n"),Lit!("r"),Lit!("t"),Lit!("v")),Join!(Lit!("x"),HexDigit,HexDigit),Join!(Lit!("u"),HexDigit,HexDigit,HexDigit,HexDigit),Join!(Lit!("U"),HexDigit,HexDigit,HexDigit,HexDigit,HexDigit,HexDigit,HexDigit,HexDigit)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("EscapeSequence", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StringPostfix : Or!(Lit!("c"),Lit!("w"),Lit!("d"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StringPostfix", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TokenString : Seq!(Join!(Lit!("q{"),ZeroOrMore!(Join!(NegLookAhead!(Lit!("}")),Any))),Lit!("}"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TokenString", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class CharacterLiteral : Join!(Quote,NegLookAhead!(Quote),Or!(EscapeSequence,Any),Quote)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("CharacterLiteral", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class IntegerLiteral : Or!(DecimalInteger,BinaryInteger,HexadecimalInteger)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("IntegerLiteral", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DecimalInteger : Seq!(Integer,Option!(IntegerSuffix))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DecimalInteger", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Integer : Seq!(Digit,ZeroOrMore!(Or!(Digit,Lit!("_"))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Integer", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class IntegerSuffix : Or!(Lit!("Lu"),Lit!("LU"),Lit!("uL"),Lit!("UL"),Lit!("L"),Lit!("u"),Lit!("U"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("IntegerSuffix", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class BinaryInteger : Seq!(Or!(Lit!("0b"),Lit!("0B")),Or!(Lit!("0"),Lit!("1")),ZeroOrMore!(Or!(Or!(Lit!("0"),Lit!("1")),Lit!("_"))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("BinaryInteger", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class HexadecimalInteger : Seq!(Or!(Lit!("0x"),Lit!("0X")),HexDigit,ZeroOrMore!(Or!(HexDigit,Lit!("_"))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("HexadecimalInteger", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class HexDigit : Or!(Range!('0','9'),Range!('a','f'),Range!('A','F'))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("HexDigit", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class FloatLiteral : Seq!(Option!(Sign),Integer,Option!(Seq!(Lit!("."),Option!(Integer))),Option!(Seq!(Or!(Lit!("e"),Lit!("E")),Option!(Sign),Integer)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("FloatLiteral", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Sign : Option!(Or!(Lit!("-"),Lit!("+")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Sign", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AttributeSpecifier : Or!(Seq!(Attribute,DeclarationBlock),Seq!(Attribute,Lit!(":")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AttributeSpecifier", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Attribute : Or!(LinkageAttribute,AlignAttribute,Pragma,Lit!("deprecated"),ProtectionAttribute,Lit!("static"),Lit!("extern"),Lit!("final"),Lit!("synchronized"),Lit!("override"),Lit!("abstract"),Lit!("const"),Lit!("auto"),Lit!("scope"),Lit!("__gshared"),Lit!("shared"),Lit!("immutable"),Lit!("inout"),Lit!("@disable"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Attribute", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DeclarationBlock : Or!(DeclDef,Seq!(Lit!("{"),DeclDefs,Lit!("}")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DeclarationBlock", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class LinkageAttribute : Seq!(Lit!("extern"),Lit!("("),LinkageType,Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("LinkageAttribute", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class LinkageType : Or!(Lit!("C++"),Lit!("C"),Lit!("D"),Lit!("Windows"),Lit!("Pascal"),Lit!("System"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("LinkageType", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AlignAttribute : Seq!(Lit!("align"),Option!(Seq!(Lit!("("),IntegerLiteral,Lit!(")"))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AlignAttribute", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ProtectionAttribute : Or!(Lit!("private"),Lit!("package"),Lit!("protected"),Lit!("public"),Lit!("export"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ProtectionAttribute", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ClassDeclaration : Seq!(Lit!("class"),Identifier,Option!(BaseClassList),ClassBody,ClassTemplateDeclaration)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ClassDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class BaseClassList : Seq!(Lit!(":"),List!(Identifier))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("BaseClassList", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ClassBody : Seq!(Lit!("{"),Option!(ClassBodyDeclarations),Lit!("}"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ClassBody", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ClassBodyDeclarations : Seq!(ClassBodyDeclaration,Option!(ClassBodyDeclarations))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ClassBodyDeclarations", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ClassBodyDeclaration : Or!(DeclDef,Invariant,ClassAllocator,ClassDeallocator)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ClassBodyDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Constructor : Or!(Seq!(Lit!("this"),Parameters,FunctionBody),TemplatedConstructor)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Constructor", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Destructor : Seq!(Lit!("~"),Lit!("this"),Lit!("("),Lit!(")"),FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Destructor", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StaticConstructor : Seq!(Lit!("static"),Lit!("this"),Lit!("("),Lit!(")"),FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StaticConstructor", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StaticDestructor : Seq!(Lit!("static"),Lit!("~"),Lit!("this"),Lit!("("),Lit!(")"),FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StaticDestructor", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class SharedStaticConstructor : Seq!(Lit!("shared"),Lit!("static"),Lit!("this"),Lit!("("),Lit!(")"),FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("SharedStaticConstructor", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class SharedStaticDestructor : Seq!(Lit!("shared"),Lit!("static"),Lit!("~"),Lit!("this"),Lit!("("),Lit!(")"),FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("SharedStaticDestructor", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Invariant : Seq!(Lit!("invariant"),Lit!("("),Lit!(")"),BlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Invariant", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ClassAllocator : Seq!(Lit!("new"),Parameters,FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ClassAllocator", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ClassDeallocator : Seq!(Lit!("delete"),Parameters,FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ClassDeallocator", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AliasThis : Seq!(Lit!("alias"),Identifier,Lit!("this"),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AliasThis", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class NewAnonClassExpression : Seq!(Lit!("new"),Option!(AllocatorArguments),Lit!("class"),Option!(ClassArguments),Identifier,Option!(List!(Identifier)),ClassBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("NewAnonClassExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ClassArguments : Seq!(Lit!("("),Option!(ArgumentList),Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ClassArguments", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class EnumDeclaration : Seq!(Lit!("enum"),Option!(EnumTag),Option!(Seq!(Lit!(":"),EnumBaseType)),EnumBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("EnumDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class EnumTag : Identifier
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("EnumTag", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class EnumBaseType : Type
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("EnumBaseType", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class EnumBody : Or!(Lit!(";"),Seq!(Lit!("{"),List!(EnumMember),Lit!("}")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("EnumBody", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class EnumMember : Or!(Seq!(Type,Lit!("="),AssignExpression),Seq!(Identifier,Option!(Seq!(Lit!("="),AssignExpression))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("EnumMember", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class FunctionBody : Or!(BlockStatement,BodyStatement,Seq!(InStatement,BodyStatement),Seq!(OutStatement,BodyStatement),Seq!(InStatement,OutStatement,BodyStatement),Seq!(OutStatement,InStatement,BodyStatement))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("FunctionBody", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class InStatement : Seq!(Lit!("in"),BlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("InStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class OutStatement : Seq!(Lit!("out"),Option!(Seq!(Lit!("("),Identifier,Lit!(")"))),BlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("OutStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class BodyStatement : Seq!(Lit!("body"),BlockStatement)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("BodyStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmInstruction : Or!(Seq!(Lit!("align"),IntegerExpression),Lit!("even"),Lit!("naked"),Seq!(Or!(Lit!("db"),Lit!("ds"),Lit!("di"),Lit!("dl"),Lit!("df"),Lit!("dd"),Lit!("de")),List!(Operand)),Seq!(Identifier,Lit!(":"),AsmInstruction),OpCode,Seq!(OpCode,List!(Operand)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmInstruction", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class IntegerExpression : Or!(IntegerLiteral,Identifier)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("IntegerExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Operand : AsmExp
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Operand", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmExp : Seq!(AsmLogOrExp,Option!(Seq!(Lit!("?"),AsmExp,Lit!(":"),AsmExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmLogOrExp : Seq!(AsmLogAndExp,Option!(Seq!(Lit!("||"),AsmLogAndExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmLogOrExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmLogAndExp : Seq!(AsmOrExp,Option!(Seq!(Lit!("&&"),AsmOrExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmLogAndExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmOrExp : Seq!(AsmXorExp,Option!(Seq!(Lit!("|"),AsmXorExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmOrExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmXorExp : Seq!(AsmAndExp,Option!(Seq!(Lit!("^"),AsmAndExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmXorExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmAndExp : Seq!(AsmEqualExp,Option!(Seq!(Lit!("&"),AsmEqualExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmAndExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmEqualExp : Seq!(AsmRelExp,Option!(Seq!(Or!(Lit!("=="),Lit!("!=")),AsmRelExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmEqualExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmRelExp : Seq!(AsmShiftExp,Option!(Seq!(Or!(Lit!("<="),Lit!(">="),Lit!("<"),Lit!(">")),AsmShiftExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmRelExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmShiftExp : Seq!(AsmAddExp,Option!(Seq!(Or!(Lit!(">>>"),Lit!("<<"),Lit!(">>")),AsmAddExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmShiftExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmAddExp : Seq!(AsmMulExp,Option!(Seq!(Or!(Lit!("+"),Lit!("-")),AsmMulExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmAddExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmMulExp : Seq!(AsmBrExp,Option!(Seq!(Or!(Lit!("*"),Lit!("/"),Lit!("%")),AsmBrExp)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmMulExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmBrExp : Seq!(AsmUnaExp,Option!(Seq!(Lit!("["),AsmExp,Lit!("]"))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmBrExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmUnaExp : Or!(Seq!(AsmTypePrefix,AsmExp),Seq!(Or!(Lit!("offsetof"),Lit!("seg")),AsmExp),Seq!(Or!(Lit!("+"),Lit!("-"),Lit!("!"),Lit!("~")),AsmUnaExp),AsmPrimaryExp)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmUnaExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmPrimaryExp : Or!(IntegerLiteral,FloatLiteral,Lit!("__LOCAL_SIZE"),Lit!("$"),Register,DotIdentifier)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmPrimaryExp", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DotIdentifier : Seq!(Identifier,Option!(Seq!(Lit!("."),DotIdentifier)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DotIdentifier", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AsmTypePrefix : Seq!(Or!(Lit!("near"),Lit!("far"),Lit!("byte"),Lit!("short"),Lit!("int"),Lit!("word"),Lit!("dword"),Lit!("qword"),Lit!("float"),Lit!("double"),Lit!("real")),Lit!("ptr"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AsmTypePrefix", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Register : Identifier
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Register", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class OpCode : Identifier
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("OpCode", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class InterfaceDeclaration : Or!(Seq!(Lit!("interface"),Identifier,Option!(BaseInterfaceList),InterfaceBody),InterfaceTemplateDeclaration)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("InterfaceDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class BaseInterfaceList : Seq!(Lit!(":"),List!(Identifier))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("BaseInterfaceList", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class InterfaceBody : Seq!(Lit!("{"),Option!(DeclDefs),Lit!("}"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("InterfaceBody", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Pragma : Seq!(Lit!("pragma"),Lit!("("),Identifier,Option!(Seq!(Lit!(","),ArgumentList)),Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Pragma", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class AggregateDeclaration : Or!(Seq!(Or!(Lit!("struct"),Lit!("union")),Identifier,Or!(StructBody,Lit!(";"))),StructTemplateDeclaration,UnionTemplateDeclaration)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("AggregateDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StructBody : Seq!(Lit!("{"),Option!(StructBodyDeclarations),Lit!("}"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StructBody", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StructBodyDeclarations : Seq!(StructBodyDeclaration,Option!(StructBodyDeclarations))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StructBodyDeclarations", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StructBodyDeclaration : Or!(DeclDef,StructAllocator,StructDeallocator,StructPostblit,AliasThis)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StructBodyDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StructAllocator : ClassAllocator
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StructAllocator", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StructDeallocator : ClassDeallocator
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StructDeallocator", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StructPostblit : Seq!(Lit!("this(this)"),FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StructPostblit", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateDeclaration : Seq!(Lit!("template"),TemplateIdentifier,Lit!("("),TemplateParameterList,Lit!(")"),Option!(Constraint))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateIdentifier : Identifier
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateIdentifier", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateParameterList : List!(TemplateParameter)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateParameterList", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateParameter : Or!(TemplateTypeParameter,TemplateValueParameter,TemplateAliasParameter,TemplateTupleParameter,TemplateThisParameter)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateParameter", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateInstance : Seq!(TemplateIdentifier,Or!(Seq!(Lit!("!("),List!(TemplateArgument),Lit!(")")),Seq!(Lit!("!"),TemplateSingleArgument)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateInstance", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateArgument : Or!(Type,AssignExpression,Symbol)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateArgument", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Symbol : Seq!(Option!(Lit!(".")),SymbolTail)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Symbol", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class SymbolTail : Or!(Seq!(TemplateInstance,Option!(Seq!(Lit!("."),SymbolTail))),Seq!(Identifier,Option!(Seq!(Lit!("."),SymbolTail))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("SymbolTail", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateSingleArgument : Or!(BasicTypeX,CharacterLiteral,StringLiteral,IntegerLiteral,FloatLiteral,Lit!("true"),Lit!("false"),Lit!("null"),Lit!("__LINE__"),Lit!("__FILE__"),Identifier)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateSingleArgument", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateTypeParameter : Seq!(Identifier,Option!(TTPSpecialization),Option!(TTPDefault))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateTypeParameter", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TTPSpecialization : Seq!(Lit!(":"),Type)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TTPSpecialization", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TTPDefault : Seq!(Lit!("="),Type)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TTPDefault", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateThisParameter : Seq!(Lit!("this"),TemplateTypeParameter)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateThisParameter", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateValueParameter : Seq!(BasicType,Declarator,Option!(TVPSpecialization),Option!(TVPDefault))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateValueParameter", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TVPSpecialization : Seq!(Lit!(":"),ConditionalExpression)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TVPSpecialization", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TVPDefault : Seq!(Lit!("="),Or!(Lit!("__FILE__"),Lit!("__LINE__"),AssignExpression))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TVPDefault", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateAliasParameter : Seq!(Lit!("alias"),Or!(Seq!(BasicType,Declarator),Identifier),Option!(TAPSpecialization),Option!(TAPDefault))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateAliasParameter", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TAPSpecialization : Seq!(Lit!(":"),Or!(Type,ConditionalExpression))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TAPSpecialization", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TAPDefault : Seq!(Lit!("="),Or!(Type,ConditionalExpression))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TAPDefault", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateTupleParameter : Seq!(Identifier,Lit!("..."))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateTupleParameter", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplatedConstructor : Seq!(Lit!("this"),Lit!("("),TemplateParameterList,Lit!(")"),Parameters,Option!(Constraint),FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplatedConstructor", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ClassTemplateDeclaration : Seq!(Lit!("class"),Identifier,Lit!("("),TemplateParameterList,Lit!(")"),Option!(Constraint),Option!(BaseClassList),ClassBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ClassTemplateDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StructTemplateDeclaration : Seq!(Lit!("struct"),Identifier,Lit!("("),TemplateParameterList,Lit!(")"),Option!(Constraint),StructBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StructTemplateDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class UnionTemplateDeclaration : Seq!(Lit!("union"),Identifier,Lit!("("),TemplateParameterList,Lit!(")"),Option!(Constraint),StructBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("UnionTemplateDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class InterfaceTemplateDeclaration : Seq!(Lit!("interface"),Identifier,Lit!("("),TemplateParameterList,Lit!(")"),Option!(Constraint),Option!(BaseInterfaceList),InterfaceBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("InterfaceTemplateDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Constraint : Seq!(Lit!("if"),Lit!("("),Expression,Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Constraint", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateMixinDeclaration : Seq!(Lit!("mixin"),Lit!("template"),TemplateIdentifier,Lit!("("),TemplateParameterList,Lit!(")"),Option!(Constraint),Lit!("{"),DeclDefs,Lit!("}"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateMixinDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TemplateMixin : Seq!(Lit!("mixin"),TemplateIdentifier,Option!(Seq!(Lit!("!("),List!(TemplateArgument),Lit!(")"))),Option!(MixinIdentifier),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TemplateMixin", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class MixinIdentifier : Identifier
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("MixinIdentifier", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TraitsExpression : Seq!(Lit!("__traits"),Lit!("("),TraitsKeyword,Lit!(","),List!(TraitsArgument),Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TraitsExpression", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TraitsKeyword : Or!(Lit!("isAbstractClass"),Lit!("isArithmetic"),Lit!("isAssociativeArray"),Lit!("isFinalClass"),Lit!("isFloating"),Lit!("isIntegral"),Lit!("isScalar"),Lit!("isStaticArray"),Lit!("isUnsigned"),Lit!("isVitualFunction"),Lit!("isVirtualMethod"),Lit!("isAbstractFunction"),Lit!("isFinalFunction"),Lit!("isStaticFunction"),Lit!("isRef"),Lit!("isOut"),Lit!("isLazy"),Lit!("hasMember"),Lit!("identifier"),Lit!("getMember"),Lit!("getOverloads"),Lit!("getVirtualFunctions"),Lit!("getVirtualMethods"),Lit!("parent"),Lit!("classInstanceSize"),Lit!("allMembers"),Lit!("derivedMembers"),Lit!("isSame"),Lit!("compiles"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TraitsKeyword", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class TraitsArgument : Or!(AssignExpression,Type)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("TraitsArgument", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class UnitTest : Seq!(Lit!("unittest"),FunctionBody)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("UnitTest", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ConditionalDeclaration : Or!(Seq!(Condition,Lit!(":"),Declarations),Seq!(Condition,CCDeclarationBlock,Option!(Seq!(Lit!("else"),CCDeclarationBlock))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ConditionalDeclaration", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class CCDeclarationBlock : Or!(Declaration,Seq!(Lit!("{"),Option!(Declaration),Lit!("}")))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("CCDeclarationBlock", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Declarations : Seq!(Declaration,Option!(Declarations))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Declarations", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class ConditionalStatement : Seq!(Condition,NoScopeNonEmptyStatement,Option!(Seq!(Lit!("else"),NoScopeNonEmptyStatement)))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("ConditionalStatement", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Condition : Or!(VersionCondition,DebugCondition,StaticIfCondition)
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Condition", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class VersionCondition : Seq!(Lit!("version"),Lit!("("),Or!(IntegerLiteral,Lit!("unittest"),Identifier),Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("VersionCondition", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class VersionSpecification : Seq!(Lit!("version"),Lit!("="),Or!(Identifier,IntegerLiteral),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("VersionSpecification", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DebugCondition : Seq!(Lit!("debug"),Option!(Seq!(Lit!("("),Or!(IntegerLiteral,Identifier),Lit!(")"))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DebugCondition", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class DebugSpecification : Seq!(Lit!("debug"),Lit!("="),Or!(Identifier,IntegerLiteral),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("DebugSpecification", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StaticIfCondition : Seq!(Lit!("static"),Lit!("if"),Lit!("("),AssignExpression,Lit!(")"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StaticIfCondition", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class StaticAssert : Seq!(Lit!("static"),Lit!("assert"),Lit!("("),AssignExpression,Option!(Seq!(Lit!(","),AssignExpression)),Lit!(")"),Lit!(";"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("StaticAssert", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Identifier : Fuse!(Join!(NegLookAhead!(Keyword),Or!(Range!('a','z'),Range!('A','Z'),Lit!("_")),ZeroOrMore!(Or!(Range!('a','z'),Range!('A','Z'),Range!('0','9'),Lit!("_")))))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Identifier", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

class Keyword : Or!(Lit!("abstract"),Lit!("alias"),Lit!("align"),Lit!("asm"),Lit!("assert"),Lit!("auto"),Lit!("body"),Lit!("bool"),Lit!("break"),Lit!("byte"),Lit!("case"),Lit!("cast"),Lit!("catch"),Lit!("cdouble"),Lit!("cent"),Lit!("cfloat"),Lit!("char"),Lit!("class"),Lit!("const"),Lit!("continue"),Lit!("creal"),Lit!("dchar"),Lit!("debug"),Lit!("default"),Lit!("delegate"),Lit!("delete"),Lit!("deprecated"),Lit!("double"),Lit!("do"),Lit!("else"),Lit!("enum"),Lit!("export"),Lit!("extern"),Lit!("false"),Lit!("finally"),Lit!("final"),Lit!("float"),Lit!("foreach_reverse"),Lit!("foreach"),Lit!("for"),Lit!("function"),Lit!("goto"),Lit!("idouble"),Lit!("if"),Lit!("ifloat"),Lit!("immutable"),Lit!("import"),Lit!("inout"),Lit!("interface"),Lit!("invariant"),Lit!("int"),Lit!("in"),Lit!("ireal"),Lit!("is"),Lit!("lazy"),Lit!("long"),Lit!("macro"),Lit!("mixin"),Lit!("module"),Lit!("new"),Lit!("nothrow"),Lit!("null"),Lit!("out"),Lit!("override"),Lit!("package"),Lit!("pragma"),Lit!("private"),Lit!("protected"),Lit!("public"),Lit!("pure"),Lit!("real"),Lit!("ref"),Lit!("return"),Lit!("scope"),Lit!("shared"),Lit!("short"),Lit!("static"),Lit!("struct"),Lit!("super"),Lit!("switch"),Lit!("synchronized"),Lit!("template"),Lit!("this"),Lit!("throw"),Lit!("true"),Lit!("try"),Lit!("typedef"),Lit!("typeid"),Lit!("typeof"),Lit!("ubyte"),Lit!("ucent"),Lit!("uint"),Lit!("ulong"),Lit!("union"),Lit!("unittest"),Lit!("ushort"),Lit!("version"),Lit!("void"),Lit!("volatile"),Lit!("wchar"),Lit!("while"),Lit!("with"),Lit!("__FILE__"),Lit!("__LINE__"),Lit!("__gshared"),Lit!("__thread"),Lit!("__traits"))
{
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output( p.next
                     , p.namedCaptures
                     , ParseTree("Keyword", p.success, p.capture,  [p])
                     );
    }
    
    mixin(stringToInputMixin());
}

