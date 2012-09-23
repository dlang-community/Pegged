/**
This module was automatically generated from the following grammar:


D:

Module < Spacing ModuleDeclaration? DeclDefs?

DeclDefs < DeclDef+

DeclDef < AttributeSpecifier
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
         
ModuleDeclaration < "module" qualifiedIdentifier ";"

ImportDeclaration < "import" ImportList ";"
                   / "static" "import"  ImportList ";"
                   
ImportList < ImportBindings
            / Import ("," ImportList)?

Import < qualifiedIdentifier "=" qualifiedIdentifier
        / qualifiedIdentifier

###### Also a space-sep list is needed ##
#List(Elem) < pegged.peg.list(Elem, ',')

ImportBindings < Import ":" list(ImportBind,',') 

ImportBind < Identifier ("=" Identifier)?

MixinDeclaration < "mixin" "(" AssignExpression ")" ";"

# declaration.html
Declaration < AliasDeclaration
             / AliasThisDeclaration
             / Decl

# Why so limited? This disallow things like: alias __traits(allMembers, C) result;
AliasDeclaration < "alias" BasicType Declarator

AliasThisDeclaration < "alias" Identifier "this"  # no ";"?

Decl < BasicType Declarators ";"
      / BasicType Declarator FunctionBody
      / AutoDeclaration
      / StorageClasses Decl

Declarators < DeclaratorInitializer ("," list(DeclaratorIdentifier, ','))?

DeclaratorInitializer < Declarator ("=" Initializer)?

DeclaratorIdentifier < Identifier ("=" Initializer)?

BasicType < BasicTypeX
           / "." IdentifierList
           / IdentifierList
           / Typeof "." IdentifierList
           / "const(" Type ")"
           / "immutable(" Type ")"
           / "shared(" Type ")"
           / "inout(" Type ")" 

BasicTypeX < "bool"
            / "byte" / "ubyte"
            / "short" / "ushort"
            / "int" / "uint"
            / "long" / "ulong"
            / "char" / "wchar" / "dchar"
            / "float" / "double" / "real"   
            / "void"

BasicType2 < "*"
            / "[" "]"
            / "[" AssignExpression "]"
            / "[" AssignExpression ".." AssignExpression "]"
            / "[" Type "]"
            / "delegate" Parameters FunctionAttributes?
            / "function" Parameters FunctionAttributes?

## Maybe that could factored ##
Declarator < BasicType2* "(" Declarator ")" DeclaratorSuffixes?
            / BasicType2*     Identifier     DeclaratorSuffixes?

DeclaratorSuffixes < DeclaratorSuffix+
            
DeclaratorSuffix < "[" "]"
                  / "[" AssignExpression "]"
                  / "[" Type "]"
                  / TemplateParameterList? Parameters MemberFunctionAttributes? Constraint?

## Could be written otherwise? #
IdentifierList <  TemplateInstance ("." IdentifierList)?
                / Identifier ("." IdentifierList)?
               
StorageClasses < StorageClass+

StorageClass < "abstract"
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

Property < "@" ( "property"
                / "safe"
                / "trusted"
                / "system"
                / "disable")

Type < BasicType Declarator2?

Declarator2 < BasicType2* ("(" Declarator2 ")")? DeclaratorSuffixes?

Parameters < "(" ParameterList? ")"

ParameterList < "..."
               / Parameter (:',' Parameter)*

Parameter <- InOut? space+ BasicType Declarator ("..." / "=" DefaultInitializerExpression)?
           / InOut? Type "..."?

InOut < InOutX InOut?

InOutX < "auto"
        / "const"
        / "final"
        / "immutable"
        / "inout"
        / "in " # Kludge, to avoid eating int's. Bug bug bug.
        / "lazy"
        / "out"
        / "ref"
        / "scope"
        / "shared"

FunctionAttributes < FunctionAttribute+

FunctionAttribute < "nothrow"
                   / "pure"
                   / Property
                   
MemberFunctionAttributes < MemberFunctionAttribute+

MemberFunctionAttribute < "const"
                         / "immutable"
                         / "inout"
                         / "shared"
                         / FunctionAttribute

DefaultInitializerExpression < AssignExpression
                              / "__FILE__"
                              / "__LINE__"

Initializer < VoidInitializer / NonVoidInitializer

NonVoidInitializer < AssignExpression
                    / ArrayInitializer
                    / StructInitializer

ArrayInitializer < "[" "]"
                  / "[" ArrayMemberInitializations "]"

## Crap
ArrayMemberInitializations < ArrayMemberInitialization ("," ArrayMemberInitialization?)*

## Verify the order, with PEG
ArrayMemberInitialization < NonVoidInitializer
                           / AssignExpression ":" NonVoidInitializer

StructInitializer < "{" "}"
                   / "{" StructMemberInitializers "}"

StructMemberInitializers < StructMemberInitializer ("," StructMemberInitializer?)*

StructMemberInitializer < NonVoidInitializer
                         / Identifier : NonVoidInitializer
                         
AutoDeclaration < StorageClasses AutoDeclarationX ";"

AutoDeclarationX < list(Identifier "=" Initializer,',')

Typeof < "typeof" "(" Expression ")"
        / "typeof" "(" "return" ")"

VoidInitializer < "void"

## File statement.html

Statement < ";"
           / NonEmptyStatement
           / ScopeBlockStatement
           
NoScopeNonEmptyStatement < NonEmptyStatement
                          / BlockStatement

NoScopeStatement < ";"
                  / NonEmptyStatement
                  / BlockStatement

NonEmptyOrScopeBlockStatement < NonEmptyStatement
                               / ScopeBlockStatement
                               
NonEmptyStatement < NonEmptyStatementNoCaseNoDefault
                   / CaseStatement
                   / CaseRangeStatement
                   / DefaultStatement
                   
NonEmptyStatementNoCaseNoDefault < 
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

ScopeStatement < NonEmptyStatement / BlockStatement

ScopeBlockStatement < ScopeStatement

LabeledStatement < Identifier ":" NoScopeStatement

BlockStatement < "{" StatementList? "}"
                          
StatementList < Statement+

ExpressionStatement < Expression ";"

DeclarationStatement < Declaration

IfStatement < "if" "(" IfCondition ")" ThenStatement "else" ElseStatement

IfCondition < Expression
             / "auto" Identifier "=" Expression
             / BasicType Declarator "=" Expression
             
ThenStatement < ScopeStatement
 
ElseStatement < ScopeStatement
 
WhileStatement < "while" "(" Expression ")" ScopeStatement
 
DoStatement < "do" ScopeStatement "while" "(" Expression ")" ";"
 
ForStatement < "for" "(" Initialize Test? ";" Increment? ")" ScopeStatement
 
Initialize < ";" / NoScopeNonEmptyStatement
 
Test < Expression
 
Increment < Expression
 
ForeachStatement < ("foreach" / "foreach_reverse")
                    "(" list(ForeachType,',') ";" Aggregate ")"
                     NoScopeNonEmptyStatement
                     
ForeachType < "ref"? BasicType Declarator
             / "ref"? Identifier
             
Aggregate < Expression

ForeachRangeStatement < "(" ForeachType ";" Expression ".." Expression ")"

SwitchStatement < "switch" "(" Expression ")" ScopeStatement

CaseStatement < "case" ArgumentList ":" ScopeStatementList

CaseRangeStatement < "case" AssignExpression ":" 
                      ".." 
                      "case" AssignExpression ":"
                      ScopeStatementList
                      
DefaultStatement < "default" ":" ScopeStatementList

ScopeStatementList < StatementListNoCaseNoDefault

StatementListNoCaseNoDefault < StatementNoCaseNoDefault+

StatementNoCaseNoDefault < ";" 
                          / NonEmptyStatementNoCaseNoDefault
                          / ScopeBlockStatement
                          
FinalSwitchStatement < "final" "switch" "(" Expression ")"
                        ScopeStatement
                        
ContinueStatement < "continue" Identifier? ";"

BreakStatement < "break" Identifier? ";"

ReturnStatement < "return" Expression? ";"

GotoStatement < "goto" ( "default" ";"
                        / "case" ";"
                        / "case" Expression ";"
                        / Identifier ";")
               
WithStatement < "with" 
                 "(" ( Expression / Symbol / TemplateInstance) ")" 
                 ScopeStatement

SynchronizedStatement < "synchronized" 
                        ( "(" Expression ")" )?
                        ScopeStatement

TryStatement < "try" ScopeStatement Catches? FinallyStatement?

Catches < LastCatch / Catch Catches?

LastCatch < "catch" NoScopeNonEmptyStatement

Catch < "catch" "(" CatchParameter ")" NoScopeNonEmptyStatement

CatchParameter < BasicType Identifier

FinallyStatement < "finally" NoScopeNonEmptyStatement

ThrowStatement < "throw" Expression ";"  

ScopeGuardStatement < ( "scope(exit)" 
                       / "scope(success)" 
                       / "scope(failure)")
                       NonEmptyOrScopeBlockStatement

AsmStatement < "asm" "{" AsmInstructionList? "}"

AsmInstructionList < AsmInstruction ";" AsmInstructionList?

PragmaStatement < Pragma NoScopeStatement

MixinStatement < "mixin" "(" AssignExpression ")" ";"

### File expression.html ###

Expression < AssignExpression

AssignExpression < ConditionalExpression (Op AssignExpression)?

Op < ">>>=" 
    / "^^=" / ">>=" / "<<="
    / "~=" / "+=" / "-=" / "*=" / "^=" / "|=" / "&=" / "/="
    / "="

ConditionalExpression < OrOrExpression 
                        ("?" Expression ":" ConditionalExpression)?

OrOrExpression < AndAndExpression ("||" OrOrExpression)?

AndAndExpression < (CmpExpression / OrExpression) ("&&" AndAndExpression)?
                    
OrExpression < XorExpression ("|" OrExpression)?

XorExpression < AndExpression ("^" XorExpression)?

AndExpression < ShiftExpression ("&" AndExpression)?

CmpExpression <  EqualExpression
               / IdentityExpression
               / RelExpression
               / InExpression
               / ShiftExpression
               
EqualExpression < ShiftExpression ("==" / "!=") ShiftExpression

IdentityExpression < ShiftExpression ("!is" / "is") ShiftExpression

RelExpression < ShiftExpression RelOp ShiftExpression

RelOp < "!<>="
       / "!<>" / "!<=" / "!>=" / "<>="
       / "<=" / ">=" / "<>" / "!>" / "!<"
       / "<" / ">"

InExpression < ShiftExpression (("!in" / "in") ShiftExpression)?

ShiftExpression < AddExpression ((">>>" / ">>" / "<<") AddExpression)?

AddExpression < (MulExpression / CatExpression)
                 (("+" / "-") MulExpression)?
                 
CatExpression < MulExpression ("~" AddExpression)?

MulExpression < UnaryExpression
                 (("*" / "/" / "%") UnaryExpression)?
                 
UnaryExpression < UnaryOp UnaryExpression
                 / ComplementExpression
                 / "(" Type ")" "." Identifier
                 / NewExpression
                 / DeleteExpression
                 / CastExpression
                 / PowExpression
                 
UnaryOp < "++" / "--"
         / "+" / "-" / "&" / "*" / "/" / "!"
         
ComplementExpression < "~" UnaryExpression

NewExpression < ("new" AllocatorArguments? Type 
                  ("[" AssignExpression "]" / "(" ArgumentList ")" )?)
               / NewAnonClassExpression

AllocatorArguments < "(" ArgumentList ")"

ArgumentList < AssignExpression ("," AssignExpression)*

DeleteExpression < "delete" UnaryExpression

CastExpression < "cast" "(" (Type / CastEqual)? ")" UnaryExpression

CastEqual < "const" "shared"
           / "shared" "const"
           / "inout" "shared"
           / "shared" "inout"
           / "const"
           / "inout"
           / "immutable"
           / "shared"
           
PowExpression < PostfixExpression ("^^" UnaryExpression)?

# Changed
PostfixExpression < PrimaryExpression (IndexExpression / SliceExpression)*
                    ( "." NewExpression
                    / "." TemplateIdentifier
                    / "." Identifier
                    / "++"
                    / "--"
                    / "(" ArgumentList? ")"
                    )?
                                    
# Changed
IndexExpression < "[" ArgumentList "]"

# Changed
SliceExpression < "[" "]"
                  "[" AssignExpression ".." AssignExpression "]"
                                     
PrimaryExpression < "this"
                   / "super"
                   / "null"
                   / "true"
                   / "false"
                   / "$"
                   / "__FILE__"
                   / "__LINE__"
                   / TemplateInstance
                   / "." TemplateInstance
                   / Identifier
                   / "." Identifier
                   / FloatLiteral
                   / IntegerLiteral
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

StringLiterals < StringLiteral+

ArrayLiteral < "[" ArgumentList? "]"

AssocArrayLiteral < "[" list(KeyValuePair,',') "]"

KeyValuePair < AssignExpression ":" AssignExpression

Lambda < Identifier "=>" AssignExpression
        / ParameterAttributes "=>" AssignExpression

FunctionLiteral < (("function" / "delegate") Type?)? ParameterAttributes? FunctionBody

ParameterAttributes < Parameters FunctionAttributes?

AssertExpression < "assert" "(" AssignExpression ("," AssignExpression)? ")"

MixinExpression < "mixin" "(" AssignExpression ")"

ImportExpression < "import" "(" AssignExpression ")"

TypeidExpression < "typeid" "(" ( Type / Expression ) ")"

IsExpression < "is" "(" Type
                  ( ":" TypeSpecialization
                  / "==" TypeSpecialization
                  / Identifier ( ":" TypeSpecialization ("," TemplateParameterList)?
                               / "==" TypeSpecialization ("," TemplateParameterList)?
                               )?
                  
                  )?
                ")"

TypeSpecialization < Type
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
### file attribute.html

AttributeSpecifier < Attribute DeclarationBlock
                    / Attribute ":"

Attribute < LinkageAttribute
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
           
DeclarationBlock < DeclDef
                  / "{" DeclDefs "}"
                    
LinkageAttribute < "extern" "(" LinkageType ")"

LinkageType < "C++" / "C" / "D" / "Windows" / "Pascal" / "System"

AlignAttribute < "align" ("(" IntegerLiteral ")")?

ProtectionAttribute < "private"
                     / "package"
                     / "protected"
                     / "public"
                     / "export"

### class.html

ClassDeclaration < "class" Identifier BaseClassList? ClassBody 
                 / ClassTemplateDeclaration

### I don't why the grammar distinguish SuperClass and Interface
### They cannot be differentiated at this step
BaseClassList < ":" list(Identifier,',')

ClassBody < "{" ClassBodyDeclarations? "}"

ClassBodyDeclarations < ClassBodyDeclaration ClassBodyDeclarations?

ClassBodyDeclaration < DeclDef
                      / Invariant
                      / ClassAllocator
                      / ClassDeallocator
                      
Constructor < "this" Parameters FunctionBody
             / TemplatedConstructor
             
Destructor < "~" "this" "(" ")" FunctionBody

StaticConstructor < "static" "this" "(" ")" FunctionBody

StaticDestructor < "static" "~" "this" "(" ")" FunctionBody

SharedStaticConstructor < "shared" "static" "this" "(" ")" FunctionBody

SharedStaticDestructor < "shared" "static" "~" "this" "(" ")" FunctionBody

Invariant < "invariant" "(" ")" BlockStatement

ClassAllocator < "new" Parameters FunctionBody

ClassDeallocator < "delete" Parameters FunctionBody

AliasThis < "alias" Identifier "this" ";"

NewAnonClassExpression < "new" AllocatorArguments? "class" ClassArguments? Identifier list(Identifier,',')? ClassBody 

ClassArguments < "(" ArgumentList? ")"

### enum.html

EnumDeclaration < "enum" EnumTag? (":" EnumBaseType)? EnumBody

EnumTag < Identifier

EnumBaseType < Type

EnumBody < ";" / "{" list(EnumMember,',') "}"

EnumMember < Type "=" AssignExpression
            / Identifier ("=" AssignExpression)?
            
### function.html

FunctionBody < BlockStatement
              / BodyStatement
              / InStatement BodyStatement
              / OutStatement BodyStatement
              / InStatement OutStatement BodyStatement
              / OutStatement InStatement BodyStatement
              
InStatement < "in" BlockStatement

OutStatement < "out" ("(" Identifier ")" )? BlockStatement

BodyStatement < "body" BlockStatement

### iasm.html

AsmInstruction < "align" IntegerExpression
                / "even"
                / "naked"
                / ("db" / "ds" / "di" / "dl" / "df" / "dd" / "de") list(Operand,',')
                / Identifier ":" AsmInstruction
                / OpCode
                / OpCode list(Operand,',')
                
IntegerExpression < IntegerLiteral / Identifier

Operand < AsmExp

AsmExp < AsmLogOrExp ("?" AsmExp ":" AsmExp)?

AsmLogOrExp < AsmLogAndExp ("||" AsmLogAndExp)?

AsmLogAndExp < AsmOrExp ("&&" AsmOrExp)?

AsmOrExp < AsmXorExp ("|" AsmXorExp)?

AsmXorExp < AsmAndExp ("^" AsmAndExp)?

AsmAndExp < AsmEqualExp ("&" AsmEqualExp)?

AsmEqualExp < AsmRelExp (("=="/"!=") AsmRelExp)?

AsmRelExp < AsmShiftExp (("<="/">="/"<"/">") AsmShiftExp)?

AsmShiftExp < AsmAddExp ((">>>"/"<<"/">>") AsmAddExp)?

AsmAddExp < AsmMulExp (("+"/"-") AsmMulExp)?

AsmMulExp < AsmBrExp (("*"/"/"/"%") AsmBrExp)?

AsmBrExp < AsmUnaExp ("[" AsmExp "]")?

AsmUnaExp < AsmTypePrefix AsmExp
           / ("offsetof" / "seg") AsmExp
           / ("+" / "-" / "!" / "~") AsmUnaExp
           / AsmPrimaryExp
           
AsmPrimaryExp < FloatLiteral
              / IntegerLiteral
              / "__LOCAL_SIZE"
              / "$"
              / Register
              / DotIdentifier
               
DotIdentifier < Identifier ("." DotIdentifier)?

AsmTypePrefix < ( "near"
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
Register < Identifier
OpCode < Identifier                 
                 
### interface.html

InterfaceDeclaration < "interface" Identifier BaseInterfaceList? InterfaceBody
                      / InterfaceTemplateDeclaration
                      
BaseInterfaceList < ":" list(Identifier,',')

InterfaceBody < "{" DeclDefs? "}"

### pragma.html

Pragma < "pragma" "(" Identifier ("," ArgumentList)? ")"

### struct.html

AggregateDeclaration < ("struct" / "union") Identifier (StructBody / ";")
                      / StructTemplateDeclaration
                      / UnionTemplateDeclaration
                      
StructBody < "{" StructBodyDeclarations? "}"

StructBodyDeclarations < StructBodyDeclaration StructBodyDeclarations?

StructBodyDeclaration < DeclDef
                       / StructAllocator 
                       / StructDeallocator
                       / StructPostblit
                       / AliasThis
                       
StructAllocator < ClassAllocator

StructDeallocator < ClassDeallocator

StructPostblit < "this(this)" FunctionBody

### template.html

TemplateDeclaration < "template" TemplateIdentifier "(" TemplateParameterList ")" Constraint?

TemplateIdentifier < Identifier

TemplateParameterList < list(TemplateParameter,',')

TemplateParameter < TemplateTypeParameter
                   / TemplateValueParameter
                   / TemplateAliasParameter
                   / TemplateTupleParameter
                   / TemplateThisParameter
                   
TemplateInstance < TemplateIdentifier ( "!(" list(TemplateArgument,',') ")"
                                       / "!" TemplateSingleArgument)
                                       
TemplateArgument < Type
                  / AssignExpression
                  / Symbol
                  
Symbol < "."? SymbolTail

SymbolTail < TemplateInstance ("." SymbolTail)?
            / Identifier ("." SymbolTail)? 
            
TemplateSingleArgument < BasicTypeX
                        / CharacterLiteral
                        / StringLiteral
                        / FloatLiteral
                        / IntegerLiteral
                        / "true"
                        / "false"
                        / "null"
                        / "__LINE__"
                        / "__FILE__"
                        / Identifier
                        
TemplateTypeParameter < Identifier TTPSpecialization? TTPDefault?

TTPSpecialization < ":" Type

TTPDefault < "=" Type

TemplateThisParameter < "this" TemplateTypeParameter

TemplateValueParameter < BasicType Declarator TVPSpecialization? TVPDefault?

TVPSpecialization < ":" ConditionalExpression

TVPDefault < "=" ("__FILE__" / "__LINE__" / AssignExpression)

TemplateAliasParameter < "alias" (BasicType Declarator / Identifier) TAPSpecialization? TAPDefault?

TAPSpecialization < ":" (Type / ConditionalExpression)

TAPDefault < "=" (Type / ConditionalExpression)

TemplateTupleParameter < Identifier "..."

TemplatedConstructor < "this" "(" TemplateParameterList ")" Parameters Constraint? FunctionBody

ClassTemplateDeclaration < "class" Identifier "(" TemplateParameterList ")" Constraint? BaseClassList? ClassBody

StructTemplateDeclaration < "struct" Identifier "(" TemplateParameterList ")" Constraint? StructBody

UnionTemplateDeclaration < "union" Identifier "(" TemplateParameterList ")" Constraint? StructBody

InterfaceTemplateDeclaration < "interface" Identifier "(" TemplateParameterList ")" Constraint? BaseInterfaceList? InterfaceBody

Constraint < "if" "(" Expression ")"

### template-mixin.html

TemplateMixinDeclaration < "mixin" "template" TemplateIdentifier "(" TemplateParameterList ")" Constraint? "{" DeclDefs "}"

TemplateMixin < "mixin" TemplateIdentifier (("!(" list(TemplateArgument,',') ")")? MixinIdentifier?) ";"

MixinIdentifier < Identifier

### traits.html

TraitsExpression < "__traits" "(" TraitsKeyword "," list(TraitsArgument,',') ")"

TraitsKeyword < "isAbstractClass"
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

TraitsArgument < AssignExpression
                / Type
              
### unittest.html

UnitTest < "unittest" FunctionBody

### version.html

ConditionalDeclaration < Condition ":" Declarations
                        / Condition CCDeclarationBlock ("else" CCDeclarationBlock)?
                        
CCDeclarationBlock < Declaration
                    / "{" Declaration? "}"
                    
Declarations < Declaration+

ConditionalStatement < Condition NoScopeNonEmptyStatement ("else" NoScopeNonEmptyStatement)?

Condition < VersionCondition
           / DebugCondition
           / StaticIfCondition
           
VersionCondition < "version" "(" (IntegerLiteral / "unittest" / Identifier) ")"

VersionSpecification < "version" "=" (Identifier/IntegerLiteral) ";"

DebugCondition < "debug" ("(" (IntegerLiteral / Identifier) ")" )?

DebugSpecification < "debug" "=" (Identifier / IntegerLiteral) ";"

StaticIfCondition < "static" "if" "(" AssignExpression ")"

StaticAssert < "static" "assert" "(" AssignExpression 
                                     ("," AssignExpression)? 
                                   ")" ";"
                                   
# I had to add it. Otherwise, keywords are recognized as identifiers
                                   
Identifier <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*

Keyword < "abstract" / "alias" / "align" / "asm" / "assert" / "auto" / "body" / "bool" / "break" / "byte" 
         / "case" / "cast" / "catch" / "cdouble" / "cent" / "cfloat" / "char" / "class" / "const" / "continue" / "creal" / "dchar" 
         / "debug" / "default" / "delegate" / "delete" / "deprecated" / "double" / "do" / "else" / "enum" / "export" / "extern" 
         / "false" / "finally" / "final" / "float" / "foreach_reverse" / "foreach" / "for" / "function" / "goto" / "idouble" / "if" 
         / "ifloat" / "immutable" / "import" / "inout" / "interface" / "invariant" / "int" / "in" / "ireal" / "is" / "lazy" 
         / "long" / "macro" / "mixin" / "module" / "new" / "nothrow" / "null" / "out" / "override" / "package" / "pragma" 
         / "private" / "protected" / "public" / "pure" / "real" / "ref" / "return" / "scope" / "shared" / "short" / "static" 
         / "struct" / "super" / "switch" / "synchronized" / "template" / "this" / "throw" / "true" / "try" / "typedef" / "typeid" 
         / "typeof" / "ubyte" / "ucent" / "uint" / "ulong" / "union" / "unittest" / "ushort" / "version" / "void" / "volatile" 
         / "wchar" / "while" / "with" / "__FILE__" / "__LINE__" / "__gshared" / "__thread" / "__traits"

                    
## file lex.html

Comment < BlockComment
         / LineComment
         / NestingBlockComment

BlockComment < '/ *' (!'* /' .)* '* /'

LineComment < '//' (!endOfLine .)* endOfLine

NestingBlockComment < '/ +' (NestingBlockComment / Text) '+ /'
# / + (please, don't delete this line, it opens a nested block comment in generated module which is closed on the next line
Text < (!'+ /' .)*

StringLiteral < WysiwygString
               / AlternateWysiwygString
               / doublequotedString
               # No HexString
               # No DelimitedString
               / TokenString

WysiwygString <- 'r' doublequote (!doublequote .)* doublequote StringPostfix?

AlternateWysiwygString <- backquote (!backquote .)* backquote StringPostfix?

doublequotedString <- doublequote (DQChar)* doublequote StringPostfix?

DQChar <- EscapeSequence 
        / !doublequote .
        
EscapeSequence <- backslash ( quote
                            / doublequote
                            / backslash
                            / [abfnrtv]
                            / 'x' HexDigit HexDigit
                            / 'u' HexDigit HexDigit HexDigit HexDigit
                            / 'U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
                            )

StringPostfix < "c" / "w" / "d"

TokenString <- "q{" (!"}" .)* "}"

CharacterLiteral <- quote (!quote (EscapeSequence / .)) quote

### I'm fed up, I simplify

IntegerLiteral <- DecimalInteger
                / BinaryInteger
                / HexadecimalInteger
                
DecimalInteger <- Integer IntegerSuffix?

Integer <- digit (digit/"_")*

IntegerSuffix <- "Lu" / "LU" / "uL" / "UL"
               / "L" / "u" / "U"
               
BinaryInteger <- ("0b" / "0B") [01] ([01] / "_")*

HexadecimalInteger <- ("0x"/"0X") HexDigit (HexDigit / "_")*

HexDigit < [0-9a-fA-F]

FloatLiteral <- Sign? Integer "." Integer? (("e" / "E") Sign? Integer)?

Sign <- ("-" / "+")?


*/
module pegged.examples.dparser;

public import pegged.peg;
struct GenericD(TParseTree)
{
    struct D
    {
    enum name = "D";
    static bool isRule(string s)
    {
        switch(s)
        {
            case "D.Module":
            case "D.DeclDefs":
            case "D.DeclDef":
            case "D.ModuleDeclaration":
            case "D.ImportDeclaration":
            case "D.ImportList":
            case "D.Import":
            case "D.ImportBindings":
            case "D.ImportBind":
            case "D.MixinDeclaration":
            case "D.Declaration":
            case "D.AliasDeclaration":
            case "D.AliasThisDeclaration":
            case "D.Decl":
            case "D.Declarators":
            case "D.DeclaratorInitializer":
            case "D.DeclaratorIdentifier":
            case "D.BasicType":
            case "D.BasicTypeX":
            case "D.BasicType2":
            case "D.Declarator":
            case "D.DeclaratorSuffixes":
            case "D.DeclaratorSuffix":
            case "D.IdentifierList":
            case "D.StorageClasses":
            case "D.StorageClass":
            case "D.Property":
            case "D.Type":
            case "D.Declarator2":
            case "D.Parameters":
            case "D.ParameterList":
            case "D.Parameter":
            case "D.InOut":
            case "D.InOutX":
            case "D.FunctionAttributes":
            case "D.FunctionAttribute":
            case "D.MemberFunctionAttributes":
            case "D.MemberFunctionAttribute":
            case "D.DefaultInitializerExpression":
            case "D.Initializer":
            case "D.NonVoidInitializer":
            case "D.ArrayInitializer":
            case "D.ArrayMemberInitializations":
            case "D.ArrayMemberInitialization":
            case "D.StructInitializer":
            case "D.StructMemberInitializers":
            case "D.StructMemberInitializer":
            case "D.AutoDeclaration":
            case "D.AutoDeclarationX":
            case "D.Typeof":
            case "D.VoidInitializer":
            case "D.Statement":
            case "D.NoScopeNonEmptyStatement":
            case "D.NoScopeStatement":
            case "D.NonEmptyOrScopeBlockStatement":
            case "D.NonEmptyStatement":
            case "D.NonEmptyStatementNoCaseNoDefault":
            case "D.ScopeStatement":
            case "D.ScopeBlockStatement":
            case "D.LabeledStatement":
            case "D.BlockStatement":
            case "D.StatementList":
            case "D.ExpressionStatement":
            case "D.DeclarationStatement":
            case "D.IfStatement":
            case "D.IfCondition":
            case "D.ThenStatement":
            case "D.ElseStatement":
            case "D.WhileStatement":
            case "D.DoStatement":
            case "D.ForStatement":
            case "D.Initialize":
            case "D.Test":
            case "D.Increment":
            case "D.ForeachStatement":
            case "D.ForeachType":
            case "D.Aggregate":
            case "D.ForeachRangeStatement":
            case "D.SwitchStatement":
            case "D.CaseStatement":
            case "D.CaseRangeStatement":
            case "D.DefaultStatement":
            case "D.ScopeStatementList":
            case "D.StatementListNoCaseNoDefault":
            case "D.StatementNoCaseNoDefault":
            case "D.FinalSwitchStatement":
            case "D.ContinueStatement":
            case "D.BreakStatement":
            case "D.ReturnStatement":
            case "D.GotoStatement":
            case "D.WithStatement":
            case "D.SynchronizedStatement":
            case "D.TryStatement":
            case "D.Catches":
            case "D.LastCatch":
            case "D.Catch":
            case "D.CatchParameter":
            case "D.FinallyStatement":
            case "D.ThrowStatement":
            case "D.ScopeGuardStatement":
            case "D.AsmStatement":
            case "D.AsmInstructionList":
            case "D.PragmaStatement":
            case "D.MixinStatement":
            case "D.Expression":
            case "D.AssignExpression":
            case "D.Op":
            case "D.ConditionalExpression":
            case "D.OrOrExpression":
            case "D.AndAndExpression":
            case "D.OrExpression":
            case "D.XorExpression":
            case "D.AndExpression":
            case "D.CmpExpression":
            case "D.EqualExpression":
            case "D.IdentityExpression":
            case "D.RelExpression":
            case "D.RelOp":
            case "D.InExpression":
            case "D.ShiftExpression":
            case "D.AddExpression":
            case "D.CatExpression":
            case "D.MulExpression":
            case "D.UnaryExpression":
            case "D.UnaryOp":
            case "D.ComplementExpression":
            case "D.NewExpression":
            case "D.AllocatorArguments":
            case "D.ArgumentList":
            case "D.DeleteExpression":
            case "D.CastExpression":
            case "D.CastEqual":
            case "D.PowExpression":
            case "D.PostfixExpression":
            case "D.IndexExpression":
            case "D.SliceExpression":
            case "D.PrimaryExpression":
            case "D.StringLiterals":
            case "D.ArrayLiteral":
            case "D.AssocArrayLiteral":
            case "D.KeyValuePair":
            case "D.Lambda":
            case "D.FunctionLiteral":
            case "D.ParameterAttributes":
            case "D.AssertExpression":
            case "D.MixinExpression":
            case "D.ImportExpression":
            case "D.TypeidExpression":
            case "D.IsExpression":
            case "D.TypeSpecialization":
            case "D.AttributeSpecifier":
            case "D.Attribute":
            case "D.DeclarationBlock":
            case "D.LinkageAttribute":
            case "D.LinkageType":
            case "D.AlignAttribute":
            case "D.ProtectionAttribute":
            case "D.ClassDeclaration":
            case "D.BaseClassList":
            case "D.ClassBody":
            case "D.ClassBodyDeclarations":
            case "D.ClassBodyDeclaration":
            case "D.Constructor":
            case "D.Destructor":
            case "D.StaticConstructor":
            case "D.StaticDestructor":
            case "D.SharedStaticConstructor":
            case "D.SharedStaticDestructor":
            case "D.Invariant":
            case "D.ClassAllocator":
            case "D.ClassDeallocator":
            case "D.AliasThis":
            case "D.NewAnonClassExpression":
            case "D.ClassArguments":
            case "D.EnumDeclaration":
            case "D.EnumTag":
            case "D.EnumBaseType":
            case "D.EnumBody":
            case "D.EnumMember":
            case "D.FunctionBody":
            case "D.InStatement":
            case "D.OutStatement":
            case "D.BodyStatement":
            case "D.AsmInstruction":
            case "D.IntegerExpression":
            case "D.Operand":
            case "D.AsmExp":
            case "D.AsmLogOrExp":
            case "D.AsmLogAndExp":
            case "D.AsmOrExp":
            case "D.AsmXorExp":
            case "D.AsmAndExp":
            case "D.AsmEqualExp":
            case "D.AsmRelExp":
            case "D.AsmShiftExp":
            case "D.AsmAddExp":
            case "D.AsmMulExp":
            case "D.AsmBrExp":
            case "D.AsmUnaExp":
            case "D.AsmPrimaryExp":
            case "D.DotIdentifier":
            case "D.AsmTypePrefix":
            case "D.Register":
            case "D.OpCode":
            case "D.InterfaceDeclaration":
            case "D.BaseInterfaceList":
            case "D.InterfaceBody":
            case "D.Pragma":
            case "D.AggregateDeclaration":
            case "D.StructBody":
            case "D.StructBodyDeclarations":
            case "D.StructBodyDeclaration":
            case "D.StructAllocator":
            case "D.StructDeallocator":
            case "D.StructPostblit":
            case "D.TemplateDeclaration":
            case "D.TemplateIdentifier":
            case "D.TemplateParameterList":
            case "D.TemplateParameter":
            case "D.TemplateInstance":
            case "D.TemplateArgument":
            case "D.Symbol":
            case "D.SymbolTail":
            case "D.TemplateSingleArgument":
            case "D.TemplateTypeParameter":
            case "D.TTPSpecialization":
            case "D.TTPDefault":
            case "D.TemplateThisParameter":
            case "D.TemplateValueParameter":
            case "D.TVPSpecialization":
            case "D.TVPDefault":
            case "D.TemplateAliasParameter":
            case "D.TAPSpecialization":
            case "D.TAPDefault":
            case "D.TemplateTupleParameter":
            case "D.TemplatedConstructor":
            case "D.ClassTemplateDeclaration":
            case "D.StructTemplateDeclaration":
            case "D.UnionTemplateDeclaration":
            case "D.InterfaceTemplateDeclaration":
            case "D.Constraint":
            case "D.TemplateMixinDeclaration":
            case "D.TemplateMixin":
            case "D.MixinIdentifier":
            case "D.TraitsExpression":
            case "D.TraitsKeyword":
            case "D.TraitsArgument":
            case "D.UnitTest":
            case "D.ConditionalDeclaration":
            case "D.CCDeclarationBlock":
            case "D.Declarations":
            case "D.ConditionalStatement":
            case "D.Condition":
            case "D.VersionCondition":
            case "D.VersionSpecification":
            case "D.DebugCondition":
            case "D.DebugSpecification":
            case "D.StaticIfCondition":
            case "D.StaticAssert":
            case "D.Identifier":
            case "D.Keyword":
            case "D.Comment":
            case "D.BlockComment":
            case "D.LineComment":
            case "D.NestingBlockComment":
            case "D.Text":
            case "D.StringLiteral":
            case "D.WysiwygString":
            case "D.AlternateWysiwygString":
            case "D.doublequotedString":
            case "D.DQChar":
            case "D.EscapeSequence":
            case "D.StringPostfix":
            case "D.TokenString":
            case "D.CharacterLiteral":
            case "D.IntegerLiteral":
            case "D.DecimalInteger":
            case "D.Integer":
            case "D.IntegerSuffix":
            case "D.BinaryInteger":
            case "D.HexadecimalInteger":
            case "D.HexDigit":
            case "D.FloatLiteral":
            case "D.Sign":
                return true;
            default:
                return false;
        }
    }
    mixin decimateTree;
    alias spacing Spacing;

    static TParseTree Module(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Spacing, option!(ModuleDeclaration), option!(DeclDefs)), name ~ ".Module")(p);
    }

    static TParseTree Module(string s)
    {
        return named!(spaceAnd!(Spacing, Spacing, option!(ModuleDeclaration), option!(DeclDefs)), name ~ ".Module")(TParseTree("", false,[], s));
    }

    static TParseTree DeclDefs(TParseTree p)
    {
        return named!(oneOrMore!(DeclDef), name ~ ".DeclDefs")(p);
    }

    static TParseTree DeclDefs(string s)
    {
        return named!(oneOrMore!(DeclDef), name ~ ".DeclDefs")(TParseTree("", false,[], s));
    }

    static TParseTree DeclDef(TParseTree p)
    {
        return named!(or!(AttributeSpecifier, ImportDeclaration, EnumDeclaration, ClassDeclaration, InterfaceDeclaration, AggregateDeclaration, Declaration, Constructor, Destructor, UnitTest, StaticConstructor, StaticDestructor, SharedStaticConstructor, SharedStaticDestructor, ConditionalDeclaration, DebugSpecification, VersionSpecification, StaticAssert, TemplateDeclaration, TemplateMixinDeclaration, TemplateMixin, MixinDeclaration), name ~ ".DeclDef")(p);
    }

    static TParseTree DeclDef(string s)
    {
        return named!(or!(AttributeSpecifier, ImportDeclaration, EnumDeclaration, ClassDeclaration, InterfaceDeclaration, AggregateDeclaration, Declaration, Constructor, Destructor, UnitTest, StaticConstructor, StaticDestructor, SharedStaticConstructor, SharedStaticDestructor, ConditionalDeclaration, DebugSpecification, VersionSpecification, StaticAssert, TemplateDeclaration, TemplateMixinDeclaration, TemplateMixin, MixinDeclaration), name ~ ".DeclDef")(TParseTree("", false,[], s));
    }

    static TParseTree ModuleDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("module"), qualifiedIdentifier, literal!(";")), name ~ ".ModuleDeclaration")(p);
    }

    static TParseTree ModuleDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("module"), qualifiedIdentifier, literal!(";")), name ~ ".ModuleDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree ImportDeclaration(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("import"), ImportList, literal!(";")), spaceAnd!(Spacing, literal!("static"), literal!("import"), ImportList, literal!(";"))), name ~ ".ImportDeclaration")(p);
    }

    static TParseTree ImportDeclaration(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("import"), ImportList, literal!(";")), spaceAnd!(Spacing, literal!("static"), literal!("import"), ImportList, literal!(";"))), name ~ ".ImportDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree ImportList(TParseTree p)
    {
        return named!(or!(ImportBindings, spaceAnd!(Spacing, Import, option!(spaceAnd!(Spacing, literal!(","), ImportList)))), name ~ ".ImportList")(p);
    }

    static TParseTree ImportList(string s)
    {
        return named!(or!(ImportBindings, spaceAnd!(Spacing, Import, option!(spaceAnd!(Spacing, literal!(","), ImportList)))), name ~ ".ImportList")(TParseTree("", false,[], s));
    }

    static TParseTree Import(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, qualifiedIdentifier, literal!("="), qualifiedIdentifier), qualifiedIdentifier), name ~ ".Import")(p);
    }

    static TParseTree Import(string s)
    {
        return named!(or!(spaceAnd!(Spacing, qualifiedIdentifier, literal!("="), qualifiedIdentifier), qualifiedIdentifier), name ~ ".Import")(TParseTree("", false,[], s));
    }

    static TParseTree ImportBindings(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Import, literal!(":"), list!(ImportBind, literal!(","))), name ~ ".ImportBindings")(p);
    }

    static TParseTree ImportBindings(string s)
    {
        return named!(spaceAnd!(Spacing, Import, literal!(":"), list!(ImportBind, literal!(","))), name ~ ".ImportBindings")(TParseTree("", false,[], s));
    }

    static TParseTree ImportBind(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("="), Identifier))), name ~ ".ImportBind")(p);
    }

    static TParseTree ImportBind(string s)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("="), Identifier))), name ~ ".ImportBind")(TParseTree("", false,[], s));
    }

    static TParseTree MixinDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("("), AssignExpression, literal!(")"), literal!(";")), name ~ ".MixinDeclaration")(p);
    }

    static TParseTree MixinDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("("), AssignExpression, literal!(")"), literal!(";")), name ~ ".MixinDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree Declaration(TParseTree p)
    {
        return named!(or!(AliasDeclaration, AliasThisDeclaration, Decl), name ~ ".Declaration")(p);
    }

    static TParseTree Declaration(string s)
    {
        return named!(or!(AliasDeclaration, AliasThisDeclaration, Decl), name ~ ".Declaration")(TParseTree("", false,[], s));
    }

    static TParseTree AliasDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), BasicType, Declarator), name ~ ".AliasDeclaration")(p);
    }

    static TParseTree AliasDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), BasicType, Declarator), name ~ ".AliasDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree AliasThisDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), Identifier, literal!("this")), name ~ ".AliasThisDeclaration")(p);
    }

    static TParseTree AliasThisDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), Identifier, literal!("this")), name ~ ".AliasThisDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree Decl(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, BasicType, Declarators, literal!(";")), spaceAnd!(Spacing, BasicType, Declarator, FunctionBody), AutoDeclaration, spaceAnd!(Spacing, StorageClasses, Decl)), name ~ ".Decl")(p);
    }

    static TParseTree Decl(string s)
    {
        return named!(or!(spaceAnd!(Spacing, BasicType, Declarators, literal!(";")), spaceAnd!(Spacing, BasicType, Declarator, FunctionBody), AutoDeclaration, spaceAnd!(Spacing, StorageClasses, Decl)), name ~ ".Decl")(TParseTree("", false,[], s));
    }

    static TParseTree Declarators(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, DeclaratorInitializer, option!(spaceAnd!(Spacing, literal!(","), list!(DeclaratorIdentifier, literal!(","))))), name ~ ".Declarators")(p);
    }

    static TParseTree Declarators(string s)
    {
        return named!(spaceAnd!(Spacing, DeclaratorInitializer, option!(spaceAnd!(Spacing, literal!(","), list!(DeclaratorIdentifier, literal!(","))))), name ~ ".Declarators")(TParseTree("", false,[], s));
    }

    static TParseTree DeclaratorInitializer(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Declarator, option!(spaceAnd!(Spacing, literal!("="), Initializer))), name ~ ".DeclaratorInitializer")(p);
    }

    static TParseTree DeclaratorInitializer(string s)
    {
        return named!(spaceAnd!(Spacing, Declarator, option!(spaceAnd!(Spacing, literal!("="), Initializer))), name ~ ".DeclaratorInitializer")(TParseTree("", false,[], s));
    }

    static TParseTree DeclaratorIdentifier(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("="), Initializer))), name ~ ".DeclaratorIdentifier")(p);
    }

    static TParseTree DeclaratorIdentifier(string s)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("="), Initializer))), name ~ ".DeclaratorIdentifier")(TParseTree("", false,[], s));
    }

    static TParseTree BasicType(TParseTree p)
    {
        return named!(or!(BasicTypeX, spaceAnd!(Spacing, literal!("."), IdentifierList), IdentifierList, spaceAnd!(Spacing, Typeof, literal!("."), IdentifierList), spaceAnd!(Spacing, literal!("const("), Type, literal!(")")), spaceAnd!(Spacing, literal!("immutable("), Type, literal!(")")), spaceAnd!(Spacing, literal!("shared("), Type, literal!(")")), spaceAnd!(Spacing, literal!("inout("), Type, literal!(")"))), name ~ ".BasicType")(p);
    }

    static TParseTree BasicType(string s)
    {
        return named!(or!(BasicTypeX, spaceAnd!(Spacing, literal!("."), IdentifierList), IdentifierList, spaceAnd!(Spacing, Typeof, literal!("."), IdentifierList), spaceAnd!(Spacing, literal!("const("), Type, literal!(")")), spaceAnd!(Spacing, literal!("immutable("), Type, literal!(")")), spaceAnd!(Spacing, literal!("shared("), Type, literal!(")")), spaceAnd!(Spacing, literal!("inout("), Type, literal!(")"))), name ~ ".BasicType")(TParseTree("", false,[], s));
    }

    static TParseTree BasicTypeX(TParseTree p)
    {
        return named!(or!(literal!("bool"), literal!("byte"), literal!("ubyte"), literal!("short"), literal!("ushort"), literal!("int"), literal!("uint"), literal!("long"), literal!("ulong"), literal!("char"), literal!("wchar"), literal!("dchar"), literal!("float"), literal!("double"), literal!("real"), literal!("void")), name ~ ".BasicTypeX")(p);
    }

    static TParseTree BasicTypeX(string s)
    {
        return named!(or!(literal!("bool"), literal!("byte"), literal!("ubyte"), literal!("short"), literal!("ushort"), literal!("int"), literal!("uint"), literal!("long"), literal!("ulong"), literal!("char"), literal!("wchar"), literal!("dchar"), literal!("float"), literal!("double"), literal!("real"), literal!("void")), name ~ ".BasicTypeX")(TParseTree("", false,[], s));
    }

    static TParseTree BasicType2(TParseTree p)
    {
        return named!(or!(literal!("*"), spaceAnd!(Spacing, literal!("["), literal!("]")), spaceAnd!(Spacing, literal!("["), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("["), AssignExpression, literal!(".."), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("["), Type, literal!("]")), spaceAnd!(Spacing, literal!("delegate"), Parameters, option!(FunctionAttributes)), spaceAnd!(Spacing, literal!("function"), Parameters, option!(FunctionAttributes))), name ~ ".BasicType2")(p);
    }

    static TParseTree BasicType2(string s)
    {
        return named!(or!(literal!("*"), spaceAnd!(Spacing, literal!("["), literal!("]")), spaceAnd!(Spacing, literal!("["), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("["), AssignExpression, literal!(".."), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("["), Type, literal!("]")), spaceAnd!(Spacing, literal!("delegate"), Parameters, option!(FunctionAttributes)), spaceAnd!(Spacing, literal!("function"), Parameters, option!(FunctionAttributes))), name ~ ".BasicType2")(TParseTree("", false,[], s));
    }

    static TParseTree Declarator(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, zeroOrMore!(BasicType2), literal!("("), Declarator, literal!(")"), option!(DeclaratorSuffixes)), spaceAnd!(Spacing, zeroOrMore!(BasicType2), Identifier, option!(DeclaratorSuffixes))), name ~ ".Declarator")(p);
    }

    static TParseTree Declarator(string s)
    {
        return named!(or!(spaceAnd!(Spacing, zeroOrMore!(BasicType2), literal!("("), Declarator, literal!(")"), option!(DeclaratorSuffixes)), spaceAnd!(Spacing, zeroOrMore!(BasicType2), Identifier, option!(DeclaratorSuffixes))), name ~ ".Declarator")(TParseTree("", false,[], s));
    }

    static TParseTree DeclaratorSuffixes(TParseTree p)
    {
        return named!(oneOrMore!(DeclaratorSuffix), name ~ ".DeclaratorSuffixes")(p);
    }

    static TParseTree DeclaratorSuffixes(string s)
    {
        return named!(oneOrMore!(DeclaratorSuffix), name ~ ".DeclaratorSuffixes")(TParseTree("", false,[], s));
    }

    static TParseTree DeclaratorSuffix(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("["), literal!("]")), spaceAnd!(Spacing, literal!("["), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("["), Type, literal!("]")), spaceAnd!(Spacing, option!(TemplateParameterList), Parameters, option!(MemberFunctionAttributes), option!(Constraint))), name ~ ".DeclaratorSuffix")(p);
    }

    static TParseTree DeclaratorSuffix(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("["), literal!("]")), spaceAnd!(Spacing, literal!("["), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("["), Type, literal!("]")), spaceAnd!(Spacing, option!(TemplateParameterList), Parameters, option!(MemberFunctionAttributes), option!(Constraint))), name ~ ".DeclaratorSuffix")(TParseTree("", false,[], s));
    }

    static TParseTree IdentifierList(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, TemplateInstance, option!(spaceAnd!(Spacing, literal!("."), IdentifierList))), spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("."), IdentifierList)))), name ~ ".IdentifierList")(p);
    }

    static TParseTree IdentifierList(string s)
    {
        return named!(or!(spaceAnd!(Spacing, TemplateInstance, option!(spaceAnd!(Spacing, literal!("."), IdentifierList))), spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("."), IdentifierList)))), name ~ ".IdentifierList")(TParseTree("", false,[], s));
    }

    static TParseTree StorageClasses(TParseTree p)
    {
        return named!(oneOrMore!(StorageClass), name ~ ".StorageClasses")(p);
    }

    static TParseTree StorageClasses(string s)
    {
        return named!(oneOrMore!(StorageClass), name ~ ".StorageClasses")(TParseTree("", false,[], s));
    }

    static TParseTree StorageClass(TParseTree p)
    {
        return named!(or!(literal!("abstract"), literal!("auto"), literal!("const"), literal!("deprecated"), literal!("enum"), literal!("extern"), literal!("final"), literal!("immutable"), literal!("inout"), literal!("shared"), literal!("nothrow"), literal!("override"), literal!("pure"), literal!("__gshared"), Property, literal!("scope"), literal!("static"), literal!("synchronized")), name ~ ".StorageClass")(p);
    }

    static TParseTree StorageClass(string s)
    {
        return named!(or!(literal!("abstract"), literal!("auto"), literal!("const"), literal!("deprecated"), literal!("enum"), literal!("extern"), literal!("final"), literal!("immutable"), literal!("inout"), literal!("shared"), literal!("nothrow"), literal!("override"), literal!("pure"), literal!("__gshared"), Property, literal!("scope"), literal!("static"), literal!("synchronized")), name ~ ".StorageClass")(TParseTree("", false,[], s));
    }

    static TParseTree Property(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("@"), or!(literal!("property"), literal!("safe"), literal!("trusted"), literal!("system"), literal!("disable"))), name ~ ".Property")(p);
    }

    static TParseTree Property(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("@"), or!(literal!("property"), literal!("safe"), literal!("trusted"), literal!("system"), literal!("disable"))), name ~ ".Property")(TParseTree("", false,[], s));
    }

    static TParseTree Type(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, BasicType, option!(Declarator2)), name ~ ".Type")(p);
    }

    static TParseTree Type(string s)
    {
        return named!(spaceAnd!(Spacing, BasicType, option!(Declarator2)), name ~ ".Type")(TParseTree("", false,[], s));
    }

    static TParseTree Declarator2(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, zeroOrMore!(BasicType2), option!(spaceAnd!(Spacing, literal!("("), Declarator2, literal!(")"))), option!(DeclaratorSuffixes)), name ~ ".Declarator2")(p);
    }

    static TParseTree Declarator2(string s)
    {
        return named!(spaceAnd!(Spacing, zeroOrMore!(BasicType2), option!(spaceAnd!(Spacing, literal!("("), Declarator2, literal!(")"))), option!(DeclaratorSuffixes)), name ~ ".Declarator2")(TParseTree("", false,[], s));
    }

    static TParseTree Parameters(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("("), option!(ParameterList), literal!(")")), name ~ ".Parameters")(p);
    }

    static TParseTree Parameters(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("("), option!(ParameterList), literal!(")")), name ~ ".Parameters")(TParseTree("", false,[], s));
    }

    static TParseTree ParameterList(TParseTree p)
    {
        return named!(or!(literal!("..."), spaceAnd!(Spacing, Parameter, zeroOrMore!(spaceAnd!(Spacing, discard!(literal!(",")), Parameter)))), name ~ ".ParameterList")(p);
    }

    static TParseTree ParameterList(string s)
    {
        return named!(or!(literal!("..."), spaceAnd!(Spacing, Parameter, zeroOrMore!(spaceAnd!(Spacing, discard!(literal!(",")), Parameter)))), name ~ ".ParameterList")(TParseTree("", false,[], s));
    }

    static TParseTree Parameter(TParseTree p)
    {
        return named!(or!(and!(option!(InOut), oneOrMore!(space), BasicType, Declarator, option!(or!(literal!("..."), and!(literal!("="), DefaultInitializerExpression)))), and!(option!(InOut), Type, option!(literal!("...")))), name ~ ".Parameter")(p);
    }

    static TParseTree Parameter(string s)
    {
        return named!(or!(and!(option!(InOut), oneOrMore!(space), BasicType, Declarator, option!(or!(literal!("..."), and!(literal!("="), DefaultInitializerExpression)))), and!(option!(InOut), Type, option!(literal!("...")))), name ~ ".Parameter")(TParseTree("", false,[], s));
    }

    static TParseTree InOut(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, InOutX, option!(InOut)), name ~ ".InOut")(p);
    }

    static TParseTree InOut(string s)
    {
        return named!(spaceAnd!(Spacing, InOutX, option!(InOut)), name ~ ".InOut")(TParseTree("", false,[], s));
    }

    static TParseTree InOutX(TParseTree p)
    {
        return named!(or!(literal!("auto"), literal!("const"), literal!("final"), literal!("immutable"), literal!("inout"), literal!("in "), literal!("lazy"), literal!("out"), literal!("ref"), literal!("scope"), literal!("shared")), name ~ ".InOutX")(p);
    }

    static TParseTree InOutX(string s)
    {
        return named!(or!(literal!("auto"), literal!("const"), literal!("final"), literal!("immutable"), literal!("inout"), literal!("in "), literal!("lazy"), literal!("out"), literal!("ref"), literal!("scope"), literal!("shared")), name ~ ".InOutX")(TParseTree("", false,[], s));
    }

    static TParseTree FunctionAttributes(TParseTree p)
    {
        return named!(oneOrMore!(FunctionAttribute), name ~ ".FunctionAttributes")(p);
    }

    static TParseTree FunctionAttributes(string s)
    {
        return named!(oneOrMore!(FunctionAttribute), name ~ ".FunctionAttributes")(TParseTree("", false,[], s));
    }

    static TParseTree FunctionAttribute(TParseTree p)
    {
        return named!(or!(literal!("nothrow"), literal!("pure"), Property), name ~ ".FunctionAttribute")(p);
    }

    static TParseTree FunctionAttribute(string s)
    {
        return named!(or!(literal!("nothrow"), literal!("pure"), Property), name ~ ".FunctionAttribute")(TParseTree("", false,[], s));
    }

    static TParseTree MemberFunctionAttributes(TParseTree p)
    {
        return named!(oneOrMore!(MemberFunctionAttribute), name ~ ".MemberFunctionAttributes")(p);
    }

    static TParseTree MemberFunctionAttributes(string s)
    {
        return named!(oneOrMore!(MemberFunctionAttribute), name ~ ".MemberFunctionAttributes")(TParseTree("", false,[], s));
    }

    static TParseTree MemberFunctionAttribute(TParseTree p)
    {
        return named!(or!(literal!("const"), literal!("immutable"), literal!("inout"), literal!("shared"), FunctionAttribute), name ~ ".MemberFunctionAttribute")(p);
    }

    static TParseTree MemberFunctionAttribute(string s)
    {
        return named!(or!(literal!("const"), literal!("immutable"), literal!("inout"), literal!("shared"), FunctionAttribute), name ~ ".MemberFunctionAttribute")(TParseTree("", false,[], s));
    }

    static TParseTree DefaultInitializerExpression(TParseTree p)
    {
        return named!(or!(AssignExpression, literal!("__FILE__"), literal!("__LINE__")), name ~ ".DefaultInitializerExpression")(p);
    }

    static TParseTree DefaultInitializerExpression(string s)
    {
        return named!(or!(AssignExpression, literal!("__FILE__"), literal!("__LINE__")), name ~ ".DefaultInitializerExpression")(TParseTree("", false,[], s));
    }

    static TParseTree Initializer(TParseTree p)
    {
        return named!(or!(VoidInitializer, NonVoidInitializer), name ~ ".Initializer")(p);
    }

    static TParseTree Initializer(string s)
    {
        return named!(or!(VoidInitializer, NonVoidInitializer), name ~ ".Initializer")(TParseTree("", false,[], s));
    }

    static TParseTree NonVoidInitializer(TParseTree p)
    {
        return named!(or!(AssignExpression, ArrayInitializer, StructInitializer), name ~ ".NonVoidInitializer")(p);
    }

    static TParseTree NonVoidInitializer(string s)
    {
        return named!(or!(AssignExpression, ArrayInitializer, StructInitializer), name ~ ".NonVoidInitializer")(TParseTree("", false,[], s));
    }

    static TParseTree ArrayInitializer(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("["), literal!("]")), spaceAnd!(Spacing, literal!("["), ArrayMemberInitializations, literal!("]"))), name ~ ".ArrayInitializer")(p);
    }

    static TParseTree ArrayInitializer(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("["), literal!("]")), spaceAnd!(Spacing, literal!("["), ArrayMemberInitializations, literal!("]"))), name ~ ".ArrayInitializer")(TParseTree("", false,[], s));
    }

    static TParseTree ArrayMemberInitializations(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, ArrayMemberInitialization, zeroOrMore!(spaceAnd!(Spacing, literal!(","), option!(ArrayMemberInitialization)))), name ~ ".ArrayMemberInitializations")(p);
    }

    static TParseTree ArrayMemberInitializations(string s)
    {
        return named!(spaceAnd!(Spacing, ArrayMemberInitialization, zeroOrMore!(spaceAnd!(Spacing, literal!(","), option!(ArrayMemberInitialization)))), name ~ ".ArrayMemberInitializations")(TParseTree("", false,[], s));
    }

    static TParseTree ArrayMemberInitialization(TParseTree p)
    {
        return named!(or!(NonVoidInitializer, spaceAnd!(Spacing, AssignExpression, literal!(":"), NonVoidInitializer)), name ~ ".ArrayMemberInitialization")(p);
    }

    static TParseTree ArrayMemberInitialization(string s)
    {
        return named!(or!(NonVoidInitializer, spaceAnd!(Spacing, AssignExpression, literal!(":"), NonVoidInitializer)), name ~ ".ArrayMemberInitialization")(TParseTree("", false,[], s));
    }

    static TParseTree StructInitializer(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("{"), literal!("}")), spaceAnd!(Spacing, literal!("{"), StructMemberInitializers, literal!("}"))), name ~ ".StructInitializer")(p);
    }

    static TParseTree StructInitializer(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("{"), literal!("}")), spaceAnd!(Spacing, literal!("{"), StructMemberInitializers, literal!("}"))), name ~ ".StructInitializer")(TParseTree("", false,[], s));
    }

    static TParseTree StructMemberInitializers(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, StructMemberInitializer, zeroOrMore!(spaceAnd!(Spacing, literal!(","), option!(StructMemberInitializer)))), name ~ ".StructMemberInitializers")(p);
    }

    static TParseTree StructMemberInitializers(string s)
    {
        return named!(spaceAnd!(Spacing, StructMemberInitializer, zeroOrMore!(spaceAnd!(Spacing, literal!(","), option!(StructMemberInitializer)))), name ~ ".StructMemberInitializers")(TParseTree("", false,[], s));
    }

    static TParseTree StructMemberInitializer(TParseTree p)
    {
        return named!(or!(NonVoidInitializer, spaceAnd!(Spacing, Identifier, discard!(NonVoidInitializer))), name ~ ".StructMemberInitializer")(p);
    }

    static TParseTree StructMemberInitializer(string s)
    {
        return named!(or!(NonVoidInitializer, spaceAnd!(Spacing, Identifier, discard!(NonVoidInitializer))), name ~ ".StructMemberInitializer")(TParseTree("", false,[], s));
    }

    static TParseTree AutoDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, StorageClasses, AutoDeclarationX, literal!(";")), name ~ ".AutoDeclaration")(p);
    }

    static TParseTree AutoDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, StorageClasses, AutoDeclarationX, literal!(";")), name ~ ".AutoDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree AutoDeclarationX(TParseTree p)
    {
        return named!(list!(spaceAnd!(Spacing, Identifier, literal!("="), Initializer), literal!(",")), name ~ ".AutoDeclarationX")(p);
    }

    static TParseTree AutoDeclarationX(string s)
    {
        return named!(list!(spaceAnd!(Spacing, Identifier, literal!("="), Initializer), literal!(",")), name ~ ".AutoDeclarationX")(TParseTree("", false,[], s));
    }

    static TParseTree Typeof(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("typeof"), literal!("("), Expression, literal!(")")), spaceAnd!(Spacing, literal!("typeof"), literal!("("), literal!("return"), literal!(")"))), name ~ ".Typeof")(p);
    }

    static TParseTree Typeof(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("typeof"), literal!("("), Expression, literal!(")")), spaceAnd!(Spacing, literal!("typeof"), literal!("("), literal!("return"), literal!(")"))), name ~ ".Typeof")(TParseTree("", false,[], s));
    }

    static TParseTree VoidInitializer(TParseTree p)
    {
        return named!(literal!("void"), name ~ ".VoidInitializer")(p);
    }

    static TParseTree VoidInitializer(string s)
    {
        return named!(literal!("void"), name ~ ".VoidInitializer")(TParseTree("", false,[], s));
    }

    static TParseTree Statement(TParseTree p)
    {
        return named!(or!(literal!(";"), NonEmptyStatement, ScopeBlockStatement), name ~ ".Statement")(p);
    }

    static TParseTree Statement(string s)
    {
        return named!(or!(literal!(";"), NonEmptyStatement, ScopeBlockStatement), name ~ ".Statement")(TParseTree("", false,[], s));
    }

    static TParseTree NoScopeNonEmptyStatement(TParseTree p)
    {
        return named!(or!(NonEmptyStatement, BlockStatement), name ~ ".NoScopeNonEmptyStatement")(p);
    }

    static TParseTree NoScopeNonEmptyStatement(string s)
    {
        return named!(or!(NonEmptyStatement, BlockStatement), name ~ ".NoScopeNonEmptyStatement")(TParseTree("", false,[], s));
    }

    static TParseTree NoScopeStatement(TParseTree p)
    {
        return named!(or!(literal!(";"), NonEmptyStatement, BlockStatement), name ~ ".NoScopeStatement")(p);
    }

    static TParseTree NoScopeStatement(string s)
    {
        return named!(or!(literal!(";"), NonEmptyStatement, BlockStatement), name ~ ".NoScopeStatement")(TParseTree("", false,[], s));
    }

    static TParseTree NonEmptyOrScopeBlockStatement(TParseTree p)
    {
        return named!(or!(NonEmptyStatement, ScopeBlockStatement), name ~ ".NonEmptyOrScopeBlockStatement")(p);
    }

    static TParseTree NonEmptyOrScopeBlockStatement(string s)
    {
        return named!(or!(NonEmptyStatement, ScopeBlockStatement), name ~ ".NonEmptyOrScopeBlockStatement")(TParseTree("", false,[], s));
    }

    static TParseTree NonEmptyStatement(TParseTree p)
    {
        return named!(or!(NonEmptyStatementNoCaseNoDefault, CaseStatement, CaseRangeStatement, DefaultStatement), name ~ ".NonEmptyStatement")(p);
    }

    static TParseTree NonEmptyStatement(string s)
    {
        return named!(or!(NonEmptyStatementNoCaseNoDefault, CaseStatement, CaseRangeStatement, DefaultStatement), name ~ ".NonEmptyStatement")(TParseTree("", false,[], s));
    }

    static TParseTree NonEmptyStatementNoCaseNoDefault(TParseTree p)
    {
        return named!(or!(LabeledStatement, ExpressionStatement, DeclarationStatement, IfStatement, WhileStatement, DoStatement, ForStatement, ForeachStatement, SwitchStatement, FinalSwitchStatement, ContinueStatement, BreakStatement, ReturnStatement, GotoStatement, WithStatement, SynchronizedStatement, TryStatement, ScopeGuardStatement, ThrowStatement, AsmStatement, PragmaStatement, MixinStatement, ForeachRangeStatement, ConditionalStatement, StaticAssert, TemplateMixin, ImportDeclaration), name ~ ".NonEmptyStatementNoCaseNoDefault")(p);
    }

    static TParseTree NonEmptyStatementNoCaseNoDefault(string s)
    {
        return named!(or!(LabeledStatement, ExpressionStatement, DeclarationStatement, IfStatement, WhileStatement, DoStatement, ForStatement, ForeachStatement, SwitchStatement, FinalSwitchStatement, ContinueStatement, BreakStatement, ReturnStatement, GotoStatement, WithStatement, SynchronizedStatement, TryStatement, ScopeGuardStatement, ThrowStatement, AsmStatement, PragmaStatement, MixinStatement, ForeachRangeStatement, ConditionalStatement, StaticAssert, TemplateMixin, ImportDeclaration), name ~ ".NonEmptyStatementNoCaseNoDefault")(TParseTree("", false,[], s));
    }

    static TParseTree ScopeStatement(TParseTree p)
    {
        return named!(or!(NonEmptyStatement, BlockStatement), name ~ ".ScopeStatement")(p);
    }

    static TParseTree ScopeStatement(string s)
    {
        return named!(or!(NonEmptyStatement, BlockStatement), name ~ ".ScopeStatement")(TParseTree("", false,[], s));
    }

    static TParseTree ScopeBlockStatement(TParseTree p)
    {
        return named!(ScopeStatement, name ~ ".ScopeBlockStatement")(p);
    }

    static TParseTree ScopeBlockStatement(string s)
    {
        return named!(ScopeStatement, name ~ ".ScopeBlockStatement")(TParseTree("", false,[], s));
    }

    static TParseTree LabeledStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, literal!(":"), NoScopeStatement), name ~ ".LabeledStatement")(p);
    }

    static TParseTree LabeledStatement(string s)
    {
        return named!(spaceAnd!(Spacing, Identifier, literal!(":"), NoScopeStatement), name ~ ".LabeledStatement")(TParseTree("", false,[], s));
    }

    static TParseTree BlockStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(StatementList), literal!("}")), name ~ ".BlockStatement")(p);
    }

    static TParseTree BlockStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(StatementList), literal!("}")), name ~ ".BlockStatement")(TParseTree("", false,[], s));
    }

    static TParseTree StatementList(TParseTree p)
    {
        return named!(oneOrMore!(Statement), name ~ ".StatementList")(p);
    }

    static TParseTree StatementList(string s)
    {
        return named!(oneOrMore!(Statement), name ~ ".StatementList")(TParseTree("", false,[], s));
    }

    static TParseTree ExpressionStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Expression, literal!(";")), name ~ ".ExpressionStatement")(p);
    }

    static TParseTree ExpressionStatement(string s)
    {
        return named!(spaceAnd!(Spacing, Expression, literal!(";")), name ~ ".ExpressionStatement")(TParseTree("", false,[], s));
    }

    static TParseTree DeclarationStatement(TParseTree p)
    {
        return named!(Declaration, name ~ ".DeclarationStatement")(p);
    }

    static TParseTree DeclarationStatement(string s)
    {
        return named!(Declaration, name ~ ".DeclarationStatement")(TParseTree("", false,[], s));
    }

    static TParseTree IfStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("if"), literal!("("), IfCondition, literal!(")"), ThenStatement, literal!("else"), ElseStatement), name ~ ".IfStatement")(p);
    }

    static TParseTree IfStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("if"), literal!("("), IfCondition, literal!(")"), ThenStatement, literal!("else"), ElseStatement), name ~ ".IfStatement")(TParseTree("", false,[], s));
    }

    static TParseTree IfCondition(TParseTree p)
    {
        return named!(or!(Expression, spaceAnd!(Spacing, literal!("auto"), Identifier, literal!("="), Expression), spaceAnd!(Spacing, BasicType, Declarator, literal!("="), Expression)), name ~ ".IfCondition")(p);
    }

    static TParseTree IfCondition(string s)
    {
        return named!(or!(Expression, spaceAnd!(Spacing, literal!("auto"), Identifier, literal!("="), Expression), spaceAnd!(Spacing, BasicType, Declarator, literal!("="), Expression)), name ~ ".IfCondition")(TParseTree("", false,[], s));
    }

    static TParseTree ThenStatement(TParseTree p)
    {
        return named!(ScopeStatement, name ~ ".ThenStatement")(p);
    }

    static TParseTree ThenStatement(string s)
    {
        return named!(ScopeStatement, name ~ ".ThenStatement")(TParseTree("", false,[], s));
    }

    static TParseTree ElseStatement(TParseTree p)
    {
        return named!(ScopeStatement, name ~ ".ElseStatement")(p);
    }

    static TParseTree ElseStatement(string s)
    {
        return named!(ScopeStatement, name ~ ".ElseStatement")(TParseTree("", false,[], s));
    }

    static TParseTree WhileStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("while"), literal!("("), Expression, literal!(")"), ScopeStatement), name ~ ".WhileStatement")(p);
    }

    static TParseTree WhileStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("while"), literal!("("), Expression, literal!(")"), ScopeStatement), name ~ ".WhileStatement")(TParseTree("", false,[], s));
    }

    static TParseTree DoStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("do"), ScopeStatement, literal!("while"), literal!("("), Expression, literal!(")"), literal!(";")), name ~ ".DoStatement")(p);
    }

    static TParseTree DoStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("do"), ScopeStatement, literal!("while"), literal!("("), Expression, literal!(")"), literal!(";")), name ~ ".DoStatement")(TParseTree("", false,[], s));
    }

    static TParseTree ForStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("for"), literal!("("), Initialize, option!(Test), literal!(";"), option!(Increment), literal!(")"), ScopeStatement), name ~ ".ForStatement")(p);
    }

    static TParseTree ForStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("for"), literal!("("), Initialize, option!(Test), literal!(";"), option!(Increment), literal!(")"), ScopeStatement), name ~ ".ForStatement")(TParseTree("", false,[], s));
    }

    static TParseTree Initialize(TParseTree p)
    {
        return named!(or!(literal!(";"), NoScopeNonEmptyStatement), name ~ ".Initialize")(p);
    }

    static TParseTree Initialize(string s)
    {
        return named!(or!(literal!(";"), NoScopeNonEmptyStatement), name ~ ".Initialize")(TParseTree("", false,[], s));
    }

    static TParseTree Test(TParseTree p)
    {
        return named!(Expression, name ~ ".Test")(p);
    }

    static TParseTree Test(string s)
    {
        return named!(Expression, name ~ ".Test")(TParseTree("", false,[], s));
    }

    static TParseTree Increment(TParseTree p)
    {
        return named!(Expression, name ~ ".Increment")(p);
    }

    static TParseTree Increment(string s)
    {
        return named!(Expression, name ~ ".Increment")(TParseTree("", false,[], s));
    }

    static TParseTree ForeachStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(literal!("foreach"), literal!("foreach_reverse")), literal!("("), list!(ForeachType, literal!(",")), literal!(";"), Aggregate, literal!(")"), NoScopeNonEmptyStatement), name ~ ".ForeachStatement")(p);
    }

    static TParseTree ForeachStatement(string s)
    {
        return named!(spaceAnd!(Spacing, or!(literal!("foreach"), literal!("foreach_reverse")), literal!("("), list!(ForeachType, literal!(",")), literal!(";"), Aggregate, literal!(")"), NoScopeNonEmptyStatement), name ~ ".ForeachStatement")(TParseTree("", false,[], s));
    }

    static TParseTree ForeachType(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, option!(literal!("ref")), BasicType, Declarator), spaceAnd!(Spacing, option!(literal!("ref")), Identifier)), name ~ ".ForeachType")(p);
    }

    static TParseTree ForeachType(string s)
    {
        return named!(or!(spaceAnd!(Spacing, option!(literal!("ref")), BasicType, Declarator), spaceAnd!(Spacing, option!(literal!("ref")), Identifier)), name ~ ".ForeachType")(TParseTree("", false,[], s));
    }

    static TParseTree Aggregate(TParseTree p)
    {
        return named!(Expression, name ~ ".Aggregate")(p);
    }

    static TParseTree Aggregate(string s)
    {
        return named!(Expression, name ~ ".Aggregate")(TParseTree("", false,[], s));
    }

    static TParseTree ForeachRangeStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("("), ForeachType, literal!(";"), Expression, literal!(".."), Expression, literal!(")")), name ~ ".ForeachRangeStatement")(p);
    }

    static TParseTree ForeachRangeStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("("), ForeachType, literal!(";"), Expression, literal!(".."), Expression, literal!(")")), name ~ ".ForeachRangeStatement")(TParseTree("", false,[], s));
    }

    static TParseTree SwitchStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("switch"), literal!("("), Expression, literal!(")"), ScopeStatement), name ~ ".SwitchStatement")(p);
    }

    static TParseTree SwitchStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("switch"), literal!("("), Expression, literal!(")"), ScopeStatement), name ~ ".SwitchStatement")(TParseTree("", false,[], s));
    }

    static TParseTree CaseStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("case"), ArgumentList, literal!(":"), ScopeStatementList), name ~ ".CaseStatement")(p);
    }

    static TParseTree CaseStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("case"), ArgumentList, literal!(":"), ScopeStatementList), name ~ ".CaseStatement")(TParseTree("", false,[], s));
    }

    static TParseTree CaseRangeStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("case"), AssignExpression, literal!(":"), literal!(".."), literal!("case"), AssignExpression, literal!(":"), ScopeStatementList), name ~ ".CaseRangeStatement")(p);
    }

    static TParseTree CaseRangeStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("case"), AssignExpression, literal!(":"), literal!(".."), literal!("case"), AssignExpression, literal!(":"), ScopeStatementList), name ~ ".CaseRangeStatement")(TParseTree("", false,[], s));
    }

    static TParseTree DefaultStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("default"), literal!(":"), ScopeStatementList), name ~ ".DefaultStatement")(p);
    }

    static TParseTree DefaultStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("default"), literal!(":"), ScopeStatementList), name ~ ".DefaultStatement")(TParseTree("", false,[], s));
    }

    static TParseTree ScopeStatementList(TParseTree p)
    {
        return named!(StatementListNoCaseNoDefault, name ~ ".ScopeStatementList")(p);
    }

    static TParseTree ScopeStatementList(string s)
    {
        return named!(StatementListNoCaseNoDefault, name ~ ".ScopeStatementList")(TParseTree("", false,[], s));
    }

    static TParseTree StatementListNoCaseNoDefault(TParseTree p)
    {
        return named!(oneOrMore!(StatementNoCaseNoDefault), name ~ ".StatementListNoCaseNoDefault")(p);
    }

    static TParseTree StatementListNoCaseNoDefault(string s)
    {
        return named!(oneOrMore!(StatementNoCaseNoDefault), name ~ ".StatementListNoCaseNoDefault")(TParseTree("", false,[], s));
    }

    static TParseTree StatementNoCaseNoDefault(TParseTree p)
    {
        return named!(or!(literal!(";"), NonEmptyStatementNoCaseNoDefault, ScopeBlockStatement), name ~ ".StatementNoCaseNoDefault")(p);
    }

    static TParseTree StatementNoCaseNoDefault(string s)
    {
        return named!(or!(literal!(";"), NonEmptyStatementNoCaseNoDefault, ScopeBlockStatement), name ~ ".StatementNoCaseNoDefault")(TParseTree("", false,[], s));
    }

    static TParseTree FinalSwitchStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("final"), literal!("switch"), literal!("("), Expression, literal!(")"), ScopeStatement), name ~ ".FinalSwitchStatement")(p);
    }

    static TParseTree FinalSwitchStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("final"), literal!("switch"), literal!("("), Expression, literal!(")"), ScopeStatement), name ~ ".FinalSwitchStatement")(TParseTree("", false,[], s));
    }

    static TParseTree ContinueStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("continue"), option!(Identifier), literal!(";")), name ~ ".ContinueStatement")(p);
    }

    static TParseTree ContinueStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("continue"), option!(Identifier), literal!(";")), name ~ ".ContinueStatement")(TParseTree("", false,[], s));
    }

    static TParseTree BreakStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("break"), option!(Identifier), literal!(";")), name ~ ".BreakStatement")(p);
    }

    static TParseTree BreakStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("break"), option!(Identifier), literal!(";")), name ~ ".BreakStatement")(TParseTree("", false,[], s));
    }

    static TParseTree ReturnStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("return"), option!(Expression), literal!(";")), name ~ ".ReturnStatement")(p);
    }

    static TParseTree ReturnStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("return"), option!(Expression), literal!(";")), name ~ ".ReturnStatement")(TParseTree("", false,[], s));
    }

    static TParseTree GotoStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("goto"), or!(spaceAnd!(Spacing, literal!("default"), literal!(";")), spaceAnd!(Spacing, literal!("case"), literal!(";")), spaceAnd!(Spacing, literal!("case"), Expression, literal!(";")), spaceAnd!(Spacing, Identifier, literal!(";")))), name ~ ".GotoStatement")(p);
    }

    static TParseTree GotoStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("goto"), or!(spaceAnd!(Spacing, literal!("default"), literal!(";")), spaceAnd!(Spacing, literal!("case"), literal!(";")), spaceAnd!(Spacing, literal!("case"), Expression, literal!(";")), spaceAnd!(Spacing, Identifier, literal!(";")))), name ~ ".GotoStatement")(TParseTree("", false,[], s));
    }

    static TParseTree WithStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("with"), literal!("("), or!(Expression, Symbol, TemplateInstance), literal!(")"), ScopeStatement), name ~ ".WithStatement")(p);
    }

    static TParseTree WithStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("with"), literal!("("), or!(Expression, Symbol, TemplateInstance), literal!(")"), ScopeStatement), name ~ ".WithStatement")(TParseTree("", false,[], s));
    }

    static TParseTree SynchronizedStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("synchronized"), option!(spaceAnd!(Spacing, literal!("("), Expression, literal!(")"))), ScopeStatement), name ~ ".SynchronizedStatement")(p);
    }

    static TParseTree SynchronizedStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("synchronized"), option!(spaceAnd!(Spacing, literal!("("), Expression, literal!(")"))), ScopeStatement), name ~ ".SynchronizedStatement")(TParseTree("", false,[], s));
    }

    static TParseTree TryStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("try"), ScopeStatement, option!(Catches), option!(FinallyStatement)), name ~ ".TryStatement")(p);
    }

    static TParseTree TryStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("try"), ScopeStatement, option!(Catches), option!(FinallyStatement)), name ~ ".TryStatement")(TParseTree("", false,[], s));
    }

    static TParseTree Catches(TParseTree p)
    {
        return named!(or!(LastCatch, spaceAnd!(Spacing, Catch, option!(Catches))), name ~ ".Catches")(p);
    }

    static TParseTree Catches(string s)
    {
        return named!(or!(LastCatch, spaceAnd!(Spacing, Catch, option!(Catches))), name ~ ".Catches")(TParseTree("", false,[], s));
    }

    static TParseTree LastCatch(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("catch"), NoScopeNonEmptyStatement), name ~ ".LastCatch")(p);
    }

    static TParseTree LastCatch(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("catch"), NoScopeNonEmptyStatement), name ~ ".LastCatch")(TParseTree("", false,[], s));
    }

    static TParseTree Catch(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("catch"), literal!("("), CatchParameter, literal!(")"), NoScopeNonEmptyStatement), name ~ ".Catch")(p);
    }

    static TParseTree Catch(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("catch"), literal!("("), CatchParameter, literal!(")"), NoScopeNonEmptyStatement), name ~ ".Catch")(TParseTree("", false,[], s));
    }

    static TParseTree CatchParameter(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, BasicType, Identifier), name ~ ".CatchParameter")(p);
    }

    static TParseTree CatchParameter(string s)
    {
        return named!(spaceAnd!(Spacing, BasicType, Identifier), name ~ ".CatchParameter")(TParseTree("", false,[], s));
    }

    static TParseTree FinallyStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("finally"), NoScopeNonEmptyStatement), name ~ ".FinallyStatement")(p);
    }

    static TParseTree FinallyStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("finally"), NoScopeNonEmptyStatement), name ~ ".FinallyStatement")(TParseTree("", false,[], s));
    }

    static TParseTree ThrowStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("throw"), Expression, literal!(";")), name ~ ".ThrowStatement")(p);
    }

    static TParseTree ThrowStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("throw"), Expression, literal!(";")), name ~ ".ThrowStatement")(TParseTree("", false,[], s));
    }

    static TParseTree ScopeGuardStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(literal!("scope(exit)"), literal!("scope(success)"), literal!("scope(failure)")), NonEmptyOrScopeBlockStatement), name ~ ".ScopeGuardStatement")(p);
    }

    static TParseTree ScopeGuardStatement(string s)
    {
        return named!(spaceAnd!(Spacing, or!(literal!("scope(exit)"), literal!("scope(success)"), literal!("scope(failure)")), NonEmptyOrScopeBlockStatement), name ~ ".ScopeGuardStatement")(TParseTree("", false,[], s));
    }

    static TParseTree AsmStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("asm"), literal!("{"), option!(AsmInstructionList), literal!("}")), name ~ ".AsmStatement")(p);
    }

    static TParseTree AsmStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("asm"), literal!("{"), option!(AsmInstructionList), literal!("}")), name ~ ".AsmStatement")(TParseTree("", false,[], s));
    }

    static TParseTree AsmInstructionList(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmInstruction, literal!(";"), option!(AsmInstructionList)), name ~ ".AsmInstructionList")(p);
    }

    static TParseTree AsmInstructionList(string s)
    {
        return named!(spaceAnd!(Spacing, AsmInstruction, literal!(";"), option!(AsmInstructionList)), name ~ ".AsmInstructionList")(TParseTree("", false,[], s));
    }

    static TParseTree PragmaStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Pragma, NoScopeStatement), name ~ ".PragmaStatement")(p);
    }

    static TParseTree PragmaStatement(string s)
    {
        return named!(spaceAnd!(Spacing, Pragma, NoScopeStatement), name ~ ".PragmaStatement")(TParseTree("", false,[], s));
    }

    static TParseTree MixinStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("("), AssignExpression, literal!(")"), literal!(";")), name ~ ".MixinStatement")(p);
    }

    static TParseTree MixinStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("("), AssignExpression, literal!(")"), literal!(";")), name ~ ".MixinStatement")(TParseTree("", false,[], s));
    }

    static TParseTree Expression(TParseTree p)
    {
        return named!(AssignExpression, name ~ ".Expression")(p);
    }

    static TParseTree Expression(string s)
    {
        return named!(AssignExpression, name ~ ".Expression")(TParseTree("", false,[], s));
    }

    static TParseTree AssignExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, ConditionalExpression, option!(spaceAnd!(Spacing, Op, AssignExpression))), name ~ ".AssignExpression")(p);
    }

    static TParseTree AssignExpression(string s)
    {
        return named!(spaceAnd!(Spacing, ConditionalExpression, option!(spaceAnd!(Spacing, Op, AssignExpression))), name ~ ".AssignExpression")(TParseTree("", false,[], s));
    }

    static TParseTree Op(TParseTree p)
    {
        return named!(or!(literal!(">>>="), literal!("^^="), literal!(">>="), literal!("<<="), literal!("~="), literal!("+="), literal!("-="), literal!("*="), literal!("^="), literal!("|="), literal!("&="), literal!("/="), literal!("=")), name ~ ".Op")(p);
    }

    static TParseTree Op(string s)
    {
        return named!(or!(literal!(">>>="), literal!("^^="), literal!(">>="), literal!("<<="), literal!("~="), literal!("+="), literal!("-="), literal!("*="), literal!("^="), literal!("|="), literal!("&="), literal!("/="), literal!("=")), name ~ ".Op")(TParseTree("", false,[], s));
    }

    static TParseTree ConditionalExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, OrOrExpression, option!(spaceAnd!(Spacing, literal!("?"), Expression, literal!(":"), ConditionalExpression))), name ~ ".ConditionalExpression")(p);
    }

    static TParseTree ConditionalExpression(string s)
    {
        return named!(spaceAnd!(Spacing, OrOrExpression, option!(spaceAnd!(Spacing, literal!("?"), Expression, literal!(":"), ConditionalExpression))), name ~ ".ConditionalExpression")(TParseTree("", false,[], s));
    }

    static TParseTree OrOrExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AndAndExpression, option!(spaceAnd!(Spacing, literal!("||"), OrOrExpression))), name ~ ".OrOrExpression")(p);
    }

    static TParseTree OrOrExpression(string s)
    {
        return named!(spaceAnd!(Spacing, AndAndExpression, option!(spaceAnd!(Spacing, literal!("||"), OrOrExpression))), name ~ ".OrOrExpression")(TParseTree("", false,[], s));
    }

    static TParseTree AndAndExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(CmpExpression, OrExpression), option!(spaceAnd!(Spacing, literal!("&&"), AndAndExpression))), name ~ ".AndAndExpression")(p);
    }

    static TParseTree AndAndExpression(string s)
    {
        return named!(spaceAnd!(Spacing, or!(CmpExpression, OrExpression), option!(spaceAnd!(Spacing, literal!("&&"), AndAndExpression))), name ~ ".AndAndExpression")(TParseTree("", false,[], s));
    }

    static TParseTree OrExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, XorExpression, option!(spaceAnd!(Spacing, literal!("|"), OrExpression))), name ~ ".OrExpression")(p);
    }

    static TParseTree OrExpression(string s)
    {
        return named!(spaceAnd!(Spacing, XorExpression, option!(spaceAnd!(Spacing, literal!("|"), OrExpression))), name ~ ".OrExpression")(TParseTree("", false,[], s));
    }

    static TParseTree XorExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AndExpression, option!(spaceAnd!(Spacing, literal!("^"), XorExpression))), name ~ ".XorExpression")(p);
    }

    static TParseTree XorExpression(string s)
    {
        return named!(spaceAnd!(Spacing, AndExpression, option!(spaceAnd!(Spacing, literal!("^"), XorExpression))), name ~ ".XorExpression")(TParseTree("", false,[], s));
    }

    static TParseTree AndExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, option!(spaceAnd!(Spacing, literal!("&"), AndExpression))), name ~ ".AndExpression")(p);
    }

    static TParseTree AndExpression(string s)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, option!(spaceAnd!(Spacing, literal!("&"), AndExpression))), name ~ ".AndExpression")(TParseTree("", false,[], s));
    }

    static TParseTree CmpExpression(TParseTree p)
    {
        return named!(or!(EqualExpression, IdentityExpression, RelExpression, InExpression, ShiftExpression), name ~ ".CmpExpression")(p);
    }

    static TParseTree CmpExpression(string s)
    {
        return named!(or!(EqualExpression, IdentityExpression, RelExpression, InExpression, ShiftExpression), name ~ ".CmpExpression")(TParseTree("", false,[], s));
    }

    static TParseTree EqualExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, or!(literal!("=="), literal!("!=")), ShiftExpression), name ~ ".EqualExpression")(p);
    }

    static TParseTree EqualExpression(string s)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, or!(literal!("=="), literal!("!=")), ShiftExpression), name ~ ".EqualExpression")(TParseTree("", false,[], s));
    }

    static TParseTree IdentityExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, or!(literal!("!is"), literal!("is")), ShiftExpression), name ~ ".IdentityExpression")(p);
    }

    static TParseTree IdentityExpression(string s)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, or!(literal!("!is"), literal!("is")), ShiftExpression), name ~ ".IdentityExpression")(TParseTree("", false,[], s));
    }

    static TParseTree RelExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, RelOp, ShiftExpression), name ~ ".RelExpression")(p);
    }

    static TParseTree RelExpression(string s)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, RelOp, ShiftExpression), name ~ ".RelExpression")(TParseTree("", false,[], s));
    }

    static TParseTree RelOp(TParseTree p)
    {
        return named!(or!(literal!("!<>="), literal!("!<>"), literal!("!<="), literal!("!>="), literal!("<>="), literal!("<="), literal!(">="), literal!("<>"), literal!("!>"), literal!("!<"), literal!("<"), literal!(">")), name ~ ".RelOp")(p);
    }

    static TParseTree RelOp(string s)
    {
        return named!(or!(literal!("!<>="), literal!("!<>"), literal!("!<="), literal!("!>="), literal!("<>="), literal!("<="), literal!(">="), literal!("<>"), literal!("!>"), literal!("!<"), literal!("<"), literal!(">")), name ~ ".RelOp")(TParseTree("", false,[], s));
    }

    static TParseTree InExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, option!(spaceAnd!(Spacing, or!(literal!("!in"), literal!("in")), ShiftExpression))), name ~ ".InExpression")(p);
    }

    static TParseTree InExpression(string s)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, option!(spaceAnd!(Spacing, or!(literal!("!in"), literal!("in")), ShiftExpression))), name ~ ".InExpression")(TParseTree("", false,[], s));
    }

    static TParseTree ShiftExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AddExpression, option!(spaceAnd!(Spacing, or!(literal!(">>>"), literal!(">>"), literal!("<<")), AddExpression))), name ~ ".ShiftExpression")(p);
    }

    static TParseTree ShiftExpression(string s)
    {
        return named!(spaceAnd!(Spacing, AddExpression, option!(spaceAnd!(Spacing, or!(literal!(">>>"), literal!(">>"), literal!("<<")), AddExpression))), name ~ ".ShiftExpression")(TParseTree("", false,[], s));
    }

    static TParseTree AddExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(MulExpression, CatExpression), option!(spaceAnd!(Spacing, or!(literal!("+"), literal!("-")), MulExpression))), name ~ ".AddExpression")(p);
    }

    static TParseTree AddExpression(string s)
    {
        return named!(spaceAnd!(Spacing, or!(MulExpression, CatExpression), option!(spaceAnd!(Spacing, or!(literal!("+"), literal!("-")), MulExpression))), name ~ ".AddExpression")(TParseTree("", false,[], s));
    }

    static TParseTree CatExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, MulExpression, option!(spaceAnd!(Spacing, literal!("~"), AddExpression))), name ~ ".CatExpression")(p);
    }

    static TParseTree CatExpression(string s)
    {
        return named!(spaceAnd!(Spacing, MulExpression, option!(spaceAnd!(Spacing, literal!("~"), AddExpression))), name ~ ".CatExpression")(TParseTree("", false,[], s));
    }

    static TParseTree MulExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, UnaryExpression, option!(spaceAnd!(Spacing, or!(literal!("*"), literal!("/"), literal!("%")), UnaryExpression))), name ~ ".MulExpression")(p);
    }

    static TParseTree MulExpression(string s)
    {
        return named!(spaceAnd!(Spacing, UnaryExpression, option!(spaceAnd!(Spacing, or!(literal!("*"), literal!("/"), literal!("%")), UnaryExpression))), name ~ ".MulExpression")(TParseTree("", false,[], s));
    }

    static TParseTree UnaryExpression(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, UnaryOp, UnaryExpression), ComplementExpression, spaceAnd!(Spacing, literal!("("), Type, literal!(")"), literal!("."), Identifier), NewExpression, DeleteExpression, CastExpression, PowExpression), name ~ ".UnaryExpression")(p);
    }

    static TParseTree UnaryExpression(string s)
    {
        return named!(or!(spaceAnd!(Spacing, UnaryOp, UnaryExpression), ComplementExpression, spaceAnd!(Spacing, literal!("("), Type, literal!(")"), literal!("."), Identifier), NewExpression, DeleteExpression, CastExpression, PowExpression), name ~ ".UnaryExpression")(TParseTree("", false,[], s));
    }

    static TParseTree UnaryOp(TParseTree p)
    {
        return named!(or!(literal!("++"), literal!("--"), literal!("+"), literal!("-"), literal!("&"), literal!("*"), literal!("/"), literal!("!")), name ~ ".UnaryOp")(p);
    }

    static TParseTree UnaryOp(string s)
    {
        return named!(or!(literal!("++"), literal!("--"), literal!("+"), literal!("-"), literal!("&"), literal!("*"), literal!("/"), literal!("!")), name ~ ".UnaryOp")(TParseTree("", false,[], s));
    }

    static TParseTree ComplementExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("~"), UnaryExpression), name ~ ".ComplementExpression")(p);
    }

    static TParseTree ComplementExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("~"), UnaryExpression), name ~ ".ComplementExpression")(TParseTree("", false,[], s));
    }

    static TParseTree NewExpression(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("new"), option!(AllocatorArguments), Type, option!(or!(spaceAnd!(Spacing, literal!("["), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("("), ArgumentList, literal!(")"))))), NewAnonClassExpression), name ~ ".NewExpression")(p);
    }

    static TParseTree NewExpression(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("new"), option!(AllocatorArguments), Type, option!(or!(spaceAnd!(Spacing, literal!("["), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("("), ArgumentList, literal!(")"))))), NewAnonClassExpression), name ~ ".NewExpression")(TParseTree("", false,[], s));
    }

    static TParseTree AllocatorArguments(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("("), ArgumentList, literal!(")")), name ~ ".AllocatorArguments")(p);
    }

    static TParseTree AllocatorArguments(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("("), ArgumentList, literal!(")")), name ~ ".AllocatorArguments")(TParseTree("", false,[], s));
    }

    static TParseTree ArgumentList(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AssignExpression, zeroOrMore!(spaceAnd!(Spacing, literal!(","), AssignExpression))), name ~ ".ArgumentList")(p);
    }

    static TParseTree ArgumentList(string s)
    {
        return named!(spaceAnd!(Spacing, AssignExpression, zeroOrMore!(spaceAnd!(Spacing, literal!(","), AssignExpression))), name ~ ".ArgumentList")(TParseTree("", false,[], s));
    }

    static TParseTree DeleteExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("delete"), UnaryExpression), name ~ ".DeleteExpression")(p);
    }

    static TParseTree DeleteExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("delete"), UnaryExpression), name ~ ".DeleteExpression")(TParseTree("", false,[], s));
    }

    static TParseTree CastExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("cast"), literal!("("), option!(or!(Type, CastEqual)), literal!(")"), UnaryExpression), name ~ ".CastExpression")(p);
    }

    static TParseTree CastExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("cast"), literal!("("), option!(or!(Type, CastEqual)), literal!(")"), UnaryExpression), name ~ ".CastExpression")(TParseTree("", false,[], s));
    }

    static TParseTree CastEqual(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("const"), literal!("shared")), spaceAnd!(Spacing, literal!("shared"), literal!("const")), spaceAnd!(Spacing, literal!("inout"), literal!("shared")), spaceAnd!(Spacing, literal!("shared"), literal!("inout")), literal!("const"), literal!("inout"), literal!("immutable"), literal!("shared")), name ~ ".CastEqual")(p);
    }

    static TParseTree CastEqual(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("const"), literal!("shared")), spaceAnd!(Spacing, literal!("shared"), literal!("const")), spaceAnd!(Spacing, literal!("inout"), literal!("shared")), spaceAnd!(Spacing, literal!("shared"), literal!("inout")), literal!("const"), literal!("inout"), literal!("immutable"), literal!("shared")), name ~ ".CastEqual")(TParseTree("", false,[], s));
    }

    static TParseTree PowExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, PostfixExpression, option!(spaceAnd!(Spacing, literal!("^^"), UnaryExpression))), name ~ ".PowExpression")(p);
    }

    static TParseTree PowExpression(string s)
    {
        return named!(spaceAnd!(Spacing, PostfixExpression, option!(spaceAnd!(Spacing, literal!("^^"), UnaryExpression))), name ~ ".PowExpression")(TParseTree("", false,[], s));
    }

    static TParseTree PostfixExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, PrimaryExpression, zeroOrMore!(or!(IndexExpression, SliceExpression)), option!(or!(spaceAnd!(Spacing, literal!("."), NewExpression), spaceAnd!(Spacing, literal!("."), TemplateIdentifier), spaceAnd!(Spacing, literal!("."), Identifier), literal!("++"), literal!("--"), spaceAnd!(Spacing, literal!("("), option!(ArgumentList), literal!(")"))))), name ~ ".PostfixExpression")(p);
    }

    static TParseTree PostfixExpression(string s)
    {
        return named!(spaceAnd!(Spacing, PrimaryExpression, zeroOrMore!(or!(IndexExpression, SliceExpression)), option!(or!(spaceAnd!(Spacing, literal!("."), NewExpression), spaceAnd!(Spacing, literal!("."), TemplateIdentifier), spaceAnd!(Spacing, literal!("."), Identifier), literal!("++"), literal!("--"), spaceAnd!(Spacing, literal!("("), option!(ArgumentList), literal!(")"))))), name ~ ".PostfixExpression")(TParseTree("", false,[], s));
    }

    static TParseTree IndexExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("["), ArgumentList, literal!("]")), name ~ ".IndexExpression")(p);
    }

    static TParseTree IndexExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("["), ArgumentList, literal!("]")), name ~ ".IndexExpression")(TParseTree("", false,[], s));
    }

    static TParseTree SliceExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("["), literal!("]"), literal!("["), AssignExpression, literal!(".."), AssignExpression, literal!("]")), name ~ ".SliceExpression")(p);
    }

    static TParseTree SliceExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("["), literal!("]"), literal!("["), AssignExpression, literal!(".."), AssignExpression, literal!("]")), name ~ ".SliceExpression")(TParseTree("", false,[], s));
    }

    static TParseTree PrimaryExpression(TParseTree p)
    {
        return named!(or!(literal!("this"), literal!("super"), literal!("null"), literal!("true"), literal!("false"), literal!("$"), literal!("__FILE__"), literal!("__LINE__"), TemplateInstance, spaceAnd!(Spacing, literal!("."), TemplateInstance), Identifier, spaceAnd!(Spacing, literal!("."), Identifier), FloatLiteral, IntegerLiteral, CharacterLiteral, StringLiterals, ArrayLiteral, AssocArrayLiteral, Lambda, FunctionLiteral, AssertExpression, MixinExpression, ImportExpression, spaceAnd!(Spacing, BasicType, literal!("."), Identifier), Typeof, TypeidExpression, IsExpression, spaceAnd!(Spacing, literal!("("), Expression, literal!(")")), TraitsExpression), name ~ ".PrimaryExpression")(p);
    }

    static TParseTree PrimaryExpression(string s)
    {
        return named!(or!(literal!("this"), literal!("super"), literal!("null"), literal!("true"), literal!("false"), literal!("$"), literal!("__FILE__"), literal!("__LINE__"), TemplateInstance, spaceAnd!(Spacing, literal!("."), TemplateInstance), Identifier, spaceAnd!(Spacing, literal!("."), Identifier), FloatLiteral, IntegerLiteral, CharacterLiteral, StringLiterals, ArrayLiteral, AssocArrayLiteral, Lambda, FunctionLiteral, AssertExpression, MixinExpression, ImportExpression, spaceAnd!(Spacing, BasicType, literal!("."), Identifier), Typeof, TypeidExpression, IsExpression, spaceAnd!(Spacing, literal!("("), Expression, literal!(")")), TraitsExpression), name ~ ".PrimaryExpression")(TParseTree("", false,[], s));
    }

    static TParseTree StringLiterals(TParseTree p)
    {
        return named!(oneOrMore!(StringLiteral), name ~ ".StringLiterals")(p);
    }

    static TParseTree StringLiterals(string s)
    {
        return named!(oneOrMore!(StringLiteral), name ~ ".StringLiterals")(TParseTree("", false,[], s));
    }

    static TParseTree ArrayLiteral(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("["), option!(ArgumentList), literal!("]")), name ~ ".ArrayLiteral")(p);
    }

    static TParseTree ArrayLiteral(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("["), option!(ArgumentList), literal!("]")), name ~ ".ArrayLiteral")(TParseTree("", false,[], s));
    }

    static TParseTree AssocArrayLiteral(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("["), list!(KeyValuePair, literal!(",")), literal!("]")), name ~ ".AssocArrayLiteral")(p);
    }

    static TParseTree AssocArrayLiteral(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("["), list!(KeyValuePair, literal!(",")), literal!("]")), name ~ ".AssocArrayLiteral")(TParseTree("", false,[], s));
    }

    static TParseTree KeyValuePair(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AssignExpression, literal!(":"), AssignExpression), name ~ ".KeyValuePair")(p);
    }

    static TParseTree KeyValuePair(string s)
    {
        return named!(spaceAnd!(Spacing, AssignExpression, literal!(":"), AssignExpression), name ~ ".KeyValuePair")(TParseTree("", false,[], s));
    }

    static TParseTree Lambda(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Identifier, literal!("=>"), AssignExpression), spaceAnd!(Spacing, ParameterAttributes, literal!("=>"), AssignExpression)), name ~ ".Lambda")(p);
    }

    static TParseTree Lambda(string s)
    {
        return named!(or!(spaceAnd!(Spacing, Identifier, literal!("=>"), AssignExpression), spaceAnd!(Spacing, ParameterAttributes, literal!("=>"), AssignExpression)), name ~ ".Lambda")(TParseTree("", false,[], s));
    }

    static TParseTree FunctionLiteral(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, option!(spaceAnd!(Spacing, or!(literal!("function"), literal!("delegate")), option!(Type))), option!(ParameterAttributes), FunctionBody), name ~ ".FunctionLiteral")(p);
    }

    static TParseTree FunctionLiteral(string s)
    {
        return named!(spaceAnd!(Spacing, option!(spaceAnd!(Spacing, or!(literal!("function"), literal!("delegate")), option!(Type))), option!(ParameterAttributes), FunctionBody), name ~ ".FunctionLiteral")(TParseTree("", false,[], s));
    }

    static TParseTree ParameterAttributes(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Parameters, option!(FunctionAttributes)), name ~ ".ParameterAttributes")(p);
    }

    static TParseTree ParameterAttributes(string s)
    {
        return named!(spaceAnd!(Spacing, Parameters, option!(FunctionAttributes)), name ~ ".ParameterAttributes")(TParseTree("", false,[], s));
    }

    static TParseTree AssertExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("assert"), literal!("("), AssignExpression, option!(spaceAnd!(Spacing, literal!(","), AssignExpression)), literal!(")")), name ~ ".AssertExpression")(p);
    }

    static TParseTree AssertExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("assert"), literal!("("), AssignExpression, option!(spaceAnd!(Spacing, literal!(","), AssignExpression)), literal!(")")), name ~ ".AssertExpression")(TParseTree("", false,[], s));
    }

    static TParseTree MixinExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("("), AssignExpression, literal!(")")), name ~ ".MixinExpression")(p);
    }

    static TParseTree MixinExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("("), AssignExpression, literal!(")")), name ~ ".MixinExpression")(TParseTree("", false,[], s));
    }

    static TParseTree ImportExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("import"), literal!("("), AssignExpression, literal!(")")), name ~ ".ImportExpression")(p);
    }

    static TParseTree ImportExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("import"), literal!("("), AssignExpression, literal!(")")), name ~ ".ImportExpression")(TParseTree("", false,[], s));
    }

    static TParseTree TypeidExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("typeid"), literal!("("), or!(Type, Expression), literal!(")")), name ~ ".TypeidExpression")(p);
    }

    static TParseTree TypeidExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("typeid"), literal!("("), or!(Type, Expression), literal!(")")), name ~ ".TypeidExpression")(TParseTree("", false,[], s));
    }

    static TParseTree IsExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("is"), literal!("("), Type, option!(or!(spaceAnd!(Spacing, literal!(":"), TypeSpecialization), spaceAnd!(Spacing, literal!("=="), TypeSpecialization), spaceAnd!(Spacing, Identifier, option!(or!(spaceAnd!(Spacing, literal!(":"), TypeSpecialization, option!(spaceAnd!(Spacing, literal!(","), TemplateParameterList))), spaceAnd!(Spacing, literal!("=="), TypeSpecialization, option!(spaceAnd!(Spacing, literal!(","), TemplateParameterList)))))))), literal!(")")), name ~ ".IsExpression")(p);
    }

    static TParseTree IsExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("is"), literal!("("), Type, option!(or!(spaceAnd!(Spacing, literal!(":"), TypeSpecialization), spaceAnd!(Spacing, literal!("=="), TypeSpecialization), spaceAnd!(Spacing, Identifier, option!(or!(spaceAnd!(Spacing, literal!(":"), TypeSpecialization, option!(spaceAnd!(Spacing, literal!(","), TemplateParameterList))), spaceAnd!(Spacing, literal!("=="), TypeSpecialization, option!(spaceAnd!(Spacing, literal!(","), TemplateParameterList)))))))), literal!(")")), name ~ ".IsExpression")(TParseTree("", false,[], s));
    }

    static TParseTree TypeSpecialization(TParseTree p)
    {
        return named!(or!(Type, literal!("struct"), literal!("union"), literal!("class"), literal!("interface"), literal!("enum"), literal!("function"), literal!("delegate"), literal!("super"), literal!("const"), literal!("immutable"), literal!("inout"), literal!("shared"), literal!("return")), name ~ ".TypeSpecialization")(p);
    }

    static TParseTree TypeSpecialization(string s)
    {
        return named!(or!(Type, literal!("struct"), literal!("union"), literal!("class"), literal!("interface"), literal!("enum"), literal!("function"), literal!("delegate"), literal!("super"), literal!("const"), literal!("immutable"), literal!("inout"), literal!("shared"), literal!("return")), name ~ ".TypeSpecialization")(TParseTree("", false,[], s));
    }

    static TParseTree AttributeSpecifier(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Attribute, DeclarationBlock), spaceAnd!(Spacing, Attribute, literal!(":"))), name ~ ".AttributeSpecifier")(p);
    }

    static TParseTree AttributeSpecifier(string s)
    {
        return named!(or!(spaceAnd!(Spacing, Attribute, DeclarationBlock), spaceAnd!(Spacing, Attribute, literal!(":"))), name ~ ".AttributeSpecifier")(TParseTree("", false,[], s));
    }

    static TParseTree Attribute(TParseTree p)
    {
        return named!(or!(LinkageAttribute, AlignAttribute, Pragma, literal!("deprecated"), ProtectionAttribute, literal!("static"), literal!("extern"), literal!("final"), literal!("synchronized"), literal!("override"), literal!("abstract"), literal!("const"), literal!("auto"), literal!("scope"), literal!("__gshared"), literal!("shared"), literal!("immutable"), literal!("inout"), literal!("@disable")), name ~ ".Attribute")(p);
    }

    static TParseTree Attribute(string s)
    {
        return named!(or!(LinkageAttribute, AlignAttribute, Pragma, literal!("deprecated"), ProtectionAttribute, literal!("static"), literal!("extern"), literal!("final"), literal!("synchronized"), literal!("override"), literal!("abstract"), literal!("const"), literal!("auto"), literal!("scope"), literal!("__gshared"), literal!("shared"), literal!("immutable"), literal!("inout"), literal!("@disable")), name ~ ".Attribute")(TParseTree("", false,[], s));
    }

    static TParseTree DeclarationBlock(TParseTree p)
    {
        return named!(or!(DeclDef, spaceAnd!(Spacing, literal!("{"), DeclDefs, literal!("}"))), name ~ ".DeclarationBlock")(p);
    }

    static TParseTree DeclarationBlock(string s)
    {
        return named!(or!(DeclDef, spaceAnd!(Spacing, literal!("{"), DeclDefs, literal!("}"))), name ~ ".DeclarationBlock")(TParseTree("", false,[], s));
    }

    static TParseTree LinkageAttribute(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("extern"), literal!("("), LinkageType, literal!(")")), name ~ ".LinkageAttribute")(p);
    }

    static TParseTree LinkageAttribute(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("extern"), literal!("("), LinkageType, literal!(")")), name ~ ".LinkageAttribute")(TParseTree("", false,[], s));
    }

    static TParseTree LinkageType(TParseTree p)
    {
        return named!(or!(literal!("C++"), literal!("C"), literal!("D"), literal!("Windows"), literal!("Pascal"), literal!("System")), name ~ ".LinkageType")(p);
    }

    static TParseTree LinkageType(string s)
    {
        return named!(or!(literal!("C++"), literal!("C"), literal!("D"), literal!("Windows"), literal!("Pascal"), literal!("System")), name ~ ".LinkageType")(TParseTree("", false,[], s));
    }

    static TParseTree AlignAttribute(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("align"), option!(spaceAnd!(Spacing, literal!("("), IntegerLiteral, literal!(")")))), name ~ ".AlignAttribute")(p);
    }

    static TParseTree AlignAttribute(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("align"), option!(spaceAnd!(Spacing, literal!("("), IntegerLiteral, literal!(")")))), name ~ ".AlignAttribute")(TParseTree("", false,[], s));
    }

    static TParseTree ProtectionAttribute(TParseTree p)
    {
        return named!(or!(literal!("private"), literal!("package"), literal!("protected"), literal!("public"), literal!("export")), name ~ ".ProtectionAttribute")(p);
    }

    static TParseTree ProtectionAttribute(string s)
    {
        return named!(or!(literal!("private"), literal!("package"), literal!("protected"), literal!("public"), literal!("export")), name ~ ".ProtectionAttribute")(TParseTree("", false,[], s));
    }

    static TParseTree ClassDeclaration(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("class"), Identifier, option!(BaseClassList), ClassBody), ClassTemplateDeclaration), name ~ ".ClassDeclaration")(p);
    }

    static TParseTree ClassDeclaration(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("class"), Identifier, option!(BaseClassList), ClassBody), ClassTemplateDeclaration), name ~ ".ClassDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree BaseClassList(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), list!(Identifier, literal!(","))), name ~ ".BaseClassList")(p);
    }

    static TParseTree BaseClassList(string s)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), list!(Identifier, literal!(","))), name ~ ".BaseClassList")(TParseTree("", false,[], s));
    }

    static TParseTree ClassBody(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(ClassBodyDeclarations), literal!("}")), name ~ ".ClassBody")(p);
    }

    static TParseTree ClassBody(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(ClassBodyDeclarations), literal!("}")), name ~ ".ClassBody")(TParseTree("", false,[], s));
    }

    static TParseTree ClassBodyDeclarations(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, ClassBodyDeclaration, option!(ClassBodyDeclarations)), name ~ ".ClassBodyDeclarations")(p);
    }

    static TParseTree ClassBodyDeclarations(string s)
    {
        return named!(spaceAnd!(Spacing, ClassBodyDeclaration, option!(ClassBodyDeclarations)), name ~ ".ClassBodyDeclarations")(TParseTree("", false,[], s));
    }

    static TParseTree ClassBodyDeclaration(TParseTree p)
    {
        return named!(or!(DeclDef, Invariant, ClassAllocator, ClassDeallocator), name ~ ".ClassBodyDeclaration")(p);
    }

    static TParseTree ClassBodyDeclaration(string s)
    {
        return named!(or!(DeclDef, Invariant, ClassAllocator, ClassDeallocator), name ~ ".ClassBodyDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree Constructor(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("this"), Parameters, FunctionBody), TemplatedConstructor), name ~ ".Constructor")(p);
    }

    static TParseTree Constructor(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("this"), Parameters, FunctionBody), TemplatedConstructor), name ~ ".Constructor")(TParseTree("", false,[], s));
    }

    static TParseTree Destructor(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("~"), literal!("this"), literal!("("), literal!(")"), FunctionBody), name ~ ".Destructor")(p);
    }

    static TParseTree Destructor(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("~"), literal!("this"), literal!("("), literal!(")"), FunctionBody), name ~ ".Destructor")(TParseTree("", false,[], s));
    }

    static TParseTree StaticConstructor(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("this"), literal!("("), literal!(")"), FunctionBody), name ~ ".StaticConstructor")(p);
    }

    static TParseTree StaticConstructor(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("this"), literal!("("), literal!(")"), FunctionBody), name ~ ".StaticConstructor")(TParseTree("", false,[], s));
    }

    static TParseTree StaticDestructor(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("~"), literal!("this"), literal!("("), literal!(")"), FunctionBody), name ~ ".StaticDestructor")(p);
    }

    static TParseTree StaticDestructor(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("~"), literal!("this"), literal!("("), literal!(")"), FunctionBody), name ~ ".StaticDestructor")(TParseTree("", false,[], s));
    }

    static TParseTree SharedStaticConstructor(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("shared"), literal!("static"), literal!("this"), literal!("("), literal!(")"), FunctionBody), name ~ ".SharedStaticConstructor")(p);
    }

    static TParseTree SharedStaticConstructor(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("shared"), literal!("static"), literal!("this"), literal!("("), literal!(")"), FunctionBody), name ~ ".SharedStaticConstructor")(TParseTree("", false,[], s));
    }

    static TParseTree SharedStaticDestructor(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("shared"), literal!("static"), literal!("~"), literal!("this"), literal!("("), literal!(")"), FunctionBody), name ~ ".SharedStaticDestructor")(p);
    }

    static TParseTree SharedStaticDestructor(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("shared"), literal!("static"), literal!("~"), literal!("this"), literal!("("), literal!(")"), FunctionBody), name ~ ".SharedStaticDestructor")(TParseTree("", false,[], s));
    }

    static TParseTree Invariant(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("invariant"), literal!("("), literal!(")"), BlockStatement), name ~ ".Invariant")(p);
    }

    static TParseTree Invariant(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("invariant"), literal!("("), literal!(")"), BlockStatement), name ~ ".Invariant")(TParseTree("", false,[], s));
    }

    static TParseTree ClassAllocator(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("new"), Parameters, FunctionBody), name ~ ".ClassAllocator")(p);
    }

    static TParseTree ClassAllocator(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("new"), Parameters, FunctionBody), name ~ ".ClassAllocator")(TParseTree("", false,[], s));
    }

    static TParseTree ClassDeallocator(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("delete"), Parameters, FunctionBody), name ~ ".ClassDeallocator")(p);
    }

    static TParseTree ClassDeallocator(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("delete"), Parameters, FunctionBody), name ~ ".ClassDeallocator")(TParseTree("", false,[], s));
    }

    static TParseTree AliasThis(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), Identifier, literal!("this"), literal!(";")), name ~ ".AliasThis")(p);
    }

    static TParseTree AliasThis(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), Identifier, literal!("this"), literal!(";")), name ~ ".AliasThis")(TParseTree("", false,[], s));
    }

    static TParseTree NewAnonClassExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("new"), option!(AllocatorArguments), literal!("class"), option!(ClassArguments), Identifier, option!(list!(Identifier, literal!(","))), ClassBody), name ~ ".NewAnonClassExpression")(p);
    }

    static TParseTree NewAnonClassExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("new"), option!(AllocatorArguments), literal!("class"), option!(ClassArguments), Identifier, option!(list!(Identifier, literal!(","))), ClassBody), name ~ ".NewAnonClassExpression")(TParseTree("", false,[], s));
    }

    static TParseTree ClassArguments(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("("), option!(ArgumentList), literal!(")")), name ~ ".ClassArguments")(p);
    }

    static TParseTree ClassArguments(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("("), option!(ArgumentList), literal!(")")), name ~ ".ClassArguments")(TParseTree("", false,[], s));
    }

    static TParseTree EnumDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("enum"), option!(EnumTag), option!(spaceAnd!(Spacing, literal!(":"), EnumBaseType)), EnumBody), name ~ ".EnumDeclaration")(p);
    }

    static TParseTree EnumDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("enum"), option!(EnumTag), option!(spaceAnd!(Spacing, literal!(":"), EnumBaseType)), EnumBody), name ~ ".EnumDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree EnumTag(TParseTree p)
    {
        return named!(Identifier, name ~ ".EnumTag")(p);
    }

    static TParseTree EnumTag(string s)
    {
        return named!(Identifier, name ~ ".EnumTag")(TParseTree("", false,[], s));
    }

    static TParseTree EnumBaseType(TParseTree p)
    {
        return named!(Type, name ~ ".EnumBaseType")(p);
    }

    static TParseTree EnumBaseType(string s)
    {
        return named!(Type, name ~ ".EnumBaseType")(TParseTree("", false,[], s));
    }

    static TParseTree EnumBody(TParseTree p)
    {
        return named!(or!(literal!(";"), spaceAnd!(Spacing, literal!("{"), list!(EnumMember, literal!(",")), literal!("}"))), name ~ ".EnumBody")(p);
    }

    static TParseTree EnumBody(string s)
    {
        return named!(or!(literal!(";"), spaceAnd!(Spacing, literal!("{"), list!(EnumMember, literal!(",")), literal!("}"))), name ~ ".EnumBody")(TParseTree("", false,[], s));
    }

    static TParseTree EnumMember(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Type, literal!("="), AssignExpression), spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("="), AssignExpression)))), name ~ ".EnumMember")(p);
    }

    static TParseTree EnumMember(string s)
    {
        return named!(or!(spaceAnd!(Spacing, Type, literal!("="), AssignExpression), spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("="), AssignExpression)))), name ~ ".EnumMember")(TParseTree("", false,[], s));
    }

    static TParseTree FunctionBody(TParseTree p)
    {
        return named!(or!(BlockStatement, BodyStatement, spaceAnd!(Spacing, InStatement, BodyStatement), spaceAnd!(Spacing, OutStatement, BodyStatement), spaceAnd!(Spacing, InStatement, OutStatement, BodyStatement), spaceAnd!(Spacing, OutStatement, InStatement, BodyStatement)), name ~ ".FunctionBody")(p);
    }

    static TParseTree FunctionBody(string s)
    {
        return named!(or!(BlockStatement, BodyStatement, spaceAnd!(Spacing, InStatement, BodyStatement), spaceAnd!(Spacing, OutStatement, BodyStatement), spaceAnd!(Spacing, InStatement, OutStatement, BodyStatement), spaceAnd!(Spacing, OutStatement, InStatement, BodyStatement)), name ~ ".FunctionBody")(TParseTree("", false,[], s));
    }

    static TParseTree InStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("in"), BlockStatement), name ~ ".InStatement")(p);
    }

    static TParseTree InStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("in"), BlockStatement), name ~ ".InStatement")(TParseTree("", false,[], s));
    }

    static TParseTree OutStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("out"), option!(spaceAnd!(Spacing, literal!("("), Identifier, literal!(")"))), BlockStatement), name ~ ".OutStatement")(p);
    }

    static TParseTree OutStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("out"), option!(spaceAnd!(Spacing, literal!("("), Identifier, literal!(")"))), BlockStatement), name ~ ".OutStatement")(TParseTree("", false,[], s));
    }

    static TParseTree BodyStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("body"), BlockStatement), name ~ ".BodyStatement")(p);
    }

    static TParseTree BodyStatement(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("body"), BlockStatement), name ~ ".BodyStatement")(TParseTree("", false,[], s));
    }

    static TParseTree AsmInstruction(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("align"), IntegerExpression), literal!("even"), literal!("naked"), spaceAnd!(Spacing, or!(literal!("db"), literal!("ds"), literal!("di"), literal!("dl"), literal!("df"), literal!("dd"), literal!("de")), list!(Operand, literal!(","))), spaceAnd!(Spacing, Identifier, literal!(":"), AsmInstruction), OpCode, spaceAnd!(Spacing, OpCode, list!(Operand, literal!(",")))), name ~ ".AsmInstruction")(p);
    }

    static TParseTree AsmInstruction(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("align"), IntegerExpression), literal!("even"), literal!("naked"), spaceAnd!(Spacing, or!(literal!("db"), literal!("ds"), literal!("di"), literal!("dl"), literal!("df"), literal!("dd"), literal!("de")), list!(Operand, literal!(","))), spaceAnd!(Spacing, Identifier, literal!(":"), AsmInstruction), OpCode, spaceAnd!(Spacing, OpCode, list!(Operand, literal!(",")))), name ~ ".AsmInstruction")(TParseTree("", false,[], s));
    }

    static TParseTree IntegerExpression(TParseTree p)
    {
        return named!(or!(IntegerLiteral, Identifier), name ~ ".IntegerExpression")(p);
    }

    static TParseTree IntegerExpression(string s)
    {
        return named!(or!(IntegerLiteral, Identifier), name ~ ".IntegerExpression")(TParseTree("", false,[], s));
    }

    static TParseTree Operand(TParseTree p)
    {
        return named!(AsmExp, name ~ ".Operand")(p);
    }

    static TParseTree Operand(string s)
    {
        return named!(AsmExp, name ~ ".Operand")(TParseTree("", false,[], s));
    }

    static TParseTree AsmExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmLogOrExp, option!(spaceAnd!(Spacing, literal!("?"), AsmExp, literal!(":"), AsmExp))), name ~ ".AsmExp")(p);
    }

    static TParseTree AsmExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmLogOrExp, option!(spaceAnd!(Spacing, literal!("?"), AsmExp, literal!(":"), AsmExp))), name ~ ".AsmExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmLogOrExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmLogAndExp, option!(spaceAnd!(Spacing, literal!("||"), AsmLogAndExp))), name ~ ".AsmLogOrExp")(p);
    }

    static TParseTree AsmLogOrExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmLogAndExp, option!(spaceAnd!(Spacing, literal!("||"), AsmLogAndExp))), name ~ ".AsmLogOrExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmLogAndExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmOrExp, option!(spaceAnd!(Spacing, literal!("&&"), AsmOrExp))), name ~ ".AsmLogAndExp")(p);
    }

    static TParseTree AsmLogAndExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmOrExp, option!(spaceAnd!(Spacing, literal!("&&"), AsmOrExp))), name ~ ".AsmLogAndExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmOrExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmXorExp, option!(spaceAnd!(Spacing, literal!("|"), AsmXorExp))), name ~ ".AsmOrExp")(p);
    }

    static TParseTree AsmOrExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmXorExp, option!(spaceAnd!(Spacing, literal!("|"), AsmXorExp))), name ~ ".AsmOrExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmXorExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmAndExp, option!(spaceAnd!(Spacing, literal!("^"), AsmAndExp))), name ~ ".AsmXorExp")(p);
    }

    static TParseTree AsmXorExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmAndExp, option!(spaceAnd!(Spacing, literal!("^"), AsmAndExp))), name ~ ".AsmXorExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmAndExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmEqualExp, option!(spaceAnd!(Spacing, literal!("&"), AsmEqualExp))), name ~ ".AsmAndExp")(p);
    }

    static TParseTree AsmAndExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmEqualExp, option!(spaceAnd!(Spacing, literal!("&"), AsmEqualExp))), name ~ ".AsmAndExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmEqualExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmRelExp, option!(spaceAnd!(Spacing, or!(literal!("=="), literal!("!=")), AsmRelExp))), name ~ ".AsmEqualExp")(p);
    }

    static TParseTree AsmEqualExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmRelExp, option!(spaceAnd!(Spacing, or!(literal!("=="), literal!("!=")), AsmRelExp))), name ~ ".AsmEqualExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmRelExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmShiftExp, option!(spaceAnd!(Spacing, or!(literal!("<="), literal!(">="), literal!("<"), literal!(">")), AsmShiftExp))), name ~ ".AsmRelExp")(p);
    }

    static TParseTree AsmRelExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmShiftExp, option!(spaceAnd!(Spacing, or!(literal!("<="), literal!(">="), literal!("<"), literal!(">")), AsmShiftExp))), name ~ ".AsmRelExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmShiftExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmAddExp, option!(spaceAnd!(Spacing, or!(literal!(">>>"), literal!("<<"), literal!(">>")), AsmAddExp))), name ~ ".AsmShiftExp")(p);
    }

    static TParseTree AsmShiftExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmAddExp, option!(spaceAnd!(Spacing, or!(literal!(">>>"), literal!("<<"), literal!(">>")), AsmAddExp))), name ~ ".AsmShiftExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmAddExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmMulExp, option!(spaceAnd!(Spacing, or!(literal!("+"), literal!("-")), AsmMulExp))), name ~ ".AsmAddExp")(p);
    }

    static TParseTree AsmAddExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmMulExp, option!(spaceAnd!(Spacing, or!(literal!("+"), literal!("-")), AsmMulExp))), name ~ ".AsmAddExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmMulExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmBrExp, option!(spaceAnd!(Spacing, or!(literal!("*"), literal!("/"), literal!("%")), AsmBrExp))), name ~ ".AsmMulExp")(p);
    }

    static TParseTree AsmMulExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmBrExp, option!(spaceAnd!(Spacing, or!(literal!("*"), literal!("/"), literal!("%")), AsmBrExp))), name ~ ".AsmMulExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmBrExp(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmUnaExp, option!(spaceAnd!(Spacing, literal!("["), AsmExp, literal!("]")))), name ~ ".AsmBrExp")(p);
    }

    static TParseTree AsmBrExp(string s)
    {
        return named!(spaceAnd!(Spacing, AsmUnaExp, option!(spaceAnd!(Spacing, literal!("["), AsmExp, literal!("]")))), name ~ ".AsmBrExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmUnaExp(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, AsmTypePrefix, AsmExp), spaceAnd!(Spacing, or!(literal!("offsetof"), literal!("seg")), AsmExp), spaceAnd!(Spacing, or!(literal!("+"), literal!("-"), literal!("!"), literal!("~")), AsmUnaExp), AsmPrimaryExp), name ~ ".AsmUnaExp")(p);
    }

    static TParseTree AsmUnaExp(string s)
    {
        return named!(or!(spaceAnd!(Spacing, AsmTypePrefix, AsmExp), spaceAnd!(Spacing, or!(literal!("offsetof"), literal!("seg")), AsmExp), spaceAnd!(Spacing, or!(literal!("+"), literal!("-"), literal!("!"), literal!("~")), AsmUnaExp), AsmPrimaryExp), name ~ ".AsmUnaExp")(TParseTree("", false,[], s));
    }

    static TParseTree AsmPrimaryExp(TParseTree p)
    {
        return named!(or!(FloatLiteral, IntegerLiteral, literal!("__LOCAL_SIZE"), literal!("$"), Register, DotIdentifier), name ~ ".AsmPrimaryExp")(p);
    }

    static TParseTree AsmPrimaryExp(string s)
    {
        return named!(or!(FloatLiteral, IntegerLiteral, literal!("__LOCAL_SIZE"), literal!("$"), Register, DotIdentifier), name ~ ".AsmPrimaryExp")(TParseTree("", false,[], s));
    }

    static TParseTree DotIdentifier(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("."), DotIdentifier))), name ~ ".DotIdentifier")(p);
    }

    static TParseTree DotIdentifier(string s)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("."), DotIdentifier))), name ~ ".DotIdentifier")(TParseTree("", false,[], s));
    }

    static TParseTree AsmTypePrefix(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(literal!("near"), literal!("far"), literal!("byte"), literal!("short"), literal!("int"), literal!("word"), literal!("dword"), literal!("qword"), literal!("float"), literal!("double"), literal!("real")), literal!("ptr")), name ~ ".AsmTypePrefix")(p);
    }

    static TParseTree AsmTypePrefix(string s)
    {
        return named!(spaceAnd!(Spacing, or!(literal!("near"), literal!("far"), literal!("byte"), literal!("short"), literal!("int"), literal!("word"), literal!("dword"), literal!("qword"), literal!("float"), literal!("double"), literal!("real")), literal!("ptr")), name ~ ".AsmTypePrefix")(TParseTree("", false,[], s));
    }

    static TParseTree Register(TParseTree p)
    {
        return named!(Identifier, name ~ ".Register")(p);
    }

    static TParseTree Register(string s)
    {
        return named!(Identifier, name ~ ".Register")(TParseTree("", false,[], s));
    }

    static TParseTree OpCode(TParseTree p)
    {
        return named!(Identifier, name ~ ".OpCode")(p);
    }

    static TParseTree OpCode(string s)
    {
        return named!(Identifier, name ~ ".OpCode")(TParseTree("", false,[], s));
    }

    static TParseTree InterfaceDeclaration(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("interface"), Identifier, option!(BaseInterfaceList), InterfaceBody), InterfaceTemplateDeclaration), name ~ ".InterfaceDeclaration")(p);
    }

    static TParseTree InterfaceDeclaration(string s)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("interface"), Identifier, option!(BaseInterfaceList), InterfaceBody), InterfaceTemplateDeclaration), name ~ ".InterfaceDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree BaseInterfaceList(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), list!(Identifier, literal!(","))), name ~ ".BaseInterfaceList")(p);
    }

    static TParseTree BaseInterfaceList(string s)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), list!(Identifier, literal!(","))), name ~ ".BaseInterfaceList")(TParseTree("", false,[], s));
    }

    static TParseTree InterfaceBody(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(DeclDefs), literal!("}")), name ~ ".InterfaceBody")(p);
    }

    static TParseTree InterfaceBody(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(DeclDefs), literal!("}")), name ~ ".InterfaceBody")(TParseTree("", false,[], s));
    }

    static TParseTree Pragma(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("pragma"), literal!("("), Identifier, option!(spaceAnd!(Spacing, literal!(","), ArgumentList)), literal!(")")), name ~ ".Pragma")(p);
    }

    static TParseTree Pragma(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("pragma"), literal!("("), Identifier, option!(spaceAnd!(Spacing, literal!(","), ArgumentList)), literal!(")")), name ~ ".Pragma")(TParseTree("", false,[], s));
    }

    static TParseTree AggregateDeclaration(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, or!(literal!("struct"), literal!("union")), Identifier, or!(StructBody, literal!(";"))), StructTemplateDeclaration, UnionTemplateDeclaration), name ~ ".AggregateDeclaration")(p);
    }

    static TParseTree AggregateDeclaration(string s)
    {
        return named!(or!(spaceAnd!(Spacing, or!(literal!("struct"), literal!("union")), Identifier, or!(StructBody, literal!(";"))), StructTemplateDeclaration, UnionTemplateDeclaration), name ~ ".AggregateDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree StructBody(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(StructBodyDeclarations), literal!("}")), name ~ ".StructBody")(p);
    }

    static TParseTree StructBody(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(StructBodyDeclarations), literal!("}")), name ~ ".StructBody")(TParseTree("", false,[], s));
    }

    static TParseTree StructBodyDeclarations(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, StructBodyDeclaration, option!(StructBodyDeclarations)), name ~ ".StructBodyDeclarations")(p);
    }

    static TParseTree StructBodyDeclarations(string s)
    {
        return named!(spaceAnd!(Spacing, StructBodyDeclaration, option!(StructBodyDeclarations)), name ~ ".StructBodyDeclarations")(TParseTree("", false,[], s));
    }

    static TParseTree StructBodyDeclaration(TParseTree p)
    {
        return named!(or!(DeclDef, StructAllocator, StructDeallocator, StructPostblit, AliasThis), name ~ ".StructBodyDeclaration")(p);
    }

    static TParseTree StructBodyDeclaration(string s)
    {
        return named!(or!(DeclDef, StructAllocator, StructDeallocator, StructPostblit, AliasThis), name ~ ".StructBodyDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree StructAllocator(TParseTree p)
    {
        return named!(ClassAllocator, name ~ ".StructAllocator")(p);
    }

    static TParseTree StructAllocator(string s)
    {
        return named!(ClassAllocator, name ~ ".StructAllocator")(TParseTree("", false,[], s));
    }

    static TParseTree StructDeallocator(TParseTree p)
    {
        return named!(ClassDeallocator, name ~ ".StructDeallocator")(p);
    }

    static TParseTree StructDeallocator(string s)
    {
        return named!(ClassDeallocator, name ~ ".StructDeallocator")(TParseTree("", false,[], s));
    }

    static TParseTree StructPostblit(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("this(this)"), FunctionBody), name ~ ".StructPostblit")(p);
    }

    static TParseTree StructPostblit(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("this(this)"), FunctionBody), name ~ ".StructPostblit")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("template"), TemplateIdentifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint)), name ~ ".TemplateDeclaration")(p);
    }

    static TParseTree TemplateDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("template"), TemplateIdentifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint)), name ~ ".TemplateDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateIdentifier(TParseTree p)
    {
        return named!(Identifier, name ~ ".TemplateIdentifier")(p);
    }

    static TParseTree TemplateIdentifier(string s)
    {
        return named!(Identifier, name ~ ".TemplateIdentifier")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateParameterList(TParseTree p)
    {
        return named!(list!(TemplateParameter, literal!(",")), name ~ ".TemplateParameterList")(p);
    }

    static TParseTree TemplateParameterList(string s)
    {
        return named!(list!(TemplateParameter, literal!(",")), name ~ ".TemplateParameterList")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateParameter(TParseTree p)
    {
        return named!(or!(TemplateTypeParameter, TemplateValueParameter, TemplateAliasParameter, TemplateTupleParameter, TemplateThisParameter), name ~ ".TemplateParameter")(p);
    }

    static TParseTree TemplateParameter(string s)
    {
        return named!(or!(TemplateTypeParameter, TemplateValueParameter, TemplateAliasParameter, TemplateTupleParameter, TemplateThisParameter), name ~ ".TemplateParameter")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateInstance(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, TemplateIdentifier, or!(spaceAnd!(Spacing, literal!("!("), list!(TemplateArgument, literal!(",")), literal!(")")), spaceAnd!(Spacing, literal!("!"), TemplateSingleArgument))), name ~ ".TemplateInstance")(p);
    }

    static TParseTree TemplateInstance(string s)
    {
        return named!(spaceAnd!(Spacing, TemplateIdentifier, or!(spaceAnd!(Spacing, literal!("!("), list!(TemplateArgument, literal!(",")), literal!(")")), spaceAnd!(Spacing, literal!("!"), TemplateSingleArgument))), name ~ ".TemplateInstance")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateArgument(TParseTree p)
    {
        return named!(or!(Type, AssignExpression, Symbol), name ~ ".TemplateArgument")(p);
    }

    static TParseTree TemplateArgument(string s)
    {
        return named!(or!(Type, AssignExpression, Symbol), name ~ ".TemplateArgument")(TParseTree("", false,[], s));
    }

    static TParseTree Symbol(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, option!(literal!(".")), SymbolTail), name ~ ".Symbol")(p);
    }

    static TParseTree Symbol(string s)
    {
        return named!(spaceAnd!(Spacing, option!(literal!(".")), SymbolTail), name ~ ".Symbol")(TParseTree("", false,[], s));
    }

    static TParseTree SymbolTail(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, TemplateInstance, option!(spaceAnd!(Spacing, literal!("."), SymbolTail))), spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("."), SymbolTail)))), name ~ ".SymbolTail")(p);
    }

    static TParseTree SymbolTail(string s)
    {
        return named!(or!(spaceAnd!(Spacing, TemplateInstance, option!(spaceAnd!(Spacing, literal!("."), SymbolTail))), spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("."), SymbolTail)))), name ~ ".SymbolTail")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateSingleArgument(TParseTree p)
    {
        return named!(or!(BasicTypeX, CharacterLiteral, StringLiteral, FloatLiteral, IntegerLiteral, literal!("true"), literal!("false"), literal!("null"), literal!("__LINE__"), literal!("__FILE__"), Identifier), name ~ ".TemplateSingleArgument")(p);
    }

    static TParseTree TemplateSingleArgument(string s)
    {
        return named!(or!(BasicTypeX, CharacterLiteral, StringLiteral, FloatLiteral, IntegerLiteral, literal!("true"), literal!("false"), literal!("null"), literal!("__LINE__"), literal!("__FILE__"), Identifier), name ~ ".TemplateSingleArgument")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateTypeParameter(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(TTPSpecialization), option!(TTPDefault)), name ~ ".TemplateTypeParameter")(p);
    }

    static TParseTree TemplateTypeParameter(string s)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(TTPSpecialization), option!(TTPDefault)), name ~ ".TemplateTypeParameter")(TParseTree("", false,[], s));
    }

    static TParseTree TTPSpecialization(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), Type), name ~ ".TTPSpecialization")(p);
    }

    static TParseTree TTPSpecialization(string s)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), Type), name ~ ".TTPSpecialization")(TParseTree("", false,[], s));
    }

    static TParseTree TTPDefault(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("="), Type), name ~ ".TTPDefault")(p);
    }

    static TParseTree TTPDefault(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("="), Type), name ~ ".TTPDefault")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateThisParameter(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("this"), TemplateTypeParameter), name ~ ".TemplateThisParameter")(p);
    }

    static TParseTree TemplateThisParameter(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("this"), TemplateTypeParameter), name ~ ".TemplateThisParameter")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateValueParameter(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, BasicType, Declarator, option!(TVPSpecialization), option!(TVPDefault)), name ~ ".TemplateValueParameter")(p);
    }

    static TParseTree TemplateValueParameter(string s)
    {
        return named!(spaceAnd!(Spacing, BasicType, Declarator, option!(TVPSpecialization), option!(TVPDefault)), name ~ ".TemplateValueParameter")(TParseTree("", false,[], s));
    }

    static TParseTree TVPSpecialization(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), ConditionalExpression), name ~ ".TVPSpecialization")(p);
    }

    static TParseTree TVPSpecialization(string s)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), ConditionalExpression), name ~ ".TVPSpecialization")(TParseTree("", false,[], s));
    }

    static TParseTree TVPDefault(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("="), or!(literal!("__FILE__"), literal!("__LINE__"), AssignExpression)), name ~ ".TVPDefault")(p);
    }

    static TParseTree TVPDefault(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("="), or!(literal!("__FILE__"), literal!("__LINE__"), AssignExpression)), name ~ ".TVPDefault")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateAliasParameter(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), or!(spaceAnd!(Spacing, BasicType, Declarator), Identifier), option!(TAPSpecialization), option!(TAPDefault)), name ~ ".TemplateAliasParameter")(p);
    }

    static TParseTree TemplateAliasParameter(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), or!(spaceAnd!(Spacing, BasicType, Declarator), Identifier), option!(TAPSpecialization), option!(TAPDefault)), name ~ ".TemplateAliasParameter")(TParseTree("", false,[], s));
    }

    static TParseTree TAPSpecialization(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), or!(Type, ConditionalExpression)), name ~ ".TAPSpecialization")(p);
    }

    static TParseTree TAPSpecialization(string s)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), or!(Type, ConditionalExpression)), name ~ ".TAPSpecialization")(TParseTree("", false,[], s));
    }

    static TParseTree TAPDefault(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("="), or!(Type, ConditionalExpression)), name ~ ".TAPDefault")(p);
    }

    static TParseTree TAPDefault(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("="), or!(Type, ConditionalExpression)), name ~ ".TAPDefault")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateTupleParameter(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, literal!("...")), name ~ ".TemplateTupleParameter")(p);
    }

    static TParseTree TemplateTupleParameter(string s)
    {
        return named!(spaceAnd!(Spacing, Identifier, literal!("...")), name ~ ".TemplateTupleParameter")(TParseTree("", false,[], s));
    }

    static TParseTree TemplatedConstructor(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("this"), literal!("("), TemplateParameterList, literal!(")"), Parameters, option!(Constraint), FunctionBody), name ~ ".TemplatedConstructor")(p);
    }

    static TParseTree TemplatedConstructor(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("this"), literal!("("), TemplateParameterList, literal!(")"), Parameters, option!(Constraint), FunctionBody), name ~ ".TemplatedConstructor")(TParseTree("", false,[], s));
    }

    static TParseTree ClassTemplateDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("class"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), option!(BaseClassList), ClassBody), name ~ ".ClassTemplateDeclaration")(p);
    }

    static TParseTree ClassTemplateDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("class"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), option!(BaseClassList), ClassBody), name ~ ".ClassTemplateDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree StructTemplateDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("struct"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), StructBody), name ~ ".StructTemplateDeclaration")(p);
    }

    static TParseTree StructTemplateDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("struct"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), StructBody), name ~ ".StructTemplateDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree UnionTemplateDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("union"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), StructBody), name ~ ".UnionTemplateDeclaration")(p);
    }

    static TParseTree UnionTemplateDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("union"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), StructBody), name ~ ".UnionTemplateDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree InterfaceTemplateDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("interface"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), option!(BaseInterfaceList), InterfaceBody), name ~ ".InterfaceTemplateDeclaration")(p);
    }

    static TParseTree InterfaceTemplateDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("interface"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), option!(BaseInterfaceList), InterfaceBody), name ~ ".InterfaceTemplateDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree Constraint(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("if"), literal!("("), Expression, literal!(")")), name ~ ".Constraint")(p);
    }

    static TParseTree Constraint(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("if"), literal!("("), Expression, literal!(")")), name ~ ".Constraint")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateMixinDeclaration(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("template"), TemplateIdentifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), literal!("{"), DeclDefs, literal!("}")), name ~ ".TemplateMixinDeclaration")(p);
    }

    static TParseTree TemplateMixinDeclaration(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("template"), TemplateIdentifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), literal!("{"), DeclDefs, literal!("}")), name ~ ".TemplateMixinDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree TemplateMixin(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), TemplateIdentifier, option!(spaceAnd!(Spacing, literal!("!("), list!(TemplateArgument, literal!(",")), literal!(")"))), option!(MixinIdentifier), literal!(";")), name ~ ".TemplateMixin")(p);
    }

    static TParseTree TemplateMixin(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), TemplateIdentifier, option!(spaceAnd!(Spacing, literal!("!("), list!(TemplateArgument, literal!(",")), literal!(")"))), option!(MixinIdentifier), literal!(";")), name ~ ".TemplateMixin")(TParseTree("", false,[], s));
    }

    static TParseTree MixinIdentifier(TParseTree p)
    {
        return named!(Identifier, name ~ ".MixinIdentifier")(p);
    }

    static TParseTree MixinIdentifier(string s)
    {
        return named!(Identifier, name ~ ".MixinIdentifier")(TParseTree("", false,[], s));
    }

    static TParseTree TraitsExpression(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("__traits"), literal!("("), TraitsKeyword, literal!(","), list!(TraitsArgument, literal!(",")), literal!(")")), name ~ ".TraitsExpression")(p);
    }

    static TParseTree TraitsExpression(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("__traits"), literal!("("), TraitsKeyword, literal!(","), list!(TraitsArgument, literal!(",")), literal!(")")), name ~ ".TraitsExpression")(TParseTree("", false,[], s));
    }

    static TParseTree TraitsKeyword(TParseTree p)
    {
        return named!(or!(literal!("isAbstractClass"), literal!("isArithmetic"), literal!("isAssociativeArray"), literal!("isFinalClass"), literal!("isFloating"), literal!("isIntegral"), literal!("isScalar"), literal!("isStaticArray"), literal!("isUnsigned"), literal!("isVitualFunction"), literal!("isVirtualMethod"), literal!("isAbstractFunction"), literal!("isFinalFunction"), literal!("isStaticFunction"), literal!("isRef"), literal!("isOut"), literal!("isLazy"), literal!("hasMember"), literal!("identifier"), literal!("getMember"), literal!("getOverloads"), literal!("getVirtualFunctions"), literal!("getVirtualMethods"), literal!("parent"), literal!("classInstanceSize"), literal!("allMembers"), literal!("derivedMembers"), literal!("isSame"), literal!("compiles")), name ~ ".TraitsKeyword")(p);
    }

    static TParseTree TraitsKeyword(string s)
    {
        return named!(or!(literal!("isAbstractClass"), literal!("isArithmetic"), literal!("isAssociativeArray"), literal!("isFinalClass"), literal!("isFloating"), literal!("isIntegral"), literal!("isScalar"), literal!("isStaticArray"), literal!("isUnsigned"), literal!("isVitualFunction"), literal!("isVirtualMethod"), literal!("isAbstractFunction"), literal!("isFinalFunction"), literal!("isStaticFunction"), literal!("isRef"), literal!("isOut"), literal!("isLazy"), literal!("hasMember"), literal!("identifier"), literal!("getMember"), literal!("getOverloads"), literal!("getVirtualFunctions"), literal!("getVirtualMethods"), literal!("parent"), literal!("classInstanceSize"), literal!("allMembers"), literal!("derivedMembers"), literal!("isSame"), literal!("compiles")), name ~ ".TraitsKeyword")(TParseTree("", false,[], s));
    }

    static TParseTree TraitsArgument(TParseTree p)
    {
        return named!(or!(AssignExpression, Type), name ~ ".TraitsArgument")(p);
    }

    static TParseTree TraitsArgument(string s)
    {
        return named!(or!(AssignExpression, Type), name ~ ".TraitsArgument")(TParseTree("", false,[], s));
    }

    static TParseTree UnitTest(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("unittest"), FunctionBody), name ~ ".UnitTest")(p);
    }

    static TParseTree UnitTest(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("unittest"), FunctionBody), name ~ ".UnitTest")(TParseTree("", false,[], s));
    }

    static TParseTree ConditionalDeclaration(TParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Condition, literal!(":"), Declarations), spaceAnd!(Spacing, Condition, CCDeclarationBlock, option!(spaceAnd!(Spacing, literal!("else"), CCDeclarationBlock)))), name ~ ".ConditionalDeclaration")(p);
    }

    static TParseTree ConditionalDeclaration(string s)
    {
        return named!(or!(spaceAnd!(Spacing, Condition, literal!(":"), Declarations), spaceAnd!(Spacing, Condition, CCDeclarationBlock, option!(spaceAnd!(Spacing, literal!("else"), CCDeclarationBlock)))), name ~ ".ConditionalDeclaration")(TParseTree("", false,[], s));
    }

    static TParseTree CCDeclarationBlock(TParseTree p)
    {
        return named!(or!(Declaration, spaceAnd!(Spacing, literal!("{"), option!(Declaration), literal!("}"))), name ~ ".CCDeclarationBlock")(p);
    }

    static TParseTree CCDeclarationBlock(string s)
    {
        return named!(or!(Declaration, spaceAnd!(Spacing, literal!("{"), option!(Declaration), literal!("}"))), name ~ ".CCDeclarationBlock")(TParseTree("", false,[], s));
    }

    static TParseTree Declarations(TParseTree p)
    {
        return named!(oneOrMore!(Declaration), name ~ ".Declarations")(p);
    }

    static TParseTree Declarations(string s)
    {
        return named!(oneOrMore!(Declaration), name ~ ".Declarations")(TParseTree("", false,[], s));
    }

    static TParseTree ConditionalStatement(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, Condition, NoScopeNonEmptyStatement, option!(spaceAnd!(Spacing, literal!("else"), NoScopeNonEmptyStatement))), name ~ ".ConditionalStatement")(p);
    }

    static TParseTree ConditionalStatement(string s)
    {
        return named!(spaceAnd!(Spacing, Condition, NoScopeNonEmptyStatement, option!(spaceAnd!(Spacing, literal!("else"), NoScopeNonEmptyStatement))), name ~ ".ConditionalStatement")(TParseTree("", false,[], s));
    }

    static TParseTree Condition(TParseTree p)
    {
        return named!(or!(VersionCondition, DebugCondition, StaticIfCondition), name ~ ".Condition")(p);
    }

    static TParseTree Condition(string s)
    {
        return named!(or!(VersionCondition, DebugCondition, StaticIfCondition), name ~ ".Condition")(TParseTree("", false,[], s));
    }

    static TParseTree VersionCondition(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("version"), literal!("("), or!(IntegerLiteral, literal!("unittest"), Identifier), literal!(")")), name ~ ".VersionCondition")(p);
    }

    static TParseTree VersionCondition(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("version"), literal!("("), or!(IntegerLiteral, literal!("unittest"), Identifier), literal!(")")), name ~ ".VersionCondition")(TParseTree("", false,[], s));
    }

    static TParseTree VersionSpecification(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("version"), literal!("="), or!(Identifier, IntegerLiteral), literal!(";")), name ~ ".VersionSpecification")(p);
    }

    static TParseTree VersionSpecification(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("version"), literal!("="), or!(Identifier, IntegerLiteral), literal!(";")), name ~ ".VersionSpecification")(TParseTree("", false,[], s));
    }

    static TParseTree DebugCondition(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("debug"), option!(spaceAnd!(Spacing, literal!("("), or!(IntegerLiteral, Identifier), literal!(")")))), name ~ ".DebugCondition")(p);
    }

    static TParseTree DebugCondition(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("debug"), option!(spaceAnd!(Spacing, literal!("("), or!(IntegerLiteral, Identifier), literal!(")")))), name ~ ".DebugCondition")(TParseTree("", false,[], s));
    }

    static TParseTree DebugSpecification(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("debug"), literal!("="), or!(Identifier, IntegerLiteral), literal!(";")), name ~ ".DebugSpecification")(p);
    }

    static TParseTree DebugSpecification(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("debug"), literal!("="), or!(Identifier, IntegerLiteral), literal!(";")), name ~ ".DebugSpecification")(TParseTree("", false,[], s));
    }

    static TParseTree StaticIfCondition(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("if"), literal!("("), AssignExpression, literal!(")")), name ~ ".StaticIfCondition")(p);
    }

    static TParseTree StaticIfCondition(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("if"), literal!("("), AssignExpression, literal!(")")), name ~ ".StaticIfCondition")(TParseTree("", false,[], s));
    }

    static TParseTree StaticAssert(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("assert"), literal!("("), AssignExpression, option!(spaceAnd!(Spacing, literal!(","), AssignExpression)), literal!(")"), literal!(";")), name ~ ".StaticAssert")(p);
    }

    static TParseTree StaticAssert(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("assert"), literal!("("), AssignExpression, option!(spaceAnd!(Spacing, literal!(","), AssignExpression)), literal!(")"), literal!(";")), name ~ ".StaticAssert")(TParseTree("", false,[], s));
    }

    static TParseTree Identifier(TParseTree p)
    {
        return named!(fuse!(and!(negLookahead!(Keyword), or!(charRange!('a', 'z'), charRange!('A', 'Z'), literal!("_")), zeroOrMore!(or!(charRange!('a', 'z'), charRange!('A', 'Z'), charRange!('0', '9'), literal!("_"))))), name ~ ".Identifier")(p);
    }

    static TParseTree Identifier(string s)
    {
        return named!(fuse!(and!(negLookahead!(Keyword), or!(charRange!('a', 'z'), charRange!('A', 'Z'), literal!("_")), zeroOrMore!(or!(charRange!('a', 'z'), charRange!('A', 'Z'), charRange!('0', '9'), literal!("_"))))), name ~ ".Identifier")(TParseTree("", false,[], s));
    }

    static TParseTree Keyword(TParseTree p)
    {
        return named!(or!(literal!("abstract"), literal!("alias"), literal!("align"), literal!("asm"), literal!("assert"), literal!("auto"), literal!("body"), literal!("bool"), literal!("break"), literal!("byte"), literal!("case"), literal!("cast"), literal!("catch"), literal!("cdouble"), literal!("cent"), literal!("cfloat"), literal!("char"), literal!("class"), literal!("const"), literal!("continue"), literal!("creal"), literal!("dchar"), literal!("debug"), literal!("default"), literal!("delegate"), literal!("delete"), literal!("deprecated"), literal!("double"), literal!("do"), literal!("else"), literal!("enum"), literal!("export"), literal!("extern"), literal!("false"), literal!("finally"), literal!("final"), literal!("float"), literal!("foreach_reverse"), literal!("foreach"), literal!("for"), literal!("function"), literal!("goto"), literal!("idouble"), literal!("if"), literal!("ifloat"), literal!("immutable"), literal!("import"), literal!("inout"), literal!("interface"), literal!("invariant"), literal!("
int"), literal!("in"), literal!("ireal"), literal!("is"), literal!("lazy"), literal!("long"), literal!("macro"), literal!("mixin"), literal!("module"), literal!("new"), literal!("nothrow"), literal!("null"), literal!("out"), literal!("override"), literal!("package"), literal!("pragma"), literal!("private"), literal!("protected"), literal!("public"), literal!("pure"), literal!("real"), literal!("ref"), literal!("return"), literal!("scope"), literal!("shared"), literal!("short"), literal!("static"), literal!("struct"), literal!("super"), literal!("switch"), literal!("synchronized"), literal!("template"), literal!("this"), literal!("throw"), literal!("true"), literal!("try"), literal!("typedef"), literal!("typeid"), literal!("typeof"), literal!("ubyte"), literal!("ucent"), literal!("uint"), literal!("ulong"), literal!("union"), literal!("unittest"), literal!("ushort"), literal!("version"), literal!("void"), literal!("volatile"), literal!("wchar"), literal!("while"), literal!("with"), literal!("__FILE__"), 
literal!("__LINE__"), literal!("__gshared"), literal!("__thread"), literal!("__traits")), name ~ ".Keyword")(p);
    }

    static TParseTree Keyword(string s)
    {
        return named!(or!(literal!("abstract"), literal!("alias"), literal!("align"), literal!("asm"), literal!("assert"), literal!("auto"), literal!("body"), literal!("bool"), literal!("break"), literal!("byte"), literal!("case"), literal!("cast"), literal!("catch"), literal!("cdouble"), literal!("cent"), literal!("cfloat"), literal!("char"), literal!("class"), literal!("const"), literal!("continue"), literal!("creal"), literal!("dchar"), literal!("debug"), literal!("default"), literal!("delegate"), literal!("delete"), literal!("deprecated"), literal!("double"), literal!("do"), literal!("else"), literal!("enum"), literal!("export"), literal!("extern"), literal!("false"), literal!("finally"), literal!("final"), literal!("float"), literal!("foreach_reverse"), literal!("foreach"), literal!("for"), literal!("function"), literal!("goto"), literal!("idouble"), literal!("if"), literal!("ifloat"), literal!("immutable"), literal!("import"), literal!("inout"), literal!("interface"), literal!("invariant"), literal!("
int"), literal!("in"), literal!("ireal"), literal!("is"), literal!("lazy"), literal!("long"), literal!("macro"), literal!("mixin"), literal!("module"), literal!("new"), literal!("nothrow"), literal!("null"), literal!("out"), literal!("override"), literal!("package"), literal!("pragma"), literal!("private"), literal!("protected"), literal!("public"), literal!("pure"), literal!("real"), literal!("ref"), literal!("return"), literal!("scope"), literal!("shared"), literal!("short"), literal!("static"), literal!("struct"), literal!("super"), literal!("switch"), literal!("synchronized"), literal!("template"), literal!("this"), literal!("throw"), literal!("true"), literal!("try"), literal!("typedef"), literal!("typeid"), literal!("typeof"), literal!("ubyte"), literal!("ucent"), literal!("uint"), literal!("ulong"), literal!("union"), literal!("unittest"), literal!("ushort"), literal!("version"), literal!("void"), literal!("volatile"), literal!("wchar"), literal!("while"), literal!("with"), literal!("__FILE__"), 
literal!("__LINE__"), literal!("__gshared"), literal!("__thread"), literal!("__traits")), name ~ ".Keyword")(TParseTree("", false,[], s));
    }

    static TParseTree Comment(TParseTree p)
    {
        return named!(or!(BlockComment, LineComment, NestingBlockComment), name ~ ".Comment")(p);
    }

    static TParseTree Comment(string s)
    {
        return named!(or!(BlockComment, LineComment, NestingBlockComment), name ~ ".Comment")(TParseTree("", false,[], s));
    }

    static TParseTree BlockComment(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("/ *"), zeroOrMore!(spaceAnd!(Spacing, negLookahead!(literal!("* /")), pegged.peg.any)), literal!("* /")), name ~ ".BlockComment")(p);
    }

    static TParseTree BlockComment(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("/ *"), zeroOrMore!(spaceAnd!(Spacing, negLookahead!(literal!("* /")), pegged.peg.any)), literal!("* /")), name ~ ".BlockComment")(TParseTree("", false,[], s));
    }

    static TParseTree LineComment(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("//"), zeroOrMore!(spaceAnd!(Spacing, negLookahead!(endOfLine), pegged.peg.any)), endOfLine), name ~ ".LineComment")(p);
    }

    static TParseTree LineComment(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("//"), zeroOrMore!(spaceAnd!(Spacing, negLookahead!(endOfLine), pegged.peg.any)), endOfLine), name ~ ".LineComment")(TParseTree("", false,[], s));
    }

    static TParseTree NestingBlockComment(TParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("/ +"), or!(NestingBlockComment, Text), literal!("+ /")), name ~ ".NestingBlockComment")(p);
    }

    static TParseTree NestingBlockComment(string s)
    {
        return named!(spaceAnd!(Spacing, literal!("/ +"), or!(NestingBlockComment, Text), literal!("+ /")), name ~ ".NestingBlockComment")(TParseTree("", false,[], s));
    }

    static TParseTree Text(TParseTree p)
    {
        return named!(zeroOrMore!(spaceAnd!(Spacing, negLookahead!(literal!("+ /")), pegged.peg.any)), name ~ ".Text")(p);
    }

    static TParseTree Text(string s)
    {
        return named!(zeroOrMore!(spaceAnd!(Spacing, negLookahead!(literal!("+ /")), pegged.peg.any)), name ~ ".Text")(TParseTree("", false,[], s));
    }

    static TParseTree StringLiteral(TParseTree p)
    {
        return named!(or!(WysiwygString, AlternateWysiwygString, doublequotedString, TokenString), name ~ ".StringLiteral")(p);
    }

    static TParseTree StringLiteral(string s)
    {
        return named!(or!(WysiwygString, AlternateWysiwygString, doublequotedString, TokenString), name ~ ".StringLiteral")(TParseTree("", false,[], s));
    }

    static TParseTree WysiwygString(TParseTree p)
    {
        return named!(and!(literal!("r"), doublequote, zeroOrMore!(and!(negLookahead!(doublequote), pegged.peg.any)), doublequote, option!(StringPostfix)), name ~ ".WysiwygString")(p);
    }

    static TParseTree WysiwygString(string s)
    {
        return named!(and!(literal!("r"), doublequote, zeroOrMore!(and!(negLookahead!(doublequote), pegged.peg.any)), doublequote, option!(StringPostfix)), name ~ ".WysiwygString")(TParseTree("", false,[], s));
    }

    static TParseTree AlternateWysiwygString(TParseTree p)
    {
        return named!(and!(backquote, zeroOrMore!(and!(negLookahead!(backquote), pegged.peg.any)), backquote, option!(StringPostfix)), name ~ ".AlternateWysiwygString")(p);
    }

    static TParseTree AlternateWysiwygString(string s)
    {
        return named!(and!(backquote, zeroOrMore!(and!(negLookahead!(backquote), pegged.peg.any)), backquote, option!(StringPostfix)), name ~ ".AlternateWysiwygString")(TParseTree("", false,[], s));
    }

    static TParseTree doublequotedString(TParseTree p)
    {
        return named!(and!(doublequote, zeroOrMore!(DQChar), doublequote, option!(StringPostfix)), name ~ ".doublequotedString")(p);
    }

    static TParseTree doublequotedString(string s)
    {
        return named!(and!(doublequote, zeroOrMore!(DQChar), doublequote, option!(StringPostfix)), name ~ ".doublequotedString")(TParseTree("", false,[], s));
    }

    static TParseTree DQChar(TParseTree p)
    {
        return named!(or!(EscapeSequence, and!(negLookahead!(doublequote), pegged.peg.any)), name ~ ".DQChar")(p);
    }

    static TParseTree DQChar(string s)
    {
        return named!(or!(EscapeSequence, and!(negLookahead!(doublequote), pegged.peg.any)), name ~ ".DQChar")(TParseTree("", false,[], s));
    }

    static TParseTree EscapeSequence(TParseTree p)
    {
        return named!(and!(backslash, or!(quote, doublequote, backslash, or!(literal!("a"), literal!("b"), literal!("f"), literal!("n"), literal!("r"), literal!("t"), literal!("v")), and!(literal!("x"), HexDigit, HexDigit), and!(literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), and!(literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit))), name ~ ".EscapeSequence")(p);
    }

    static TParseTree EscapeSequence(string s)
    {
        return named!(and!(backslash, or!(quote, doublequote, backslash, or!(literal!("a"), literal!("b"), literal!("f"), literal!("n"), literal!("r"), literal!("t"), literal!("v")), and!(literal!("x"), HexDigit, HexDigit), and!(literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), and!(literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit))), name ~ ".EscapeSequence")(TParseTree("", false,[], s));
    }

    static TParseTree StringPostfix(TParseTree p)
    {
        return named!(or!(literal!("c"), literal!("w"), literal!("d")), name ~ ".StringPostfix")(p);
    }

    static TParseTree StringPostfix(string s)
    {
        return named!(or!(literal!("c"), literal!("w"), literal!("d")), name ~ ".StringPostfix")(TParseTree("", false,[], s));
    }

    static TParseTree TokenString(TParseTree p)
    {
        return named!(and!(literal!("q{"), zeroOrMore!(and!(negLookahead!(literal!("}")), pegged.peg.any)), literal!("}")), name ~ ".TokenString")(p);
    }

    static TParseTree TokenString(string s)
    {
        return named!(and!(literal!("q{"), zeroOrMore!(and!(negLookahead!(literal!("}")), pegged.peg.any)), literal!("}")), name ~ ".TokenString")(TParseTree("", false,[], s));
    }

    static TParseTree CharacterLiteral(TParseTree p)
    {
        return named!(and!(quote, negLookahead!(quote), or!(EscapeSequence, pegged.peg.any), quote), name ~ ".CharacterLiteral")(p);
    }

    static TParseTree CharacterLiteral(string s)
    {
        return named!(and!(quote, negLookahead!(quote), or!(EscapeSequence, pegged.peg.any), quote), name ~ ".CharacterLiteral")(TParseTree("", false,[], s));
    }

    static TParseTree IntegerLiteral(TParseTree p)
    {
        return named!(or!(DecimalInteger, BinaryInteger, HexadecimalInteger), name ~ ".IntegerLiteral")(p);
    }

    static TParseTree IntegerLiteral(string s)
    {
        return named!(or!(DecimalInteger, BinaryInteger, HexadecimalInteger), name ~ ".IntegerLiteral")(TParseTree("", false,[], s));
    }

    static TParseTree DecimalInteger(TParseTree p)
    {
        return named!(and!(Integer, option!(IntegerSuffix)), name ~ ".DecimalInteger")(p);
    }

    static TParseTree DecimalInteger(string s)
    {
        return named!(and!(Integer, option!(IntegerSuffix)), name ~ ".DecimalInteger")(TParseTree("", false,[], s));
    }

    static TParseTree Integer(TParseTree p)
    {
        return named!(and!(digit, zeroOrMore!(or!(digit, literal!("_")))), name ~ ".Integer")(p);
    }

    static TParseTree Integer(string s)
    {
        return named!(and!(digit, zeroOrMore!(or!(digit, literal!("_")))), name ~ ".Integer")(TParseTree("", false,[], s));
    }

    static TParseTree IntegerSuffix(TParseTree p)
    {
        return named!(or!(literal!("Lu"), literal!("LU"), literal!("uL"), literal!("UL"), literal!("L"), literal!("u"), literal!("U")), name ~ ".IntegerSuffix")(p);
    }

    static TParseTree IntegerSuffix(string s)
    {
        return named!(or!(literal!("Lu"), literal!("LU"), literal!("uL"), literal!("UL"), literal!("L"), literal!("u"), literal!("U")), name ~ ".IntegerSuffix")(TParseTree("", false,[], s));
    }

    static TParseTree BinaryInteger(TParseTree p)
    {
        return named!(and!(or!(literal!("0b"), literal!("0B")), or!(literal!("0"), literal!("1")), zeroOrMore!(or!(or!(literal!("0"), literal!("1")), literal!("_")))), name ~ ".BinaryInteger")(p);
    }

    static TParseTree BinaryInteger(string s)
    {
        return named!(and!(or!(literal!("0b"), literal!("0B")), or!(literal!("0"), literal!("1")), zeroOrMore!(or!(or!(literal!("0"), literal!("1")), literal!("_")))), name ~ ".BinaryInteger")(TParseTree("", false,[], s));
    }

    static TParseTree HexadecimalInteger(TParseTree p)
    {
        return named!(and!(or!(literal!("0x"), literal!("0X")), HexDigit, zeroOrMore!(or!(HexDigit, literal!("_")))), name ~ ".HexadecimalInteger")(p);
    }

    static TParseTree HexadecimalInteger(string s)
    {
        return named!(and!(or!(literal!("0x"), literal!("0X")), HexDigit, zeroOrMore!(or!(HexDigit, literal!("_")))), name ~ ".HexadecimalInteger")(TParseTree("", false,[], s));
    }

    static TParseTree HexDigit(TParseTree p)
    {
        return named!(or!(charRange!('0', '9'), charRange!('a', 'f'), charRange!('A', 'F')), name ~ ".HexDigit")(p);
    }

    static TParseTree HexDigit(string s)
    {
        return named!(or!(charRange!('0', '9'), charRange!('a', 'f'), charRange!('A', 'F')), name ~ ".HexDigit")(TParseTree("", false,[], s));
    }

    static TParseTree FloatLiteral(TParseTree p)
    {
        return named!(and!(option!(Sign), Integer, literal!("."), option!(Integer), option!(and!(or!(literal!("e"), literal!("E")), option!(Sign), Integer))), name ~ ".FloatLiteral")(p);
    }

    static TParseTree FloatLiteral(string s)
    {
        return named!(and!(option!(Sign), Integer, literal!("."), option!(Integer), option!(and!(or!(literal!("e"), literal!("E")), option!(Sign), Integer))), name ~ ".FloatLiteral")(TParseTree("", false,[], s));
    }

    static TParseTree Sign(TParseTree p)
    {
        return named!(option!(or!(literal!("-"), literal!("+"))), name ~ ".Sign")(p);
    }

    static TParseTree Sign(string s)
    {
        return named!(option!(or!(literal!("-"), literal!("+"))), name ~ ".Sign")(TParseTree("", false,[], s));
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Module(p));
        result.children = [result];
        result.name = "D";
        return result;
    }

    static TParseTree opCall(string input)
    {
        return D(TParseTree(``, false, [], input, 0, 0));
    }
    }
}

alias GenericD!(ParseTree).D D;

