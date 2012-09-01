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
List(Elem) < Elem (',' Elem)*

ImportBindings < Import ":" List(ImportBind) 

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

Declarators < DeclaratorInitializer ("," List(DeclaratorIdentifier))?

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
        / "in"
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

AutoDeclarationX < List(Identifier "=" Initializer)

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
                    "(" List(ForeachType) ";" Aggregate ")"
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

AssocArrayLiteral < "[" List(KeyValuePair) "]"

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
BaseClassList < ":" List(Identifier)

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

NewAnonClassExpression < "new" AllocatorArguments? "class" ClassArguments? Identifier List(Identifier)? ClassBody 

ClassArguments < "(" ArgumentList? ")"

### enum.html

EnumDeclaration < "enum" EnumTag? (":" EnumBaseType)? EnumBody

EnumTag < Identifier

EnumBaseType < Type

EnumBody < ";" / "{" List(EnumMember) "}"

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
                / ("db" / "ds" / "di" / "dl" / "df" / "dd" / "de") List(Operand)
                / Identifier ":" AsmInstruction
                / OpCode
                / OpCode List(Operand)
                
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
                      
BaseInterfaceList < ":" List(Identifier)

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

TemplateParameterList < List(TemplateParameter)

TemplateParameter < TemplateTypeParameter
                   / TemplateValueParameter
                   / TemplateAliasParameter
                   / TemplateTupleParameter
                   / TemplateThisParameter
                   
TemplateInstance < TemplateIdentifier ( "!(" List(TemplateArgument) ")"
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

TemplateMixin < "mixin" TemplateIdentifier (("!(" List(TemplateArgument) ")")? MixinIdentifier?) ";"

MixinIdentifier < Identifier

### traits.html

TraitsExpression < "__traits" "(" TraitsKeyword "," List(TraitsArgument) ")"

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

import pegged.peg;
struct D
{
    enum names = [`Module`:true, `DeclDefs`:true, `DeclDef`:true, `ModuleDeclaration`:true, `ImportDeclaration`:true, `ImportList`:true, `Import`:true,
    `List`:true, `ImportBindings`:true, `ImportBind`:true, `MixinDeclaration`:true, `Declaration`:true, `AliasDeclaration`:true, `AliasThisDeclaration`:true,
    `Decl`:true, `Declarators`:true, `DeclaratorInitializer`:true, `DeclaratorIdentifier`:true, `BasicType`:true, `BasicTypeX`:true, `BasicType2`:true,
    `Declarator`:true, `DeclaratorSuffixes`:true, `DeclaratorSuffix`:true, `IdentifierList`:true, `StorageClasses`:true, `StorageClass`:true, `Property`:true,
    `Type`:true, `Declarator2`:true, `Parameters`:true, `ParameterList`:true, `Parameter`:true, `InOut`:true, `InOutX`:true, `FunctionAttributes`:true,
    `FunctionAttribute`:true, `MemberFunctionAttributes`:true, `MemberFunctionAttribute`:true, `DefaultInitializerExpression`:true, `Initializer`:true,
    `NonVoidInitializer`:true, `ArrayInitializer`:true, `ArrayMemberInitializations`:true, `ArrayMemberInitialization`:true, `StructInitializer`:true,
    `StructMemberInitializers`:true, `StructMemberInitializer`:true, `AutoDeclaration`:true, `AutoDeclarationX`:true, `Typeof`:true, `VoidInitializer`:true,
    `Statement`:true, `NoScopeNonEmptyStatement`:true, `NoScopeStatement`:true, `NonEmptyOrScopeBlockStatement`:true, `NonEmptyStatement`:true,
    `NonEmptyStatementNoCaseNoDefault`:true, `ScopeStatement`:true, `ScopeBlockStatement`:true, `LabeledStatement`:true, `BlockStatement`:true,
    `StatementList`:true, `ExpressionStatement`:true, `DeclarationStatement`:true, `IfStatement`:true, `IfCondition`:true, `ThenStatement`:true,
    `ElseStatement`:true, `WhileStatement`:true, `DoStatement`:true, `ForStatement`:true, `Initialize`:true, `Test`:true, `Increment`:true,
    `ForeachStatement`:true, `ForeachType`:true, `Aggregate`:true, `ForeachRangeStatement`:true, `SwitchStatement`:true, `CaseStatement`:true,
    `CaseRangeStatement`:true, `DefaultStatement`:true, `ScopeStatementList`:true, `StatementListNoCaseNoDefault`:true, 
    `StatementNoCaseNoDefault`:true, `FinalSwitchStatement`:true, `ContinueStatement`:true, `BreakStatement`:true, `ReturnStatement`:true, 
    `GotoStatement`:true, `WithStatement`:true, `SynchronizedStatement`:true, `TryStatement`:true, `Catches`:true, `LastCatch`:true, `Catch`:true,
    `CatchParameter`:true, `FinallyStatement`:true, `ThrowStatement`:true, `ScopeGuardStatement`:true, `AsmStatement`:true, `AsmInstructionList`:true,
    `PragmaStatement`:true, `MixinStatement`:true, `Expression`:true, `AssignExpression`:true, `Op`:true, `ConditionalExpression`:true, `OrOrExpression`:true,
    `AndAndExpression`:true, `OrExpression`:true, `XorExpression`:true, `AndExpression`:true, `CmpExpression`:true, `EqualExpression`:true,
    `IdentityExpression`:true, `RelExpression`:true, `RelOp`:true, `InExpression`:true, `ShiftExpression`:true, `AddExpression`:true, `CatExpression`:true,
    `MulExpression`:true, `UnaryExpression`:true, `UnaryOp`:true, `ComplementExpression`:true, `NewExpression`:true, `AllocatorArguments`:true,
    `ArgumentList`:true, `DeleteExpression`:true, `CastExpression`:true, `CastEqual`:true, `PowExpression`:true, `PostfixExpression`:true, 
    `IndexExpression`:true, `SliceExpression`:true, `PrimaryExpression`:true, `StringLiterals`:true, `ArrayLiteral`:true, `AssocArrayLiteral`:true,
    `KeyValuePair`:true, `Lambda`:true, `FunctionLiteral`:true, `ParameterAttributes`:true, `AssertExpression`:true, `MixinExpression`:true,
    `ImportExpression`:true, `TypeidExpression`:true, `IsExpression`:true, `TypeSpecialization`:true, `AttributeSpecifier`:true, `Attribute`:true,
    `DeclarationBlock`:true, `LinkageAttribute`:true, `LinkageType`:true, `AlignAttribute`:true, `ProtectionAttribute`:true, `ClassDeclaration`:true,
    `BaseClassList`:true, `ClassBody`:true, `ClassBodyDeclarations`:true, `ClassBodyDeclaration`:true, `Constructor`:true, `Destructor`:true,
    `StaticConstructor`:true, `StaticDestructor`:true, `SharedStaticConstructor`:true, `SharedStaticDestructor`:true, `Invariant`:true, `ClassAllocator`:true,
    `ClassDeallocator`:true, `AliasThis`:true, `NewAnonClassExpression`:true, `ClassArguments`:true, `EnumDeclaration`:true, `EnumTag`:true, 
    `EnumBaseType`:true, `EnumBody`:true, `EnumMember`:true, `FunctionBody`:true, `InStatement`:true, `OutStatement`:true, `BodyStatement`:true,
    `AsmInstruction`:true, `IntegerExpression`:true, `Operand`:true, `AsmExp`:true, `AsmLogOrExp`:true, `AsmLogAndExp`:true, `AsmOrExp`:true, `AsmXorExp`:true,
    `AsmAndExp`:true, `AsmEqualExp`:true, `AsmRelExp`:true, `AsmShiftExp`:true, `AsmAddExp`:true, `AsmMulExp`:true, `AsmBrExp`:true, `AsmUnaExp`:true,
    `AsmPrimaryExp`:true, `DotIdentifier`:true, `AsmTypePrefix`:true, `Register`:true, `OpCode`:true, `InterfaceDeclaration`:true, `BaseInterfaceList`:true,
    `InterfaceBody`:true, `Pragma`:true, `AggregateDeclaration`:true, `StructBody`:true, `StructBodyDeclarations`:true, `StructBodyDeclaration`:true,
    `StructAllocator`:true, `StructDeallocator`:true, `StructPostblit`:true, `TemplateDeclaration`:true, `TemplateIdentifier`:true, `TemplateParameterList`:true,
    `TemplateParameter`:true, `TemplateInstance`:true, `TemplateArgument`:true, `Symbol`:true, `SymbolTail`:true, `TemplateSingleArgument`:true,
    `TemplateTypeParameter`:true, `TTPSpecialization`:true, `TTPDefault`:true, `TemplateThisParameter`:true, `TemplateValueParameter`:true,
    `TVPSpecialization`:true, `TVPDefault`:true, `TemplateAliasParameter`:true, `TAPSpecialization`:true, `TAPDefault`:true, `TemplateTupleParameter`:true,
    `TemplatedConstructor`:true, `ClassTemplateDeclaration`:true, `StructTemplateDeclaration`:true, `UnionTemplateDeclaration`:true,
    `InterfaceTemplateDeclaration`:true, `Constraint`:true, `TemplateMixinDeclaration`:true, `TemplateMixin`:true, `MixinIdentifier`:true,
    `TraitsExpression`:true, `TraitsKeyword`:true, `TraitsArgument`:true, `UnitTest`:true, `ConditionalDeclaration`:true, `CCDeclarationBlock`:true,
    `Declarations`:true, `ConditionalStatement`:true, `Condition`:true, `VersionCondition`:true, `VersionSpecification`:true, `DebugCondition`:true,
    `DebugSpecification`:true, `StaticIfCondition`:true, `StaticAssert`:true, `Identifier`:true, `Keyword`:true, `Comment`:true, `BlockComment`:true,
    `LineComment`:true, `NestingBlockComment`:true, `Text`:true, `StringLiteral`:true, `WysiwygString`:true, `AlternateWysiwygString`:true,
    `doublequotedString`:true, `DQChar`:true, `EscapeSequence`:true, `StringPostfix`:true, `TokenString`:true, `CharacterLiteral`:true, `IntegerLiteral`:true,
    `DecimalInteger`:true, `Integer`:true, `IntegerSuffix`:true, `BinaryInteger`:true, `HexadecimalInteger`:true, `HexDigit`:true, `FloatLiteral`:true,
    `Sign`:true];

    mixin decimateTree;

    alias spacing Spacing;

    static ParseTree Module(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Spacing, option!(ModuleDeclaration), option!(DeclDefs)), "Module")(p);
    }

    static ParseTree DeclDefs(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, oneOrMore!(DeclDef)), "DeclDefs")(p);
    }

    static ParseTree DeclDef(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, AttributeSpecifier), spaceAnd!(Spacing, ImportDeclaration), spaceAnd!(Spacing, EnumDeclaration), spaceAnd!(Spacing, ClassDeclaration), spaceAnd!(Spacing, InterfaceDeclaration), spaceAnd!(Spacing, AggregateDeclaration), spaceAnd!(Spacing, Declaration), spaceAnd!(Spacing, Constructor), spaceAnd!(Spacing, Destructor), spaceAnd!(Spacing, UnitTest), spaceAnd!(Spacing, StaticConstructor), spaceAnd!(Spacing, StaticDestructor), spaceAnd!(Spacing, SharedStaticConstructor), spaceAnd!(Spacing, SharedStaticDestructor), spaceAnd!(Spacing, ConditionalDeclaration), spaceAnd!(Spacing, DebugSpecification), spaceAnd!(Spacing, VersionSpecification), spaceAnd!(Spacing, StaticAssert), spaceAnd!(Spacing, TemplateDeclaration), spaceAnd!(Spacing, TemplateMixinDeclaration), spaceAnd!(Spacing, TemplateMixin), spaceAnd!(Spacing, MixinDeclaration)), "DeclDef")(p);
    }

    static ParseTree ModuleDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("module"), qualifiedIdentifier, literal!(";")), "ModuleDeclaration")(p);
    }

    static ParseTree ImportDeclaration(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("import"), ImportList, literal!(";")), spaceAnd!(Spacing, literal!("static"), literal!("import"), ImportList, literal!(";"))), "ImportDeclaration")(p);
    }

    static ParseTree ImportList(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, ImportBindings), spaceAnd!(Spacing, Import, option!(spaceAnd!(Spacing, literal!(","), ImportList)))), "ImportList")(p);
    }

    static ParseTree Import(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, qualifiedIdentifier, literal!("="), qualifiedIdentifier), spaceAnd!(Spacing, qualifiedIdentifier)), "Import")(p);
    }

    static ParseTree List(alias Elem)(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Elem, zeroOrMore!(spaceAnd!(Spacing, literal!(","), Elem))), "List")(p);
    }

    static ParseTree ImportBindings(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Import, literal!(":"), List!(spaceAnd!(Spacing, ImportBind))), "ImportBindings")(p);
    }

    static ParseTree ImportBind(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("="), Identifier))), "ImportBind")(p);
    }

    static ParseTree MixinDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("("), AssignExpression, literal!(")"), literal!(";")), "MixinDeclaration")(p);
    }

    static ParseTree Declaration(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, AliasDeclaration), spaceAnd!(Spacing, AliasThisDeclaration), spaceAnd!(Spacing, Decl)), "Declaration")(p);
    }

    static ParseTree AliasDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), BasicType, Declarator), "AliasDeclaration")(p);
    }

    static ParseTree AliasThisDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), Identifier, literal!("this")), "AliasThisDeclaration")(p);
    }

    static ParseTree Decl(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, BasicType, Declarators, literal!(";")), spaceAnd!(Spacing, BasicType, Declarator, FunctionBody), spaceAnd!(Spacing, AutoDeclaration), spaceAnd!(Spacing, StorageClasses, Decl)), "Decl")(p);
    }

    static ParseTree Declarators(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, DeclaratorInitializer, option!(spaceAnd!(Spacing, literal!(","), List!(spaceAnd!(Spacing, DeclaratorIdentifier))))), "Declarators")(p);
    }

    static ParseTree DeclaratorInitializer(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Declarator, option!(spaceAnd!(Spacing, literal!("="), Initializer))), "DeclaratorInitializer")(p);
    }

    static ParseTree DeclaratorIdentifier(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("="), Initializer))), "DeclaratorIdentifier")(p);
    }

    static ParseTree BasicType(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, BasicTypeX), spaceAnd!(Spacing, literal!("."), IdentifierList), spaceAnd!(Spacing, IdentifierList), spaceAnd!(Spacing, Typeof, literal!("."), IdentifierList), spaceAnd!(Spacing, literal!("const("), Type, literal!(")")), spaceAnd!(Spacing, literal!("immutable("), Type, literal!(")")), spaceAnd!(Spacing, literal!("shared("), Type, literal!(")")), spaceAnd!(Spacing, literal!("inout("), Type, literal!(")"))), "BasicType")(p);
    }

    static ParseTree BasicTypeX(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("bool")), spaceAnd!(Spacing, literal!("byte")), spaceAnd!(Spacing, literal!("ubyte")), spaceAnd!(Spacing, literal!("short")), spaceAnd!(Spacing, literal!("ushort")), spaceAnd!(Spacing, literal!("int")), spaceAnd!(Spacing, literal!("uint")), spaceAnd!(Spacing, literal!("long")), spaceAnd!(Spacing, literal!("ulong")), spaceAnd!(Spacing, literal!("char")), spaceAnd!(Spacing, literal!("wchar")), spaceAnd!(Spacing, literal!("dchar")), spaceAnd!(Spacing, literal!("float")), spaceAnd!(Spacing, literal!("double")), spaceAnd!(Spacing, literal!("real")), spaceAnd!(Spacing, literal!("void"))), "BasicTypeX")(p);
    }

    static ParseTree BasicType2(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("*")), spaceAnd!(Spacing, literal!("["), literal!("]")), spaceAnd!(Spacing, literal!("["), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("["), AssignExpression, literal!(".."), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("["), Type, literal!("]")), spaceAnd!(Spacing, literal!("delegate"), Parameters, option!(FunctionAttributes)), spaceAnd!(Spacing, literal!("function"), Parameters, option!(FunctionAttributes))), "BasicType2")(p);
    }

    static ParseTree Declarator(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, zeroOrMore!(BasicType2), literal!("("), Declarator, literal!(")"), option!(DeclaratorSuffixes)), spaceAnd!(Spacing, zeroOrMore!(BasicType2), Identifier, option!(DeclaratorSuffixes))), "Declarator")(p);
    }

    static ParseTree DeclaratorSuffixes(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, oneOrMore!(DeclaratorSuffix)), "DeclaratorSuffixes")(p);
    }

    static ParseTree DeclaratorSuffix(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("["), literal!("]")), spaceAnd!(Spacing, literal!("["), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("["), Type, literal!("]")), spaceAnd!(Spacing, option!(TemplateParameterList), Parameters, option!(MemberFunctionAttributes), option!(Constraint))), "DeclaratorSuffix")(p);
    }

    static ParseTree IdentifierList(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, TemplateInstance, option!(spaceAnd!(Spacing, literal!("."), IdentifierList))), spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("."), IdentifierList)))), "IdentifierList")(p);
    }

    static ParseTree StorageClasses(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, oneOrMore!(StorageClass)), "StorageClasses")(p);
    }

    static ParseTree StorageClass(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("abstract")), spaceAnd!(Spacing, literal!("auto")), spaceAnd!(Spacing, literal!("const")), spaceAnd!(Spacing, literal!("deprecated")), spaceAnd!(Spacing, literal!("enum")), spaceAnd!(Spacing, literal!("extern")), spaceAnd!(Spacing, literal!("final")), spaceAnd!(Spacing, literal!("immutable")), spaceAnd!(Spacing, literal!("inout")), spaceAnd!(Spacing, literal!("shared")), spaceAnd!(Spacing, literal!("nothrow")), spaceAnd!(Spacing, literal!("override")), spaceAnd!(Spacing, literal!("pure")), spaceAnd!(Spacing, literal!("__gshared")), spaceAnd!(Spacing, Property), spaceAnd!(Spacing, literal!("scope")), spaceAnd!(Spacing, literal!("static")), spaceAnd!(Spacing, literal!("synchronized"))), "StorageClass")(p);
    }

    static ParseTree Property(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("@"), or!(spaceAnd!(Spacing, literal!("property")), spaceAnd!(Spacing, literal!("safe")), spaceAnd!(Spacing, literal!("trusted")), spaceAnd!(Spacing, literal!("system")), spaceAnd!(Spacing, literal!("disable")))), "Property")(p);
    }

    static ParseTree Type(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, BasicType, option!(Declarator2)), "Type")(p);
    }

    static ParseTree Declarator2(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, zeroOrMore!(BasicType2), option!(spaceAnd!(Spacing, literal!("("), Declarator2, literal!(")"))), option!(DeclaratorSuffixes)), "Declarator2")(p);
    }

    static ParseTree Parameters(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("("), option!(ParameterList), literal!(")")), "Parameters")(p);
    }

    static ParseTree ParameterList(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("...")), spaceAnd!(Spacing, Parameter, zeroOrMore!(spaceAnd!(Spacing, discard!(literal!(",")), Parameter)))), "ParameterList")(p);
    }

    static ParseTree Parameter(ParseTree p)
    {
        return named!(or!(and!(option!(InOut), oneOrMore!(space), BasicType, Declarator, option!(or!(and!(literal!("...")), and!(literal!("="), DefaultInitializerExpression)))), and!(option!(InOut), Type, option!(literal!("...")))), "Parameter")(p);
    }

    static ParseTree InOut(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, InOutX, option!(InOut)), "InOut")(p);
    }

    static ParseTree InOutX(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("auto")), spaceAnd!(Spacing, literal!("const")), spaceAnd!(Spacing, literal!("final")), spaceAnd!(Spacing, literal!("immutable")), spaceAnd!(Spacing, literal!("inout")), spaceAnd!(Spacing, literal!("in")), spaceAnd!(Spacing, literal!("lazy")), spaceAnd!(Spacing, literal!("out")), spaceAnd!(Spacing, literal!("ref")), spaceAnd!(Spacing, literal!("scope")), spaceAnd!(Spacing, literal!("shared"))), "InOutX")(p);
    }

    static ParseTree FunctionAttributes(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, oneOrMore!(FunctionAttribute)), "FunctionAttributes")(p);
    }

    static ParseTree FunctionAttribute(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("nothrow")), spaceAnd!(Spacing, literal!("pure")), spaceAnd!(Spacing, Property)), "FunctionAttribute")(p);
    }

    static ParseTree MemberFunctionAttributes(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, oneOrMore!(MemberFunctionAttribute)), "MemberFunctionAttributes")(p);
    }

    static ParseTree MemberFunctionAttribute(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("const")), spaceAnd!(Spacing, literal!("immutable")), spaceAnd!(Spacing, literal!("inout")), spaceAnd!(Spacing, literal!("shared")), spaceAnd!(Spacing, FunctionAttribute)), "MemberFunctionAttribute")(p);
    }

    static ParseTree DefaultInitializerExpression(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, AssignExpression), spaceAnd!(Spacing, literal!("__FILE__")), spaceAnd!(Spacing, literal!("__LINE__"))), "DefaultInitializerExpression")(p);
    }

    static ParseTree Initializer(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, VoidInitializer), spaceAnd!(Spacing, NonVoidInitializer)), "Initializer")(p);
    }

    static ParseTree NonVoidInitializer(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, AssignExpression), spaceAnd!(Spacing, ArrayInitializer), spaceAnd!(Spacing, StructInitializer)), "NonVoidInitializer")(p);
    }

    static ParseTree ArrayInitializer(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("["), literal!("]")), spaceAnd!(Spacing, literal!("["), ArrayMemberInitializations, literal!("]"))), "ArrayInitializer")(p);
    }

    static ParseTree ArrayMemberInitializations(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ArrayMemberInitialization, zeroOrMore!(spaceAnd!(Spacing, literal!(","), option!(ArrayMemberInitialization)))), "ArrayMemberInitializations")(p);
    }

    static ParseTree ArrayMemberInitialization(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, NonVoidInitializer), spaceAnd!(Spacing, AssignExpression, literal!(":"), NonVoidInitializer)), "ArrayMemberInitialization")(p);
    }

    static ParseTree StructInitializer(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("{"), literal!("}")), spaceAnd!(Spacing, literal!("{"), StructMemberInitializers, literal!("}"))), "StructInitializer")(p);
    }

    static ParseTree StructMemberInitializers(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, StructMemberInitializer, zeroOrMore!(spaceAnd!(Spacing, literal!(","), option!(StructMemberInitializer)))), "StructMemberInitializers")(p);
    }

    static ParseTree StructMemberInitializer(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, NonVoidInitializer), spaceAnd!(Spacing, Identifier, discard!(NonVoidInitializer))), "StructMemberInitializer")(p);
    }

    static ParseTree AutoDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, StorageClasses, AutoDeclarationX, literal!(";")), "AutoDeclaration")(p);
    }

    static ParseTree AutoDeclarationX(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, List!(spaceAnd!(Spacing, Identifier, literal!("="), Initializer))), "AutoDeclarationX")(p);
    }

    static ParseTree Typeof(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("typeof"), literal!("("), Expression, literal!(")")), spaceAnd!(Spacing, literal!("typeof"), literal!("("), literal!("return"), literal!(")"))), "Typeof")(p);
    }

    static ParseTree VoidInitializer(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("void")), "VoidInitializer")(p);
    }

    static ParseTree Statement(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!(";")), spaceAnd!(Spacing, NonEmptyStatement), spaceAnd!(Spacing, ScopeBlockStatement)), "Statement")(p);
    }

    static ParseTree NoScopeNonEmptyStatement(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, NonEmptyStatement), spaceAnd!(Spacing, BlockStatement)), "NoScopeNonEmptyStatement")(p);
    }

    static ParseTree NoScopeStatement(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!(";")), spaceAnd!(Spacing, NonEmptyStatement), spaceAnd!(Spacing, BlockStatement)), "NoScopeStatement")(p);
    }

    static ParseTree NonEmptyOrScopeBlockStatement(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, NonEmptyStatement), spaceAnd!(Spacing, ScopeBlockStatement)), "NonEmptyOrScopeBlockStatement")(p);
    }

    static ParseTree NonEmptyStatement(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, NonEmptyStatementNoCaseNoDefault), spaceAnd!(Spacing, CaseStatement), spaceAnd!(Spacing, CaseRangeStatement), spaceAnd!(Spacing, DefaultStatement)), "NonEmptyStatement")(p);
    }

    static ParseTree NonEmptyStatementNoCaseNoDefault(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, LabeledStatement), spaceAnd!(Spacing, ExpressionStatement), spaceAnd!(Spacing, DeclarationStatement), spaceAnd!(Spacing, IfStatement), spaceAnd!(Spacing, WhileStatement), spaceAnd!(Spacing, DoStatement), spaceAnd!(Spacing, ForStatement), spaceAnd!(Spacing, ForeachStatement), spaceAnd!(Spacing, SwitchStatement), spaceAnd!(Spacing, FinalSwitchStatement), spaceAnd!(Spacing, ContinueStatement), spaceAnd!(Spacing, BreakStatement), spaceAnd!(Spacing, ReturnStatement), spaceAnd!(Spacing, GotoStatement), spaceAnd!(Spacing, WithStatement), spaceAnd!(Spacing, SynchronizedStatement), spaceAnd!(Spacing, TryStatement), spaceAnd!(Spacing, ScopeGuardStatement), spaceAnd!(Spacing, ThrowStatement), spaceAnd!(Spacing, AsmStatement), spaceAnd!(Spacing, PragmaStatement), spaceAnd!(Spacing, MixinStatement), spaceAnd!(Spacing, ForeachRangeStatement), spaceAnd!(Spacing, ConditionalStatement), spaceAnd!(Spacing, StaticAssert), spaceAnd!(Spacing, TemplateMixin), spaceAnd!(Spacing, 
ImportDeclaration)), "NonEmptyStatementNoCaseNoDefault")(p);
    }

    static ParseTree ScopeStatement(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, NonEmptyStatement), spaceAnd!(Spacing, BlockStatement)), "ScopeStatement")(p);
    }

    static ParseTree ScopeBlockStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ScopeStatement), "ScopeBlockStatement")(p);
    }

    static ParseTree LabeledStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, literal!(":"), NoScopeStatement), "LabeledStatement")(p);
    }

    static ParseTree BlockStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(StatementList), literal!("}")), "BlockStatement")(p);
    }

    static ParseTree StatementList(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, oneOrMore!(Statement)), "StatementList")(p);
    }

    static ParseTree ExpressionStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Expression, literal!(";")), "ExpressionStatement")(p);
    }

    static ParseTree DeclarationStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Declaration), "DeclarationStatement")(p);
    }

    static ParseTree IfStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("if"), literal!("("), IfCondition, literal!(")"), ThenStatement, literal!("else"), ElseStatement), "IfStatement")(p);
    }

    static ParseTree IfCondition(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Expression), spaceAnd!(Spacing, literal!("auto"), Identifier, literal!("="), Expression), spaceAnd!(Spacing, BasicType, Declarator, literal!("="), Expression)), "IfCondition")(p);
    }

    static ParseTree ThenStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ScopeStatement), "ThenStatement")(p);
    }

    static ParseTree ElseStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ScopeStatement), "ElseStatement")(p);
    }

    static ParseTree WhileStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("while"), literal!("("), Expression, literal!(")"), ScopeStatement), "WhileStatement")(p);
    }

    static ParseTree DoStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("do"), ScopeStatement, literal!("while"), literal!("("), Expression, literal!(")"), literal!(";")), "DoStatement")(p);
    }

    static ParseTree ForStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("for"), literal!("("), Initialize, option!(Test), literal!(";"), option!(Increment), literal!(")"), ScopeStatement), "ForStatement")(p);
    }

    static ParseTree Initialize(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!(";")), spaceAnd!(Spacing, NoScopeNonEmptyStatement)), "Initialize")(p);
    }

    static ParseTree Test(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Expression), "Test")(p);
    }

    static ParseTree Increment(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Expression), "Increment")(p);
    }

    static ParseTree ForeachStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("foreach")), spaceAnd!(Spacing, literal!("foreach_reverse"))), literal!("("), List!(spaceAnd!(Spacing, ForeachType)), literal!(";"), Aggregate, literal!(")"), NoScopeNonEmptyStatement), "ForeachStatement")(p);
    }

    static ParseTree ForeachType(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, option!(literal!("ref")), BasicType, Declarator), spaceAnd!(Spacing, option!(literal!("ref")), Identifier)), "ForeachType")(p);
    }

    static ParseTree Aggregate(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Expression), "Aggregate")(p);
    }

    static ParseTree ForeachRangeStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("("), ForeachType, literal!(";"), Expression, literal!(".."), Expression, literal!(")")), "ForeachRangeStatement")(p);
    }

    static ParseTree SwitchStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("switch"), literal!("("), Expression, literal!(")"), ScopeStatement), "SwitchStatement")(p);
    }

    static ParseTree CaseStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("case"), ArgumentList, literal!(":"), ScopeStatementList), "CaseStatement")(p);
    }

    static ParseTree CaseRangeStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("case"), AssignExpression, literal!(":"), literal!(".."), literal!("case"), AssignExpression, literal!(":"), ScopeStatementList), "CaseRangeStatement")(p);
    }

    static ParseTree DefaultStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("default"), literal!(":"), ScopeStatementList), "DefaultStatement")(p);
    }

    static ParseTree ScopeStatementList(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, StatementListNoCaseNoDefault), "ScopeStatementList")(p);
    }

    static ParseTree StatementListNoCaseNoDefault(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, oneOrMore!(StatementNoCaseNoDefault)), "StatementListNoCaseNoDefault")(p);
    }

    static ParseTree StatementNoCaseNoDefault(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!(";")), spaceAnd!(Spacing, NonEmptyStatementNoCaseNoDefault), spaceAnd!(Spacing, ScopeBlockStatement)), "StatementNoCaseNoDefault")(p);
    }

    static ParseTree FinalSwitchStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("final"), literal!("switch"), literal!("("), Expression, literal!(")"), ScopeStatement), "FinalSwitchStatement")(p);
    }

    static ParseTree ContinueStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("continue"), option!(Identifier), literal!(";")), "ContinueStatement")(p);
    }

    static ParseTree BreakStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("break"), option!(Identifier), literal!(";")), "BreakStatement")(p);
    }

    static ParseTree ReturnStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("return"), option!(Expression), literal!(";")), "ReturnStatement")(p);
    }

    static ParseTree GotoStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("goto"), or!(spaceAnd!(Spacing, literal!("default"), literal!(";")), spaceAnd!(Spacing, literal!("case"), literal!(";")), spaceAnd!(Spacing, literal!("case"), Expression, literal!(";")), spaceAnd!(Spacing, Identifier, literal!(";")))), "GotoStatement")(p);
    }

    static ParseTree WithStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("with"), literal!("("), or!(spaceAnd!(Spacing, Expression), spaceAnd!(Spacing, Symbol), spaceAnd!(Spacing, TemplateInstance)), literal!(")"), ScopeStatement), "WithStatement")(p);
    }

    static ParseTree SynchronizedStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("synchronized"), option!(spaceAnd!(Spacing, literal!("("), Expression, literal!(")"))), ScopeStatement), "SynchronizedStatement")(p);
    }

    static ParseTree TryStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("try"), ScopeStatement, option!(Catches), option!(FinallyStatement)), "TryStatement")(p);
    }

    static ParseTree Catches(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, LastCatch), spaceAnd!(Spacing, Catch, option!(Catches))), "Catches")(p);
    }

    static ParseTree LastCatch(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("catch"), NoScopeNonEmptyStatement), "LastCatch")(p);
    }

    static ParseTree Catch(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("catch"), literal!("("), CatchParameter, literal!(")"), NoScopeNonEmptyStatement), "Catch")(p);
    }

    static ParseTree CatchParameter(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, BasicType, Identifier), "CatchParameter")(p);
    }

    static ParseTree FinallyStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("finally"), NoScopeNonEmptyStatement), "FinallyStatement")(p);
    }

    static ParseTree ThrowStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("throw"), Expression, literal!(";")), "ThrowStatement")(p);
    }

    static ParseTree ScopeGuardStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("scope(exit)")), spaceAnd!(Spacing, literal!("scope(success)")), spaceAnd!(Spacing, literal!("scope(failure)"))), NonEmptyOrScopeBlockStatement), "ScopeGuardStatement")(p);
    }

    static ParseTree AsmStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("asm"), literal!("{"), option!(AsmInstructionList), literal!("}")), "AsmStatement")(p);
    }

    static ParseTree AsmInstructionList(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmInstruction, literal!(";"), option!(AsmInstructionList)), "AsmInstructionList")(p);
    }

    static ParseTree PragmaStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Pragma, NoScopeStatement), "PragmaStatement")(p);
    }

    static ParseTree MixinStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("("), AssignExpression, literal!(")"), literal!(";")), "MixinStatement")(p);
    }

    static ParseTree Expression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AssignExpression), "Expression")(p);
    }

    static ParseTree AssignExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ConditionalExpression, option!(spaceAnd!(Spacing, Op, AssignExpression))), "AssignExpression")(p);
    }

    static ParseTree Op(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!(">>>=")), spaceAnd!(Spacing, literal!("^^=")), spaceAnd!(Spacing, literal!(">>=")), spaceAnd!(Spacing, literal!("<<=")), spaceAnd!(Spacing, literal!("~=")), spaceAnd!(Spacing, literal!("+=")), spaceAnd!(Spacing, literal!("-=")), spaceAnd!(Spacing, literal!("*=")), spaceAnd!(Spacing, literal!("^=")), spaceAnd!(Spacing, literal!("|=")), spaceAnd!(Spacing, literal!("&=")), spaceAnd!(Spacing, literal!("/=")), spaceAnd!(Spacing, literal!("="))), "Op")(p);
    }

    static ParseTree ConditionalExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, OrOrExpression, option!(spaceAnd!(Spacing, literal!("?"), Expression, literal!(":"), ConditionalExpression))), "ConditionalExpression")(p);
    }

    static ParseTree OrOrExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AndAndExpression, option!(spaceAnd!(Spacing, literal!("||"), OrOrExpression))), "OrOrExpression")(p);
    }

    static ParseTree AndAndExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, CmpExpression), spaceAnd!(Spacing, OrExpression)), option!(spaceAnd!(Spacing, literal!("&&"), AndAndExpression))), "AndAndExpression")(p);
    }

    static ParseTree OrExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, XorExpression, option!(spaceAnd!(Spacing, literal!("|"), OrExpression))), "OrExpression")(p);
    }

    static ParseTree XorExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AndExpression, option!(spaceAnd!(Spacing, literal!("^"), XorExpression))), "XorExpression")(p);
    }

    static ParseTree AndExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, option!(spaceAnd!(Spacing, literal!("&"), AndExpression))), "AndExpression")(p);
    }

    static ParseTree CmpExpression(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, EqualExpression), spaceAnd!(Spacing, IdentityExpression), spaceAnd!(Spacing, RelExpression), spaceAnd!(Spacing, InExpression), spaceAnd!(Spacing, ShiftExpression)), "CmpExpression")(p);
    }

    static ParseTree EqualExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, or!(spaceAnd!(Spacing, literal!("==")), spaceAnd!(Spacing, literal!("!="))), ShiftExpression), "EqualExpression")(p);
    }

    static ParseTree IdentityExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, or!(spaceAnd!(Spacing, literal!("!is")), spaceAnd!(Spacing, literal!("is"))), ShiftExpression), "IdentityExpression")(p);
    }

    static ParseTree RelExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, RelOp, ShiftExpression), "RelExpression")(p);
    }

    static ParseTree RelOp(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("!<>=")), spaceAnd!(Spacing, literal!("!<>")), spaceAnd!(Spacing, literal!("!<=")), spaceAnd!(Spacing, literal!("!>=")), spaceAnd!(Spacing, literal!("<>=")), spaceAnd!(Spacing, literal!("<=")), spaceAnd!(Spacing, literal!(">=")), spaceAnd!(Spacing, literal!("<>")), spaceAnd!(Spacing, literal!("!>")), spaceAnd!(Spacing, literal!("!<")), spaceAnd!(Spacing, literal!("<")), spaceAnd!(Spacing, literal!(">"))), "RelOp")(p);
    }

    static ParseTree InExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ShiftExpression, option!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("!in")), spaceAnd!(Spacing, literal!("in"))), ShiftExpression))), "InExpression")(p);
    }

    static ParseTree ShiftExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AddExpression, option!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!(">>>")), spaceAnd!(Spacing, literal!(">>")), spaceAnd!(Spacing, literal!("<<"))), AddExpression))), "ShiftExpression")(p);
    }

    static ParseTree AddExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, MulExpression), spaceAnd!(Spacing, CatExpression)), option!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("+")), spaceAnd!(Spacing, literal!("-"))), MulExpression))), "AddExpression")(p);
    }

    static ParseTree CatExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, MulExpression, option!(spaceAnd!(Spacing, literal!("~"), AddExpression))), "CatExpression")(p);
    }

    static ParseTree MulExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, UnaryExpression, option!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("*")), spaceAnd!(Spacing, literal!("/")), spaceAnd!(Spacing, literal!("%"))), UnaryExpression))), "MulExpression")(p);
    }

    static ParseTree UnaryExpression(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, UnaryOp, UnaryExpression), spaceAnd!(Spacing, ComplementExpression), spaceAnd!(Spacing, literal!("("), Type, literal!(")"), literal!("."), Identifier), spaceAnd!(Spacing, NewExpression), spaceAnd!(Spacing, DeleteExpression), spaceAnd!(Spacing, CastExpression), spaceAnd!(Spacing, PowExpression)), "UnaryExpression")(p);
    }

    static ParseTree UnaryOp(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("++")), spaceAnd!(Spacing, literal!("--")), spaceAnd!(Spacing, literal!("+")), spaceAnd!(Spacing, literal!("-")), spaceAnd!(Spacing, literal!("&")), spaceAnd!(Spacing, literal!("*")), spaceAnd!(Spacing, literal!("/")), spaceAnd!(Spacing, literal!("!"))), "UnaryOp")(p);
    }

    static ParseTree ComplementExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("~"), UnaryExpression), "ComplementExpression")(p);
    }

    static ParseTree NewExpression(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, spaceAnd!(Spacing, literal!("new"), option!(AllocatorArguments), Type, option!(or!(spaceAnd!(Spacing, literal!("["), AssignExpression, literal!("]")), spaceAnd!(Spacing, literal!("("), ArgumentList, literal!(")")))))), spaceAnd!(Spacing, NewAnonClassExpression)), "NewExpression")(p);
    }

    static ParseTree AllocatorArguments(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("("), ArgumentList, literal!(")")), "AllocatorArguments")(p);
    }

    static ParseTree ArgumentList(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AssignExpression, zeroOrMore!(spaceAnd!(Spacing, literal!(","), AssignExpression))), "ArgumentList")(p);
    }

    static ParseTree DeleteExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("delete"), UnaryExpression), "DeleteExpression")(p);
    }

    static ParseTree CastExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("cast"), literal!("("), option!(or!(spaceAnd!(Spacing, Type), spaceAnd!(Spacing, CastEqual))), literal!(")"), UnaryExpression), "CastExpression")(p);
    }

    static ParseTree CastEqual(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("const"), literal!("shared")), spaceAnd!(Spacing, literal!("shared"), literal!("const")), spaceAnd!(Spacing, literal!("inout"), literal!("shared")), spaceAnd!(Spacing, literal!("shared"), literal!("inout")), spaceAnd!(Spacing, literal!("const")), spaceAnd!(Spacing, literal!("inout")), spaceAnd!(Spacing, literal!("immutable")), spaceAnd!(Spacing, literal!("shared"))), "CastEqual")(p);
    }

    static ParseTree PowExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, PostfixExpression, option!(spaceAnd!(Spacing, literal!("^^"), UnaryExpression))), "PowExpression")(p);
    }

    static ParseTree PostfixExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, PrimaryExpression, zeroOrMore!(or!(spaceAnd!(Spacing, IndexExpression), spaceAnd!(Spacing, SliceExpression))), option!(or!(spaceAnd!(Spacing, literal!("."), NewExpression), spaceAnd!(Spacing, literal!("."), TemplateIdentifier), spaceAnd!(Spacing, literal!("."), Identifier), spaceAnd!(Spacing, literal!("++")), spaceAnd!(Spacing, literal!("--")), spaceAnd!(Spacing, literal!("("), option!(ArgumentList), literal!(")"))))), "PostfixExpression")(p);
    }

    static ParseTree IndexExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("["), ArgumentList, literal!("]")), "IndexExpression")(p);
    }

    static ParseTree SliceExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("["), literal!("]"), literal!("["), AssignExpression, literal!(".."), AssignExpression, literal!("]")), "SliceExpression")(p);
    }

    static ParseTree PrimaryExpression(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("this")), spaceAnd!(Spacing, literal!("super")), spaceAnd!(Spacing, literal!("null")), spaceAnd!(Spacing, literal!("true")), spaceAnd!(Spacing, literal!("false")), spaceAnd!(Spacing, literal!("$")), spaceAnd!(Spacing, literal!("__FILE__")), spaceAnd!(Spacing, literal!("__LINE__")), spaceAnd!(Spacing, TemplateInstance), spaceAnd!(Spacing, literal!("."), TemplateInstance), spaceAnd!(Spacing, Identifier), spaceAnd!(Spacing, literal!("."), Identifier), spaceAnd!(Spacing, FloatLiteral), spaceAnd!(Spacing, IntegerLiteral), spaceAnd!(Spacing, CharacterLiteral), spaceAnd!(Spacing, StringLiterals), spaceAnd!(Spacing, ArrayLiteral), spaceAnd!(Spacing, AssocArrayLiteral), spaceAnd!(Spacing, Lambda), spaceAnd!(Spacing, FunctionLiteral), spaceAnd!(Spacing, AssertExpression), spaceAnd!(Spacing, MixinExpression), spaceAnd!(Spacing, ImportExpression), spaceAnd!(Spacing, BasicType, literal!("."), Identifier), spaceAnd!(Spacing, Typeof), spaceAnd!(Spacing, TypeidExpression)
, spaceAnd!(Spacing, IsExpression), spaceAnd!(Spacing, literal!("("), Expression, literal!(")")), spaceAnd!(Spacing, TraitsExpression)), "PrimaryExpression")(p);
    }

    static ParseTree StringLiterals(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, oneOrMore!(StringLiteral)), "StringLiterals")(p);
    }

    static ParseTree ArrayLiteral(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("["), option!(ArgumentList), literal!("]")), "ArrayLiteral")(p);
    }

    static ParseTree AssocArrayLiteral(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("["), List!(spaceAnd!(Spacing, KeyValuePair)), literal!("]")), "AssocArrayLiteral")(p);
    }

    static ParseTree KeyValuePair(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AssignExpression, literal!(":"), AssignExpression), "KeyValuePair")(p);
    }

    static ParseTree Lambda(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Identifier, literal!("=>"), AssignExpression), spaceAnd!(Spacing, ParameterAttributes, literal!("=>"), AssignExpression)), "Lambda")(p);
    }

    static ParseTree FunctionLiteral(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, option!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("function")), spaceAnd!(Spacing, literal!("delegate"))), option!(Type))), option!(ParameterAttributes), FunctionBody), "FunctionLiteral")(p);
    }

    static ParseTree ParameterAttributes(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Parameters, option!(FunctionAttributes)), "ParameterAttributes")(p);
    }

    static ParseTree AssertExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("assert"), literal!("("), AssignExpression, option!(spaceAnd!(Spacing, literal!(","), AssignExpression)), literal!(")")), "AssertExpression")(p);
    }

    static ParseTree MixinExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("("), AssignExpression, literal!(")")), "MixinExpression")(p);
    }

    static ParseTree ImportExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("import"), literal!("("), AssignExpression, literal!(")")), "ImportExpression")(p);
    }

    static ParseTree TypeidExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("typeid"), literal!("("), or!(spaceAnd!(Spacing, Type), spaceAnd!(Spacing, Expression)), literal!(")")), "TypeidExpression")(p);
    }

    static ParseTree IsExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("is"), literal!("("), Type, option!(or!(spaceAnd!(Spacing, literal!(":"), TypeSpecialization), spaceAnd!(Spacing, literal!("=="), TypeSpecialization), spaceAnd!(Spacing, Identifier, option!(or!(spaceAnd!(Spacing, literal!(":"), TypeSpecialization, option!(spaceAnd!(Spacing, literal!(","), TemplateParameterList))), spaceAnd!(Spacing, literal!("=="), TypeSpecialization, option!(spaceAnd!(Spacing, literal!(","), TemplateParameterList)))))))), literal!(")")), "IsExpression")(p);
    }

    static ParseTree TypeSpecialization(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Type), spaceAnd!(Spacing, literal!("struct")), spaceAnd!(Spacing, literal!("union")), spaceAnd!(Spacing, literal!("class")), spaceAnd!(Spacing, literal!("interface")), spaceAnd!(Spacing, literal!("enum")), spaceAnd!(Spacing, literal!("function")), spaceAnd!(Spacing, literal!("delegate")), spaceAnd!(Spacing, literal!("super")), spaceAnd!(Spacing, literal!("const")), spaceAnd!(Spacing, literal!("immutable")), spaceAnd!(Spacing, literal!("inout")), spaceAnd!(Spacing, literal!("shared")), spaceAnd!(Spacing, literal!("return"))), "TypeSpecialization")(p);
    }

    static ParseTree AttributeSpecifier(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Attribute, DeclarationBlock), spaceAnd!(Spacing, Attribute, literal!(":"))), "AttributeSpecifier")(p);
    }

    static ParseTree Attribute(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, LinkageAttribute), spaceAnd!(Spacing, AlignAttribute), spaceAnd!(Spacing, Pragma), spaceAnd!(Spacing, literal!("deprecated")), spaceAnd!(Spacing, ProtectionAttribute), spaceAnd!(Spacing, literal!("static")), spaceAnd!(Spacing, literal!("extern")), spaceAnd!(Spacing, literal!("final")), spaceAnd!(Spacing, literal!("synchronized")), spaceAnd!(Spacing, literal!("override")), spaceAnd!(Spacing, literal!("abstract")), spaceAnd!(Spacing, literal!("const")), spaceAnd!(Spacing, literal!("auto")), spaceAnd!(Spacing, literal!("scope")), spaceAnd!(Spacing, literal!("__gshared")), spaceAnd!(Spacing, literal!("shared")), spaceAnd!(Spacing, literal!("immutable")), spaceAnd!(Spacing, literal!("inout")), spaceAnd!(Spacing, literal!("@disable"))), "Attribute")(p);
    }

    static ParseTree DeclarationBlock(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, DeclDef), spaceAnd!(Spacing, literal!("{"), DeclDefs, literal!("}"))), "DeclarationBlock")(p);
    }

    static ParseTree LinkageAttribute(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("extern"), literal!("("), LinkageType, literal!(")")), "LinkageAttribute")(p);
    }

    static ParseTree LinkageType(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("C++")), spaceAnd!(Spacing, literal!("C")), spaceAnd!(Spacing, literal!("D")), spaceAnd!(Spacing, literal!("Windows")), spaceAnd!(Spacing, literal!("Pascal")), spaceAnd!(Spacing, literal!("System"))), "LinkageType")(p);
    }

    static ParseTree AlignAttribute(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("align"), option!(spaceAnd!(Spacing, literal!("("), IntegerLiteral, literal!(")")))), "AlignAttribute")(p);
    }

    static ParseTree ProtectionAttribute(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("private")), spaceAnd!(Spacing, literal!("package")), spaceAnd!(Spacing, literal!("protected")), spaceAnd!(Spacing, literal!("public")), spaceAnd!(Spacing, literal!("export"))), "ProtectionAttribute")(p);
    }

    static ParseTree ClassDeclaration(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("class"), Identifier, option!(BaseClassList), ClassBody), spaceAnd!(Spacing, ClassTemplateDeclaration)), "ClassDeclaration")(p);
    }

    static ParseTree BaseClassList(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), List!(spaceAnd!(Spacing, Identifier))), "BaseClassList")(p);
    }

    static ParseTree ClassBody(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(ClassBodyDeclarations), literal!("}")), "ClassBody")(p);
    }

    static ParseTree ClassBodyDeclarations(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ClassBodyDeclaration, option!(ClassBodyDeclarations)), "ClassBodyDeclarations")(p);
    }

    static ParseTree ClassBodyDeclaration(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, DeclDef), spaceAnd!(Spacing, Invariant), spaceAnd!(Spacing, ClassAllocator), spaceAnd!(Spacing, ClassDeallocator)), "ClassBodyDeclaration")(p);
    }

    static ParseTree Constructor(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("this"), Parameters, FunctionBody), spaceAnd!(Spacing, TemplatedConstructor)), "Constructor")(p);
    }

    static ParseTree Destructor(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("~"), literal!("this"), literal!("("), literal!(")"), FunctionBody), "Destructor")(p);
    }

    static ParseTree StaticConstructor(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("this"), literal!("("), literal!(")"), FunctionBody), "StaticConstructor")(p);
    }

    static ParseTree StaticDestructor(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("~"), literal!("this"), literal!("("), literal!(")"), FunctionBody), "StaticDestructor")(p);
    }

    static ParseTree SharedStaticConstructor(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("shared"), literal!("static"), literal!("this"), literal!("("), literal!(")"), FunctionBody), "SharedStaticConstructor")(p);
    }

    static ParseTree SharedStaticDestructor(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("shared"), literal!("static"), literal!("~"), literal!("this"), literal!("("), literal!(")"), FunctionBody), "SharedStaticDestructor")(p);
    }

    static ParseTree Invariant(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("invariant"), literal!("("), literal!(")"), BlockStatement), "Invariant")(p);
    }

    static ParseTree ClassAllocator(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("new"), Parameters, FunctionBody), "ClassAllocator")(p);
    }

    static ParseTree ClassDeallocator(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("delete"), Parameters, FunctionBody), "ClassDeallocator")(p);
    }

    static ParseTree AliasThis(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), Identifier, literal!("this"), literal!(";")), "AliasThis")(p);
    }

    static ParseTree NewAnonClassExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("new"), option!(AllocatorArguments), literal!("class"), option!(ClassArguments), Identifier, option!(List!(spaceAnd!(Spacing, Identifier))), ClassBody), "NewAnonClassExpression")(p);
    }

    static ParseTree ClassArguments(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("("), option!(ArgumentList), literal!(")")), "ClassArguments")(p);
    }

    static ParseTree EnumDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("enum"), option!(EnumTag), option!(spaceAnd!(Spacing, literal!(":"), EnumBaseType)), EnumBody), "EnumDeclaration")(p);
    }

    static ParseTree EnumTag(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier), "EnumTag")(p);
    }

    static ParseTree EnumBaseType(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Type), "EnumBaseType")(p);
    }

    static ParseTree EnumBody(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!(";")), spaceAnd!(Spacing, literal!("{"), List!(spaceAnd!(Spacing, EnumMember)), literal!("}"))), "EnumBody")(p);
    }

    static ParseTree EnumMember(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Type, literal!("="), AssignExpression), spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("="), AssignExpression)))), "EnumMember")(p);
    }

    static ParseTree FunctionBody(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, BlockStatement), spaceAnd!(Spacing, BodyStatement), spaceAnd!(Spacing, InStatement, BodyStatement), spaceAnd!(Spacing, OutStatement, BodyStatement), spaceAnd!(Spacing, InStatement, OutStatement, BodyStatement), spaceAnd!(Spacing, OutStatement, InStatement, BodyStatement)), "FunctionBody")(p);
    }

    static ParseTree InStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("in"), BlockStatement), "InStatement")(p);
    }

    static ParseTree OutStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("out"), option!(spaceAnd!(Spacing, literal!("("), Identifier, literal!(")"))), BlockStatement), "OutStatement")(p);
    }

    static ParseTree BodyStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("body"), BlockStatement), "BodyStatement")(p);
    }

    static ParseTree AsmInstruction(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("align"), IntegerExpression), spaceAnd!(Spacing, literal!("even")), spaceAnd!(Spacing, literal!("naked")), spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("db")), spaceAnd!(Spacing, literal!("ds")), spaceAnd!(Spacing, literal!("di")), spaceAnd!(Spacing, literal!("dl")), spaceAnd!(Spacing, literal!("df")), spaceAnd!(Spacing, literal!("dd")), spaceAnd!(Spacing, literal!("de"))), List!(spaceAnd!(Spacing, Operand))), spaceAnd!(Spacing, Identifier, literal!(":"), AsmInstruction), spaceAnd!(Spacing, OpCode), spaceAnd!(Spacing, OpCode, List!(spaceAnd!(Spacing, Operand)))), "AsmInstruction")(p);
    }

    static ParseTree IntegerExpression(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, IntegerLiteral), spaceAnd!(Spacing, Identifier)), "IntegerExpression")(p);
    }

    static ParseTree Operand(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmExp), "Operand")(p);
    }

    static ParseTree AsmExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmLogOrExp, option!(spaceAnd!(Spacing, literal!("?"), AsmExp, literal!(":"), AsmExp))), "AsmExp")(p);
    }

    static ParseTree AsmLogOrExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmLogAndExp, option!(spaceAnd!(Spacing, literal!("||"), AsmLogAndExp))), "AsmLogOrExp")(p);
    }

    static ParseTree AsmLogAndExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmOrExp, option!(spaceAnd!(Spacing, literal!("&&"), AsmOrExp))), "AsmLogAndExp")(p);
    }

    static ParseTree AsmOrExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmXorExp, option!(spaceAnd!(Spacing, literal!("|"), AsmXorExp))), "AsmOrExp")(p);
    }

    static ParseTree AsmXorExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmAndExp, option!(spaceAnd!(Spacing, literal!("^"), AsmAndExp))), "AsmXorExp")(p);
    }

    static ParseTree AsmAndExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmEqualExp, option!(spaceAnd!(Spacing, literal!("&"), AsmEqualExp))), "AsmAndExp")(p);
    }

    static ParseTree AsmEqualExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmRelExp, option!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("==")), spaceAnd!(Spacing, literal!("!="))), AsmRelExp))), "AsmEqualExp")(p);
    }

    static ParseTree AsmRelExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmShiftExp, option!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("<=")), spaceAnd!(Spacing, literal!(">=")), spaceAnd!(Spacing, literal!("<")), spaceAnd!(Spacing, literal!(">"))), AsmShiftExp))), "AsmRelExp")(p);
    }

    static ParseTree AsmShiftExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmAddExp, option!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!(">>>")), spaceAnd!(Spacing, literal!("<<")), spaceAnd!(Spacing, literal!(">>"))), AsmAddExp))), "AsmShiftExp")(p);
    }

    static ParseTree AsmAddExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmMulExp, option!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("+")), spaceAnd!(Spacing, literal!("-"))), AsmMulExp))), "AsmAddExp")(p);
    }

    static ParseTree AsmMulExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmBrExp, option!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("*")), spaceAnd!(Spacing, literal!("/")), spaceAnd!(Spacing, literal!("%"))), AsmBrExp))), "AsmMulExp")(p);
    }

    static ParseTree AsmBrExp(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, AsmUnaExp, option!(spaceAnd!(Spacing, literal!("["), AsmExp, literal!("]")))), "AsmBrExp")(p);
    }

    static ParseTree AsmUnaExp(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, AsmTypePrefix, AsmExp), spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("offsetof")), spaceAnd!(Spacing, literal!("seg"))), AsmExp), spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("+")), spaceAnd!(Spacing, literal!("-")), spaceAnd!(Spacing, literal!("!")), spaceAnd!(Spacing, literal!("~"))), AsmUnaExp), spaceAnd!(Spacing, AsmPrimaryExp)), "AsmUnaExp")(p);
    }

    static ParseTree AsmPrimaryExp(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, FloatLiteral), spaceAnd!(Spacing, IntegerLiteral), spaceAnd!(Spacing, literal!("__LOCAL_SIZE")), spaceAnd!(Spacing, literal!("$")), spaceAnd!(Spacing, Register), spaceAnd!(Spacing, DotIdentifier)), "AsmPrimaryExp")(p);
    }

    static ParseTree DotIdentifier(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("."), DotIdentifier))), "DotIdentifier")(p);
    }

    static ParseTree AsmTypePrefix(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("near")), spaceAnd!(Spacing, literal!("far")), spaceAnd!(Spacing, literal!("byte")), spaceAnd!(Spacing, literal!("short")), spaceAnd!(Spacing, literal!("int")), spaceAnd!(Spacing, literal!("word")), spaceAnd!(Spacing, literal!("dword")), spaceAnd!(Spacing, literal!("qword")), spaceAnd!(Spacing, literal!("float")), spaceAnd!(Spacing, literal!("double")), spaceAnd!(Spacing, literal!("real"))), literal!("ptr")), "AsmTypePrefix")(p);
    }

    static ParseTree Register(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier), "Register")(p);
    }

    static ParseTree OpCode(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier), "OpCode")(p);
    }

    static ParseTree InterfaceDeclaration(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("interface"), Identifier, option!(BaseInterfaceList), InterfaceBody), spaceAnd!(Spacing, InterfaceTemplateDeclaration)), "InterfaceDeclaration")(p);
    }

    static ParseTree BaseInterfaceList(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), List!(spaceAnd!(Spacing, Identifier))), "BaseInterfaceList")(p);
    }

    static ParseTree InterfaceBody(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(DeclDefs), literal!("}")), "InterfaceBody")(p);
    }

    static ParseTree Pragma(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("pragma"), literal!("("), Identifier, option!(spaceAnd!(Spacing, literal!(","), ArgumentList)), literal!(")")), "Pragma")(p);
    }

    static ParseTree AggregateDeclaration(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, or!(spaceAnd!(Spacing, literal!("struct")), spaceAnd!(Spacing, literal!("union"))), Identifier, or!(spaceAnd!(Spacing, StructBody), spaceAnd!(Spacing, literal!(";")))), spaceAnd!(Spacing, StructTemplateDeclaration), spaceAnd!(Spacing, UnionTemplateDeclaration)), "AggregateDeclaration")(p);
    }

    static ParseTree StructBody(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("{"), option!(StructBodyDeclarations), literal!("}")), "StructBody")(p);
    }

    static ParseTree StructBodyDeclarations(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, StructBodyDeclaration, option!(StructBodyDeclarations)), "StructBodyDeclarations")(p);
    }

    static ParseTree StructBodyDeclaration(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, DeclDef), spaceAnd!(Spacing, StructAllocator), spaceAnd!(Spacing, StructDeallocator), spaceAnd!(Spacing, StructPostblit), spaceAnd!(Spacing, AliasThis)), "StructBodyDeclaration")(p);
    }

    static ParseTree StructAllocator(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ClassAllocator), "StructAllocator")(p);
    }

    static ParseTree StructDeallocator(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, ClassDeallocator), "StructDeallocator")(p);
    }

    static ParseTree StructPostblit(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("this(this)"), FunctionBody), "StructPostblit")(p);
    }

    static ParseTree TemplateDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("template"), TemplateIdentifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint)), "TemplateDeclaration")(p);
    }

    static ParseTree TemplateIdentifier(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier), "TemplateIdentifier")(p);
    }

    static ParseTree TemplateParameterList(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, List!(spaceAnd!(Spacing, TemplateParameter))), "TemplateParameterList")(p);
    }

    static ParseTree TemplateParameter(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, TemplateTypeParameter), spaceAnd!(Spacing, TemplateValueParameter), spaceAnd!(Spacing, TemplateAliasParameter), spaceAnd!(Spacing, TemplateTupleParameter), spaceAnd!(Spacing, TemplateThisParameter)), "TemplateParameter")(p);
    }

    static ParseTree TemplateInstance(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, TemplateIdentifier, or!(spaceAnd!(Spacing, literal!("!("), List!(spaceAnd!(Spacing, TemplateArgument)), literal!(")")), spaceAnd!(Spacing, literal!("!"), TemplateSingleArgument))), "TemplateInstance")(p);
    }

    static ParseTree TemplateArgument(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Type), spaceAnd!(Spacing, AssignExpression), spaceAnd!(Spacing, Symbol)), "TemplateArgument")(p);
    }

    static ParseTree Symbol(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, option!(literal!(".")), SymbolTail), "Symbol")(p);
    }

    static ParseTree SymbolTail(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, TemplateInstance, option!(spaceAnd!(Spacing, literal!("."), SymbolTail))), spaceAnd!(Spacing, Identifier, option!(spaceAnd!(Spacing, literal!("."), SymbolTail)))), "SymbolTail")(p);
    }

    static ParseTree TemplateSingleArgument(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, BasicTypeX), spaceAnd!(Spacing, CharacterLiteral), spaceAnd!(Spacing, StringLiteral), spaceAnd!(Spacing, FloatLiteral), spaceAnd!(Spacing, IntegerLiteral), spaceAnd!(Spacing, literal!("true")), spaceAnd!(Spacing, literal!("false")), spaceAnd!(Spacing, literal!("null")), spaceAnd!(Spacing, literal!("__LINE__")), spaceAnd!(Spacing, literal!("__FILE__")), spaceAnd!(Spacing, Identifier)), "TemplateSingleArgument")(p);
    }

    static ParseTree TemplateTypeParameter(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, option!(TTPSpecialization), option!(TTPDefault)), "TemplateTypeParameter")(p);
    }

    static ParseTree TTPSpecialization(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), Type), "TTPSpecialization")(p);
    }

    static ParseTree TTPDefault(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("="), Type), "TTPDefault")(p);
    }

    static ParseTree TemplateThisParameter(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("this"), TemplateTypeParameter), "TemplateThisParameter")(p);
    }

    static ParseTree TemplateValueParameter(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, BasicType, Declarator, option!(TVPSpecialization), option!(TVPDefault)), "TemplateValueParameter")(p);
    }

    static ParseTree TVPSpecialization(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), ConditionalExpression), "TVPSpecialization")(p);
    }

    static ParseTree TVPDefault(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("="), or!(spaceAnd!(Spacing, literal!("__FILE__")), spaceAnd!(Spacing, literal!("__LINE__")), spaceAnd!(Spacing, AssignExpression))), "TVPDefault")(p);
    }

    static ParseTree TemplateAliasParameter(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("alias"), or!(spaceAnd!(Spacing, BasicType, Declarator), spaceAnd!(Spacing, Identifier)), option!(TAPSpecialization), option!(TAPDefault)), "TemplateAliasParameter")(p);
    }

    static ParseTree TAPSpecialization(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!(":"), or!(spaceAnd!(Spacing, Type), spaceAnd!(Spacing, ConditionalExpression))), "TAPSpecialization")(p);
    }

    static ParseTree TAPDefault(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("="), or!(spaceAnd!(Spacing, Type), spaceAnd!(Spacing, ConditionalExpression))), "TAPDefault")(p);
    }

    static ParseTree TemplateTupleParameter(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier, literal!("...")), "TemplateTupleParameter")(p);
    }

    static ParseTree TemplatedConstructor(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("this"), literal!("("), TemplateParameterList, literal!(")"), Parameters, option!(Constraint), FunctionBody), "TemplatedConstructor")(p);
    }

    static ParseTree ClassTemplateDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("class"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), option!(BaseClassList), ClassBody), "ClassTemplateDeclaration")(p);
    }

    static ParseTree StructTemplateDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("struct"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), StructBody), "StructTemplateDeclaration")(p);
    }

    static ParseTree UnionTemplateDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("union"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), StructBody), "UnionTemplateDeclaration")(p);
    }

    static ParseTree InterfaceTemplateDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("interface"), Identifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), option!(BaseInterfaceList), InterfaceBody), "InterfaceTemplateDeclaration")(p);
    }

    static ParseTree Constraint(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("if"), literal!("("), Expression, literal!(")")), "Constraint")(p);
    }

    static ParseTree TemplateMixinDeclaration(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), literal!("template"), TemplateIdentifier, literal!("("), TemplateParameterList, literal!(")"), option!(Constraint), literal!("{"), DeclDefs, literal!("}")), "TemplateMixinDeclaration")(p);
    }

    static ParseTree TemplateMixin(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("mixin"), TemplateIdentifier, spaceAnd!(Spacing, option!(spaceAnd!(Spacing, literal!("!("), List!(spaceAnd!(Spacing, TemplateArgument)), literal!(")"))), option!(MixinIdentifier)), literal!(";")), "TemplateMixin")(p);
    }

    static ParseTree MixinIdentifier(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Identifier), "MixinIdentifier")(p);
    }

    static ParseTree TraitsExpression(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("__traits"), literal!("("), TraitsKeyword, literal!(","), List!(spaceAnd!(Spacing, TraitsArgument)), literal!(")")), "TraitsExpression")(p);
    }

    static ParseTree TraitsKeyword(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("isAbstractClass")), spaceAnd!(Spacing, literal!("isArithmetic")), spaceAnd!(Spacing, literal!("isAssociativeArray")), spaceAnd!(Spacing, literal!("isFinalClass")), spaceAnd!(Spacing, literal!("isFloating")), spaceAnd!(Spacing, literal!("isIntegral")), spaceAnd!(Spacing, literal!("isScalar")), spaceAnd!(Spacing, literal!("isStaticArray")), spaceAnd!(Spacing, literal!("isUnsigned")), spaceAnd!(Spacing, literal!("isVitualFunction")), spaceAnd!(Spacing, literal!("isVirtualMethod")), spaceAnd!(Spacing, literal!("isAbstractFunction")), spaceAnd!(Spacing, literal!("isFinalFunction")), spaceAnd!(Spacing, literal!("isStaticFunction")), spaceAnd!(Spacing, literal!("isRef")), spaceAnd!(Spacing, literal!("isOut")), spaceAnd!(Spacing, literal!("isLazy")), spaceAnd!(Spacing, literal!("hasMember")), spaceAnd!(Spacing, literal!("identifier")), spaceAnd!(Spacing, literal!("getMember")), spaceAnd!(Spacing, literal!("getOverloads")), spaceAnd!(Spacing, literal!("
getVirtualFunctions")), spaceAnd!(Spacing, literal!("getVirtualMethods")), spaceAnd!(Spacing, literal!("parent")), spaceAnd!(Spacing, literal!("classInstanceSize")), spaceAnd!(Spacing, literal!("allMembers")), spaceAnd!(Spacing, literal!("derivedMembers")), spaceAnd!(Spacing, literal!("isSame")), spaceAnd!(Spacing, literal!("compiles"))), "TraitsKeyword")(p);
    }

    static ParseTree TraitsArgument(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, AssignExpression), spaceAnd!(Spacing, Type)), "TraitsArgument")(p);
    }

    static ParseTree UnitTest(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("unittest"), FunctionBody), "UnitTest")(p);
    }

    static ParseTree ConditionalDeclaration(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Condition, literal!(":"), Declarations), spaceAnd!(Spacing, Condition, CCDeclarationBlock, option!(spaceAnd!(Spacing, literal!("else"), CCDeclarationBlock)))), "ConditionalDeclaration")(p);
    }

    static ParseTree CCDeclarationBlock(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, Declaration), spaceAnd!(Spacing, literal!("{"), option!(Declaration), literal!("}"))), "CCDeclarationBlock")(p);
    }

    static ParseTree Declarations(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, oneOrMore!(Declaration)), "Declarations")(p);
    }

    static ParseTree ConditionalStatement(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, Condition, NoScopeNonEmptyStatement, option!(spaceAnd!(Spacing, literal!("else"), NoScopeNonEmptyStatement))), "ConditionalStatement")(p);
    }

    static ParseTree Condition(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, VersionCondition), spaceAnd!(Spacing, DebugCondition), spaceAnd!(Spacing, StaticIfCondition)), "Condition")(p);
    }

    static ParseTree VersionCondition(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("version"), literal!("("), or!(spaceAnd!(Spacing, IntegerLiteral), spaceAnd!(Spacing, literal!("unittest")), spaceAnd!(Spacing, Identifier)), literal!(")")), "VersionCondition")(p);
    }

    static ParseTree VersionSpecification(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("version"), literal!("="), or!(spaceAnd!(Spacing, Identifier), spaceAnd!(Spacing, IntegerLiteral)), literal!(";")), "VersionSpecification")(p);
    }

    static ParseTree DebugCondition(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("debug"), option!(spaceAnd!(Spacing, literal!("("), or!(spaceAnd!(Spacing, IntegerLiteral), spaceAnd!(Spacing, Identifier)), literal!(")")))), "DebugCondition")(p);
    }

    static ParseTree DebugSpecification(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("debug"), literal!("="), or!(spaceAnd!(Spacing, Identifier), spaceAnd!(Spacing, IntegerLiteral)), literal!(";")), "DebugSpecification")(p);
    }

    static ParseTree StaticIfCondition(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("if"), literal!("("), AssignExpression, literal!(")")), "StaticIfCondition")(p);
    }

    static ParseTree StaticAssert(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("static"), literal!("assert"), literal!("("), AssignExpression, option!(spaceAnd!(Spacing, literal!(","), AssignExpression)), literal!(")"), literal!(";")), "StaticAssert")(p);
    }

    static ParseTree Identifier(ParseTree p)
    {
        return named!(fuse!(and!(negLookahead!(Keyword), or!(charRange!('a', 'z'), charRange!('A', 'Z'), literal!("_")), zeroOrMore!(or!(charRange!('a', 'z'), charRange!('A', 'Z'), charRange!('0', '9'), literal!("_"))))), "Identifier")(p);
    }

    static ParseTree Keyword(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("abstract")), spaceAnd!(Spacing, literal!("alias")), spaceAnd!(Spacing, literal!("align")), spaceAnd!(Spacing, literal!("asm")), spaceAnd!(Spacing, literal!("assert")), spaceAnd!(Spacing, literal!("auto")), spaceAnd!(Spacing, literal!("body")), spaceAnd!(Spacing, literal!("bool")), spaceAnd!(Spacing, literal!("break")), spaceAnd!(Spacing, literal!("byte")), spaceAnd!(Spacing, literal!("case")), spaceAnd!(Spacing, literal!("cast")), spaceAnd!(Spacing, literal!("catch")), spaceAnd!(Spacing, literal!("cdouble")), spaceAnd!(Spacing, literal!("cent")), spaceAnd!(Spacing, literal!("cfloat")), spaceAnd!(Spacing, literal!("char")), spaceAnd!(Spacing, literal!("class")), spaceAnd!(Spacing, literal!("const")), spaceAnd!(Spacing, literal!("continue")), spaceAnd!(Spacing, literal!("creal")), spaceAnd!(Spacing, literal!("dchar")), spaceAnd!(Spacing, literal!("debug")), spaceAnd!(Spacing, literal!("default")), spaceAnd!(Spacing, literal!("delegate")), spaceAnd!(Spacing,
 literal!("delete")), spaceAnd!(Spacing, literal!("deprecated")), spaceAnd!(Spacing, literal!("double")), spaceAnd!(Spacing, literal!("do")), spaceAnd!(Spacing, literal!("else")), spaceAnd!(Spacing, literal!("enum")), spaceAnd!(Spacing, literal!("export")), spaceAnd!(Spacing, literal!("extern")), spaceAnd!(Spacing, literal!("false")), spaceAnd!(Spacing, literal!("finally")), spaceAnd!(Spacing, literal!("final")), spaceAnd!(Spacing, literal!("float")), spaceAnd!(Spacing, literal!("foreach_reverse")), spaceAnd!(Spacing, literal!("foreach")), spaceAnd!(Spacing, literal!("for")), spaceAnd!(Spacing, literal!("function")), spaceAnd!(Spacing, literal!("goto")), spaceAnd!(Spacing, literal!("idouble")), spaceAnd!(Spacing, literal!("if")), spaceAnd!(Spacing, literal!("ifloat")), spaceAnd!(Spacing, literal!("immutable")), spaceAnd!(Spacing, literal!("import")), spaceAnd!(Spacing, literal!("inout")), spaceAnd!(Spacing, literal!("interface")), spaceAnd!(Spacing, literal!("invariant")), spaceAnd!(Spacing, literal!("int")),
 spaceAnd!(Spacing, literal!("in")), spaceAnd!(Spacing, literal!("ireal")), spaceAnd!(Spacing, literal!("is")), spaceAnd!(Spacing, literal!("lazy")), spaceAnd!(Spacing, literal!("long")), spaceAnd!(Spacing, literal!("macro")), spaceAnd!(Spacing, literal!("mixin")), spaceAnd!(Spacing, literal!("module")), spaceAnd!(Spacing, literal!("new")), spaceAnd!(Spacing, literal!("nothrow")), spaceAnd!(Spacing, literal!("null")), spaceAnd!(Spacing, literal!("out")), spaceAnd!(Spacing, literal!("override")), spaceAnd!(Spacing, literal!("package")), spaceAnd!(Spacing, literal!("pragma")), spaceAnd!(Spacing, literal!("private")), spaceAnd!(Spacing, literal!("protected")), spaceAnd!(Spacing, literal!("public")), spaceAnd!(Spacing, literal!("pure")), spaceAnd!(Spacing, literal!("real")), spaceAnd!(Spacing, literal!("ref")), spaceAnd!(Spacing, literal!("return")), spaceAnd!(Spacing, literal!("scope")), spaceAnd!(Spacing, literal!("shared")), spaceAnd!(Spacing, literal!("short")), spaceAnd!(Spacing, literal!("static")), 
spaceAnd!(Spacing, literal!("struct")), spaceAnd!(Spacing, literal!("super")), spaceAnd!(Spacing, literal!("switch")), spaceAnd!(Spacing, literal!("synchronized")), spaceAnd!(Spacing, literal!("template")), spaceAnd!(Spacing, literal!("this")), spaceAnd!(Spacing, literal!("throw")), spaceAnd!(Spacing, literal!("true")), spaceAnd!(Spacing, literal!("try")), spaceAnd!(Spacing, literal!("typedef")), spaceAnd!(Spacing, literal!("typeid")), spaceAnd!(Spacing, literal!("typeof")), spaceAnd!(Spacing, literal!("ubyte")), spaceAnd!(Spacing, literal!("ucent")), spaceAnd!(Spacing, literal!("uint")), spaceAnd!(Spacing, literal!("ulong")), spaceAnd!(Spacing, literal!("union")), spaceAnd!(Spacing, literal!("unittest")), spaceAnd!(Spacing, literal!("ushort")), spaceAnd!(Spacing, literal!("version")), spaceAnd!(Spacing, literal!("void")), spaceAnd!(Spacing, literal!("volatile")), spaceAnd!(Spacing, literal!("wchar")), spaceAnd!(Spacing, literal!("while")), spaceAnd!(Spacing, literal!("with")), spaceAnd!(Spacing, literal!("__
FILE__")), spaceAnd!(Spacing, literal!("__LINE__")), spaceAnd!(Spacing, literal!("__gshared")), spaceAnd!(Spacing, literal!("__thread")), spaceAnd!(Spacing, literal!("__traits"))), "Keyword")(p);
    }

    static ParseTree Comment(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, BlockComment), spaceAnd!(Spacing, LineComment), spaceAnd!(Spacing, NestingBlockComment)), "Comment")(p);
    }

    static ParseTree BlockComment(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("/*"), zeroOrMore!(spaceAnd!(Spacing, negLookahead!(literal!("*/")), pegged.peg.any)), literal!("*/")), "BlockComment")(p);
    }

    static ParseTree LineComment(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("//"), zeroOrMore!(spaceAnd!(Spacing, negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "LineComment")(p);
    }

    static ParseTree NestingBlockComment(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, literal!("/+"), or!(spaceAnd!(Spacing, NestingBlockComment), spaceAnd!(Spacing, Text)), literal!("+/")), "NestingBlockComment")(p);
    }

    static ParseTree Text(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, zeroOrMore!(spaceAnd!(Spacing, negLookahead!(literal!("+/")), pegged.peg.any))), "Text")(p);
    }

    static ParseTree StringLiteral(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, WysiwygString), spaceAnd!(Spacing, AlternateWysiwygString), spaceAnd!(Spacing, doublequotedString), spaceAnd!(Spacing, TokenString)), "StringLiteral")(p);
    }

    static ParseTree WysiwygString(ParseTree p)
    {
        return named!(and!(literal!("r"), doublequote, zeroOrMore!(and!(negLookahead!(doublequote), pegged.peg.any)), doublequote, option!(StringPostfix)), "WysiwygString")(p);
    }

    static ParseTree AlternateWysiwygString(ParseTree p)
    {
        return named!(and!(backquote, zeroOrMore!(and!(negLookahead!(backquote), pegged.peg.any)), backquote, option!(StringPostfix)), "AlternateWysiwygString")(p);
    }

    static ParseTree doublequotedString(ParseTree p)
    {
        return named!(and!(doublequote, zeroOrMore!(and!(DQChar)), doublequote, option!(StringPostfix)), "doublequotedString")(p);
    }

    static ParseTree DQChar(ParseTree p)
    {
        return named!(or!(and!(EscapeSequence), and!(negLookahead!(doublequote), pegged.peg.any)), "DQChar")(p);
    }

    static ParseTree EscapeSequence(ParseTree p)
    {
        return named!(and!(backslash, or!(and!(quote), and!(doublequote), and!(backslash), and!(or!(literal!("a"), literal!("b"), literal!("f"), literal!("n"), literal!("r"), literal!("t"), literal!("v"))), and!(literal!("x"), HexDigit, HexDigit), and!(literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), and!(literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit))), "EscapeSequence")(p);
    }

    static ParseTree StringPostfix(ParseTree p)
    {
        return named!(or!(spaceAnd!(Spacing, literal!("c")), spaceAnd!(Spacing, literal!("w")), spaceAnd!(Spacing, literal!("d"))), "StringPostfix")(p);
    }

    static ParseTree TokenString(ParseTree p)
    {
        return named!(and!(literal!("q{"), zeroOrMore!(and!(negLookahead!(literal!("}")), pegged.peg.any)), literal!("}")), "TokenString")(p);
    }

    static ParseTree CharacterLiteral(ParseTree p)
    {
        return named!(and!(quote, and!(negLookahead!(quote), or!(and!(EscapeSequence), and!(pegged.peg.any))), quote), "CharacterLiteral")(p);
    }

    static ParseTree IntegerLiteral(ParseTree p)
    {
        return named!(or!(and!(DecimalInteger), and!(BinaryInteger), and!(HexadecimalInteger)), "IntegerLiteral")(p);
    }

    static ParseTree DecimalInteger(ParseTree p)
    {
        return named!(and!(Integer, option!(IntegerSuffix)), "DecimalInteger")(p);
    }

    static ParseTree Integer(ParseTree p)
    {
        return named!(and!(digit, zeroOrMore!(or!(and!(digit), and!(literal!("_"))))), "Integer")(p);
    }

    static ParseTree IntegerSuffix(ParseTree p)
    {
        return named!(or!(and!(literal!("Lu")), and!(literal!("LU")), and!(literal!("uL")), and!(literal!("UL")), and!(literal!("L")), and!(literal!("u")), and!(literal!("U"))), "IntegerSuffix")(p);
    }

    static ParseTree BinaryInteger(ParseTree p)
    {
        return named!(and!(or!(and!(literal!("0b")), and!(literal!("0B"))), or!(literal!("0"), literal!("1")), zeroOrMore!(or!(and!(or!(literal!("0"), literal!("1"))), and!(literal!("_"))))), "BinaryInteger")(p);
    }

    static ParseTree HexadecimalInteger(ParseTree p)
    {
        return named!(and!(or!(and!(literal!("0x")), and!(literal!("0X"))), HexDigit, zeroOrMore!(or!(and!(HexDigit), and!(literal!("_"))))), "HexadecimalInteger")(p);
    }

    static ParseTree HexDigit(ParseTree p)
    {
        return named!(spaceAnd!(Spacing, or!(charRange!('0', '9'), charRange!('a', 'f'), charRange!('A', 'F'))), "HexDigit")(p);
    }

    static ParseTree FloatLiteral(ParseTree p)
    {
        return named!(and!(option!(Sign), Integer, literal!("."), option!(Integer), option!(and!(or!(and!(literal!("e")), and!(literal!("E"))), option!(Sign), Integer))), "FloatLiteral")(p);
    }

    static ParseTree Sign(ParseTree p)
    {
        return named!(and!(option!(or!(and!(literal!("-")), and!(literal!("+"))))), "Sign")(p);
    }

    static ParseTree opCall(ParseTree p)
    {
        ParseTree result = decimateTree(Module(p));
        result.children = [result];
        result.name = "D";
        return result;
    }

    static ParseTree opCall(string input)
    {
        return D(ParseTree(``, false, [], input, 0, 0));
    }
}

