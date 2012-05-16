module pegged.examples.dgrammar;

enum string Dgrammar = `
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
		 
ModuleDeclaration < "module" QualifiedIdentifier ";"

ImportDeclaration < "import" ImportList ";"
                   / "static" "import"  ImportList ";"
				   
ImportList < Import ("," ImportList)?
            / ImportBindings

Import < Identifier "=" QualifiedIdentifier
        / QualifiedIdentifier

List(Elem) < Elem (',' Elem)*

ImportBindings < Import ":" List(ImportBind) 
###### Also a space-sep list is needed ##

ImportBind < Identifier ("=" Identifier)?

MixinDeclaration < "mixin" "(" AssignExpression ")" ";"

# declaration.html
Declaration < AliasDeclaration
             / AliasThisDeclaration
             / Decl

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
            / "[" AssignExpression ".." AssignExpression "]"
            / "[" Type "]"
            / "delegate" Parameters FunctionAttributes?
            / "function" Parameters FunctionAttributes?

## Maybe that could factored ##
Declarator < BasicType2? "(" Declarator ")" DeclaratorSuffixes?
            / BasicType2?     Identifier     DeclaratorSuffixes?

DeclaratorSuffixes < DeclaratorSuffix+
            
DeclaratorSuffix < "[" "]"
                  / "[" AssignExpression "]"
                  / "[" Type "]"
                  / TemplateParameterList? Parameters MemberFunctionAttributes? Constraint?

## Could be written otherwise? #
IdentifierList < Identifier ("." IdentifierList)?
                / TemplateInstance ("." IdentifierList)?
               
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

Declarator2 < BasicType2? ("(" Declarator2 ")")? DeclaratorSuffixes?

Parameters < "(" ParameterList? ")"

ParameterList < "..."
               / Parameter+

Parameter < InOut? BasicType Declarator ("..." / "=" DefaultInitializerExpression)?
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

Expression < List(AssignExpression)

AssignExpression < ConditionalExpression (Op AssignExpression)?

Op < ">>>=" 
    / "^^=" / ">>=" / "<<="
    / "~=" / "+=" / "-=" / "*=" / "^=" / "|=" / "&=" / "/="
    / "="

ConditionalExpression < OrOrExpression 
                        ("?" Expression ":" ConditionalExpression)?

OrOrExpression < AndAndExpression ("||" OrOrExpression)?

AndAndExpression < (OrExpression / CmpExpression) ("&&" AndAndExpression)?
                    
OrExpression < XorExpression ("|" OrExpression)?

XorExpression < AndExpression ("^" XorExpression)?

AndExpression < ShiftExpression ("&" AndExpression)?

CmpExpression < ShiftExpression
               / EqualExpression
               / IdentityExpression
               / RelExpression
               / InExpression
               
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
                 
CatExpression < AddExpression ("~" MulExpression)?

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

ArgumentList < List(AssignExpression)

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

PostfixExpression < PrimaryExpression
                   / IndexExpression
                   / SliceExpression
                   / PostfixExpression ( "." NewExpression
                                       / "." TemplateIdentifier
                                       / "." Identifier
                                       / "++"
                                       / "--"
                                       / "(" ArgumentList? ")"
                                       )
                                       
IndexExpression < PostfixExpression "[" ArgumentList "]"

SliceExpression < PostfixExpression ( "[" "]"
                                     / "[" AssignExpression ".." AssignExpression "]"
                                     )
                                     
PrimaryExpression < "this"
                   / "super"
                   / "null"
                   / "true"
                   / "false"
                   / "$"
                   / "__FILE__"
                   / "__LINE__"
                   / Identifier
                   / "." Identifier
                   / TemplateInstance
                   / "." TemplateInstance
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

StringLiterals < StringLiteral+

ArrayLiteral < "[" ArgumentList "]"

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

ClassDeclaration < "class" Identifier BaseClassList? ClassBody ClassTemplateDeclaration

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
           
AsmPrimaryExp < IntegerLiteral
               / FloatLiteral
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
                        / IntegerLiteral
                        / FloatLiteral
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

BlockComment < '/*' (!'*/' .)* '*/'

LineComment < '//' (!EOL .)* EOL

NestingBlockComment < '/+' (NestingBlockComment / Text) '+/'

Text < (!'+/' .)*

StringLiteral < WysiwygString
               / AlternateWysiwygString
               / DoubleQuotedString
               # No HexString
               # No DelimitedString
               / TokenString

WysiwygString <- 'r' DoubleQuote (!DoubleQuote .)* DoubleQuote StringPostfix?

AlternateWysiwygString <- BackQuote (!BackQuote .)* BackQuote StringPostfix?

DoubleQuotedString <- DoubleQuote (DQChar)* DoubleQuote StringPostfix?

DQChar <- EscapeSequence 
        / !DoubleQuote .
        
EscapeSequence <- BackSlash ( Quote
                            / DoubleQuote
                            / BackSlash
                            / [abfnrtv]
                            / 'x' HexDigit HexDigit
                            / 'u' HexDigit HexDigit HexDigit HexDigit
                            / 'U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
                            )

StringPostfix < "c" / "w" / "d"

TokenString <- "q{" (!"}" .)* "}"

CharacterLiteral <- Quote (!Quote (EscapeSequence / .)) Quote

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

HexDigit < [0-9a-fA-F]

FloatLiteral <- Sign? Integer ("." Integer?)? (("e" / "E") Sign? Integer)?

Sign <- ("-" / "+")?

`;