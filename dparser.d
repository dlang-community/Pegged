/++
This module was automatically generated from the following grammar:


D:

Module <- ModuleDeclaration? DeclDefs?

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
        / MacroDeclaration

### MACROS ADDITION TO THE D GRAMMAR ###

MacroDeclaration < "macro" MacroName MacroParameterList
                   MacroLevel?
                   MacroBeforeBody "return" MacroAfterBody

MacroName < identifier

MacroParameterList < :"(" List(MacroParameter)? :")"

MacroParameter < identifier identifier

MacroLevel < :":" identifier

#Mind the '<-' arrow!
MacroBeforeBody <- :"{"
                   ~(!(endOfLine "}") .)*
                   :endOfLine :"}"

MacroAfterBody < :"{" Statement :"}"


###

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

AliasDeclaration < "alias" ( BasicType Declarator
                           / List(AliasInitializer))

AliasInitializer < Identifier "=" Type

AliasThisDeclaration < "alias" ( Identifier "this"
                               / "this" "=" Identifier)

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

Parameter < InOut? BasicType Declarator ("..." / "=" DefaultInitializerExpression)?
          / InOut? Type "..."?

InOut < InOutX InOut?

InOutX < "auto"
        / "const"
        / "final"
        / "immutable"
        / "inout"
        / "in "
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

IfStatement < "if" "(" IfCondition ")" ThenStatement ("else" ElseStatement)?

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

Spacing <- (blank / Comment)*

Comment <- BlockComment
         / LineComment
         / NestingBlockComment

BlockComment <~ :'/ *' (!'* /' .)* :'* /'

LineComment <~ :'//' (!endOfLine .)* :endOfLine

#NestingBlockComment < :'/ +' (NestingBlockComment / Text) :'+ /'
# / + (please, don't delete this line, it opens a nested block comment in generated module which is closed on the next line
#Text < (!'+ /' .)*
NestingBlockComment <~ :"/+" (!("/+"/"+/") .)* NestingBlockComment? (!("/+"/"+/") .)* :"+/"

StringLiteral < WysiwygString
               / AlternateWysiwygString
               / DoublequotedString
               # No HexString
               # No DelimitedString
               / TokenString

WysiwygString <- 'r' doublequote (!doublequote .)* doublequote StringPostfix?

AlternateWysiwygString <- backquote (!backquote .)* backquote StringPostfix?

DoublequotedString <- doublequote (DQChar)* doublequote StringPostfix?

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


+/
module dparser;

public import pegged.peg;
struct GenericD(TParseTree)
{
    struct D
    {
    enum name = "D";
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static ParseTree function(ParseTree) beforeModule = &fail;
    static ParseTree function(ParseTree) afterModule = &fail;
    static ParseTree function(ParseTree) beforeDeclDefs = &fail;
    static ParseTree function(ParseTree) afterDeclDefs = &fail;
    static ParseTree function(ParseTree) beforeDeclDef = &fail;
    static ParseTree function(ParseTree) afterDeclDef = &fail;
    static ParseTree function(ParseTree) beforeMacroDeclaration = &fail;
    static ParseTree function(ParseTree) afterMacroDeclaration = &fail;
    static ParseTree function(ParseTree) beforeMacroName = &fail;
    static ParseTree function(ParseTree) afterMacroName = &fail;
    static ParseTree function(ParseTree) beforeMacroParameterList = &fail;
    static ParseTree function(ParseTree) afterMacroParameterList = &fail;
    static ParseTree function(ParseTree) beforeMacroParameter = &fail;
    static ParseTree function(ParseTree) afterMacroParameter = &fail;
    static ParseTree function(ParseTree) beforeMacroLevel = &fail;
    static ParseTree function(ParseTree) afterMacroLevel = &fail;
    static ParseTree function(ParseTree) beforeMacroBeforeBody = &fail;
    static ParseTree function(ParseTree) afterMacroBeforeBody = &fail;
    static ParseTree function(ParseTree) beforeMacroAfterBody = &fail;
    static ParseTree function(ParseTree) afterMacroAfterBody = &fail;
    static ParseTree function(ParseTree) beforeModuleDeclaration = &fail;
    static ParseTree function(ParseTree) afterModuleDeclaration = &fail;
    static ParseTree function(ParseTree) beforeImportDeclaration = &fail;
    static ParseTree function(ParseTree) afterImportDeclaration = &fail;
    static ParseTree function(ParseTree) beforeImportList = &fail;
    static ParseTree function(ParseTree) afterImportList = &fail;
    static ParseTree function(ParseTree) beforeImport = &fail;
    static ParseTree function(ParseTree) afterImport = &fail;
    static ParseTree function(ParseTree) beforeList_1 = &fail;
    static ParseTree function(ParseTree) afterList_1 = &fail;
    static ParseTree function(ParseTree) beforeImportBindings = &fail;
    static ParseTree function(ParseTree) afterImportBindings = &fail;
    static ParseTree function(ParseTree) beforeImportBind = &fail;
    static ParseTree function(ParseTree) afterImportBind = &fail;
    static ParseTree function(ParseTree) beforeMixinDeclaration = &fail;
    static ParseTree function(ParseTree) afterMixinDeclaration = &fail;
    static ParseTree function(ParseTree) beforeDeclaration = &fail;
    static ParseTree function(ParseTree) afterDeclaration = &fail;
    static ParseTree function(ParseTree) beforeAliasDeclaration = &fail;
    static ParseTree function(ParseTree) afterAliasDeclaration = &fail;
    static ParseTree function(ParseTree) beforeAliasInitializer = &fail;
    static ParseTree function(ParseTree) afterAliasInitializer = &fail;
    static ParseTree function(ParseTree) beforeAliasThisDeclaration = &fail;
    static ParseTree function(ParseTree) afterAliasThisDeclaration = &fail;
    static ParseTree function(ParseTree) beforeDecl = &fail;
    static ParseTree function(ParseTree) afterDecl = &fail;
    static ParseTree function(ParseTree) beforeDeclarators = &fail;
    static ParseTree function(ParseTree) afterDeclarators = &fail;
    static ParseTree function(ParseTree) beforeDeclaratorInitializer = &fail;
    static ParseTree function(ParseTree) afterDeclaratorInitializer = &fail;
    static ParseTree function(ParseTree) beforeDeclaratorIdentifier = &fail;
    static ParseTree function(ParseTree) afterDeclaratorIdentifier = &fail;
    static ParseTree function(ParseTree) beforeBasicType = &fail;
    static ParseTree function(ParseTree) afterBasicType = &fail;
    static ParseTree function(ParseTree) beforeBasicTypeX = &fail;
    static ParseTree function(ParseTree) afterBasicTypeX = &fail;
    static ParseTree function(ParseTree) beforeBasicType2 = &fail;
    static ParseTree function(ParseTree) afterBasicType2 = &fail;
    static ParseTree function(ParseTree) beforeDeclarator = &fail;
    static ParseTree function(ParseTree) afterDeclarator = &fail;
    static ParseTree function(ParseTree) beforeDeclaratorSuffixes = &fail;
    static ParseTree function(ParseTree) afterDeclaratorSuffixes = &fail;
    static ParseTree function(ParseTree) beforeDeclaratorSuffix = &fail;
    static ParseTree function(ParseTree) afterDeclaratorSuffix = &fail;
    static ParseTree function(ParseTree) beforeIdentifierList = &fail;
    static ParseTree function(ParseTree) afterIdentifierList = &fail;
    static ParseTree function(ParseTree) beforeStorageClasses = &fail;
    static ParseTree function(ParseTree) afterStorageClasses = &fail;
    static ParseTree function(ParseTree) beforeStorageClass = &fail;
    static ParseTree function(ParseTree) afterStorageClass = &fail;
    static ParseTree function(ParseTree) beforeProperty = &fail;
    static ParseTree function(ParseTree) afterProperty = &fail;
    static ParseTree function(ParseTree) beforeType = &fail;
    static ParseTree function(ParseTree) afterType = &fail;
    static ParseTree function(ParseTree) beforeDeclarator2 = &fail;
    static ParseTree function(ParseTree) afterDeclarator2 = &fail;
    static ParseTree function(ParseTree) beforeParameters = &fail;
    static ParseTree function(ParseTree) afterParameters = &fail;
    static ParseTree function(ParseTree) beforeParameterList = &fail;
    static ParseTree function(ParseTree) afterParameterList = &fail;
    static ParseTree function(ParseTree) beforeParameter = &fail;
    static ParseTree function(ParseTree) afterParameter = &fail;
    static ParseTree function(ParseTree) beforeInOut = &fail;
    static ParseTree function(ParseTree) afterInOut = &fail;
    static ParseTree function(ParseTree) beforeInOutX = &fail;
    static ParseTree function(ParseTree) afterInOutX = &fail;
    static ParseTree function(ParseTree) beforeFunctionAttributes = &fail;
    static ParseTree function(ParseTree) afterFunctionAttributes = &fail;
    static ParseTree function(ParseTree) beforeFunctionAttribute = &fail;
    static ParseTree function(ParseTree) afterFunctionAttribute = &fail;
    static ParseTree function(ParseTree) beforeMemberFunctionAttributes = &fail;
    static ParseTree function(ParseTree) afterMemberFunctionAttributes = &fail;
    static ParseTree function(ParseTree) beforeMemberFunctionAttribute = &fail;
    static ParseTree function(ParseTree) afterMemberFunctionAttribute = &fail;
    static ParseTree function(ParseTree) beforeDefaultInitializerExpression = &fail;
    static ParseTree function(ParseTree) afterDefaultInitializerExpression = &fail;
    static ParseTree function(ParseTree) beforeInitializer = &fail;
    static ParseTree function(ParseTree) afterInitializer = &fail;
    static ParseTree function(ParseTree) beforeNonVoidInitializer = &fail;
    static ParseTree function(ParseTree) afterNonVoidInitializer = &fail;
    static ParseTree function(ParseTree) beforeArrayInitializer = &fail;
    static ParseTree function(ParseTree) afterArrayInitializer = &fail;
    static ParseTree function(ParseTree) beforeArrayMemberInitializations = &fail;
    static ParseTree function(ParseTree) afterArrayMemberInitializations = &fail;
    static ParseTree function(ParseTree) beforeArrayMemberInitialization = &fail;
    static ParseTree function(ParseTree) afterArrayMemberInitialization = &fail;
    static ParseTree function(ParseTree) beforeStructInitializer = &fail;
    static ParseTree function(ParseTree) afterStructInitializer = &fail;
    static ParseTree function(ParseTree) beforeStructMemberInitializers = &fail;
    static ParseTree function(ParseTree) afterStructMemberInitializers = &fail;
    static ParseTree function(ParseTree) beforeStructMemberInitializer = &fail;
    static ParseTree function(ParseTree) afterStructMemberInitializer = &fail;
    static ParseTree function(ParseTree) beforeAutoDeclaration = &fail;
    static ParseTree function(ParseTree) afterAutoDeclaration = &fail;
    static ParseTree function(ParseTree) beforeAutoDeclarationX = &fail;
    static ParseTree function(ParseTree) afterAutoDeclarationX = &fail;
    static ParseTree function(ParseTree) beforeTypeof = &fail;
    static ParseTree function(ParseTree) afterTypeof = &fail;
    static ParseTree function(ParseTree) beforeVoidInitializer = &fail;
    static ParseTree function(ParseTree) afterVoidInitializer = &fail;
    static ParseTree function(ParseTree) beforeStatement = &fail;
    static ParseTree function(ParseTree) afterStatement = &fail;
    static ParseTree function(ParseTree) beforeNoScopeNonEmptyStatement = &fail;
    static ParseTree function(ParseTree) afterNoScopeNonEmptyStatement = &fail;
    static ParseTree function(ParseTree) beforeNoScopeStatement = &fail;
    static ParseTree function(ParseTree) afterNoScopeStatement = &fail;
    static ParseTree function(ParseTree) beforeNonEmptyOrScopeBlockStatement = &fail;
    static ParseTree function(ParseTree) afterNonEmptyOrScopeBlockStatement = &fail;
    static ParseTree function(ParseTree) beforeNonEmptyStatement = &fail;
    static ParseTree function(ParseTree) afterNonEmptyStatement = &fail;
    static ParseTree function(ParseTree) beforeNonEmptyStatementNoCaseNoDefault = &fail;
    static ParseTree function(ParseTree) afterNonEmptyStatementNoCaseNoDefault = &fail;
    static ParseTree function(ParseTree) beforeScopeStatement = &fail;
    static ParseTree function(ParseTree) afterScopeStatement = &fail;
    static ParseTree function(ParseTree) beforeScopeBlockStatement = &fail;
    static ParseTree function(ParseTree) afterScopeBlockStatement = &fail;
    static ParseTree function(ParseTree) beforeLabeledStatement = &fail;
    static ParseTree function(ParseTree) afterLabeledStatement = &fail;
    static ParseTree function(ParseTree) beforeBlockStatement = &fail;
    static ParseTree function(ParseTree) afterBlockStatement = &fail;
    static ParseTree function(ParseTree) beforeStatementList = &fail;
    static ParseTree function(ParseTree) afterStatementList = &fail;
    static ParseTree function(ParseTree) beforeExpressionStatement = &fail;
    static ParseTree function(ParseTree) afterExpressionStatement = &fail;
    static ParseTree function(ParseTree) beforeDeclarationStatement = &fail;
    static ParseTree function(ParseTree) afterDeclarationStatement = &fail;
    static ParseTree function(ParseTree) beforeIfStatement = &fail;
    static ParseTree function(ParseTree) afterIfStatement = &fail;
    static ParseTree function(ParseTree) beforeIfCondition = &fail;
    static ParseTree function(ParseTree) afterIfCondition = &fail;
    static ParseTree function(ParseTree) beforeThenStatement = &fail;
    static ParseTree function(ParseTree) afterThenStatement = &fail;
    static ParseTree function(ParseTree) beforeElseStatement = &fail;
    static ParseTree function(ParseTree) afterElseStatement = &fail;
    static ParseTree function(ParseTree) beforeWhileStatement = &fail;
    static ParseTree function(ParseTree) afterWhileStatement = &fail;
    static ParseTree function(ParseTree) beforeDoStatement = &fail;
    static ParseTree function(ParseTree) afterDoStatement = &fail;
    static ParseTree function(ParseTree) beforeForStatement = &fail;
    static ParseTree function(ParseTree) afterForStatement = &fail;
    static ParseTree function(ParseTree) beforeInitialize = &fail;
    static ParseTree function(ParseTree) afterInitialize = &fail;
    static ParseTree function(ParseTree) beforeTest = &fail;
    static ParseTree function(ParseTree) afterTest = &fail;
    static ParseTree function(ParseTree) beforeIncrement = &fail;
    static ParseTree function(ParseTree) afterIncrement = &fail;
    static ParseTree function(ParseTree) beforeForeachStatement = &fail;
    static ParseTree function(ParseTree) afterForeachStatement = &fail;
    static ParseTree function(ParseTree) beforeForeachType = &fail;
    static ParseTree function(ParseTree) afterForeachType = &fail;
    static ParseTree function(ParseTree) beforeAggregate = &fail;
    static ParseTree function(ParseTree) afterAggregate = &fail;
    static ParseTree function(ParseTree) beforeForeachRangeStatement = &fail;
    static ParseTree function(ParseTree) afterForeachRangeStatement = &fail;
    static ParseTree function(ParseTree) beforeSwitchStatement = &fail;
    static ParseTree function(ParseTree) afterSwitchStatement = &fail;
    static ParseTree function(ParseTree) beforeCaseStatement = &fail;
    static ParseTree function(ParseTree) afterCaseStatement = &fail;
    static ParseTree function(ParseTree) beforeCaseRangeStatement = &fail;
    static ParseTree function(ParseTree) afterCaseRangeStatement = &fail;
    static ParseTree function(ParseTree) beforeDefaultStatement = &fail;
    static ParseTree function(ParseTree) afterDefaultStatement = &fail;
    static ParseTree function(ParseTree) beforeScopeStatementList = &fail;
    static ParseTree function(ParseTree) afterScopeStatementList = &fail;
    static ParseTree function(ParseTree) beforeStatementListNoCaseNoDefault = &fail;
    static ParseTree function(ParseTree) afterStatementListNoCaseNoDefault = &fail;
    static ParseTree function(ParseTree) beforeStatementNoCaseNoDefault = &fail;
    static ParseTree function(ParseTree) afterStatementNoCaseNoDefault = &fail;
    static ParseTree function(ParseTree) beforeFinalSwitchStatement = &fail;
    static ParseTree function(ParseTree) afterFinalSwitchStatement = &fail;
    static ParseTree function(ParseTree) beforeContinueStatement = &fail;
    static ParseTree function(ParseTree) afterContinueStatement = &fail;
    static ParseTree function(ParseTree) beforeBreakStatement = &fail;
    static ParseTree function(ParseTree) afterBreakStatement = &fail;
    static ParseTree function(ParseTree) beforeReturnStatement = &fail;
    static ParseTree function(ParseTree) afterReturnStatement = &fail;
    static ParseTree function(ParseTree) beforeGotoStatement = &fail;
    static ParseTree function(ParseTree) afterGotoStatement = &fail;
    static ParseTree function(ParseTree) beforeWithStatement = &fail;
    static ParseTree function(ParseTree) afterWithStatement = &fail;
    static ParseTree function(ParseTree) beforeSynchronizedStatement = &fail;
    static ParseTree function(ParseTree) afterSynchronizedStatement = &fail;
    static ParseTree function(ParseTree) beforeTryStatement = &fail;
    static ParseTree function(ParseTree) afterTryStatement = &fail;
    static ParseTree function(ParseTree) beforeCatches = &fail;
    static ParseTree function(ParseTree) afterCatches = &fail;
    static ParseTree function(ParseTree) beforeLastCatch = &fail;
    static ParseTree function(ParseTree) afterLastCatch = &fail;
    static ParseTree function(ParseTree) beforeCatch = &fail;
    static ParseTree function(ParseTree) afterCatch = &fail;
    static ParseTree function(ParseTree) beforeCatchParameter = &fail;
    static ParseTree function(ParseTree) afterCatchParameter = &fail;
    static ParseTree function(ParseTree) beforeFinallyStatement = &fail;
    static ParseTree function(ParseTree) afterFinallyStatement = &fail;
    static ParseTree function(ParseTree) beforeThrowStatement = &fail;
    static ParseTree function(ParseTree) afterThrowStatement = &fail;
    static ParseTree function(ParseTree) beforeScopeGuardStatement = &fail;
    static ParseTree function(ParseTree) afterScopeGuardStatement = &fail;
    static ParseTree function(ParseTree) beforeAsmStatement = &fail;
    static ParseTree function(ParseTree) afterAsmStatement = &fail;
    static ParseTree function(ParseTree) beforeAsmInstructionList = &fail;
    static ParseTree function(ParseTree) afterAsmInstructionList = &fail;
    static ParseTree function(ParseTree) beforePragmaStatement = &fail;
    static ParseTree function(ParseTree) afterPragmaStatement = &fail;
    static ParseTree function(ParseTree) beforeMixinStatement = &fail;
    static ParseTree function(ParseTree) afterMixinStatement = &fail;
    static ParseTree function(ParseTree) beforeExpression = &fail;
    static ParseTree function(ParseTree) afterExpression = &fail;
    static ParseTree function(ParseTree) beforeAssignExpression = &fail;
    static ParseTree function(ParseTree) afterAssignExpression = &fail;
    static ParseTree function(ParseTree) beforeOp = &fail;
    static ParseTree function(ParseTree) afterOp = &fail;
    static ParseTree function(ParseTree) beforeConditionalExpression = &fail;
    static ParseTree function(ParseTree) afterConditionalExpression = &fail;
    static ParseTree function(ParseTree) beforeOrOrExpression = &fail;
    static ParseTree function(ParseTree) afterOrOrExpression = &fail;
    static ParseTree function(ParseTree) beforeAndAndExpression = &fail;
    static ParseTree function(ParseTree) afterAndAndExpression = &fail;
    static ParseTree function(ParseTree) beforeOrExpression = &fail;
    static ParseTree function(ParseTree) afterOrExpression = &fail;
    static ParseTree function(ParseTree) beforeXorExpression = &fail;
    static ParseTree function(ParseTree) afterXorExpression = &fail;
    static ParseTree function(ParseTree) beforeAndExpression = &fail;
    static ParseTree function(ParseTree) afterAndExpression = &fail;
    static ParseTree function(ParseTree) beforeCmpExpression = &fail;
    static ParseTree function(ParseTree) afterCmpExpression = &fail;
    static ParseTree function(ParseTree) beforeEqualExpression = &fail;
    static ParseTree function(ParseTree) afterEqualExpression = &fail;
    static ParseTree function(ParseTree) beforeIdentityExpression = &fail;
    static ParseTree function(ParseTree) afterIdentityExpression = &fail;
    static ParseTree function(ParseTree) beforeRelExpression = &fail;
    static ParseTree function(ParseTree) afterRelExpression = &fail;
    static ParseTree function(ParseTree) beforeRelOp = &fail;
    static ParseTree function(ParseTree) afterRelOp = &fail;
    static ParseTree function(ParseTree) beforeInExpression = &fail;
    static ParseTree function(ParseTree) afterInExpression = &fail;
    static ParseTree function(ParseTree) beforeShiftExpression = &fail;
    static ParseTree function(ParseTree) afterShiftExpression = &fail;
    static ParseTree function(ParseTree) beforeAddExpression = &fail;
    static ParseTree function(ParseTree) afterAddExpression = &fail;
    static ParseTree function(ParseTree) beforeCatExpression = &fail;
    static ParseTree function(ParseTree) afterCatExpression = &fail;
    static ParseTree function(ParseTree) beforeMulExpression = &fail;
    static ParseTree function(ParseTree) afterMulExpression = &fail;
    static ParseTree function(ParseTree) beforeUnaryExpression = &fail;
    static ParseTree function(ParseTree) afterUnaryExpression = &fail;
    static ParseTree function(ParseTree) beforeUnaryOp = &fail;
    static ParseTree function(ParseTree) afterUnaryOp = &fail;
    static ParseTree function(ParseTree) beforeComplementExpression = &fail;
    static ParseTree function(ParseTree) afterComplementExpression = &fail;
    static ParseTree function(ParseTree) beforeNewExpression = &fail;
    static ParseTree function(ParseTree) afterNewExpression = &fail;
    static ParseTree function(ParseTree) beforeAllocatorArguments = &fail;
    static ParseTree function(ParseTree) afterAllocatorArguments = &fail;
    static ParseTree function(ParseTree) beforeArgumentList = &fail;
    static ParseTree function(ParseTree) afterArgumentList = &fail;
    static ParseTree function(ParseTree) beforeDeleteExpression = &fail;
    static ParseTree function(ParseTree) afterDeleteExpression = &fail;
    static ParseTree function(ParseTree) beforeCastExpression = &fail;
    static ParseTree function(ParseTree) afterCastExpression = &fail;
    static ParseTree function(ParseTree) beforeCastEqual = &fail;
    static ParseTree function(ParseTree) afterCastEqual = &fail;
    static ParseTree function(ParseTree) beforePowExpression = &fail;
    static ParseTree function(ParseTree) afterPowExpression = &fail;
    static ParseTree function(ParseTree) beforePostfixExpression = &fail;
    static ParseTree function(ParseTree) afterPostfixExpression = &fail;
    static ParseTree function(ParseTree) beforeIndexExpression = &fail;
    static ParseTree function(ParseTree) afterIndexExpression = &fail;
    static ParseTree function(ParseTree) beforeSliceExpression = &fail;
    static ParseTree function(ParseTree) afterSliceExpression = &fail;
    static ParseTree function(ParseTree) beforePrimaryExpression = &fail;
    static ParseTree function(ParseTree) afterPrimaryExpression = &fail;
    static ParseTree function(ParseTree) beforeStringLiterals = &fail;
    static ParseTree function(ParseTree) afterStringLiterals = &fail;
    static ParseTree function(ParseTree) beforeArrayLiteral = &fail;
    static ParseTree function(ParseTree) afterArrayLiteral = &fail;
    static ParseTree function(ParseTree) beforeAssocArrayLiteral = &fail;
    static ParseTree function(ParseTree) afterAssocArrayLiteral = &fail;
    static ParseTree function(ParseTree) beforeKeyValuePair = &fail;
    static ParseTree function(ParseTree) afterKeyValuePair = &fail;
    static ParseTree function(ParseTree) beforeLambda = &fail;
    static ParseTree function(ParseTree) afterLambda = &fail;
    static ParseTree function(ParseTree) beforeFunctionLiteral = &fail;
    static ParseTree function(ParseTree) afterFunctionLiteral = &fail;
    static ParseTree function(ParseTree) beforeParameterAttributes = &fail;
    static ParseTree function(ParseTree) afterParameterAttributes = &fail;
    static ParseTree function(ParseTree) beforeAssertExpression = &fail;
    static ParseTree function(ParseTree) afterAssertExpression = &fail;
    static ParseTree function(ParseTree) beforeMixinExpression = &fail;
    static ParseTree function(ParseTree) afterMixinExpression = &fail;
    static ParseTree function(ParseTree) beforeImportExpression = &fail;
    static ParseTree function(ParseTree) afterImportExpression = &fail;
    static ParseTree function(ParseTree) beforeTypeidExpression = &fail;
    static ParseTree function(ParseTree) afterTypeidExpression = &fail;
    static ParseTree function(ParseTree) beforeIsExpression = &fail;
    static ParseTree function(ParseTree) afterIsExpression = &fail;
    static ParseTree function(ParseTree) beforeTypeSpecialization = &fail;
    static ParseTree function(ParseTree) afterTypeSpecialization = &fail;
    static ParseTree function(ParseTree) beforeAttributeSpecifier = &fail;
    static ParseTree function(ParseTree) afterAttributeSpecifier = &fail;
    static ParseTree function(ParseTree) beforeAttribute = &fail;
    static ParseTree function(ParseTree) afterAttribute = &fail;
    static ParseTree function(ParseTree) beforeDeclarationBlock = &fail;
    static ParseTree function(ParseTree) afterDeclarationBlock = &fail;
    static ParseTree function(ParseTree) beforeLinkageAttribute = &fail;
    static ParseTree function(ParseTree) afterLinkageAttribute = &fail;
    static ParseTree function(ParseTree) beforeLinkageType = &fail;
    static ParseTree function(ParseTree) afterLinkageType = &fail;
    static ParseTree function(ParseTree) beforeAlignAttribute = &fail;
    static ParseTree function(ParseTree) afterAlignAttribute = &fail;
    static ParseTree function(ParseTree) beforeProtectionAttribute = &fail;
    static ParseTree function(ParseTree) afterProtectionAttribute = &fail;
    static ParseTree function(ParseTree) beforeClassDeclaration = &fail;
    static ParseTree function(ParseTree) afterClassDeclaration = &fail;
    static ParseTree function(ParseTree) beforeBaseClassList = &fail;
    static ParseTree function(ParseTree) afterBaseClassList = &fail;
    static ParseTree function(ParseTree) beforeClassBody = &fail;
    static ParseTree function(ParseTree) afterClassBody = &fail;
    static ParseTree function(ParseTree) beforeClassBodyDeclarations = &fail;
    static ParseTree function(ParseTree) afterClassBodyDeclarations = &fail;
    static ParseTree function(ParseTree) beforeClassBodyDeclaration = &fail;
    static ParseTree function(ParseTree) afterClassBodyDeclaration = &fail;
    static ParseTree function(ParseTree) beforeConstructor = &fail;
    static ParseTree function(ParseTree) afterConstructor = &fail;
    static ParseTree function(ParseTree) beforeDestructor = &fail;
    static ParseTree function(ParseTree) afterDestructor = &fail;
    static ParseTree function(ParseTree) beforeStaticConstructor = &fail;
    static ParseTree function(ParseTree) afterStaticConstructor = &fail;
    static ParseTree function(ParseTree) beforeStaticDestructor = &fail;
    static ParseTree function(ParseTree) afterStaticDestructor = &fail;
    static ParseTree function(ParseTree) beforeSharedStaticConstructor = &fail;
    static ParseTree function(ParseTree) afterSharedStaticConstructor = &fail;
    static ParseTree function(ParseTree) beforeSharedStaticDestructor = &fail;
    static ParseTree function(ParseTree) afterSharedStaticDestructor = &fail;
    static ParseTree function(ParseTree) beforeInvariant = &fail;
    static ParseTree function(ParseTree) afterInvariant = &fail;
    static ParseTree function(ParseTree) beforeClassAllocator = &fail;
    static ParseTree function(ParseTree) afterClassAllocator = &fail;
    static ParseTree function(ParseTree) beforeClassDeallocator = &fail;
    static ParseTree function(ParseTree) afterClassDeallocator = &fail;
    static ParseTree function(ParseTree) beforeAliasThis = &fail;
    static ParseTree function(ParseTree) afterAliasThis = &fail;
    static ParseTree function(ParseTree) beforeNewAnonClassExpression = &fail;
    static ParseTree function(ParseTree) afterNewAnonClassExpression = &fail;
    static ParseTree function(ParseTree) beforeClassArguments = &fail;
    static ParseTree function(ParseTree) afterClassArguments = &fail;
    static ParseTree function(ParseTree) beforeEnumDeclaration = &fail;
    static ParseTree function(ParseTree) afterEnumDeclaration = &fail;
    static ParseTree function(ParseTree) beforeEnumTag = &fail;
    static ParseTree function(ParseTree) afterEnumTag = &fail;
    static ParseTree function(ParseTree) beforeEnumBaseType = &fail;
    static ParseTree function(ParseTree) afterEnumBaseType = &fail;
    static ParseTree function(ParseTree) beforeEnumBody = &fail;
    static ParseTree function(ParseTree) afterEnumBody = &fail;
    static ParseTree function(ParseTree) beforeEnumMember = &fail;
    static ParseTree function(ParseTree) afterEnumMember = &fail;
    static ParseTree function(ParseTree) beforeFunctionBody = &fail;
    static ParseTree function(ParseTree) afterFunctionBody = &fail;
    static ParseTree function(ParseTree) beforeInStatement = &fail;
    static ParseTree function(ParseTree) afterInStatement = &fail;
    static ParseTree function(ParseTree) beforeOutStatement = &fail;
    static ParseTree function(ParseTree) afterOutStatement = &fail;
    static ParseTree function(ParseTree) beforeBodyStatement = &fail;
    static ParseTree function(ParseTree) afterBodyStatement = &fail;
    static ParseTree function(ParseTree) beforeAsmInstruction = &fail;
    static ParseTree function(ParseTree) afterAsmInstruction = &fail;
    static ParseTree function(ParseTree) beforeIntegerExpression = &fail;
    static ParseTree function(ParseTree) afterIntegerExpression = &fail;
    static ParseTree function(ParseTree) beforeOperand = &fail;
    static ParseTree function(ParseTree) afterOperand = &fail;
    static ParseTree function(ParseTree) beforeAsmExp = &fail;
    static ParseTree function(ParseTree) afterAsmExp = &fail;
    static ParseTree function(ParseTree) beforeAsmLogOrExp = &fail;
    static ParseTree function(ParseTree) afterAsmLogOrExp = &fail;
    static ParseTree function(ParseTree) beforeAsmLogAndExp = &fail;
    static ParseTree function(ParseTree) afterAsmLogAndExp = &fail;
    static ParseTree function(ParseTree) beforeAsmOrExp = &fail;
    static ParseTree function(ParseTree) afterAsmOrExp = &fail;
    static ParseTree function(ParseTree) beforeAsmXorExp = &fail;
    static ParseTree function(ParseTree) afterAsmXorExp = &fail;
    static ParseTree function(ParseTree) beforeAsmAndExp = &fail;
    static ParseTree function(ParseTree) afterAsmAndExp = &fail;
    static ParseTree function(ParseTree) beforeAsmEqualExp = &fail;
    static ParseTree function(ParseTree) afterAsmEqualExp = &fail;
    static ParseTree function(ParseTree) beforeAsmRelExp = &fail;
    static ParseTree function(ParseTree) afterAsmRelExp = &fail;
    static ParseTree function(ParseTree) beforeAsmShiftExp = &fail;
    static ParseTree function(ParseTree) afterAsmShiftExp = &fail;
    static ParseTree function(ParseTree) beforeAsmAddExp = &fail;
    static ParseTree function(ParseTree) afterAsmAddExp = &fail;
    static ParseTree function(ParseTree) beforeAsmMulExp = &fail;
    static ParseTree function(ParseTree) afterAsmMulExp = &fail;
    static ParseTree function(ParseTree) beforeAsmBrExp = &fail;
    static ParseTree function(ParseTree) afterAsmBrExp = &fail;
    static ParseTree function(ParseTree) beforeAsmUnaExp = &fail;
    static ParseTree function(ParseTree) afterAsmUnaExp = &fail;
    static ParseTree function(ParseTree) beforeAsmPrimaryExp = &fail;
    static ParseTree function(ParseTree) afterAsmPrimaryExp = &fail;
    static ParseTree function(ParseTree) beforeDotIdentifier = &fail;
    static ParseTree function(ParseTree) afterDotIdentifier = &fail;
    static ParseTree function(ParseTree) beforeAsmTypePrefix = &fail;
    static ParseTree function(ParseTree) afterAsmTypePrefix = &fail;
    static ParseTree function(ParseTree) beforeRegister = &fail;
    static ParseTree function(ParseTree) afterRegister = &fail;
    static ParseTree function(ParseTree) beforeOpCode = &fail;
    static ParseTree function(ParseTree) afterOpCode = &fail;
    static ParseTree function(ParseTree) beforeInterfaceDeclaration = &fail;
    static ParseTree function(ParseTree) afterInterfaceDeclaration = &fail;
    static ParseTree function(ParseTree) beforeBaseInterfaceList = &fail;
    static ParseTree function(ParseTree) afterBaseInterfaceList = &fail;
    static ParseTree function(ParseTree) beforeInterfaceBody = &fail;
    static ParseTree function(ParseTree) afterInterfaceBody = &fail;
    static ParseTree function(ParseTree) beforePragma = &fail;
    static ParseTree function(ParseTree) afterPragma = &fail;
    static ParseTree function(ParseTree) beforeAggregateDeclaration = &fail;
    static ParseTree function(ParseTree) afterAggregateDeclaration = &fail;
    static ParseTree function(ParseTree) beforeStructBody = &fail;
    static ParseTree function(ParseTree) afterStructBody = &fail;
    static ParseTree function(ParseTree) beforeStructBodyDeclarations = &fail;
    static ParseTree function(ParseTree) afterStructBodyDeclarations = &fail;
    static ParseTree function(ParseTree) beforeStructBodyDeclaration = &fail;
    static ParseTree function(ParseTree) afterStructBodyDeclaration = &fail;
    static ParseTree function(ParseTree) beforeStructAllocator = &fail;
    static ParseTree function(ParseTree) afterStructAllocator = &fail;
    static ParseTree function(ParseTree) beforeStructDeallocator = &fail;
    static ParseTree function(ParseTree) afterStructDeallocator = &fail;
    static ParseTree function(ParseTree) beforeStructPostblit = &fail;
    static ParseTree function(ParseTree) afterStructPostblit = &fail;
    static ParseTree function(ParseTree) beforeTemplateDeclaration = &fail;
    static ParseTree function(ParseTree) afterTemplateDeclaration = &fail;
    static ParseTree function(ParseTree) beforeTemplateIdentifier = &fail;
    static ParseTree function(ParseTree) afterTemplateIdentifier = &fail;
    static ParseTree function(ParseTree) beforeTemplateParameterList = &fail;
    static ParseTree function(ParseTree) afterTemplateParameterList = &fail;
    static ParseTree function(ParseTree) beforeTemplateParameter = &fail;
    static ParseTree function(ParseTree) afterTemplateParameter = &fail;
    static ParseTree function(ParseTree) beforeTemplateInstance = &fail;
    static ParseTree function(ParseTree) afterTemplateInstance = &fail;
    static ParseTree function(ParseTree) beforeTemplateArgument = &fail;
    static ParseTree function(ParseTree) afterTemplateArgument = &fail;
    static ParseTree function(ParseTree) beforeSymbol = &fail;
    static ParseTree function(ParseTree) afterSymbol = &fail;
    static ParseTree function(ParseTree) beforeSymbolTail = &fail;
    static ParseTree function(ParseTree) afterSymbolTail = &fail;
    static ParseTree function(ParseTree) beforeTemplateSingleArgument = &fail;
    static ParseTree function(ParseTree) afterTemplateSingleArgument = &fail;
    static ParseTree function(ParseTree) beforeTemplateTypeParameter = &fail;
    static ParseTree function(ParseTree) afterTemplateTypeParameter = &fail;
    static ParseTree function(ParseTree) beforeTTPSpecialization = &fail;
    static ParseTree function(ParseTree) afterTTPSpecialization = &fail;
    static ParseTree function(ParseTree) beforeTTPDefault = &fail;
    static ParseTree function(ParseTree) afterTTPDefault = &fail;
    static ParseTree function(ParseTree) beforeTemplateThisParameter = &fail;
    static ParseTree function(ParseTree) afterTemplateThisParameter = &fail;
    static ParseTree function(ParseTree) beforeTemplateValueParameter = &fail;
    static ParseTree function(ParseTree) afterTemplateValueParameter = &fail;
    static ParseTree function(ParseTree) beforeTVPSpecialization = &fail;
    static ParseTree function(ParseTree) afterTVPSpecialization = &fail;
    static ParseTree function(ParseTree) beforeTVPDefault = &fail;
    static ParseTree function(ParseTree) afterTVPDefault = &fail;
    static ParseTree function(ParseTree) beforeTemplateAliasParameter = &fail;
    static ParseTree function(ParseTree) afterTemplateAliasParameter = &fail;
    static ParseTree function(ParseTree) beforeTAPSpecialization = &fail;
    static ParseTree function(ParseTree) afterTAPSpecialization = &fail;
    static ParseTree function(ParseTree) beforeTAPDefault = &fail;
    static ParseTree function(ParseTree) afterTAPDefault = &fail;
    static ParseTree function(ParseTree) beforeTemplateTupleParameter = &fail;
    static ParseTree function(ParseTree) afterTemplateTupleParameter = &fail;
    static ParseTree function(ParseTree) beforeTemplatedConstructor = &fail;
    static ParseTree function(ParseTree) afterTemplatedConstructor = &fail;
    static ParseTree function(ParseTree) beforeClassTemplateDeclaration = &fail;
    static ParseTree function(ParseTree) afterClassTemplateDeclaration = &fail;
    static ParseTree function(ParseTree) beforeStructTemplateDeclaration = &fail;
    static ParseTree function(ParseTree) afterStructTemplateDeclaration = &fail;
    static ParseTree function(ParseTree) beforeUnionTemplateDeclaration = &fail;
    static ParseTree function(ParseTree) afterUnionTemplateDeclaration = &fail;
    static ParseTree function(ParseTree) beforeInterfaceTemplateDeclaration = &fail;
    static ParseTree function(ParseTree) afterInterfaceTemplateDeclaration = &fail;
    static ParseTree function(ParseTree) beforeConstraint = &fail;
    static ParseTree function(ParseTree) afterConstraint = &fail;
    static ParseTree function(ParseTree) beforeTemplateMixinDeclaration = &fail;
    static ParseTree function(ParseTree) afterTemplateMixinDeclaration = &fail;
    static ParseTree function(ParseTree) beforeTemplateMixin = &fail;
    static ParseTree function(ParseTree) afterTemplateMixin = &fail;
    static ParseTree function(ParseTree) beforeMixinIdentifier = &fail;
    static ParseTree function(ParseTree) afterMixinIdentifier = &fail;
    static ParseTree function(ParseTree) beforeTraitsExpression = &fail;
    static ParseTree function(ParseTree) afterTraitsExpression = &fail;
    static ParseTree function(ParseTree) beforeTraitsKeyword = &fail;
    static ParseTree function(ParseTree) afterTraitsKeyword = &fail;
    static ParseTree function(ParseTree) beforeTraitsArgument = &fail;
    static ParseTree function(ParseTree) afterTraitsArgument = &fail;
    static ParseTree function(ParseTree) beforeUnitTest = &fail;
    static ParseTree function(ParseTree) afterUnitTest = &fail;
    static ParseTree function(ParseTree) beforeConditionalDeclaration = &fail;
    static ParseTree function(ParseTree) afterConditionalDeclaration = &fail;
    static ParseTree function(ParseTree) beforeCCDeclarationBlock = &fail;
    static ParseTree function(ParseTree) afterCCDeclarationBlock = &fail;
    static ParseTree function(ParseTree) beforeDeclarations = &fail;
    static ParseTree function(ParseTree) afterDeclarations = &fail;
    static ParseTree function(ParseTree) beforeConditionalStatement = &fail;
    static ParseTree function(ParseTree) afterConditionalStatement = &fail;
    static ParseTree function(ParseTree) beforeCondition = &fail;
    static ParseTree function(ParseTree) afterCondition = &fail;
    static ParseTree function(ParseTree) beforeVersionCondition = &fail;
    static ParseTree function(ParseTree) afterVersionCondition = &fail;
    static ParseTree function(ParseTree) beforeVersionSpecification = &fail;
    static ParseTree function(ParseTree) afterVersionSpecification = &fail;
    static ParseTree function(ParseTree) beforeDebugCondition = &fail;
    static ParseTree function(ParseTree) afterDebugCondition = &fail;
    static ParseTree function(ParseTree) beforeDebugSpecification = &fail;
    static ParseTree function(ParseTree) afterDebugSpecification = &fail;
    static ParseTree function(ParseTree) beforeStaticIfCondition = &fail;
    static ParseTree function(ParseTree) afterStaticIfCondition = &fail;
    static ParseTree function(ParseTree) beforeStaticAssert = &fail;
    static ParseTree function(ParseTree) afterStaticAssert = &fail;
    static ParseTree function(ParseTree) beforeIdentifier = &fail;
    static ParseTree function(ParseTree) afterIdentifier = &fail;
    static ParseTree function(ParseTree) beforeKeyword = &fail;
    static ParseTree function(ParseTree) afterKeyword = &fail;
    static ParseTree function(ParseTree) beforeSpacing = &fail;
    static ParseTree function(ParseTree) afterSpacing = &fail;
    static ParseTree function(ParseTree) beforeComment = &fail;
    static ParseTree function(ParseTree) afterComment = &fail;
    static ParseTree function(ParseTree) beforeBlockComment = &fail;
    static ParseTree function(ParseTree) afterBlockComment = &fail;
    static ParseTree function(ParseTree) beforeLineComment = &fail;
    static ParseTree function(ParseTree) afterLineComment = &fail;
    static ParseTree function(ParseTree) beforeNestingBlockComment = &fail;
    static ParseTree function(ParseTree) afterNestingBlockComment = &fail;
    static ParseTree function(ParseTree) beforeStringLiteral = &fail;
    static ParseTree function(ParseTree) afterStringLiteral = &fail;
    static ParseTree function(ParseTree) beforeWysiwygString = &fail;
    static ParseTree function(ParseTree) afterWysiwygString = &fail;
    static ParseTree function(ParseTree) beforeAlternateWysiwygString = &fail;
    static ParseTree function(ParseTree) afterAlternateWysiwygString = &fail;
    static ParseTree function(ParseTree) beforeDoublequotedString = &fail;
    static ParseTree function(ParseTree) afterDoublequotedString = &fail;
    static ParseTree function(ParseTree) beforeDQChar = &fail;
    static ParseTree function(ParseTree) afterDQChar = &fail;
    static ParseTree function(ParseTree) beforeEscapeSequence = &fail;
    static ParseTree function(ParseTree) afterEscapeSequence = &fail;
    static ParseTree function(ParseTree) beforeStringPostfix = &fail;
    static ParseTree function(ParseTree) afterStringPostfix = &fail;
    static ParseTree function(ParseTree) beforeTokenString = &fail;
    static ParseTree function(ParseTree) afterTokenString = &fail;
    static ParseTree function(ParseTree) beforeCharacterLiteral = &fail;
    static ParseTree function(ParseTree) afterCharacterLiteral = &fail;
    static ParseTree function(ParseTree) beforeIntegerLiteral = &fail;
    static ParseTree function(ParseTree) afterIntegerLiteral = &fail;
    static ParseTree function(ParseTree) beforeDecimalInteger = &fail;
    static ParseTree function(ParseTree) afterDecimalInteger = &fail;
    static ParseTree function(ParseTree) beforeInteger = &fail;
    static ParseTree function(ParseTree) afterInteger = &fail;
    static ParseTree function(ParseTree) beforeIntegerSuffix = &fail;
    static ParseTree function(ParseTree) afterIntegerSuffix = &fail;
    static ParseTree function(ParseTree) beforeBinaryInteger = &fail;
    static ParseTree function(ParseTree) afterBinaryInteger = &fail;
    static ParseTree function(ParseTree) beforeHexadecimalInteger = &fail;
    static ParseTree function(ParseTree) afterHexadecimalInteger = &fail;
    static ParseTree function(ParseTree) beforeHexDigit = &fail;
    static ParseTree function(ParseTree) afterHexDigit = &fail;
    static ParseTree function(ParseTree) beforeFloatLiteral = &fail;
    static ParseTree function(ParseTree) afterFloatLiteral = &fail;
    static ParseTree function(ParseTree) beforeSign = &fail;
    static ParseTree function(ParseTree) afterSign = &fail;

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            mixin("ParseTree result = before" ~ name ~ "(p);
            if (result.successful)
            {
                return result;
            }
            else
            {
                result = r(p);
                if (result.successful || after" ~ name ~ " == &fail)
                {
                    return result;
                }
                result = after" ~ name ~ "(p);
                return result;
            }");
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }
    static bool isRule(string s)
    {
        switch(s)
        {
            case "D.Module":
            case "D.DeclDefs":
            case "D.DeclDef":
            case "D.MacroDeclaration":
            case "D.MacroName":
            case "D.MacroParameterList":
            case "D.MacroParameter":
            case "D.MacroLevel":
            case "D.MacroBeforeBody":
            case "D.MacroAfterBody":
            case "D.ModuleDeclaration":
            case "D.ImportDeclaration":
            case "D.ImportList":
            case "D.Import":
            case "D.ImportBindings":
            case "D.ImportBind":
            case "D.MixinDeclaration":
            case "D.Declaration":
            case "D.AliasDeclaration":
            case "D.AliasInitializer":
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
            case "D.Spacing":
            case "D.Comment":
            case "D.BlockComment":
            case "D.LineComment":
            case "D.NestingBlockComment":
            case "D.StringLiteral":
            case "D.WysiwygString":
            case "D.AlternateWysiwygString":
            case "D.DoublequotedString":
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
                if (s.length >= 8 && s[0..8] == "D.List!(") return true;
                return false;
        }
    }
    mixin decimateTree;
    static TParseTree Module(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.option!(ModuleDeclaration), pegged.peg.option!(DeclDefs)), "D.Module")(p);
        }
        else
        {
            if(auto m = tuple(`Module`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.option!(ModuleDeclaration), pegged.peg.option!(DeclDefs)), "Module"), "D.Module")(p);
                memo[tuple(`Module`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Module(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.option!(ModuleDeclaration), pegged.peg.option!(DeclDefs)), "D.Module")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.option!(ModuleDeclaration), pegged.peg.option!(DeclDefs)), "Module"), "D.Module")(TParseTree("", false,[], s));
        }
    }
    static string Module(GetName g)
    {
        return "D.Module";
    }

    static TParseTree DeclDefs(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), "D.DeclDefs")(p);
        }
        else
        {
            if(auto m = tuple(`DeclDefs`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), "DeclDefs"), "D.DeclDefs")(p);
                memo[tuple(`DeclDefs`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclDefs(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), "D.DeclDefs")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), "DeclDefs"), "D.DeclDefs")(TParseTree("", false,[], s));
        }
    }
    static string DeclDefs(GetName g)
    {
        return "D.DeclDefs";
    }

    static TParseTree DeclDef(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, EnumDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, ClassDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, InterfaceDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, AggregateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.wrapAround!(Spacing, Constructor, Spacing), pegged.peg.wrapAround!(Spacing, Destructor, Spacing), pegged.peg.wrapAround!(Spacing, UnitTest, Spacing), pegged.peg.wrapAround!(Spacing, StaticConstructor, Spacing), pegged.peg.wrapAround!(Spacing, StaticDestructor, Spacing), pegged.peg.wrapAround!(Spacing, SharedStaticConstructor, Spacing), pegged.peg.wrapAround!(Spacing, SharedStaticDestructor, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, DebugSpecification, Spacing), pegged.peg.wrapAround!(Spacing, VersionSpecification, Spacing), pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing), pegged.peg.wrapAround!(Spacing, TemplateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixinDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing), pegged.peg.wrapAround!(Spacing, MixinDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, MacroDeclaration, Spacing)), "D.DeclDef")(p);
        }
        else
        {
            if(auto m = tuple(`DeclDef`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, EnumDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, ClassDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, InterfaceDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, AggregateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.wrapAround!(Spacing, Constructor, Spacing), pegged.peg.wrapAround!(Spacing, Destructor, Spacing), pegged.peg.wrapAround!(Spacing, UnitTest, Spacing), pegged.peg.wrapAround!(Spacing, StaticConstructor, Spacing), pegged.peg.wrapAround!(Spacing, StaticDestructor, Spacing), pegged.peg.wrapAround!(Spacing, SharedStaticConstructor, Spacing), pegged.peg.wrapAround!(Spacing, SharedStaticDestructor, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, DebugSpecification, Spacing), pegged.peg.wrapAround!(Spacing, VersionSpecification, Spacing), pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing), pegged.peg.wrapAround!(Spacing, TemplateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixinDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing), pegged.peg.wrapAround!(Spacing, MixinDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, MacroDeclaration, Spacing)), "DeclDef"), "D.DeclDef")(p);
                memo[tuple(`DeclDef`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclDef(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, EnumDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, ClassDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, InterfaceDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, AggregateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.wrapAround!(Spacing, Constructor, Spacing), pegged.peg.wrapAround!(Spacing, Destructor, Spacing), pegged.peg.wrapAround!(Spacing, UnitTest, Spacing), pegged.peg.wrapAround!(Spacing, StaticConstructor, Spacing), pegged.peg.wrapAround!(Spacing, StaticDestructor, Spacing), pegged.peg.wrapAround!(Spacing, SharedStaticConstructor, Spacing), pegged.peg.wrapAround!(Spacing, SharedStaticDestructor, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, DebugSpecification, Spacing), pegged.peg.wrapAround!(Spacing, VersionSpecification, Spacing), pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing), pegged.peg.wrapAround!(Spacing, TemplateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixinDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing), pegged.peg.wrapAround!(Spacing, MixinDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, MacroDeclaration, Spacing)), "D.DeclDef")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AttributeSpecifier, Spacing), pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, EnumDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, ClassDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, InterfaceDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, AggregateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.wrapAround!(Spacing, Constructor, Spacing), pegged.peg.wrapAround!(Spacing, Destructor, Spacing), pegged.peg.wrapAround!(Spacing, UnitTest, Spacing), pegged.peg.wrapAround!(Spacing, StaticConstructor, Spacing), pegged.peg.wrapAround!(Spacing, StaticDestructor, Spacing), pegged.peg.wrapAround!(Spacing, SharedStaticConstructor, Spacing), pegged.peg.wrapAround!(Spacing, SharedStaticDestructor, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, DebugSpecification, Spacing), pegged.peg.wrapAround!(Spacing, VersionSpecification, Spacing), pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing), pegged.peg.wrapAround!(Spacing, TemplateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixinDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing), pegged.peg.wrapAround!(Spacing, MixinDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, MacroDeclaration, Spacing)), "DeclDef"), "D.DeclDef")(TParseTree("", false,[], s));
        }
    }
    static string DeclDef(GetName g)
    {
        return "D.DeclDef";
    }

    static TParseTree MacroDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, MacroName, Spacing), pegged.peg.wrapAround!(Spacing, MacroParameterList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MacroLevel, Spacing)), pegged.peg.wrapAround!(Spacing, MacroBeforeBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, MacroAfterBody, Spacing)), "D.MacroDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`MacroDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, MacroName, Spacing), pegged.peg.wrapAround!(Spacing, MacroParameterList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MacroLevel, Spacing)), pegged.peg.wrapAround!(Spacing, MacroBeforeBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, MacroAfterBody, Spacing)), "MacroDeclaration"), "D.MacroDeclaration")(p);
                memo[tuple(`MacroDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MacroDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, MacroName, Spacing), pegged.peg.wrapAround!(Spacing, MacroParameterList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MacroLevel, Spacing)), pegged.peg.wrapAround!(Spacing, MacroBeforeBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, MacroAfterBody, Spacing)), "D.MacroDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, MacroName, Spacing), pegged.peg.wrapAround!(Spacing, MacroParameterList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MacroLevel, Spacing)), pegged.peg.wrapAround!(Spacing, MacroBeforeBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, MacroAfterBody, Spacing)), "MacroDeclaration"), "D.MacroDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string MacroDeclaration(GetName g)
    {
        return "D.MacroDeclaration";
    }

    static TParseTree MacroName(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), "D.MacroName")(p);
        }
        else
        {
            if(auto m = tuple(`MacroName`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), "MacroName"), "D.MacroName")(p);
                memo[tuple(`MacroName`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MacroName(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), "D.MacroName")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), "MacroName"), "D.MacroName")(TParseTree("", false,[], s));
        }
    }
    static string MacroName(GetName g)
    {
        return "D.MacroName";
    }

    static TParseTree MacroParameterList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.MacroParameterList")(p);
        }
        else
        {
            if(auto m = tuple(`MacroParameterList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "MacroParameterList"), "D.MacroParameterList")(p);
                memo[tuple(`MacroParameterList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MacroParameterList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.MacroParameterList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "MacroParameterList"), "D.MacroParameterList")(TParseTree("", false,[], s));
        }
    }
    static string MacroParameterList(GetName g)
    {
        return "D.MacroParameterList";
    }

    static TParseTree MacroParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "D.MacroParameter")(p);
        }
        else
        {
            if(auto m = tuple(`MacroParameter`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "MacroParameter"), "D.MacroParameter")(p);
                memo[tuple(`MacroParameter`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MacroParameter(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "D.MacroParameter")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "MacroParameter"), "D.MacroParameter")(TParseTree("", false,[], s));
        }
    }
    static string MacroParameter(GetName g)
    {
        return "D.MacroParameter";
    }

    static TParseTree MacroLevel(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "D.MacroLevel")(p);
        }
        else
        {
            if(auto m = tuple(`MacroLevel`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "MacroLevel"), "D.MacroLevel")(p);
                memo[tuple(`MacroLevel`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MacroLevel(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "D.MacroLevel")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "MacroLevel"), "D.MacroLevel")(TParseTree("", false,[], s));
        }
    }
    static string MacroLevel(GetName g)
    {
        return "D.MacroLevel";
    }

    static TParseTree MacroBeforeBody(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(endOfLine, pegged.peg.literal!("}"))), pegged.peg.any))), pegged.peg.discard!(endOfLine), pegged.peg.discard!(pegged.peg.literal!("}"))), "D.MacroBeforeBody")(p);
        }
        else
        {
            if(auto m = tuple(`MacroBeforeBody`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(endOfLine, pegged.peg.literal!("}"))), pegged.peg.any))), pegged.peg.discard!(endOfLine), pegged.peg.discard!(pegged.peg.literal!("}"))), "MacroBeforeBody"), "D.MacroBeforeBody")(p);
                memo[tuple(`MacroBeforeBody`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MacroBeforeBody(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(endOfLine, pegged.peg.literal!("}"))), pegged.peg.any))), pegged.peg.discard!(endOfLine), pegged.peg.discard!(pegged.peg.literal!("}"))), "D.MacroBeforeBody")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.and!(endOfLine, pegged.peg.literal!("}"))), pegged.peg.any))), pegged.peg.discard!(endOfLine), pegged.peg.discard!(pegged.peg.literal!("}"))), "MacroBeforeBody"), "D.MacroBeforeBody")(TParseTree("", false,[], s));
        }
    }
    static string MacroBeforeBody(GetName g)
    {
        return "D.MacroBeforeBody";
    }

    static TParseTree MacroAfterBody(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.MacroAfterBody")(p);
        }
        else
        {
            if(auto m = tuple(`MacroAfterBody`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "MacroAfterBody"), "D.MacroAfterBody")(p);
                memo[tuple(`MacroAfterBody`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MacroAfterBody(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.MacroAfterBody")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "MacroAfterBody"), "D.MacroAfterBody")(TParseTree("", false,[], s));
        }
    }
    static string MacroAfterBody(GetName g)
    {
        return "D.MacroAfterBody";
    }

    static TParseTree ModuleDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.ModuleDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`ModuleDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "ModuleDeclaration"), "D.ModuleDeclaration")(p);
                memo[tuple(`ModuleDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ModuleDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.ModuleDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "ModuleDeclaration"), "D.ModuleDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string ModuleDeclaration(GetName g)
    {
        return "D.ModuleDeclaration";
    }

    static TParseTree ImportDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ImportDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`ImportDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "ImportDeclaration"), "D.ImportDeclaration")(p);
                memo[tuple(`ImportDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ImportDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ImportDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "ImportDeclaration"), "D.ImportDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string ImportDeclaration(GetName g)
    {
        return "D.ImportDeclaration";
    }

    static TParseTree ImportList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ImportBindings, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing)), Spacing)))), "D.ImportList")(p);
        }
        else
        {
            if(auto m = tuple(`ImportList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ImportBindings, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing)), Spacing)))), "ImportList"), "D.ImportList")(p);
                memo[tuple(`ImportList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ImportList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ImportBindings, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing)), Spacing)))), "D.ImportList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ImportBindings, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing)), Spacing)))), "ImportList"), "D.ImportList")(TParseTree("", false,[], s));
        }
    }
    static string ImportList(GetName g)
    {
        return "D.ImportList";
    }

    static TParseTree Import(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), "D.Import")(p);
        }
        else
        {
            if(auto m = tuple(`Import`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), "Import"), "D.Import")(p);
                memo[tuple(`Import`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Import(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), "D.Import")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), "Import"), "D.Import")(TParseTree("", false,[], s));
        }
    }
    static string Import(GetName g)
    {
        return "D.Import";
    }

    template List(alias Elem)
    {
    static TParseTree List(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "D.List!(" ~ pegged.peg.getName!(Elem) ~ ")")(p);
        }
        else
        {
            if(auto m = tuple("List!(" ~ pegged.peg.getName!(Elem) ~ ")",p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "List_1"), "D.List!(" ~ pegged.peg.getName!(Elem) ~ ")")(p);
                memo[tuple("List!(" ~ pegged.peg.getName!(Elem) ~ ")",p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree List(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "D.List!(" ~ pegged.peg.getName!(Elem) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "List_1"), "D.List!(" ~ pegged.peg.getName!(Elem) ~ ")")(TParseTree("", false,[], s));
        }
    }
    static string List(GetName g)
    {
        return "D.List!(" ~ pegged.peg.getName!(Elem) ~ ")";
    }

    }
    static TParseTree ImportBindings(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, ImportBind, Spacing)), Spacing)), "D.ImportBindings")(p);
        }
        else
        {
            if(auto m = tuple(`ImportBindings`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, ImportBind, Spacing)), Spacing)), "ImportBindings"), "D.ImportBindings")(p);
                memo[tuple(`ImportBindings`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ImportBindings(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, ImportBind, Spacing)), Spacing)), "D.ImportBindings")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, ImportBind, Spacing)), Spacing)), "ImportBindings"), "D.ImportBindings")(TParseTree("", false,[], s));
        }
    }
    static string ImportBindings(GetName g)
    {
        return "D.ImportBindings";
    }

    static TParseTree ImportBind(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "D.ImportBind")(p);
        }
        else
        {
            if(auto m = tuple(`ImportBind`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "ImportBind"), "D.ImportBind")(p);
                memo[tuple(`ImportBind`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ImportBind(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "D.ImportBind")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "ImportBind"), "D.ImportBind")(TParseTree("", false,[], s));
        }
    }
    static string ImportBind(GetName g)
    {
        return "D.ImportBind";
    }

    static TParseTree MixinDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.MixinDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`MixinDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "MixinDeclaration"), "D.MixinDeclaration")(p);
                memo[tuple(`MixinDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MixinDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.MixinDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "MixinDeclaration"), "D.MixinDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string MixinDeclaration(GetName g)
    {
        return "D.MixinDeclaration";
    }

    static TParseTree Declaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AliasDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, AliasThisDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing)), "D.Declaration")(p);
        }
        else
        {
            if(auto m = tuple(`Declaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AliasDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, AliasThisDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing)), "Declaration"), "D.Declaration")(p);
                memo[tuple(`Declaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Declaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AliasDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, AliasThisDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing)), "D.Declaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AliasDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, AliasThisDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing)), "Declaration"), "D.Declaration")(TParseTree("", false,[], s));
        }
    }
    static string Declaration(GetName g)
    {
        return "D.Declaration";
    }

    static TParseTree AliasDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing)), Spacing)), Spacing)), "D.AliasDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`AliasDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing)), Spacing)), Spacing)), "AliasDeclaration"), "D.AliasDeclaration")(p);
                memo[tuple(`AliasDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AliasDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing)), Spacing)), Spacing)), "D.AliasDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing)), Spacing)), Spacing)), "AliasDeclaration"), "D.AliasDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string AliasDeclaration(GetName g)
    {
        return "D.AliasDeclaration";
    }

    static TParseTree AliasInitializer(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "D.AliasInitializer")(p);
        }
        else
        {
            if(auto m = tuple(`AliasInitializer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "AliasInitializer"), "D.AliasInitializer")(p);
                memo[tuple(`AliasInitializer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AliasInitializer(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "D.AliasInitializer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "AliasInitializer"), "D.AliasInitializer")(TParseTree("", false,[], s));
        }
    }
    static string AliasInitializer(GetName g)
    {
        return "D.AliasInitializer";
    }

    static TParseTree AliasThisDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)), "D.AliasThisDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`AliasThisDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)), "AliasThisDeclaration"), "D.AliasThisDeclaration")(p);
                memo[tuple(`AliasThisDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AliasThisDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)), "D.AliasThisDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)), "AliasThisDeclaration"), "D.AliasThisDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string AliasThisDeclaration(GetName g)
    {
        return "D.AliasThisDeclaration";
    }

    static TParseTree Decl(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarators, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.wrapAround!(Spacing, AutoDeclaration, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "D.Decl")(p);
        }
        else
        {
            if(auto m = tuple(`Decl`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarators, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.wrapAround!(Spacing, AutoDeclaration, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "Decl"), "D.Decl")(p);
                memo[tuple(`Decl`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Decl(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarators, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.wrapAround!(Spacing, AutoDeclaration, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "D.Decl")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarators, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.wrapAround!(Spacing, AutoDeclaration, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "Decl"), "D.Decl")(TParseTree("", false,[], s));
        }
    }
    static string Decl(GetName g)
    {
        return "D.Decl";
    }

    static TParseTree Declarators(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclaratorInitializer, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing)), Spacing)), Spacing))), "D.Declarators")(p);
        }
        else
        {
            if(auto m = tuple(`Declarators`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclaratorInitializer, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing)), Spacing)), Spacing))), "Declarators"), "D.Declarators")(p);
                memo[tuple(`Declarators`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Declarators(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclaratorInitializer, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing)), Spacing)), Spacing))), "D.Declarators")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclaratorInitializer, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing)), Spacing)), Spacing))), "Declarators"), "D.Declarators")(TParseTree("", false,[], s));
        }
    }
    static string Declarators(GetName g)
    {
        return "D.Declarators";
    }

    static TParseTree DeclaratorInitializer(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "D.DeclaratorInitializer")(p);
        }
        else
        {
            if(auto m = tuple(`DeclaratorInitializer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "DeclaratorInitializer"), "D.DeclaratorInitializer")(p);
                memo[tuple(`DeclaratorInitializer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclaratorInitializer(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "D.DeclaratorInitializer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "DeclaratorInitializer"), "D.DeclaratorInitializer")(TParseTree("", false,[], s));
        }
    }
    static string DeclaratorInitializer(GetName g)
    {
        return "D.DeclaratorInitializer";
    }

    static TParseTree DeclaratorIdentifier(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "D.DeclaratorIdentifier")(p);
        }
        else
        {
            if(auto m = tuple(`DeclaratorIdentifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "DeclaratorIdentifier"), "D.DeclaratorIdentifier")(p);
                memo[tuple(`DeclaratorIdentifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclaratorIdentifier(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "D.DeclaratorIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing)), Spacing))), "DeclaratorIdentifier"), "D.DeclaratorIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string DeclaratorIdentifier(GetName g)
    {
        return "D.DeclaratorIdentifier";
    }

    static TParseTree BasicType(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.BasicType")(p);
        }
        else
        {
            if(auto m = tuple(`BasicType`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "BasicType"), "D.BasicType")(p);
                memo[tuple(`BasicType`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BasicType(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.BasicType")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "BasicType"), "D.BasicType")(TParseTree("", false,[], s));
        }
    }
    static string BasicType(GetName g)
    {
        return "D.BasicType";
    }

    static TParseTree BasicTypeX(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), "D.BasicTypeX")(p);
        }
        else
        {
            if(auto m = tuple(`BasicTypeX`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), "BasicTypeX"), "D.BasicTypeX")(p);
                memo[tuple(`BasicTypeX`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BasicTypeX(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), "D.BasicTypeX")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), "BasicTypeX"), "D.BasicTypeX")(TParseTree("", false,[], s));
        }
    }
    static string BasicTypeX(GetName g)
    {
        return "D.BasicTypeX";
    }

    static TParseTree BasicType2(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "D.BasicType2")(p);
        }
        else
        {
            if(auto m = tuple(`BasicType2`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "BasicType2"), "D.BasicType2")(p);
                memo[tuple(`BasicType2`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BasicType2(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "D.BasicType2")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "BasicType2"), "D.BasicType2")(TParseTree("", false,[], s));
        }
    }
    static string BasicType2(GetName g)
    {
        return "D.BasicType2";
    }

    static TParseTree Declarator(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "D.Declarator")(p);
        }
        else
        {
            if(auto m = tuple(`Declarator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "Declarator"), "D.Declarator")(p);
                memo[tuple(`Declarator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Declarator(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "D.Declarator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "Declarator"), "D.Declarator")(TParseTree("", false,[], s));
        }
    }
    static string Declarator(GetName g)
    {
        return "D.Declarator";
    }

    static TParseTree DeclaratorSuffixes(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffix, Spacing)), "D.DeclaratorSuffixes")(p);
        }
        else
        {
            if(auto m = tuple(`DeclaratorSuffixes`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffix, Spacing)), "DeclaratorSuffixes"), "D.DeclaratorSuffixes")(p);
                memo[tuple(`DeclaratorSuffixes`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclaratorSuffixes(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffix, Spacing)), "D.DeclaratorSuffixes")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffix, Spacing)), "DeclaratorSuffixes"), "D.DeclaratorSuffixes")(TParseTree("", false,[], s));
        }
    }
    static string DeclaratorSuffixes(GetName g)
    {
        return "D.DeclaratorSuffixes";
    }

    static TParseTree DeclaratorSuffix(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttributes, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "D.DeclaratorSuffix")(p);
        }
        else
        {
            if(auto m = tuple(`DeclaratorSuffix`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttributes, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "DeclaratorSuffix"), "D.DeclaratorSuffix")(p);
                memo[tuple(`DeclaratorSuffix`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclaratorSuffix(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttributes, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "D.DeclaratorSuffix")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttributes, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "DeclaratorSuffix"), "D.DeclaratorSuffix")(TParseTree("", false,[], s));
        }
    }
    static string DeclaratorSuffix(GetName g)
    {
        return "D.DeclaratorSuffix";
    }

    static TParseTree IdentifierList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), Spacing)))), "D.IdentifierList")(p);
        }
        else
        {
            if(auto m = tuple(`IdentifierList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), Spacing)))), "IdentifierList"), "D.IdentifierList")(p);
                memo[tuple(`IdentifierList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IdentifierList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), Spacing)))), "D.IdentifierList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), Spacing)))), "IdentifierList"), "D.IdentifierList")(TParseTree("", false,[], s));
        }
    }
    static string IdentifierList(GetName g)
    {
        return "D.IdentifierList";
    }

    static TParseTree StorageClasses(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StorageClass, Spacing)), "D.StorageClasses")(p);
        }
        else
        {
            if(auto m = tuple(`StorageClasses`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StorageClass, Spacing)), "StorageClasses"), "D.StorageClasses")(p);
                memo[tuple(`StorageClasses`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StorageClasses(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StorageClass, Spacing)), "D.StorageClasses")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StorageClass, Spacing)), "StorageClasses"), "D.StorageClasses")(TParseTree("", false,[], s));
        }
    }
    static string StorageClasses(GetName g)
    {
        return "D.StorageClasses";
    }

    static TParseTree StorageClass(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, Property, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), "D.StorageClass")(p);
        }
        else
        {
            if(auto m = tuple(`StorageClass`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, Property, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), "StorageClass"), "D.StorageClass")(p);
                memo[tuple(`StorageClass`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StorageClass(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, Property, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), "D.StorageClass")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, Property, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), "StorageClass"), "D.StorageClass")(TParseTree("", false,[], s));
        }
    }
    static string StorageClass(GetName g)
    {
        return "D.StorageClass";
    }

    static TParseTree Property(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("property"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("safe"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("trusted"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("system"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("disable"), Spacing)), Spacing)), "D.Property")(p);
        }
        else
        {
            if(auto m = tuple(`Property`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("property"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("safe"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("trusted"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("system"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("disable"), Spacing)), Spacing)), "Property"), "D.Property")(p);
                memo[tuple(`Property`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Property(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("property"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("safe"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("trusted"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("system"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("disable"), Spacing)), Spacing)), "D.Property")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("property"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("safe"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("trusted"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("system"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("disable"), Spacing)), Spacing)), "Property"), "D.Property")(TParseTree("", false,[], s));
        }
    }
    static string Property(GetName g)
    {
        return "D.Property";
    }

    static TParseTree Type(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declarator2, Spacing))), "D.Type")(p);
        }
        else
        {
            if(auto m = tuple(`Type`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declarator2, Spacing))), "Type"), "D.Type")(p);
                memo[tuple(`Type`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Type(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declarator2, Spacing))), "D.Type")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declarator2, Spacing))), "Type"), "D.Type")(TParseTree("", false,[], s));
        }
    }
    static string Type(GetName g)
    {
        return "D.Type";
    }

    static TParseTree Declarator2(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), "D.Declarator2")(p);
        }
        else
        {
            if(auto m = tuple(`Declarator2`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), "Declarator2"), "D.Declarator2")(p);
                memo[tuple(`Declarator2`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Declarator2(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), "D.Declarator2")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), "Declarator2"), "D.Declarator2")(TParseTree("", false,[], s));
        }
    }
    static string Declarator2(GetName g)
    {
        return "D.Declarator2";
    }

    static TParseTree Parameters(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.Parameters")(p);
        }
        else
        {
            if(auto m = tuple(`Parameters`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Parameters"), "D.Parameters")(p);
                memo[tuple(`Parameters`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Parameters(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.Parameters")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Parameters"), "D.Parameters")(TParseTree("", false,[], s));
        }
    }
    static string Parameters(GetName g)
    {
        return "D.Parameters";
    }

    static TParseTree ParameterList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing)))), "D.ParameterList")(p);
        }
        else
        {
            if(auto m = tuple(`ParameterList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing)))), "ParameterList"), "D.ParameterList")(p);
                memo[tuple(`ParameterList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParameterList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing)))), "D.ParameterList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing)))), "ParameterList"), "D.ParameterList")(TParseTree("", false,[], s));
        }
    }
    static string ParameterList(GetName g)
    {
        return "D.ParameterList";
    }

    static TParseTree Parameter(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultInitializerExpression, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)))), "D.Parameter")(p);
        }
        else
        {
            if(auto m = tuple(`Parameter`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultInitializerExpression, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)))), "Parameter"), "D.Parameter")(p);
                memo[tuple(`Parameter`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Parameter(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultInitializerExpression, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)))), "D.Parameter")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultInitializerExpression, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)))), "Parameter"), "D.Parameter")(TParseTree("", false,[], s));
        }
    }
    static string Parameter(GetName g)
    {
        return "D.Parameter";
    }

    static TParseTree InOut(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InOutX, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing))), "D.InOut")(p);
        }
        else
        {
            if(auto m = tuple(`InOut`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InOutX, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing))), "InOut"), "D.InOut")(p);
                memo[tuple(`InOut`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InOut(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InOutX, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing))), "D.InOut")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InOutX, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing))), "InOut"), "D.InOut")(TParseTree("", false,[], s));
        }
    }
    static string InOut(GetName g)
    {
        return "D.InOut";
    }

    static TParseTree InOutX(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in "), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), "D.InOutX")(p);
        }
        else
        {
            if(auto m = tuple(`InOutX`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in "), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), "InOutX"), "D.InOutX")(p);
                memo[tuple(`InOutX`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InOutX(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in "), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), "D.InOutX")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in "), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), "InOutX"), "D.InOutX")(TParseTree("", false,[], s));
        }
    }
    static string InOutX(GetName g)
    {
        return "D.InOutX";
    }

    static TParseTree FunctionAttributes(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "D.FunctionAttributes")(p);
        }
        else
        {
            if(auto m = tuple(`FunctionAttributes`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "FunctionAttributes"), "D.FunctionAttributes")(p);
                memo[tuple(`FunctionAttributes`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionAttributes(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "D.FunctionAttributes")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "FunctionAttributes"), "D.FunctionAttributes")(TParseTree("", false,[], s));
        }
    }
    static string FunctionAttributes(GetName g)
    {
        return "D.FunctionAttributes";
    }

    static TParseTree FunctionAttribute(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, Property, Spacing)), "D.FunctionAttribute")(p);
        }
        else
        {
            if(auto m = tuple(`FunctionAttribute`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, Property, Spacing)), "FunctionAttribute"), "D.FunctionAttribute")(p);
                memo[tuple(`FunctionAttribute`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionAttribute(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, Property, Spacing)), "D.FunctionAttribute")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, Property, Spacing)), "FunctionAttribute"), "D.FunctionAttribute")(TParseTree("", false,[], s));
        }
    }
    static string FunctionAttribute(GetName g)
    {
        return "D.FunctionAttribute";
    }

    static TParseTree MemberFunctionAttributes(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttribute, Spacing)), "D.MemberFunctionAttributes")(p);
        }
        else
        {
            if(auto m = tuple(`MemberFunctionAttributes`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttribute, Spacing)), "MemberFunctionAttributes"), "D.MemberFunctionAttributes")(p);
                memo[tuple(`MemberFunctionAttributes`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MemberFunctionAttributes(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttribute, Spacing)), "D.MemberFunctionAttributes")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttribute, Spacing)), "MemberFunctionAttributes"), "D.MemberFunctionAttributes")(TParseTree("", false,[], s));
        }
    }
    static string MemberFunctionAttributes(GetName g)
    {
        return "D.MemberFunctionAttributes";
    }

    static TParseTree MemberFunctionAttribute(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "D.MemberFunctionAttribute")(p);
        }
        else
        {
            if(auto m = tuple(`MemberFunctionAttribute`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "MemberFunctionAttribute"), "D.MemberFunctionAttribute")(p);
                memo[tuple(`MemberFunctionAttribute`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MemberFunctionAttribute(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "D.MemberFunctionAttribute")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)), "MemberFunctionAttribute"), "D.MemberFunctionAttribute")(TParseTree("", false,[], s));
        }
    }
    static string MemberFunctionAttribute(GetName g)
    {
        return "D.MemberFunctionAttribute";
    }

    static TParseTree DefaultInitializerExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), "D.DefaultInitializerExpression")(p);
        }
        else
        {
            if(auto m = tuple(`DefaultInitializerExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), "DefaultInitializerExpression"), "D.DefaultInitializerExpression")(p);
                memo[tuple(`DefaultInitializerExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DefaultInitializerExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), "D.DefaultInitializerExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), "DefaultInitializerExpression"), "D.DefaultInitializerExpression")(TParseTree("", false,[], s));
        }
    }
    static string DefaultInitializerExpression(GetName g)
    {
        return "D.DefaultInitializerExpression";
    }

    static TParseTree Initializer(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VoidInitializer, Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), "D.Initializer")(p);
        }
        else
        {
            if(auto m = tuple(`Initializer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VoidInitializer, Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), "Initializer"), "D.Initializer")(p);
                memo[tuple(`Initializer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Initializer(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VoidInitializer, Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), "D.Initializer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VoidInitializer, Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), "Initializer"), "D.Initializer")(TParseTree("", false,[], s));
        }
    }
    static string Initializer(GetName g)
    {
        return "D.Initializer";
    }

    static TParseTree NonVoidInitializer(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, ArrayInitializer, Spacing), pegged.peg.wrapAround!(Spacing, StructInitializer, Spacing)), "D.NonVoidInitializer")(p);
        }
        else
        {
            if(auto m = tuple(`NonVoidInitializer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, ArrayInitializer, Spacing), pegged.peg.wrapAround!(Spacing, StructInitializer, Spacing)), "NonVoidInitializer"), "D.NonVoidInitializer")(p);
                memo[tuple(`NonVoidInitializer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonVoidInitializer(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, ArrayInitializer, Spacing), pegged.peg.wrapAround!(Spacing, StructInitializer, Spacing)), "D.NonVoidInitializer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, ArrayInitializer, Spacing), pegged.peg.wrapAround!(Spacing, StructInitializer, Spacing)), "NonVoidInitializer"), "D.NonVoidInitializer")(TParseTree("", false,[], s));
        }
    }
    static string NonVoidInitializer(GetName g)
    {
        return "D.NonVoidInitializer";
    }

    static TParseTree ArrayInitializer(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArrayMemberInitializations, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.ArrayInitializer")(p);
        }
        else
        {
            if(auto m = tuple(`ArrayInitializer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArrayMemberInitializations, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "ArrayInitializer"), "D.ArrayInitializer")(p);
                memo[tuple(`ArrayInitializer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArrayInitializer(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArrayMemberInitializations, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.ArrayInitializer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArrayMemberInitializations, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "ArrayInitializer"), "D.ArrayInitializer")(TParseTree("", false,[], s));
        }
    }
    static string ArrayInitializer(GetName g)
    {
        return "D.ArrayInitializer";
    }

    static TParseTree ArrayMemberInitializations(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing))), Spacing))), "D.ArrayMemberInitializations")(p);
        }
        else
        {
            if(auto m = tuple(`ArrayMemberInitializations`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing))), Spacing))), "ArrayMemberInitializations"), "D.ArrayMemberInitializations")(p);
                memo[tuple(`ArrayMemberInitializations`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArrayMemberInitializations(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing))), Spacing))), "D.ArrayMemberInitializations")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing))), Spacing))), "ArrayMemberInitializations"), "D.ArrayMemberInitializations")(TParseTree("", false,[], s));
        }
    }
    static string ArrayMemberInitializations(GetName g)
    {
        return "D.ArrayMemberInitializations";
    }

    static TParseTree ArrayMemberInitialization(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "D.ArrayMemberInitialization")(p);
        }
        else
        {
            if(auto m = tuple(`ArrayMemberInitialization`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "ArrayMemberInitialization"), "D.ArrayMemberInitialization")(p);
                memo[tuple(`ArrayMemberInitialization`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArrayMemberInitialization(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "D.ArrayMemberInitialization")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "ArrayMemberInitialization"), "D.ArrayMemberInitialization")(TParseTree("", false,[], s));
        }
    }
    static string ArrayMemberInitialization(GetName g)
    {
        return "D.ArrayMemberInitialization";
    }

    static TParseTree StructInitializer(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructMemberInitializers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.StructInitializer")(p);
        }
        else
        {
            if(auto m = tuple(`StructInitializer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructMemberInitializers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "StructInitializer"), "D.StructInitializer")(p);
                memo[tuple(`StructInitializer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructInitializer(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructMemberInitializers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.StructInitializer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructMemberInitializers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "StructInitializer"), "D.StructInitializer")(TParseTree("", false,[], s));
        }
    }
    static string StructInitializer(GetName g)
    {
        return "D.StructInitializer";
    }

    static TParseTree StructMemberInitializers(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing))), Spacing))), "D.StructMemberInitializers")(p);
        }
        else
        {
            if(auto m = tuple(`StructMemberInitializers`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing))), Spacing))), "StructMemberInitializers"), "D.StructMemberInitializers")(p);
                memo[tuple(`StructMemberInitializers`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructMemberInitializers(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing))), Spacing))), "D.StructMemberInitializers")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing))), Spacing))), "StructMemberInitializers"), "D.StructMemberInitializers")(TParseTree("", false,[], s));
        }
    }
    static string StructMemberInitializers(GetName g)
    {
        return "D.StructMemberInitializers";
    }

    static TParseTree StructMemberInitializer(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)))), "D.StructMemberInitializer")(p);
        }
        else
        {
            if(auto m = tuple(`StructMemberInitializer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)))), "StructMemberInitializer"), "D.StructMemberInitializer")(p);
                memo[tuple(`StructMemberInitializer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructMemberInitializer(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)))), "D.StructMemberInitializer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)))), "StructMemberInitializer"), "D.StructMemberInitializer")(TParseTree("", false,[], s));
        }
    }
    static string StructMemberInitializer(GetName g)
    {
        return "D.StructMemberInitializer";
    }

    static TParseTree AutoDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, AutoDeclarationX, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.AutoDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`AutoDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, AutoDeclarationX, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "AutoDeclaration"), "D.AutoDeclaration")(p);
                memo[tuple(`AutoDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AutoDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, AutoDeclarationX, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.AutoDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, AutoDeclarationX, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "AutoDeclaration"), "D.AutoDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string AutoDeclaration(GetName g)
    {
        return "D.AutoDeclaration";
    }

    static TParseTree AutoDeclarationX(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing), "D.AutoDeclarationX")(p);
        }
        else
        {
            if(auto m = tuple(`AutoDeclarationX`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing), "AutoDeclarationX"), "D.AutoDeclarationX")(p);
                memo[tuple(`AutoDeclarationX`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AutoDeclarationX(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing), "D.AutoDeclarationX")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing), "AutoDeclarationX"), "D.AutoDeclarationX")(TParseTree("", false,[], s));
        }
    }
    static string AutoDeclarationX(GetName g)
    {
        return "D.AutoDeclarationX";
    }

    static TParseTree Typeof(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Typeof")(p);
        }
        else
        {
            if(auto m = tuple(`Typeof`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "Typeof"), "D.Typeof")(p);
                memo[tuple(`Typeof`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Typeof(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Typeof")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "Typeof"), "D.Typeof")(TParseTree("", false,[], s));
        }
    }
    static string Typeof(GetName g)
    {
        return "D.Typeof";
    }

    static TParseTree VoidInitializer(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), "D.VoidInitializer")(p);
        }
        else
        {
            if(auto m = tuple(`VoidInitializer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), "VoidInitializer"), "D.VoidInitializer")(p);
                memo[tuple(`VoidInitializer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VoidInitializer(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), "D.VoidInitializer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), "VoidInitializer"), "D.VoidInitializer")(TParseTree("", false,[], s));
        }
    }
    static string VoidInitializer(GetName g)
    {
        return "D.VoidInitializer";
    }

    static TParseTree Statement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "D.Statement")(p);
        }
        else
        {
            if(auto m = tuple(`Statement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "Statement"), "D.Statement")(p);
                memo[tuple(`Statement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Statement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "D.Statement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "Statement"), "D.Statement")(TParseTree("", false,[], s));
        }
    }
    static string Statement(GetName g)
    {
        return "D.Statement";
    }

    static TParseTree NoScopeNonEmptyStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.NoScopeNonEmptyStatement")(p);
        }
        else
        {
            if(auto m = tuple(`NoScopeNonEmptyStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "NoScopeNonEmptyStatement"), "D.NoScopeNonEmptyStatement")(p);
                memo[tuple(`NoScopeNonEmptyStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NoScopeNonEmptyStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.NoScopeNonEmptyStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "NoScopeNonEmptyStatement"), "D.NoScopeNonEmptyStatement")(TParseTree("", false,[], s));
        }
    }
    static string NoScopeNonEmptyStatement(GetName g)
    {
        return "D.NoScopeNonEmptyStatement";
    }

    static TParseTree NoScopeStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.NoScopeStatement")(p);
        }
        else
        {
            if(auto m = tuple(`NoScopeStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "NoScopeStatement"), "D.NoScopeStatement")(p);
                memo[tuple(`NoScopeStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NoScopeStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.NoScopeStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "NoScopeStatement"), "D.NoScopeStatement")(TParseTree("", false,[], s));
        }
    }
    static string NoScopeStatement(GetName g)
    {
        return "D.NoScopeStatement";
    }

    static TParseTree NonEmptyOrScopeBlockStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "D.NonEmptyOrScopeBlockStatement")(p);
        }
        else
        {
            if(auto m = tuple(`NonEmptyOrScopeBlockStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "NonEmptyOrScopeBlockStatement"), "D.NonEmptyOrScopeBlockStatement")(p);
                memo[tuple(`NonEmptyOrScopeBlockStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonEmptyOrScopeBlockStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "D.NonEmptyOrScopeBlockStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "NonEmptyOrScopeBlockStatement"), "D.NonEmptyOrScopeBlockStatement")(TParseTree("", false,[], s));
        }
    }
    static string NonEmptyOrScopeBlockStatement(GetName g)
    {
        return "D.NonEmptyOrScopeBlockStatement";
    }

    static TParseTree NonEmptyStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing), pegged.peg.wrapAround!(Spacing, CaseStatement, Spacing), pegged.peg.wrapAround!(Spacing, CaseRangeStatement, Spacing), pegged.peg.wrapAround!(Spacing, DefaultStatement, Spacing)), "D.NonEmptyStatement")(p);
        }
        else
        {
            if(auto m = tuple(`NonEmptyStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing), pegged.peg.wrapAround!(Spacing, CaseStatement, Spacing), pegged.peg.wrapAround!(Spacing, CaseRangeStatement, Spacing), pegged.peg.wrapAround!(Spacing, DefaultStatement, Spacing)), "NonEmptyStatement"), "D.NonEmptyStatement")(p);
                memo[tuple(`NonEmptyStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonEmptyStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing), pegged.peg.wrapAround!(Spacing, CaseStatement, Spacing), pegged.peg.wrapAround!(Spacing, CaseRangeStatement, Spacing), pegged.peg.wrapAround!(Spacing, DefaultStatement, Spacing)), "D.NonEmptyStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing), pegged.peg.wrapAround!(Spacing, CaseStatement, Spacing), pegged.peg.wrapAround!(Spacing, CaseRangeStatement, Spacing), pegged.peg.wrapAround!(Spacing, DefaultStatement, Spacing)), "NonEmptyStatement"), "D.NonEmptyStatement")(TParseTree("", false,[], s));
        }
    }
    static string NonEmptyStatement(GetName g)
    {
        return "D.NonEmptyStatement";
    }

    static TParseTree NonEmptyStatementNoCaseNoDefault(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LabeledStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, DoStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing), pegged.peg.wrapAround!(Spacing, SwitchStatement, Spacing), pegged.peg.wrapAround!(Spacing, FinalSwitchStatement, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing), pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, GotoStatement, Spacing), pegged.peg.wrapAround!(Spacing, WithStatement, Spacing), pegged.peg.wrapAround!(Spacing, SynchronizedStatement, Spacing), pegged.peg.wrapAround!(Spacing, TryStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeGuardStatement, Spacing), pegged.peg.wrapAround!(Spacing, ThrowStatement, Spacing), pegged.peg.wrapAround!(Spacing, AsmStatement, Spacing), pegged.peg.wrapAround!(Spacing, PragmaStatement, Spacing), pegged.peg.wrapAround!(Spacing, MixinStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachRangeStatement, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalStatement, Spacing), pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing), pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing)), "D.NonEmptyStatementNoCaseNoDefault")(p);
        }
        else
        {
            if(auto m = tuple(`NonEmptyStatementNoCaseNoDefault`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LabeledStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, DoStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing), pegged.peg.wrapAround!(Spacing, SwitchStatement, Spacing), pegged.peg.wrapAround!(Spacing, FinalSwitchStatement, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing), pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, GotoStatement, Spacing), pegged.peg.wrapAround!(Spacing, WithStatement, Spacing), pegged.peg.wrapAround!(Spacing, SynchronizedStatement, Spacing), pegged.peg.wrapAround!(Spacing, TryStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeGuardStatement, Spacing), pegged.peg.wrapAround!(Spacing, ThrowStatement, Spacing), pegged.peg.wrapAround!(Spacing, AsmStatement, Spacing), pegged.peg.wrapAround!(Spacing, PragmaStatement, Spacing), pegged.peg.wrapAround!(Spacing, MixinStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachRangeStatement, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalStatement, Spacing), pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing), pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing)), "NonEmptyStatementNoCaseNoDefault"), "D.NonEmptyStatementNoCaseNoDefault")(p);
                memo[tuple(`NonEmptyStatementNoCaseNoDefault`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonEmptyStatementNoCaseNoDefault(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LabeledStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, DoStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing), pegged.peg.wrapAround!(Spacing, SwitchStatement, Spacing), pegged.peg.wrapAround!(Spacing, FinalSwitchStatement, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing), pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, GotoStatement, Spacing), pegged.peg.wrapAround!(Spacing, WithStatement, Spacing), pegged.peg.wrapAround!(Spacing, SynchronizedStatement, Spacing), pegged.peg.wrapAround!(Spacing, TryStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeGuardStatement, Spacing), pegged.peg.wrapAround!(Spacing, ThrowStatement, Spacing), pegged.peg.wrapAround!(Spacing, AsmStatement, Spacing), pegged.peg.wrapAround!(Spacing, PragmaStatement, Spacing), pegged.peg.wrapAround!(Spacing, MixinStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachRangeStatement, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalStatement, Spacing), pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing), pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing)), "D.NonEmptyStatementNoCaseNoDefault")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LabeledStatement, Spacing), pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationStatement, Spacing), pegged.peg.wrapAround!(Spacing, IfStatement, Spacing), pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing), pegged.peg.wrapAround!(Spacing, DoStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing), pegged.peg.wrapAround!(Spacing, SwitchStatement, Spacing), pegged.peg.wrapAround!(Spacing, FinalSwitchStatement, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing), pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing), pegged.peg.wrapAround!(Spacing, GotoStatement, Spacing), pegged.peg.wrapAround!(Spacing, WithStatement, Spacing), pegged.peg.wrapAround!(Spacing, SynchronizedStatement, Spacing), pegged.peg.wrapAround!(Spacing, TryStatement, Spacing), pegged.peg.wrapAround!(Spacing, ScopeGuardStatement, Spacing), pegged.peg.wrapAround!(Spacing, ThrowStatement, Spacing), pegged.peg.wrapAround!(Spacing, AsmStatement, Spacing), pegged.peg.wrapAround!(Spacing, PragmaStatement, Spacing), pegged.peg.wrapAround!(Spacing, MixinStatement, Spacing), pegged.peg.wrapAround!(Spacing, ForeachRangeStatement, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalStatement, Spacing), pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing), pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing), pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing)), "NonEmptyStatementNoCaseNoDefault"), "D.NonEmptyStatementNoCaseNoDefault")(TParseTree("", false,[], s));
        }
    }
    static string NonEmptyStatementNoCaseNoDefault(GetName g)
    {
        return "D.NonEmptyStatementNoCaseNoDefault";
    }

    static TParseTree ScopeStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.ScopeStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ScopeStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "ScopeStatement"), "D.ScopeStatement")(p);
                memo[tuple(`ScopeStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ScopeStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.ScopeStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "ScopeStatement"), "D.ScopeStatement")(TParseTree("", false,[], s));
        }
    }
    static string ScopeStatement(GetName g)
    {
        return "D.ScopeStatement";
    }

    static TParseTree ScopeBlockStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "D.ScopeBlockStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ScopeBlockStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "ScopeBlockStatement"), "D.ScopeBlockStatement")(p);
                memo[tuple(`ScopeBlockStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ScopeBlockStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "D.ScopeBlockStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "ScopeBlockStatement"), "D.ScopeBlockStatement")(TParseTree("", false,[], s));
        }
    }
    static string ScopeBlockStatement(GetName g)
    {
        return "D.ScopeBlockStatement";
    }

    static TParseTree LabeledStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing)), "D.LabeledStatement")(p);
        }
        else
        {
            if(auto m = tuple(`LabeledStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing)), "LabeledStatement"), "D.LabeledStatement")(p);
                memo[tuple(`LabeledStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LabeledStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing)), "D.LabeledStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing)), "LabeledStatement"), "D.LabeledStatement")(TParseTree("", false,[], s));
        }
    }
    static string LabeledStatement(GetName g)
    {
        return "D.LabeledStatement";
    }

    static TParseTree BlockStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.BlockStatement")(p);
        }
        else
        {
            if(auto m = tuple(`BlockStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "BlockStatement"), "D.BlockStatement")(p);
                memo[tuple(`BlockStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BlockStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.BlockStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "BlockStatement"), "D.BlockStatement")(TParseTree("", false,[], s));
        }
    }
    static string BlockStatement(GetName g)
    {
        return "D.BlockStatement";
    }

    static TParseTree StatementList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "D.StatementList")(p);
        }
        else
        {
            if(auto m = tuple(`StatementList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "StatementList"), "D.StatementList")(p);
                memo[tuple(`StatementList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StatementList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "D.StatementList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)), "StatementList"), "D.StatementList")(TParseTree("", false,[], s));
        }
    }
    static string StatementList(GetName g)
    {
        return "D.StatementList";
    }

    static TParseTree ExpressionStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.ExpressionStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ExpressionStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "ExpressionStatement"), "D.ExpressionStatement")(p);
                memo[tuple(`ExpressionStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExpressionStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.ExpressionStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "ExpressionStatement"), "D.ExpressionStatement")(TParseTree("", false,[], s));
        }
    }
    static string ExpressionStatement(GetName g)
    {
        return "D.ExpressionStatement";
    }

    static TParseTree DeclarationStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), "D.DeclarationStatement")(p);
        }
        else
        {
            if(auto m = tuple(`DeclarationStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), "DeclarationStatement"), "D.DeclarationStatement")(p);
                memo[tuple(`DeclarationStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclarationStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), "D.DeclarationStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), "DeclarationStatement"), "D.DeclarationStatement")(TParseTree("", false,[], s));
        }
    }
    static string DeclarationStatement(GetName g)
    {
        return "D.DeclarationStatement";
    }

    static TParseTree IfStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IfCondition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ThenStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, ElseStatement, Spacing)), Spacing))), "D.IfStatement")(p);
        }
        else
        {
            if(auto m = tuple(`IfStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IfCondition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ThenStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, ElseStatement, Spacing)), Spacing))), "IfStatement"), "D.IfStatement")(p);
                memo[tuple(`IfStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IfStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IfCondition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ThenStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, ElseStatement, Spacing)), Spacing))), "D.IfStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IfCondition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ThenStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, ElseStatement, Spacing)), Spacing))), "IfStatement"), "D.IfStatement")(TParseTree("", false,[], s));
        }
    }
    static string IfStatement(GetName g)
    {
        return "D.IfStatement";
    }

    static TParseTree IfCondition(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.IfCondition")(p);
        }
        else
        {
            if(auto m = tuple(`IfCondition`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "IfCondition"), "D.IfCondition")(p);
                memo[tuple(`IfCondition`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IfCondition(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.IfCondition")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "IfCondition"), "D.IfCondition")(TParseTree("", false,[], s));
        }
    }
    static string IfCondition(GetName g)
    {
        return "D.IfCondition";
    }

    static TParseTree ThenStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "D.ThenStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ThenStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "ThenStatement"), "D.ThenStatement")(p);
                memo[tuple(`ThenStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ThenStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "D.ThenStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "ThenStatement"), "D.ThenStatement")(TParseTree("", false,[], s));
        }
    }
    static string ThenStatement(GetName g)
    {
        return "D.ThenStatement";
    }

    static TParseTree ElseStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "D.ElseStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ElseStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "ElseStatement"), "D.ElseStatement")(p);
                memo[tuple(`ElseStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ElseStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "D.ElseStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), "ElseStatement"), "D.ElseStatement")(TParseTree("", false,[], s));
        }
    }
    static string ElseStatement(GetName g)
    {
        return "D.ElseStatement";
    }

    static TParseTree WhileStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.WhileStatement")(p);
        }
        else
        {
            if(auto m = tuple(`WhileStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "WhileStatement"), "D.WhileStatement")(p);
                memo[tuple(`WhileStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree WhileStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.WhileStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "WhileStatement"), "D.WhileStatement")(TParseTree("", false,[], s));
        }
    }
    static string WhileStatement(GetName g)
    {
        return "D.WhileStatement";
    }

    static TParseTree DoStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.DoStatement")(p);
        }
        else
        {
            if(auto m = tuple(`DoStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DoStatement"), "D.DoStatement")(p);
                memo[tuple(`DoStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DoStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.DoStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DoStatement"), "D.DoStatement")(TParseTree("", false,[], s));
        }
    }
    static string DoStatement(GetName g)
    {
        return "D.DoStatement";
    }

    static TParseTree ForStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Test, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.ForStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ForStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Test, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "ForStatement"), "D.ForStatement")(p);
                memo[tuple(`ForStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Test, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.ForStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Test, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "ForStatement"), "D.ForStatement")(TParseTree("", false,[], s));
        }
    }
    static string ForStatement(GetName g)
    {
        return "D.ForStatement";
    }

    static TParseTree Initialize(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "D.Initialize")(p);
        }
        else
        {
            if(auto m = tuple(`Initialize`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "Initialize"), "D.Initialize")(p);
                memo[tuple(`Initialize`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Initialize(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "D.Initialize")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "Initialize"), "D.Initialize")(TParseTree("", false,[], s));
        }
    }
    static string Initialize(GetName g)
    {
        return "D.Initialize";
    }

    static TParseTree Test(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "D.Test")(p);
        }
        else
        {
            if(auto m = tuple(`Test`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "Test"), "D.Test")(p);
                memo[tuple(`Test`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Test(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "D.Test")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "Test"), "D.Test")(TParseTree("", false,[], s));
        }
    }
    static string Test(GetName g)
    {
        return "D.Test";
    }

    static TParseTree Increment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "D.Increment")(p);
        }
        else
        {
            if(auto m = tuple(`Increment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "Increment"), "D.Increment")(p);
                memo[tuple(`Increment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Increment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "D.Increment")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "Increment"), "D.Increment")(TParseTree("", false,[], s));
        }
    }
    static string Increment(GetName g)
    {
        return "D.Increment";
    }

    static TParseTree ForeachStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, ForeachType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Aggregate, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "D.ForeachStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ForeachStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, ForeachType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Aggregate, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "ForeachStatement"), "D.ForeachStatement")(p);
                memo[tuple(`ForeachStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForeachStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, ForeachType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Aggregate, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "D.ForeachStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, ForeachType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Aggregate, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "ForeachStatement"), "D.ForeachStatement")(TParseTree("", false,[], s));
        }
    }
    static string ForeachStatement(GetName g)
    {
        return "D.ForeachStatement";
    }

    static TParseTree ForeachType(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.ForeachType")(p);
        }
        else
        {
            if(auto m = tuple(`ForeachType`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "ForeachType"), "D.ForeachType")(p);
                memo[tuple(`ForeachType`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForeachType(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.ForeachType")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "ForeachType"), "D.ForeachType")(TParseTree("", false,[], s));
        }
    }
    static string ForeachType(GetName g)
    {
        return "D.ForeachType";
    }

    static TParseTree Aggregate(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "D.Aggregate")(p);
        }
        else
        {
            if(auto m = tuple(`Aggregate`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "Aggregate"), "D.Aggregate")(p);
                memo[tuple(`Aggregate`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Aggregate(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "D.Aggregate")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), "Aggregate"), "D.Aggregate")(TParseTree("", false,[], s));
        }
    }
    static string Aggregate(GetName g)
    {
        return "D.Aggregate";
    }

    static TParseTree ForeachRangeStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.ForeachRangeStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ForeachRangeStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "ForeachRangeStatement"), "D.ForeachRangeStatement")(p);
                memo[tuple(`ForeachRangeStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForeachRangeStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.ForeachRangeStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "ForeachRangeStatement"), "D.ForeachRangeStatement")(TParseTree("", false,[], s));
        }
    }
    static string ForeachRangeStatement(GetName g)
    {
        return "D.ForeachRangeStatement";
    }

    static TParseTree SwitchStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.SwitchStatement")(p);
        }
        else
        {
            if(auto m = tuple(`SwitchStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "SwitchStatement"), "D.SwitchStatement")(p);
                memo[tuple(`SwitchStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SwitchStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.SwitchStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "SwitchStatement"), "D.SwitchStatement")(TParseTree("", false,[], s));
        }
    }
    static string SwitchStatement(GetName g)
    {
        return "D.SwitchStatement";
    }

    static TParseTree CaseStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "D.CaseStatement")(p);
        }
        else
        {
            if(auto m = tuple(`CaseStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "CaseStatement"), "D.CaseStatement")(p);
                memo[tuple(`CaseStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CaseStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "D.CaseStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "CaseStatement"), "D.CaseStatement")(TParseTree("", false,[], s));
        }
    }
    static string CaseStatement(GetName g)
    {
        return "D.CaseStatement";
    }

    static TParseTree CaseRangeStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "D.CaseRangeStatement")(p);
        }
        else
        {
            if(auto m = tuple(`CaseRangeStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "CaseRangeStatement"), "D.CaseRangeStatement")(p);
                memo[tuple(`CaseRangeStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CaseRangeStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "D.CaseRangeStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "CaseRangeStatement"), "D.CaseRangeStatement")(TParseTree("", false,[], s));
        }
    }
    static string CaseRangeStatement(GetName g)
    {
        return "D.CaseRangeStatement";
    }

    static TParseTree DefaultStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "D.DefaultStatement")(p);
        }
        else
        {
            if(auto m = tuple(`DefaultStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "DefaultStatement"), "D.DefaultStatement")(p);
                memo[tuple(`DefaultStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DefaultStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "D.DefaultStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing)), "DefaultStatement"), "D.DefaultStatement")(TParseTree("", false,[], s));
        }
    }
    static string DefaultStatement(GetName g)
    {
        return "D.DefaultStatement";
    }

    static TParseTree ScopeStatementList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, StatementListNoCaseNoDefault, Spacing), "D.ScopeStatementList")(p);
        }
        else
        {
            if(auto m = tuple(`ScopeStatementList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, StatementListNoCaseNoDefault, Spacing), "ScopeStatementList"), "D.ScopeStatementList")(p);
                memo[tuple(`ScopeStatementList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ScopeStatementList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, StatementListNoCaseNoDefault, Spacing), "D.ScopeStatementList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, StatementListNoCaseNoDefault, Spacing), "ScopeStatementList"), "D.ScopeStatementList")(TParseTree("", false,[], s));
        }
    }
    static string ScopeStatementList(GetName g)
    {
        return "D.ScopeStatementList";
    }

    static TParseTree StatementListNoCaseNoDefault(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StatementNoCaseNoDefault, Spacing)), "D.StatementListNoCaseNoDefault")(p);
        }
        else
        {
            if(auto m = tuple(`StatementListNoCaseNoDefault`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StatementNoCaseNoDefault, Spacing)), "StatementListNoCaseNoDefault"), "D.StatementListNoCaseNoDefault")(p);
                memo[tuple(`StatementListNoCaseNoDefault`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StatementListNoCaseNoDefault(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StatementNoCaseNoDefault, Spacing)), "D.StatementListNoCaseNoDefault")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StatementNoCaseNoDefault, Spacing)), "StatementListNoCaseNoDefault"), "D.StatementListNoCaseNoDefault")(TParseTree("", false,[], s));
        }
    }
    static string StatementListNoCaseNoDefault(GetName g)
    {
        return "D.StatementListNoCaseNoDefault";
    }

    static TParseTree StatementNoCaseNoDefault(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "D.StatementNoCaseNoDefault")(p);
        }
        else
        {
            if(auto m = tuple(`StatementNoCaseNoDefault`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "StatementNoCaseNoDefault"), "D.StatementNoCaseNoDefault")(p);
                memo[tuple(`StatementNoCaseNoDefault`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StatementNoCaseNoDefault(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "D.StatementNoCaseNoDefault")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing), pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing)), "StatementNoCaseNoDefault"), "D.StatementNoCaseNoDefault")(TParseTree("", false,[], s));
        }
    }
    static string StatementNoCaseNoDefault(GetName g)
    {
        return "D.StatementNoCaseNoDefault";
    }

    static TParseTree FinalSwitchStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.FinalSwitchStatement")(p);
        }
        else
        {
            if(auto m = tuple(`FinalSwitchStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "FinalSwitchStatement"), "D.FinalSwitchStatement")(p);
                memo[tuple(`FinalSwitchStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FinalSwitchStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.FinalSwitchStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "FinalSwitchStatement"), "D.FinalSwitchStatement")(TParseTree("", false,[], s));
        }
    }
    static string FinalSwitchStatement(GetName g)
    {
        return "D.FinalSwitchStatement";
    }

    static TParseTree ContinueStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.ContinueStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ContinueStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "ContinueStatement"), "D.ContinueStatement")(p);
                memo[tuple(`ContinueStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ContinueStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.ContinueStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "ContinueStatement"), "D.ContinueStatement")(TParseTree("", false,[], s));
        }
    }
    static string ContinueStatement(GetName g)
    {
        return "D.ContinueStatement";
    }

    static TParseTree BreakStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.BreakStatement")(p);
        }
        else
        {
            if(auto m = tuple(`BreakStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "BreakStatement"), "D.BreakStatement")(p);
                memo[tuple(`BreakStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BreakStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.BreakStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "BreakStatement"), "D.BreakStatement")(TParseTree("", false,[], s));
        }
    }
    static string BreakStatement(GetName g)
    {
        return "D.BreakStatement";
    }

    static TParseTree ReturnStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.ReturnStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ReturnStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "ReturnStatement"), "D.ReturnStatement")(p);
                memo[tuple(`ReturnStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReturnStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.ReturnStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "ReturnStatement"), "D.ReturnStatement")(TParseTree("", false,[], s));
        }
    }
    static string ReturnStatement(GetName g)
    {
        return "D.ReturnStatement";
    }

    static TParseTree GotoStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing)), "D.GotoStatement")(p);
        }
        else
        {
            if(auto m = tuple(`GotoStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing)), "GotoStatement"), "D.GotoStatement")(p);
                memo[tuple(`GotoStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree GotoStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing)), "D.GotoStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing)), "GotoStatement"), "D.GotoStatement")(TParseTree("", false,[], s));
        }
    }
    static string GotoStatement(GetName g)
    {
        return "D.GotoStatement";
    }

    static TParseTree WithStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, Symbol, Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.WithStatement")(p);
        }
        else
        {
            if(auto m = tuple(`WithStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, Symbol, Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "WithStatement"), "D.WithStatement")(p);
                memo[tuple(`WithStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree WithStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, Symbol, Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.WithStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, Symbol, Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "WithStatement"), "D.WithStatement")(TParseTree("", false,[], s));
        }
    }
    static string WithStatement(GetName g)
    {
        return "D.WithStatement";
    }

    static TParseTree SynchronizedStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.SynchronizedStatement")(p);
        }
        else
        {
            if(auto m = tuple(`SynchronizedStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "SynchronizedStatement"), "D.SynchronizedStatement")(p);
                memo[tuple(`SynchronizedStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SynchronizedStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "D.SynchronizedStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing)), "SynchronizedStatement"), "D.SynchronizedStatement")(TParseTree("", false,[], s));
        }
    }
    static string SynchronizedStatement(GetName g)
    {
        return "D.SynchronizedStatement";
    }

    static TParseTree TryStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FinallyStatement, Spacing))), "D.TryStatement")(p);
        }
        else
        {
            if(auto m = tuple(`TryStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FinallyStatement, Spacing))), "TryStatement"), "D.TryStatement")(p);
                memo[tuple(`TryStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TryStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FinallyStatement, Spacing))), "D.TryStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FinallyStatement, Spacing))), "TryStatement"), "D.TryStatement")(TParseTree("", false,[], s));
        }
    }
    static string TryStatement(GetName g)
    {
        return "D.TryStatement";
    }

    static TParseTree Catches(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LastCatch, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Catch, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)))), "D.Catches")(p);
        }
        else
        {
            if(auto m = tuple(`Catches`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LastCatch, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Catch, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)))), "Catches"), "D.Catches")(p);
                memo[tuple(`Catches`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Catches(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LastCatch, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Catch, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)))), "D.Catches")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LastCatch, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Catch, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)))), "Catches"), "D.Catches")(TParseTree("", false,[], s));
        }
    }
    static string Catches(GetName g)
    {
        return "D.Catches";
    }

    static TParseTree LastCatch(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "D.LastCatch")(p);
        }
        else
        {
            if(auto m = tuple(`LastCatch`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "LastCatch"), "D.LastCatch")(p);
                memo[tuple(`LastCatch`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LastCatch(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "D.LastCatch")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "LastCatch"), "D.LastCatch")(TParseTree("", false,[], s));
        }
    }
    static string LastCatch(GetName g)
    {
        return "D.LastCatch";
    }

    static TParseTree Catch(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CatchParameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "D.Catch")(p);
        }
        else
        {
            if(auto m = tuple(`Catch`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CatchParameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "Catch"), "D.Catch")(p);
                memo[tuple(`Catch`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Catch(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CatchParameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "D.Catch")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CatchParameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "Catch"), "D.Catch")(TParseTree("", false,[], s));
        }
    }
    static string Catch(GetName g)
    {
        return "D.Catch";
    }

    static TParseTree CatchParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "D.CatchParameter")(p);
        }
        else
        {
            if(auto m = tuple(`CatchParameter`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "CatchParameter"), "D.CatchParameter")(p);
                memo[tuple(`CatchParameter`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CatchParameter(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "D.CatchParameter")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "CatchParameter"), "D.CatchParameter")(TParseTree("", false,[], s));
        }
    }
    static string CatchParameter(GetName g)
    {
        return "D.CatchParameter";
    }

    static TParseTree FinallyStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "D.FinallyStatement")(p);
        }
        else
        {
            if(auto m = tuple(`FinallyStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "FinallyStatement"), "D.FinallyStatement")(p);
                memo[tuple(`FinallyStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FinallyStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "D.FinallyStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), "FinallyStatement"), "D.FinallyStatement")(TParseTree("", false,[], s));
        }
    }
    static string FinallyStatement(GetName g)
    {
        return "D.FinallyStatement";
    }

    static TParseTree ThrowStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.ThrowStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ThrowStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "ThrowStatement"), "D.ThrowStatement")(p);
                memo[tuple(`ThrowStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ThrowStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.ThrowStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "ThrowStatement"), "D.ThrowStatement")(TParseTree("", false,[], s));
        }
    }
    static string ThrowStatement(GetName g)
    {
        return "D.ThrowStatement";
    }

    static TParseTree ScopeGuardStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(exit)"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(success)"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(failure)"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyOrScopeBlockStatement, Spacing)), "D.ScopeGuardStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ScopeGuardStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(exit)"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(success)"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(failure)"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyOrScopeBlockStatement, Spacing)), "ScopeGuardStatement"), "D.ScopeGuardStatement")(p);
                memo[tuple(`ScopeGuardStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ScopeGuardStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(exit)"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(success)"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(failure)"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyOrScopeBlockStatement, Spacing)), "D.ScopeGuardStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(exit)"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(success)"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(failure)"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyOrScopeBlockStatement, Spacing)), "ScopeGuardStatement"), "D.ScopeGuardStatement")(TParseTree("", false,[], s));
        }
    }
    static string ScopeGuardStatement(GetName g)
    {
        return "D.ScopeGuardStatement";
    }

    static TParseTree AsmStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.AsmStatement")(p);
        }
        else
        {
            if(auto m = tuple(`AsmStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "AsmStatement"), "D.AsmStatement")(p);
                memo[tuple(`AsmStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.AsmStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "AsmStatement"), "D.AsmStatement")(TParseTree("", false,[], s));
        }
    }
    static string AsmStatement(GetName g)
    {
        return "D.AsmStatement";
    }

    static TParseTree AsmInstructionList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing))), "D.AsmInstructionList")(p);
        }
        else
        {
            if(auto m = tuple(`AsmInstructionList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing))), "AsmInstructionList"), "D.AsmInstructionList")(p);
                memo[tuple(`AsmInstructionList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmInstructionList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing))), "D.AsmInstructionList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing))), "AsmInstructionList"), "D.AsmInstructionList")(TParseTree("", false,[], s));
        }
    }
    static string AsmInstructionList(GetName g)
    {
        return "D.AsmInstructionList";
    }

    static TParseTree PragmaStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing)), "D.PragmaStatement")(p);
        }
        else
        {
            if(auto m = tuple(`PragmaStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing)), "PragmaStatement"), "D.PragmaStatement")(p);
                memo[tuple(`PragmaStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PragmaStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing)), "D.PragmaStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing)), "PragmaStatement"), "D.PragmaStatement")(TParseTree("", false,[], s));
        }
    }
    static string PragmaStatement(GetName g)
    {
        return "D.PragmaStatement";
    }

    static TParseTree MixinStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.MixinStatement")(p);
        }
        else
        {
            if(auto m = tuple(`MixinStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "MixinStatement"), "D.MixinStatement")(p);
                memo[tuple(`MixinStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MixinStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.MixinStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "MixinStatement"), "D.MixinStatement")(TParseTree("", false,[], s));
        }
    }
    static string MixinStatement(GetName g)
    {
        return "D.MixinStatement";
    }

    static TParseTree Expression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), "D.Expression")(p);
        }
        else
        {
            if(auto m = tuple(`Expression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), "Expression"), "D.Expression")(p);
                memo[tuple(`Expression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Expression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), "D.Expression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), "Expression"), "D.Expression")(TParseTree("", false,[], s));
        }
    }
    static string Expression(GetName g)
    {
        return "D.Expression";
    }

    static TParseTree AssignExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Op, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing))), "D.AssignExpression")(p);
        }
        else
        {
            if(auto m = tuple(`AssignExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Op, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing))), "AssignExpression"), "D.AssignExpression")(p);
                memo[tuple(`AssignExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AssignExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Op, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing))), "D.AssignExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Op, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing))), "AssignExpression"), "D.AssignExpression")(TParseTree("", false,[], s));
        }
    }
    static string AssignExpression(GetName g)
    {
        return "D.AssignExpression";
    }

    static TParseTree Op(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing)), "D.Op")(p);
        }
        else
        {
            if(auto m = tuple(`Op`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing)), "Op"), "D.Op")(p);
                memo[tuple(`Op`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Op(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing)), "D.Op")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing)), "Op"), "D.Op")(TParseTree("", false,[], s));
        }
    }
    static string Op(GetName g)
    {
        return "D.Op";
    }

    static TParseTree ConditionalExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing))), "D.ConditionalExpression")(p);
        }
        else
        {
            if(auto m = tuple(`ConditionalExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing))), "ConditionalExpression"), "D.ConditionalExpression")(p);
                memo[tuple(`ConditionalExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConditionalExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing))), "D.ConditionalExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing))), "ConditionalExpression"), "D.ConditionalExpression")(TParseTree("", false,[], s));
        }
    }
    static string ConditionalExpression(GetName g)
    {
        return "D.ConditionalExpression";
    }

    static TParseTree OrOrExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing)), Spacing))), "D.OrOrExpression")(p);
        }
        else
        {
            if(auto m = tuple(`OrOrExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing)), Spacing))), "OrOrExpression"), "D.OrOrExpression")(p);
                memo[tuple(`OrOrExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OrOrExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing)), Spacing))), "D.OrOrExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing)), Spacing))), "OrOrExpression"), "D.OrOrExpression")(TParseTree("", false,[], s));
        }
    }
    static string OrOrExpression(GetName g)
    {
        return "D.OrOrExpression";
    }

    static TParseTree AndAndExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CmpExpression, Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing)), Spacing))), "D.AndAndExpression")(p);
        }
        else
        {
            if(auto m = tuple(`AndAndExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CmpExpression, Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing)), Spacing))), "AndAndExpression"), "D.AndAndExpression")(p);
                memo[tuple(`AndAndExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AndAndExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CmpExpression, Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing)), Spacing))), "D.AndAndExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CmpExpression, Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing)), Spacing))), "AndAndExpression"), "D.AndAndExpression")(TParseTree("", false,[], s));
        }
    }
    static string AndAndExpression(GetName g)
    {
        return "D.AndAndExpression";
    }

    static TParseTree OrExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, XorExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing)), Spacing))), "D.OrExpression")(p);
        }
        else
        {
            if(auto m = tuple(`OrExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, XorExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing)), Spacing))), "OrExpression"), "D.OrExpression")(p);
                memo[tuple(`OrExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OrExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, XorExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing)), Spacing))), "D.OrExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, XorExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing)), Spacing))), "OrExpression"), "D.OrExpression")(TParseTree("", false,[], s));
        }
    }
    static string OrExpression(GetName g)
    {
        return "D.OrExpression";
    }

    static TParseTree XorExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, XorExpression, Spacing)), Spacing))), "D.XorExpression")(p);
        }
        else
        {
            if(auto m = tuple(`XorExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, XorExpression, Spacing)), Spacing))), "XorExpression"), "D.XorExpression")(p);
                memo[tuple(`XorExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree XorExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, XorExpression, Spacing)), Spacing))), "D.XorExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, XorExpression, Spacing)), Spacing))), "XorExpression"), "D.XorExpression")(TParseTree("", false,[], s));
        }
    }
    static string XorExpression(GetName g)
    {
        return "D.XorExpression";
    }

    static TParseTree AndExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AndExpression, Spacing)), Spacing))), "D.AndExpression")(p);
        }
        else
        {
            if(auto m = tuple(`AndExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AndExpression, Spacing)), Spacing))), "AndExpression"), "D.AndExpression")(p);
                memo[tuple(`AndExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AndExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AndExpression, Spacing)), Spacing))), "D.AndExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AndExpression, Spacing)), Spacing))), "AndExpression"), "D.AndExpression")(TParseTree("", false,[], s));
        }
    }
    static string AndExpression(GetName g)
    {
        return "D.AndExpression";
    }

    static TParseTree CmpExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, EqualExpression, Spacing), pegged.peg.wrapAround!(Spacing, IdentityExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelExpression, Spacing), pegged.peg.wrapAround!(Spacing, InExpression, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "D.CmpExpression")(p);
        }
        else
        {
            if(auto m = tuple(`CmpExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, EqualExpression, Spacing), pegged.peg.wrapAround!(Spacing, IdentityExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelExpression, Spacing), pegged.peg.wrapAround!(Spacing, InExpression, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "CmpExpression"), "D.CmpExpression")(p);
                memo[tuple(`CmpExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CmpExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, EqualExpression, Spacing), pegged.peg.wrapAround!(Spacing, IdentityExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelExpression, Spacing), pegged.peg.wrapAround!(Spacing, InExpression, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "D.CmpExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, EqualExpression, Spacing), pegged.peg.wrapAround!(Spacing, IdentityExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelExpression, Spacing), pegged.peg.wrapAround!(Spacing, InExpression, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "CmpExpression"), "D.CmpExpression")(TParseTree("", false,[], s));
        }
    }
    static string CmpExpression(GetName g)
    {
        return "D.CmpExpression";
    }

    static TParseTree EqualExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "D.EqualExpression")(p);
        }
        else
        {
            if(auto m = tuple(`EqualExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "EqualExpression"), "D.EqualExpression")(p);
                memo[tuple(`EqualExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EqualExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "D.EqualExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "EqualExpression"), "D.EqualExpression")(TParseTree("", false,[], s));
        }
    }
    static string EqualExpression(GetName g)
    {
        return "D.EqualExpression";
    }

    static TParseTree IdentityExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "D.IdentityExpression")(p);
        }
        else
        {
            if(auto m = tuple(`IdentityExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "IdentityExpression"), "D.IdentityExpression")(p);
                memo[tuple(`IdentityExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IdentityExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "D.IdentityExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "IdentityExpression"), "D.IdentityExpression")(TParseTree("", false,[], s));
        }
    }
    static string IdentityExpression(GetName g)
    {
        return "D.IdentityExpression";
    }

    static TParseTree RelExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelOp, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "D.RelExpression")(p);
        }
        else
        {
            if(auto m = tuple(`RelExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelOp, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "RelExpression"), "D.RelExpression")(p);
                memo[tuple(`RelExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RelExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelOp, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "D.RelExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelOp, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), "RelExpression"), "D.RelExpression")(TParseTree("", false,[], s));
        }
    }
    static string RelExpression(GetName g)
    {
        return "D.RelExpression";
    }

    static TParseTree RelOp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "D.RelOp")(p);
        }
        else
        {
            if(auto m = tuple(`RelOp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "RelOp"), "D.RelOp")(p);
                memo[tuple(`RelOp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RelOp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "D.RelOp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), "RelOp"), "D.RelOp")(TParseTree("", false,[], s));
        }
    }
    static string RelOp(GetName g)
    {
        return "D.RelOp";
    }

    static TParseTree InExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), Spacing))), "D.InExpression")(p);
        }
        else
        {
            if(auto m = tuple(`InExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), Spacing))), "InExpression"), "D.InExpression")(p);
                memo[tuple(`InExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), Spacing))), "D.InExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing)), Spacing))), "InExpression"), "D.InExpression")(TParseTree("", false,[], s));
        }
    }
    static string InExpression(GetName g)
    {
        return "D.InExpression";
    }

    static TParseTree ShiftExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing)), Spacing))), "D.ShiftExpression")(p);
        }
        else
        {
            if(auto m = tuple(`ShiftExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing)), Spacing))), "ShiftExpression"), "D.ShiftExpression")(p);
                memo[tuple(`ShiftExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ShiftExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing)), Spacing))), "D.ShiftExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing)), Spacing))), "ShiftExpression"), "D.ShiftExpression")(TParseTree("", false,[], s));
        }
    }
    static string ShiftExpression(GetName g)
    {
        return "D.ShiftExpression";
    }

    static TParseTree AddExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.wrapAround!(Spacing, CatExpression, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, MulExpression, Spacing)), Spacing))), "D.AddExpression")(p);
        }
        else
        {
            if(auto m = tuple(`AddExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.wrapAround!(Spacing, CatExpression, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, MulExpression, Spacing)), Spacing))), "AddExpression"), "D.AddExpression")(p);
                memo[tuple(`AddExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AddExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.wrapAround!(Spacing, CatExpression, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, MulExpression, Spacing)), Spacing))), "D.AddExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.wrapAround!(Spacing, CatExpression, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, MulExpression, Spacing)), Spacing))), "AddExpression"), "D.AddExpression")(TParseTree("", false,[], s));
        }
    }
    static string AddExpression(GetName g)
    {
        return "D.AddExpression";
    }

    static TParseTree CatExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing)), Spacing))), "D.CatExpression")(p);
        }
        else
        {
            if(auto m = tuple(`CatExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing)), Spacing))), "CatExpression"), "D.CatExpression")(p);
                memo[tuple(`CatExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CatExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing)), Spacing))), "D.CatExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing)), Spacing))), "CatExpression"), "D.CatExpression")(TParseTree("", false,[], s));
        }
    }
    static string CatExpression(GetName g)
    {
        return "D.CatExpression";
    }

    static TParseTree MulExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), Spacing))), "D.MulExpression")(p);
        }
        else
        {
            if(auto m = tuple(`MulExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), Spacing))), "MulExpression"), "D.MulExpression")(p);
                memo[tuple(`MulExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MulExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), Spacing))), "D.MulExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), Spacing))), "MulExpression"), "D.MulExpression")(TParseTree("", false,[], s));
        }
    }
    static string MulExpression(GetName g)
    {
        return "D.MulExpression";
    }

    static TParseTree UnaryExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), pegged.peg.wrapAround!(Spacing, ComplementExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing), pegged.peg.wrapAround!(Spacing, DeleteExpression, Spacing), pegged.peg.wrapAround!(Spacing, CastExpression, Spacing), pegged.peg.wrapAround!(Spacing, PowExpression, Spacing)), "D.UnaryExpression")(p);
        }
        else
        {
            if(auto m = tuple(`UnaryExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), pegged.peg.wrapAround!(Spacing, ComplementExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing), pegged.peg.wrapAround!(Spacing, DeleteExpression, Spacing), pegged.peg.wrapAround!(Spacing, CastExpression, Spacing), pegged.peg.wrapAround!(Spacing, PowExpression, Spacing)), "UnaryExpression"), "D.UnaryExpression")(p);
                memo[tuple(`UnaryExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnaryExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), pegged.peg.wrapAround!(Spacing, ComplementExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing), pegged.peg.wrapAround!(Spacing, DeleteExpression, Spacing), pegged.peg.wrapAround!(Spacing, CastExpression, Spacing), pegged.peg.wrapAround!(Spacing, PowExpression, Spacing)), "D.UnaryExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), pegged.peg.wrapAround!(Spacing, ComplementExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing), pegged.peg.wrapAround!(Spacing, DeleteExpression, Spacing), pegged.peg.wrapAround!(Spacing, CastExpression, Spacing), pegged.peg.wrapAround!(Spacing, PowExpression, Spacing)), "UnaryExpression"), "D.UnaryExpression")(TParseTree("", false,[], s));
        }
    }
    static string UnaryExpression(GetName g)
    {
        return "D.UnaryExpression";
    }

    static TParseTree UnaryOp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing)), "D.UnaryOp")(p);
        }
        else
        {
            if(auto m = tuple(`UnaryOp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing)), "UnaryOp"), "D.UnaryOp")(p);
                memo[tuple(`UnaryOp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnaryOp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing)), "D.UnaryOp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing)), "UnaryOp"), "D.UnaryOp")(TParseTree("", false,[], s));
        }
    }
    static string UnaryOp(GetName g)
    {
        return "D.UnaryOp";
    }

    static TParseTree ComplementExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "D.ComplementExpression")(p);
        }
        else
        {
            if(auto m = tuple(`ComplementExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "ComplementExpression"), "D.ComplementExpression")(p);
                memo[tuple(`ComplementExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ComplementExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "D.ComplementExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "ComplementExpression"), "D.ComplementExpression")(TParseTree("", false,[], s));
        }
    }
    static string ComplementExpression(GetName g)
    {
        return "D.ComplementExpression";
    }

    static TParseTree NewExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, NewAnonClassExpression, Spacing)), "D.NewExpression")(p);
        }
        else
        {
            if(auto m = tuple(`NewExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, NewAnonClassExpression, Spacing)), "NewExpression"), "D.NewExpression")(p);
                memo[tuple(`NewExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NewExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, NewAnonClassExpression, Spacing)), "D.NewExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, NewAnonClassExpression, Spacing)), "NewExpression"), "D.NewExpression")(TParseTree("", false,[], s));
        }
    }
    static string NewExpression(GetName g)
    {
        return "D.NewExpression";
    }

    static TParseTree AllocatorArguments(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.AllocatorArguments")(p);
        }
        else
        {
            if(auto m = tuple(`AllocatorArguments`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "AllocatorArguments"), "D.AllocatorArguments")(p);
                memo[tuple(`AllocatorArguments`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AllocatorArguments(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.AllocatorArguments")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "AllocatorArguments"), "D.AllocatorArguments")(TParseTree("", false,[], s));
        }
    }
    static string AllocatorArguments(GetName g)
    {
        return "D.AllocatorArguments";
    }

    static TParseTree ArgumentList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing))), "D.ArgumentList")(p);
        }
        else
        {
            if(auto m = tuple(`ArgumentList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing))), "ArgumentList"), "D.ArgumentList")(p);
                memo[tuple(`ArgumentList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArgumentList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing))), "D.ArgumentList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing))), "ArgumentList"), "D.ArgumentList")(TParseTree("", false,[], s));
        }
    }
    static string ArgumentList(GetName g)
    {
        return "D.ArgumentList";
    }

    static TParseTree DeleteExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "D.DeleteExpression")(p);
        }
        else
        {
            if(auto m = tuple(`DeleteExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "DeleteExpression"), "D.DeleteExpression")(p);
                memo[tuple(`DeleteExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeleteExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "D.DeleteExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "DeleteExpression"), "D.DeleteExpression")(TParseTree("", false,[], s));
        }
    }
    static string DeleteExpression(GetName g)
    {
        return "D.DeleteExpression";
    }

    static TParseTree CastExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, CastEqual, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "D.CastExpression")(p);
        }
        else
        {
            if(auto m = tuple(`CastExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, CastEqual, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "CastExpression"), "D.CastExpression")(p);
                memo[tuple(`CastExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CastExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, CastEqual, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "D.CastExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, CastEqual, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), "CastExpression"), "D.CastExpression")(TParseTree("", false,[], s));
        }
    }
    static string CastExpression(GetName g)
    {
        return "D.CastExpression";
    }

    static TParseTree CastEqual(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), "D.CastEqual")(p);
        }
        else
        {
            if(auto m = tuple(`CastEqual`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), "CastEqual"), "D.CastEqual")(p);
                memo[tuple(`CastEqual`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CastEqual(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), "D.CastEqual")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), "CastEqual"), "D.CastEqual")(TParseTree("", false,[], s));
        }
    }
    static string CastEqual(GetName g)
    {
        return "D.CastEqual";
    }

    static TParseTree PowExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostfixExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), Spacing))), "D.PowExpression")(p);
        }
        else
        {
            if(auto m = tuple(`PowExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostfixExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), Spacing))), "PowExpression"), "D.PowExpression")(p);
                memo[tuple(`PowExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PowExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostfixExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), Spacing))), "D.PowExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostfixExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), Spacing))), "PowExpression"), "D.PowExpression")(TParseTree("", false,[], s));
        }
    }
    static string PowExpression(GetName g)
    {
        return "D.PowExpression";
    }

    static TParseTree PostfixExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpression, Spacing), pegged.peg.wrapAround!(Spacing, SliceExpression, Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "D.PostfixExpression")(p);
        }
        else
        {
            if(auto m = tuple(`PostfixExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpression, Spacing), pegged.peg.wrapAround!(Spacing, SliceExpression, Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "PostfixExpression"), "D.PostfixExpression")(p);
                memo[tuple(`PostfixExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PostfixExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpression, Spacing), pegged.peg.wrapAround!(Spacing, SliceExpression, Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "D.PostfixExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpression, Spacing), pegged.peg.wrapAround!(Spacing, SliceExpression, Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing))), "PostfixExpression"), "D.PostfixExpression")(TParseTree("", false,[], s));
        }
    }
    static string PostfixExpression(GetName g)
    {
        return "D.PostfixExpression";
    }

    static TParseTree IndexExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "D.IndexExpression")(p);
        }
        else
        {
            if(auto m = tuple(`IndexExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "IndexExpression"), "D.IndexExpression")(p);
                memo[tuple(`IndexExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IndexExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "D.IndexExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "IndexExpression"), "D.IndexExpression")(TParseTree("", false,[], s));
        }
    }
    static string IndexExpression(GetName g)
    {
        return "D.IndexExpression";
    }

    static TParseTree SliceExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "D.SliceExpression")(p);
        }
        else
        {
            if(auto m = tuple(`SliceExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "SliceExpression"), "D.SliceExpression")(p);
                memo[tuple(`SliceExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SliceExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "D.SliceExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "SliceExpression"), "D.SliceExpression")(TParseTree("", false,[], s));
        }
    }
    static string SliceExpression(GetName g)
    {
        return "D.SliceExpression";
    }

    static TParseTree PrimaryExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing), pegged.peg.wrapAround!(Spacing, StringLiterals, Spacing), pegged.peg.wrapAround!(Spacing, ArrayLiteral, Spacing), pegged.peg.wrapAround!(Spacing, AssocArrayLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Lambda, Spacing), pegged.peg.wrapAround!(Spacing, FunctionLiteral, Spacing), pegged.peg.wrapAround!(Spacing, AssertExpression, Spacing), pegged.peg.wrapAround!(Spacing, MixinExpression, Spacing), pegged.peg.wrapAround!(Spacing, ImportExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, TypeidExpression, Spacing), pegged.peg.wrapAround!(Spacing, IsExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.wrapAround!(Spacing, TraitsExpression, Spacing)), "D.PrimaryExpression")(p);
        }
        else
        {
            if(auto m = tuple(`PrimaryExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing), pegged.peg.wrapAround!(Spacing, StringLiterals, Spacing), pegged.peg.wrapAround!(Spacing, ArrayLiteral, Spacing), pegged.peg.wrapAround!(Spacing, AssocArrayLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Lambda, Spacing), pegged.peg.wrapAround!(Spacing, FunctionLiteral, Spacing), pegged.peg.wrapAround!(Spacing, AssertExpression, Spacing), pegged.peg.wrapAround!(Spacing, MixinExpression, Spacing), pegged.peg.wrapAround!(Spacing, ImportExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, TypeidExpression, Spacing), pegged.peg.wrapAround!(Spacing, IsExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.wrapAround!(Spacing, TraitsExpression, Spacing)), "PrimaryExpression"), "D.PrimaryExpression")(p);
                memo[tuple(`PrimaryExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PrimaryExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing), pegged.peg.wrapAround!(Spacing, StringLiterals, Spacing), pegged.peg.wrapAround!(Spacing, ArrayLiteral, Spacing), pegged.peg.wrapAround!(Spacing, AssocArrayLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Lambda, Spacing), pegged.peg.wrapAround!(Spacing, FunctionLiteral, Spacing), pegged.peg.wrapAround!(Spacing, AssertExpression, Spacing), pegged.peg.wrapAround!(Spacing, MixinExpression, Spacing), pegged.peg.wrapAround!(Spacing, ImportExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, TypeidExpression, Spacing), pegged.peg.wrapAround!(Spacing, IsExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.wrapAround!(Spacing, TraitsExpression, Spacing)), "D.PrimaryExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing), pegged.peg.wrapAround!(Spacing, StringLiterals, Spacing), pegged.peg.wrapAround!(Spacing, ArrayLiteral, Spacing), pegged.peg.wrapAround!(Spacing, AssocArrayLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Lambda, Spacing), pegged.peg.wrapAround!(Spacing, FunctionLiteral, Spacing), pegged.peg.wrapAround!(Spacing, AssertExpression, Spacing), pegged.peg.wrapAround!(Spacing, MixinExpression, Spacing), pegged.peg.wrapAround!(Spacing, ImportExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, TypeidExpression, Spacing), pegged.peg.wrapAround!(Spacing, IsExpression, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.wrapAround!(Spacing, TraitsExpression, Spacing)), "PrimaryExpression"), "D.PrimaryExpression")(TParseTree("", false,[], s));
        }
    }
    static string PrimaryExpression(GetName g)
    {
        return "D.PrimaryExpression";
    }

    static TParseTree StringLiterals(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), "D.StringLiterals")(p);
        }
        else
        {
            if(auto m = tuple(`StringLiterals`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), "StringLiterals"), "D.StringLiterals")(p);
                memo[tuple(`StringLiterals`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringLiterals(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), "D.StringLiterals")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), "StringLiterals"), "D.StringLiterals")(TParseTree("", false,[], s));
        }
    }
    static string StringLiterals(GetName g)
    {
        return "D.StringLiterals";
    }

    static TParseTree ArrayLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "D.ArrayLiteral")(p);
        }
        else
        {
            if(auto m = tuple(`ArrayLiteral`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "ArrayLiteral"), "D.ArrayLiteral")(p);
                memo[tuple(`ArrayLiteral`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArrayLiteral(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "D.ArrayLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "ArrayLiteral"), "D.ArrayLiteral")(TParseTree("", false,[], s));
        }
    }
    static string ArrayLiteral(GetName g)
    {
        return "D.ArrayLiteral";
    }

    static TParseTree AssocArrayLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "D.AssocArrayLiteral")(p);
        }
        else
        {
            if(auto m = tuple(`AssocArrayLiteral`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "AssocArrayLiteral"), "D.AssocArrayLiteral")(p);
                memo[tuple(`AssocArrayLiteral`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AssocArrayLiteral(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "D.AssocArrayLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "AssocArrayLiteral"), "D.AssocArrayLiteral")(TParseTree("", false,[], s));
        }
    }
    static string AssocArrayLiteral(GetName g)
    {
        return "D.AssocArrayLiteral";
    }

    static TParseTree KeyValuePair(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), "D.KeyValuePair")(p);
        }
        else
        {
            if(auto m = tuple(`KeyValuePair`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), "KeyValuePair"), "D.KeyValuePair")(p);
                memo[tuple(`KeyValuePair`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree KeyValuePair(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), "D.KeyValuePair")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), "KeyValuePair"), "D.KeyValuePair")(TParseTree("", false,[], s));
        }
    }
    static string KeyValuePair(GetName g)
    {
        return "D.KeyValuePair";
    }

    static TParseTree Lambda(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.Lambda")(p);
        }
        else
        {
            if(auto m = tuple(`Lambda`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "Lambda"), "D.Lambda")(p);
                memo[tuple(`Lambda`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Lambda(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.Lambda")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "Lambda"), "D.Lambda")(TParseTree("", false,[], s));
        }
    }
    static string Lambda(GetName g)
    {
        return "D.Lambda";
    }

    static TParseTree FunctionLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.FunctionLiteral")(p);
        }
        else
        {
            if(auto m = tuple(`FunctionLiteral`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "FunctionLiteral"), "D.FunctionLiteral")(p);
                memo[tuple(`FunctionLiteral`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionLiteral(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.FunctionLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "FunctionLiteral"), "D.FunctionLiteral")(TParseTree("", false,[], s));
        }
    }
    static string FunctionLiteral(GetName g)
    {
        return "D.FunctionLiteral";
    }

    static TParseTree ParameterAttributes(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), "D.ParameterAttributes")(p);
        }
        else
        {
            if(auto m = tuple(`ParameterAttributes`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), "ParameterAttributes"), "D.ParameterAttributes")(p);
                memo[tuple(`ParameterAttributes`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParameterAttributes(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), "D.ParameterAttributes")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), "ParameterAttributes"), "D.ParameterAttributes")(TParseTree("", false,[], s));
        }
    }
    static string ParameterAttributes(GetName g)
    {
        return "D.ParameterAttributes";
    }

    static TParseTree AssertExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.AssertExpression")(p);
        }
        else
        {
            if(auto m = tuple(`AssertExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "AssertExpression"), "D.AssertExpression")(p);
                memo[tuple(`AssertExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AssertExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.AssertExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "AssertExpression"), "D.AssertExpression")(TParseTree("", false,[], s));
        }
    }
    static string AssertExpression(GetName g)
    {
        return "D.AssertExpression";
    }

    static TParseTree MixinExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.MixinExpression")(p);
        }
        else
        {
            if(auto m = tuple(`MixinExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "MixinExpression"), "D.MixinExpression")(p);
                memo[tuple(`MixinExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MixinExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.MixinExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "MixinExpression"), "D.MixinExpression")(TParseTree("", false,[], s));
        }
    }
    static string MixinExpression(GetName g)
    {
        return "D.MixinExpression";
    }

    static TParseTree ImportExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.ImportExpression")(p);
        }
        else
        {
            if(auto m = tuple(`ImportExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "ImportExpression"), "D.ImportExpression")(p);
                memo[tuple(`ImportExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ImportExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.ImportExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "ImportExpression"), "D.ImportExpression")(TParseTree("", false,[], s));
        }
    }
    static string ImportExpression(GetName g)
    {
        return "D.ImportExpression";
    }

    static TParseTree TypeidExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.TypeidExpression")(p);
        }
        else
        {
            if(auto m = tuple(`TypeidExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "TypeidExpression"), "D.TypeidExpression")(p);
                memo[tuple(`TypeidExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeidExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.TypeidExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "TypeidExpression"), "D.TypeidExpression")(TParseTree("", false,[], s));
        }
    }
    static string TypeidExpression(GetName g)
    {
        return "D.TypeidExpression";
    }

    static TParseTree IsExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), Spacing)))), Spacing)))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.IsExpression")(p);
        }
        else
        {
            if(auto m = tuple(`IsExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), Spacing)))), Spacing)))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "IsExpression"), "D.IsExpression")(p);
                memo[tuple(`IsExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IsExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), Spacing)))), Spacing)))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.IsExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), Spacing)))), Spacing)))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "IsExpression"), "D.IsExpression")(TParseTree("", false,[], s));
        }
    }
    static string IsExpression(GetName g)
    {
        return "D.IsExpression";
    }

    static TParseTree TypeSpecialization(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing)), "D.TypeSpecialization")(p);
        }
        else
        {
            if(auto m = tuple(`TypeSpecialization`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing)), "TypeSpecialization"), "D.TypeSpecialization")(p);
                memo[tuple(`TypeSpecialization`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeSpecialization(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing)), "D.TypeSpecialization")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing)), "TypeSpecialization"), "D.TypeSpecialization")(TParseTree("", false,[], s));
        }
    }
    static string TypeSpecialization(GetName g)
    {
        return "D.TypeSpecialization";
    }

    static TParseTree AttributeSpecifier(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationBlock, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing))), "D.AttributeSpecifier")(p);
        }
        else
        {
            if(auto m = tuple(`AttributeSpecifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationBlock, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing))), "AttributeSpecifier"), "D.AttributeSpecifier")(p);
                memo[tuple(`AttributeSpecifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AttributeSpecifier(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationBlock, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing))), "D.AttributeSpecifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationBlock, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing))), "AttributeSpecifier"), "D.AttributeSpecifier")(TParseTree("", false,[], s));
        }
    }
    static string AttributeSpecifier(GetName g)
    {
        return "D.AttributeSpecifier";
    }

    static TParseTree Attribute(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LinkageAttribute, Spacing), pegged.peg.wrapAround!(Spacing, AlignAttribute, Spacing), pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, ProtectionAttribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@disable"), Spacing)), "D.Attribute")(p);
        }
        else
        {
            if(auto m = tuple(`Attribute`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LinkageAttribute, Spacing), pegged.peg.wrapAround!(Spacing, AlignAttribute, Spacing), pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, ProtectionAttribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@disable"), Spacing)), "Attribute"), "D.Attribute")(p);
                memo[tuple(`Attribute`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Attribute(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LinkageAttribute, Spacing), pegged.peg.wrapAround!(Spacing, AlignAttribute, Spacing), pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, ProtectionAttribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@disable"), Spacing)), "D.Attribute")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, LinkageAttribute, Spacing), pegged.peg.wrapAround!(Spacing, AlignAttribute, Spacing), pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, ProtectionAttribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@disable"), Spacing)), "Attribute"), "D.Attribute")(TParseTree("", false,[], s));
        }
    }
    static string Attribute(GetName g)
    {
        return "D.Attribute";
    }

    static TParseTree DeclarationBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.DeclarationBlock")(p);
        }
        else
        {
            if(auto m = tuple(`DeclarationBlock`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "DeclarationBlock"), "D.DeclarationBlock")(p);
                memo[tuple(`DeclarationBlock`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DeclarationBlock(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.DeclarationBlock")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "DeclarationBlock"), "D.DeclarationBlock")(TParseTree("", false,[], s));
        }
    }
    static string DeclarationBlock(GetName g)
    {
        return "D.DeclarationBlock";
    }

    static TParseTree LinkageAttribute(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, LinkageType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.LinkageAttribute")(p);
        }
        else
        {
            if(auto m = tuple(`LinkageAttribute`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, LinkageType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "LinkageAttribute"), "D.LinkageAttribute")(p);
                memo[tuple(`LinkageAttribute`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LinkageAttribute(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, LinkageType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.LinkageAttribute")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, LinkageType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "LinkageAttribute"), "D.LinkageAttribute")(TParseTree("", false,[], s));
        }
    }
    static string LinkageAttribute(GetName g)
    {
        return "D.LinkageAttribute";
    }

    static TParseTree LinkageType(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("D"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Windows"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Pascal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("System"), Spacing)), "D.LinkageType")(p);
        }
        else
        {
            if(auto m = tuple(`LinkageType`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("D"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Windows"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Pascal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("System"), Spacing)), "LinkageType"), "D.LinkageType")(p);
                memo[tuple(`LinkageType`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LinkageType(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("D"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Windows"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Pascal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("System"), Spacing)), "D.LinkageType")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("D"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Windows"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Pascal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("System"), Spacing)), "LinkageType"), "D.LinkageType")(TParseTree("", false,[], s));
        }
    }
    static string LinkageType(GetName g)
    {
        return "D.LinkageType";
    }

    static TParseTree AlignAttribute(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing))), "D.AlignAttribute")(p);
        }
        else
        {
            if(auto m = tuple(`AlignAttribute`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing))), "AlignAttribute"), "D.AlignAttribute")(p);
                memo[tuple(`AlignAttribute`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AlignAttribute(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing))), "D.AlignAttribute")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing))), "AlignAttribute"), "D.AlignAttribute")(TParseTree("", false,[], s));
        }
    }
    static string AlignAttribute(GetName g)
    {
        return "D.AlignAttribute";
    }

    static TParseTree ProtectionAttribute(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing)), "D.ProtectionAttribute")(p);
        }
        else
        {
            if(auto m = tuple(`ProtectionAttribute`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing)), "ProtectionAttribute"), "D.ProtectionAttribute")(p);
                memo[tuple(`ProtectionAttribute`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ProtectionAttribute(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing)), "D.ProtectionAttribute")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing)), "ProtectionAttribute"), "D.ProtectionAttribute")(TParseTree("", false,[], s));
        }
    }
    static string ProtectionAttribute(GetName g)
    {
        return "D.ProtectionAttribute";
    }

    static TParseTree ClassDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), pegged.peg.wrapAround!(Spacing, ClassTemplateDeclaration, Spacing)), "D.ClassDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`ClassDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), pegged.peg.wrapAround!(Spacing, ClassTemplateDeclaration, Spacing)), "ClassDeclaration"), "D.ClassDeclaration")(p);
                memo[tuple(`ClassDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ClassDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), pegged.peg.wrapAround!(Spacing, ClassTemplateDeclaration, Spacing)), "D.ClassDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), pegged.peg.wrapAround!(Spacing, ClassTemplateDeclaration, Spacing)), "ClassDeclaration"), "D.ClassDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string ClassDeclaration(GetName g)
    {
        return "D.ClassDeclaration";
    }

    static TParseTree BaseClassList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), "D.BaseClassList")(p);
        }
        else
        {
            if(auto m = tuple(`BaseClassList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), "BaseClassList"), "D.BaseClassList")(p);
                memo[tuple(`BaseClassList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BaseClassList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), "D.BaseClassList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), "BaseClassList"), "D.BaseClassList")(TParseTree("", false,[], s));
        }
    }
    static string BaseClassList(GetName g)
    {
        return "D.BaseClassList";
    }

    static TParseTree ClassBody(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.ClassBody")(p);
        }
        else
        {
            if(auto m = tuple(`ClassBody`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "ClassBody"), "D.ClassBody")(p);
                memo[tuple(`ClassBody`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ClassBody(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.ClassBody")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "ClassBody"), "D.ClassBody")(TParseTree("", false,[], s));
        }
    }
    static string ClassBody(GetName g)
    {
        return "D.ClassBody";
    }

    static TParseTree ClassBodyDeclarations(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing))), "D.ClassBodyDeclarations")(p);
        }
        else
        {
            if(auto m = tuple(`ClassBodyDeclarations`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing))), "ClassBodyDeclarations"), "D.ClassBodyDeclarations")(p);
                memo[tuple(`ClassBodyDeclarations`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ClassBodyDeclarations(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing))), "D.ClassBodyDeclarations")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing))), "ClassBodyDeclarations"), "D.ClassBodyDeclarations")(TParseTree("", false,[], s));
        }
    }
    static string ClassBodyDeclarations(GetName g)
    {
        return "D.ClassBodyDeclarations";
    }

    static TParseTree ClassBodyDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.wrapAround!(Spacing, Invariant, Spacing), pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing), pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing)), "D.ClassBodyDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`ClassBodyDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.wrapAround!(Spacing, Invariant, Spacing), pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing), pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing)), "ClassBodyDeclaration"), "D.ClassBodyDeclaration")(p);
                memo[tuple(`ClassBodyDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ClassBodyDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.wrapAround!(Spacing, Invariant, Spacing), pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing), pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing)), "D.ClassBodyDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.wrapAround!(Spacing, Invariant, Spacing), pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing), pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing)), "ClassBodyDeclaration"), "D.ClassBodyDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string ClassBodyDeclaration(GetName g)
    {
        return "D.ClassBodyDeclaration";
    }

    static TParseTree Constructor(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.wrapAround!(Spacing, TemplatedConstructor, Spacing)), "D.Constructor")(p);
        }
        else
        {
            if(auto m = tuple(`Constructor`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.wrapAround!(Spacing, TemplatedConstructor, Spacing)), "Constructor"), "D.Constructor")(p);
                memo[tuple(`Constructor`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Constructor(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.wrapAround!(Spacing, TemplatedConstructor, Spacing)), "D.Constructor")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.wrapAround!(Spacing, TemplatedConstructor, Spacing)), "Constructor"), "D.Constructor")(TParseTree("", false,[], s));
        }
    }
    static string Constructor(GetName g)
    {
        return "D.Constructor";
    }

    static TParseTree Destructor(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.Destructor")(p);
        }
        else
        {
            if(auto m = tuple(`Destructor`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "Destructor"), "D.Destructor")(p);
                memo[tuple(`Destructor`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Destructor(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.Destructor")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "Destructor"), "D.Destructor")(TParseTree("", false,[], s));
        }
    }
    static string Destructor(GetName g)
    {
        return "D.Destructor";
    }

    static TParseTree StaticConstructor(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.StaticConstructor")(p);
        }
        else
        {
            if(auto m = tuple(`StaticConstructor`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "StaticConstructor"), "D.StaticConstructor")(p);
                memo[tuple(`StaticConstructor`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StaticConstructor(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.StaticConstructor")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "StaticConstructor"), "D.StaticConstructor")(TParseTree("", false,[], s));
        }
    }
    static string StaticConstructor(GetName g)
    {
        return "D.StaticConstructor";
    }

    static TParseTree StaticDestructor(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.StaticDestructor")(p);
        }
        else
        {
            if(auto m = tuple(`StaticDestructor`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "StaticDestructor"), "D.StaticDestructor")(p);
                memo[tuple(`StaticDestructor`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StaticDestructor(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.StaticDestructor")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "StaticDestructor"), "D.StaticDestructor")(TParseTree("", false,[], s));
        }
    }
    static string StaticDestructor(GetName g)
    {
        return "D.StaticDestructor";
    }

    static TParseTree SharedStaticConstructor(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.SharedStaticConstructor")(p);
        }
        else
        {
            if(auto m = tuple(`SharedStaticConstructor`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "SharedStaticConstructor"), "D.SharedStaticConstructor")(p);
                memo[tuple(`SharedStaticConstructor`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SharedStaticConstructor(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.SharedStaticConstructor")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "SharedStaticConstructor"), "D.SharedStaticConstructor")(TParseTree("", false,[], s));
        }
    }
    static string SharedStaticConstructor(GetName g)
    {
        return "D.SharedStaticConstructor";
    }

    static TParseTree SharedStaticDestructor(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.SharedStaticDestructor")(p);
        }
        else
        {
            if(auto m = tuple(`SharedStaticDestructor`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "SharedStaticDestructor"), "D.SharedStaticDestructor")(p);
                memo[tuple(`SharedStaticDestructor`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SharedStaticDestructor(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.SharedStaticDestructor")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "SharedStaticDestructor"), "D.SharedStaticDestructor")(TParseTree("", false,[], s));
        }
    }
    static string SharedStaticDestructor(GetName g)
    {
        return "D.SharedStaticDestructor";
    }

    static TParseTree Invariant(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.Invariant")(p);
        }
        else
        {
            if(auto m = tuple(`Invariant`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "Invariant"), "D.Invariant")(p);
                memo[tuple(`Invariant`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Invariant(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.Invariant")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "Invariant"), "D.Invariant")(TParseTree("", false,[], s));
        }
    }
    static string Invariant(GetName g)
    {
        return "D.Invariant";
    }

    static TParseTree ClassAllocator(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.ClassAllocator")(p);
        }
        else
        {
            if(auto m = tuple(`ClassAllocator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "ClassAllocator"), "D.ClassAllocator")(p);
                memo[tuple(`ClassAllocator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ClassAllocator(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.ClassAllocator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "ClassAllocator"), "D.ClassAllocator")(TParseTree("", false,[], s));
        }
    }
    static string ClassAllocator(GetName g)
    {
        return "D.ClassAllocator";
    }

    static TParseTree ClassDeallocator(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.ClassDeallocator")(p);
        }
        else
        {
            if(auto m = tuple(`ClassDeallocator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "ClassDeallocator"), "D.ClassDeallocator")(p);
                memo[tuple(`ClassDeallocator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ClassDeallocator(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.ClassDeallocator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "ClassDeallocator"), "D.ClassDeallocator")(TParseTree("", false,[], s));
        }
    }
    static string ClassDeallocator(GetName g)
    {
        return "D.ClassDeallocator";
    }

    static TParseTree AliasThis(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.AliasThis")(p);
        }
        else
        {
            if(auto m = tuple(`AliasThis`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "AliasThis"), "D.AliasThis")(p);
                memo[tuple(`AliasThis`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AliasThis(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.AliasThis")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "AliasThis"), "D.AliasThis")(TParseTree("", false,[], s));
        }
    }
    static string AliasThis(GetName g)
    {
        return "D.AliasThis";
    }

    static TParseTree NewAnonClassExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), "D.NewAnonClassExpression")(p);
        }
        else
        {
            if(auto m = tuple(`NewAnonClassExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), "NewAnonClassExpression"), "D.NewAnonClassExpression")(p);
                memo[tuple(`NewAnonClassExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NewAnonClassExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), "D.NewAnonClassExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), "NewAnonClassExpression"), "D.NewAnonClassExpression")(TParseTree("", false,[], s));
        }
    }
    static string NewAnonClassExpression(GetName g)
    {
        return "D.NewAnonClassExpression";
    }

    static TParseTree ClassArguments(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.ClassArguments")(p);
        }
        else
        {
            if(auto m = tuple(`ClassArguments`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "ClassArguments"), "D.ClassArguments")(p);
                memo[tuple(`ClassArguments`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ClassArguments(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.ClassArguments")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "ClassArguments"), "D.ClassArguments")(TParseTree("", false,[], s));
        }
    }
    static string ClassArguments(GetName g)
    {
        return "D.ClassArguments";
    }

    static TParseTree EnumDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EnumTag, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, EnumBaseType, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, EnumBody, Spacing)), "D.EnumDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`EnumDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EnumTag, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, EnumBaseType, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, EnumBody, Spacing)), "EnumDeclaration"), "D.EnumDeclaration")(p);
                memo[tuple(`EnumDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnumDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EnumTag, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, EnumBaseType, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, EnumBody, Spacing)), "D.EnumDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EnumTag, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, EnumBaseType, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, EnumBody, Spacing)), "EnumDeclaration"), "D.EnumDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string EnumDeclaration(GetName g)
    {
        return "D.EnumDeclaration";
    }

    static TParseTree EnumTag(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "D.EnumTag")(p);
        }
        else
        {
            if(auto m = tuple(`EnumTag`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "EnumTag"), "D.EnumTag")(p);
                memo[tuple(`EnumTag`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnumTag(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "D.EnumTag")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "EnumTag"), "D.EnumTag")(TParseTree("", false,[], s));
        }
    }
    static string EnumTag(GetName g)
    {
        return "D.EnumTag";
    }

    static TParseTree EnumBaseType(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Type, Spacing), "D.EnumBaseType")(p);
        }
        else
        {
            if(auto m = tuple(`EnumBaseType`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Type, Spacing), "EnumBaseType"), "D.EnumBaseType")(p);
                memo[tuple(`EnumBaseType`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnumBaseType(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Type, Spacing), "D.EnumBaseType")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Type, Spacing), "EnumBaseType"), "D.EnumBaseType")(TParseTree("", false,[], s));
        }
    }
    static string EnumBaseType(GetName g)
    {
        return "D.EnumBaseType";
    }

    static TParseTree EnumBody(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, EnumMember, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.EnumBody")(p);
        }
        else
        {
            if(auto m = tuple(`EnumBody`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, EnumMember, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "EnumBody"), "D.EnumBody")(p);
                memo[tuple(`EnumBody`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnumBody(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, EnumMember, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.EnumBody")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, EnumMember, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "EnumBody"), "D.EnumBody")(TParseTree("", false,[], s));
        }
    }
    static string EnumBody(GetName g)
    {
        return "D.EnumBody";
    }

    static TParseTree EnumMember(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)))), "D.EnumMember")(p);
        }
        else
        {
            if(auto m = tuple(`EnumMember`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)))), "EnumMember"), "D.EnumMember")(p);
                memo[tuple(`EnumMember`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnumMember(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)))), "D.EnumMember")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)))), "EnumMember"), "D.EnumMember")(TParseTree("", false,[], s));
        }
    }
    static string EnumMember(GetName g)
    {
        return "D.EnumMember";
    }

    static TParseTree FunctionBody(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing))), "D.FunctionBody")(p);
        }
        else
        {
            if(auto m = tuple(`FunctionBody`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing))), "FunctionBody"), "D.FunctionBody")(p);
                memo[tuple(`FunctionBody`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionBody(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing))), "D.FunctionBody")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing))), "FunctionBody"), "D.FunctionBody")(TParseTree("", false,[], s));
        }
    }
    static string FunctionBody(GetName g)
    {
        return "D.FunctionBody";
    }

    static TParseTree InStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.InStatement")(p);
        }
        else
        {
            if(auto m = tuple(`InStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "InStatement"), "D.InStatement")(p);
                memo[tuple(`InStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.InStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "InStatement"), "D.InStatement")(TParseTree("", false,[], s));
        }
    }
    static string InStatement(GetName g)
    {
        return "D.InStatement";
    }

    static TParseTree OutStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.OutStatement")(p);
        }
        else
        {
            if(auto m = tuple(`OutStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "OutStatement"), "D.OutStatement")(p);
                memo[tuple(`OutStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OutStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.OutStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "OutStatement"), "D.OutStatement")(TParseTree("", false,[], s));
        }
    }
    static string OutStatement(GetName g)
    {
        return "D.OutStatement";
    }

    static TParseTree BodyStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.BodyStatement")(p);
        }
        else
        {
            if(auto m = tuple(`BodyStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "BodyStatement"), "D.BodyStatement")(p);
                memo[tuple(`BodyStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BodyStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "D.BodyStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), "BodyStatement"), "D.BodyStatement")(TParseTree("", false,[], s));
        }
    }
    static string BodyStatement(GetName g)
    {
        return "D.BodyStatement";
    }

    static TParseTree AsmInstruction(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, IntegerExpression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("even"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("naked"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("db"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ds"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("di"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dl"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("df"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dd"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("de"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Operand, Spacing)), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing)), pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Operand, Spacing)), Spacing))), "D.AsmInstruction")(p);
        }
        else
        {
            if(auto m = tuple(`AsmInstruction`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, IntegerExpression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("even"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("naked"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("db"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ds"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("di"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dl"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("df"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dd"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("de"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Operand, Spacing)), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing)), pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Operand, Spacing)), Spacing))), "AsmInstruction"), "D.AsmInstruction")(p);
                memo[tuple(`AsmInstruction`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmInstruction(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, IntegerExpression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("even"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("naked"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("db"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ds"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("di"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dl"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("df"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dd"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("de"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Operand, Spacing)), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing)), pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Operand, Spacing)), Spacing))), "D.AsmInstruction")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, IntegerExpression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("even"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("naked"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("db"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ds"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("di"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dl"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("df"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dd"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("de"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Operand, Spacing)), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing)), pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Operand, Spacing)), Spacing))), "AsmInstruction"), "D.AsmInstruction")(TParseTree("", false,[], s));
        }
    }
    static string AsmInstruction(GetName g)
    {
        return "D.AsmInstruction";
    }

    static TParseTree IntegerExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "D.IntegerExpression")(p);
        }
        else
        {
            if(auto m = tuple(`IntegerExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "IntegerExpression"), "D.IntegerExpression")(p);
                memo[tuple(`IntegerExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntegerExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "D.IntegerExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "IntegerExpression"), "D.IntegerExpression")(TParseTree("", false,[], s));
        }
    }
    static string IntegerExpression(GetName g)
    {
        return "D.IntegerExpression";
    }

    static TParseTree Operand(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), "D.Operand")(p);
        }
        else
        {
            if(auto m = tuple(`Operand`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), "Operand"), "D.Operand")(p);
                memo[tuple(`Operand`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Operand(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), "D.Operand")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), "Operand"), "D.Operand")(TParseTree("", false,[], s));
        }
    }
    static string Operand(GetName g)
    {
        return "D.Operand";
    }

    static TParseTree AsmExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), Spacing))), "D.AsmExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), Spacing))), "AsmExp"), "D.AsmExp")(p);
                memo[tuple(`AsmExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), Spacing))), "D.AsmExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), Spacing))), "AsmExp"), "D.AsmExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmExp(GetName g)
    {
        return "D.AsmExp";
    }

    static TParseTree AsmLogOrExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing)), Spacing))), "D.AsmLogOrExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmLogOrExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing)), Spacing))), "AsmLogOrExp"), "D.AsmLogOrExp")(p);
                memo[tuple(`AsmLogOrExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmLogOrExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing)), Spacing))), "D.AsmLogOrExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing)), Spacing))), "AsmLogOrExp"), "D.AsmLogOrExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmLogOrExp(GetName g)
    {
        return "D.AsmLogOrExp";
    }

    static TParseTree AsmLogAndExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing)), Spacing))), "D.AsmLogAndExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmLogAndExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing)), Spacing))), "AsmLogAndExp"), "D.AsmLogAndExp")(p);
                memo[tuple(`AsmLogAndExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmLogAndExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing)), Spacing))), "D.AsmLogAndExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing)), Spacing))), "AsmLogAndExp"), "D.AsmLogAndExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmLogAndExp(GetName g)
    {
        return "D.AsmLogAndExp";
    }

    static TParseTree AsmOrExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing)), Spacing))), "D.AsmOrExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmOrExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing)), Spacing))), "AsmOrExp"), "D.AsmOrExp")(p);
                memo[tuple(`AsmOrExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmOrExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing)), Spacing))), "D.AsmOrExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing)), Spacing))), "AsmOrExp"), "D.AsmOrExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmOrExp(GetName g)
    {
        return "D.AsmOrExp";
    }

    static TParseTree AsmXorExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing)), Spacing))), "D.AsmXorExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmXorExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing)), Spacing))), "AsmXorExp"), "D.AsmXorExp")(p);
                memo[tuple(`AsmXorExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmXorExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing)), Spacing))), "D.AsmXorExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing)), Spacing))), "AsmXorExp"), "D.AsmXorExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmXorExp(GetName g)
    {
        return "D.AsmXorExp";
    }

    static TParseTree AsmAndExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing)), Spacing))), "D.AsmAndExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmAndExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing)), Spacing))), "AsmAndExp"), "D.AsmAndExp")(p);
                memo[tuple(`AsmAndExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmAndExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing)), Spacing))), "D.AsmAndExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing)), Spacing))), "AsmAndExp"), "D.AsmAndExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmAndExp(GetName g)
    {
        return "D.AsmAndExp";
    }

    static TParseTree AsmEqualExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing)), Spacing))), "D.AsmEqualExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmEqualExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing)), Spacing))), "AsmEqualExp"), "D.AsmEqualExp")(p);
                memo[tuple(`AsmEqualExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmEqualExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing)), Spacing))), "D.AsmEqualExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing)), Spacing))), "AsmEqualExp"), "D.AsmEqualExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmEqualExp(GetName g)
    {
        return "D.AsmEqualExp";
    }

    static TParseTree AsmRelExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing)), Spacing))), "D.AsmRelExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmRelExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing)), Spacing))), "AsmRelExp"), "D.AsmRelExp")(p);
                memo[tuple(`AsmRelExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmRelExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing)), Spacing))), "D.AsmRelExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing)), Spacing))), "AsmRelExp"), "D.AsmRelExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmRelExp(GetName g)
    {
        return "D.AsmRelExp";
    }

    static TParseTree AsmShiftExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing)), Spacing))), "D.AsmShiftExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmShiftExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing)), Spacing))), "AsmShiftExp"), "D.AsmShiftExp")(p);
                memo[tuple(`AsmShiftExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmShiftExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing)), Spacing))), "D.AsmShiftExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing)), Spacing))), "AsmShiftExp"), "D.AsmShiftExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmShiftExp(GetName g)
    {
        return "D.AsmShiftExp";
    }

    static TParseTree AsmAddExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing)), Spacing))), "D.AsmAddExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmAddExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing)), Spacing))), "AsmAddExp"), "D.AsmAddExp")(p);
                memo[tuple(`AsmAddExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmAddExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing)), Spacing))), "D.AsmAddExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing)), Spacing))), "AsmAddExp"), "D.AsmAddExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmAddExp(GetName g)
    {
        return "D.AsmAddExp";
    }

    static TParseTree AsmMulExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing)), Spacing))), "D.AsmMulExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmMulExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing)), Spacing))), "AsmMulExp"), "D.AsmMulExp")(p);
                memo[tuple(`AsmMulExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmMulExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing)), Spacing))), "D.AsmMulExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing)), Spacing))), "AsmMulExp"), "D.AsmMulExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmMulExp(GetName g)
    {
        return "D.AsmMulExp";
    }

    static TParseTree AsmBrExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing))), "D.AsmBrExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmBrExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing))), "AsmBrExp"), "D.AsmBrExp")(p);
                memo[tuple(`AsmBrExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmBrExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing))), "D.AsmBrExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), Spacing))), "AsmBrExp"), "D.AsmBrExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmBrExp(GetName g)
    {
        return "D.AsmBrExp";
    }

    static TParseTree AsmUnaExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmTypePrefix, Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("offsetof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("seg"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing)), pegged.peg.wrapAround!(Spacing, AsmPrimaryExp, Spacing)), "D.AsmUnaExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmUnaExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmTypePrefix, Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("offsetof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("seg"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing)), pegged.peg.wrapAround!(Spacing, AsmPrimaryExp, Spacing)), "AsmUnaExp"), "D.AsmUnaExp")(p);
                memo[tuple(`AsmUnaExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmUnaExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmTypePrefix, Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("offsetof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("seg"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing)), pegged.peg.wrapAround!(Spacing, AsmPrimaryExp, Spacing)), "D.AsmUnaExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmTypePrefix, Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("offsetof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("seg"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing)), pegged.peg.wrapAround!(Spacing, AsmPrimaryExp, Spacing)), "AsmUnaExp"), "D.AsmUnaExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmUnaExp(GetName g)
    {
        return "D.AsmUnaExp";
    }

    static TParseTree AsmPrimaryExp(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LOCAL_SIZE"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), pegged.peg.wrapAround!(Spacing, Register, Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing)), "D.AsmPrimaryExp")(p);
        }
        else
        {
            if(auto m = tuple(`AsmPrimaryExp`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LOCAL_SIZE"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), pegged.peg.wrapAround!(Spacing, Register, Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing)), "AsmPrimaryExp"), "D.AsmPrimaryExp")(p);
                memo[tuple(`AsmPrimaryExp`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmPrimaryExp(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LOCAL_SIZE"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), pegged.peg.wrapAround!(Spacing, Register, Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing)), "D.AsmPrimaryExp")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LOCAL_SIZE"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing), pegged.peg.wrapAround!(Spacing, Register, Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing)), "AsmPrimaryExp"), "D.AsmPrimaryExp")(TParseTree("", false,[], s));
        }
    }
    static string AsmPrimaryExp(GetName g)
    {
        return "D.AsmPrimaryExp";
    }

    static TParseTree DotIdentifier(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing)), Spacing))), "D.DotIdentifier")(p);
        }
        else
        {
            if(auto m = tuple(`DotIdentifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing)), Spacing))), "DotIdentifier"), "D.DotIdentifier")(p);
                memo[tuple(`DotIdentifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DotIdentifier(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing)), Spacing))), "D.DotIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing)), Spacing))), "DotIdentifier"), "D.DotIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string DotIdentifier(GetName g)
    {
        return "D.DotIdentifier";
    }

    static TParseTree AsmTypePrefix(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("near"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("far"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("word"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dword"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("qword"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ptr"), Spacing)), "D.AsmTypePrefix")(p);
        }
        else
        {
            if(auto m = tuple(`AsmTypePrefix`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("near"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("far"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("word"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dword"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("qword"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ptr"), Spacing)), "AsmTypePrefix"), "D.AsmTypePrefix")(p);
                memo[tuple(`AsmTypePrefix`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsmTypePrefix(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("near"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("far"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("word"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dword"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("qword"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ptr"), Spacing)), "D.AsmTypePrefix")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("near"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("far"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("word"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dword"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("qword"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ptr"), Spacing)), "AsmTypePrefix"), "D.AsmTypePrefix")(TParseTree("", false,[], s));
        }
    }
    static string AsmTypePrefix(GetName g)
    {
        return "D.AsmTypePrefix";
    }

    static TParseTree Register(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "D.Register")(p);
        }
        else
        {
            if(auto m = tuple(`Register`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "Register"), "D.Register")(p);
                memo[tuple(`Register`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Register(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "D.Register")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "Register"), "D.Register")(TParseTree("", false,[], s));
        }
    }
    static string Register(GetName g)
    {
        return "D.Register";
    }

    static TParseTree OpCode(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "D.OpCode")(p);
        }
        else
        {
            if(auto m = tuple(`OpCode`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "OpCode"), "D.OpCode")(p);
                memo[tuple(`OpCode`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OpCode(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "D.OpCode")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "OpCode"), "D.OpCode")(TParseTree("", false,[], s));
        }
    }
    static string OpCode(GetName g)
    {
        return "D.OpCode";
    }

    static TParseTree InterfaceDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceTemplateDeclaration, Spacing)), "D.InterfaceDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`InterfaceDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceTemplateDeclaration, Spacing)), "InterfaceDeclaration"), "D.InterfaceDeclaration")(p);
                memo[tuple(`InterfaceDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InterfaceDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceTemplateDeclaration, Spacing)), "D.InterfaceDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceTemplateDeclaration, Spacing)), "InterfaceDeclaration"), "D.InterfaceDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string InterfaceDeclaration(GetName g)
    {
        return "D.InterfaceDeclaration";
    }

    static TParseTree BaseInterfaceList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), "D.BaseInterfaceList")(p);
        }
        else
        {
            if(auto m = tuple(`BaseInterfaceList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), "BaseInterfaceList"), "D.BaseInterfaceList")(p);
                memo[tuple(`BaseInterfaceList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BaseInterfaceList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), "D.BaseInterfaceList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), "BaseInterfaceList"), "D.BaseInterfaceList")(TParseTree("", false,[], s));
        }
    }
    static string BaseInterfaceList(GetName g)
    {
        return "D.BaseInterfaceList";
    }

    static TParseTree InterfaceBody(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.InterfaceBody")(p);
        }
        else
        {
            if(auto m = tuple(`InterfaceBody`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "InterfaceBody"), "D.InterfaceBody")(p);
                memo[tuple(`InterfaceBody`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InterfaceBody(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.InterfaceBody")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "InterfaceBody"), "D.InterfaceBody")(TParseTree("", false,[], s));
        }
    }
    static string InterfaceBody(GetName g)
    {
        return "D.InterfaceBody";
    }

    static TParseTree Pragma(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.Pragma")(p);
        }
        else
        {
            if(auto m = tuple(`Pragma`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Pragma"), "D.Pragma")(p);
                memo[tuple(`Pragma`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Pragma(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.Pragma")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Pragma"), "D.Pragma")(TParseTree("", false,[], s));
        }
    }
    static string Pragma(GetName g)
    {
        return "D.Pragma";
    }

    static TParseTree AggregateDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, StructBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, StructTemplateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, UnionTemplateDeclaration, Spacing)), "D.AggregateDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`AggregateDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, StructBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, StructTemplateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, UnionTemplateDeclaration, Spacing)), "AggregateDeclaration"), "D.AggregateDeclaration")(p);
                memo[tuple(`AggregateDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AggregateDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, StructBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, StructTemplateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, UnionTemplateDeclaration, Spacing)), "D.AggregateDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, StructBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, StructTemplateDeclaration, Spacing), pegged.peg.wrapAround!(Spacing, UnionTemplateDeclaration, Spacing)), "AggregateDeclaration"), "D.AggregateDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string AggregateDeclaration(GetName g)
    {
        return "D.AggregateDeclaration";
    }

    static TParseTree StructBody(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.StructBody")(p);
        }
        else
        {
            if(auto m = tuple(`StructBody`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "StructBody"), "D.StructBody")(p);
                memo[tuple(`StructBody`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructBody(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.StructBody")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "StructBody"), "D.StructBody")(TParseTree("", false,[], s));
        }
    }
    static string StructBody(GetName g)
    {
        return "D.StructBody";
    }

    static TParseTree StructBodyDeclarations(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing))), "D.StructBodyDeclarations")(p);
        }
        else
        {
            if(auto m = tuple(`StructBodyDeclarations`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing))), "StructBodyDeclarations"), "D.StructBodyDeclarations")(p);
                memo[tuple(`StructBodyDeclarations`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructBodyDeclarations(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing))), "D.StructBodyDeclarations")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing))), "StructBodyDeclarations"), "D.StructBodyDeclarations")(TParseTree("", false,[], s));
        }
    }
    static string StructBodyDeclarations(GetName g)
    {
        return "D.StructBodyDeclarations";
    }

    static TParseTree StructBodyDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.wrapAround!(Spacing, StructAllocator, Spacing), pegged.peg.wrapAround!(Spacing, StructDeallocator, Spacing), pegged.peg.wrapAround!(Spacing, StructPostblit, Spacing), pegged.peg.wrapAround!(Spacing, AliasThis, Spacing)), "D.StructBodyDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`StructBodyDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.wrapAround!(Spacing, StructAllocator, Spacing), pegged.peg.wrapAround!(Spacing, StructDeallocator, Spacing), pegged.peg.wrapAround!(Spacing, StructPostblit, Spacing), pegged.peg.wrapAround!(Spacing, AliasThis, Spacing)), "StructBodyDeclaration"), "D.StructBodyDeclaration")(p);
                memo[tuple(`StructBodyDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructBodyDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.wrapAround!(Spacing, StructAllocator, Spacing), pegged.peg.wrapAround!(Spacing, StructDeallocator, Spacing), pegged.peg.wrapAround!(Spacing, StructPostblit, Spacing), pegged.peg.wrapAround!(Spacing, AliasThis, Spacing)), "D.StructBodyDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing), pegged.peg.wrapAround!(Spacing, StructAllocator, Spacing), pegged.peg.wrapAround!(Spacing, StructDeallocator, Spacing), pegged.peg.wrapAround!(Spacing, StructPostblit, Spacing), pegged.peg.wrapAround!(Spacing, AliasThis, Spacing)), "StructBodyDeclaration"), "D.StructBodyDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string StructBodyDeclaration(GetName g)
    {
        return "D.StructBodyDeclaration";
    }

    static TParseTree StructAllocator(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing), "D.StructAllocator")(p);
        }
        else
        {
            if(auto m = tuple(`StructAllocator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing), "StructAllocator"), "D.StructAllocator")(p);
                memo[tuple(`StructAllocator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructAllocator(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing), "D.StructAllocator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing), "StructAllocator"), "D.StructAllocator")(TParseTree("", false,[], s));
        }
    }
    static string StructAllocator(GetName g)
    {
        return "D.StructAllocator";
    }

    static TParseTree StructDeallocator(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing), "D.StructDeallocator")(p);
        }
        else
        {
            if(auto m = tuple(`StructDeallocator`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing), "StructDeallocator"), "D.StructDeallocator")(p);
                memo[tuple(`StructDeallocator`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructDeallocator(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing), "D.StructDeallocator")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing), "StructDeallocator"), "D.StructDeallocator")(TParseTree("", false,[], s));
        }
    }
    static string StructDeallocator(GetName g)
    {
        return "D.StructDeallocator";
    }

    static TParseTree StructPostblit(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this(this)"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.StructPostblit")(p);
        }
        else
        {
            if(auto m = tuple(`StructPostblit`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this(this)"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "StructPostblit"), "D.StructPostblit")(p);
                memo[tuple(`StructPostblit`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructPostblit(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this(this)"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.StructPostblit")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this(this)"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "StructPostblit"), "D.StructPostblit")(TParseTree("", false,[], s));
        }
    }
    static string StructPostblit(GetName g)
    {
        return "D.StructPostblit";
    }

    static TParseTree TemplateDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing))), "D.TemplateDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing))), "TemplateDeclaration"), "D.TemplateDeclaration")(p);
                memo[tuple(`TemplateDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing))), "D.TemplateDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing))), "TemplateDeclaration"), "D.TemplateDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string TemplateDeclaration(GetName g)
    {
        return "D.TemplateDeclaration";
    }

    static TParseTree TemplateIdentifier(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "D.TemplateIdentifier")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateIdentifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "TemplateIdentifier"), "D.TemplateIdentifier")(p);
                memo[tuple(`TemplateIdentifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateIdentifier(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "D.TemplateIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "TemplateIdentifier"), "D.TemplateIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string TemplateIdentifier(GetName g)
    {
        return "D.TemplateIdentifier";
    }

    static TParseTree TemplateParameterList(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing)), Spacing), "D.TemplateParameterList")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateParameterList`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing)), Spacing), "TemplateParameterList"), "D.TemplateParameterList")(p);
                memo[tuple(`TemplateParameterList`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateParameterList(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing)), Spacing), "D.TemplateParameterList")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing)), Spacing), "TemplateParameterList"), "D.TemplateParameterList")(TParseTree("", false,[], s));
        }
    }
    static string TemplateParameterList(GetName g)
    {
        return "D.TemplateParameterList";
    }

    static TParseTree TemplateParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateValueParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateAliasParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateTupleParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateThisParameter, Spacing)), "D.TemplateParameter")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateParameter`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateValueParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateAliasParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateTupleParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateThisParameter, Spacing)), "TemplateParameter"), "D.TemplateParameter")(p);
                memo[tuple(`TemplateParameter`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateParameter(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateValueParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateAliasParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateTupleParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateThisParameter, Spacing)), "D.TemplateParameter")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateValueParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateAliasParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateTupleParameter, Spacing), pegged.peg.wrapAround!(Spacing, TemplateThisParameter, Spacing)), "TemplateParameter"), "D.TemplateParameter")(TParseTree("", false,[], s));
        }
    }
    static string TemplateParameter(GetName g)
    {
        return "D.TemplateParameter";
    }

    static TParseTree TemplateInstance(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateSingleArgument, Spacing))), Spacing)), "D.TemplateInstance")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateInstance`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateSingleArgument, Spacing))), Spacing)), "TemplateInstance"), "D.TemplateInstance")(p);
                memo[tuple(`TemplateInstance`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateInstance(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateSingleArgument, Spacing))), Spacing)), "D.TemplateInstance")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateSingleArgument, Spacing))), Spacing)), "TemplateInstance"), "D.TemplateInstance")(TParseTree("", false,[], s));
        }
    }
    static string TemplateInstance(GetName g)
    {
        return "D.TemplateInstance";
    }

    static TParseTree TemplateArgument(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, Symbol, Spacing)), "D.TemplateArgument")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateArgument`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, Symbol, Spacing)), "TemplateArgument"), "D.TemplateArgument")(p);
                memo[tuple(`TemplateArgument`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateArgument(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, Symbol, Spacing)), "D.TemplateArgument")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, Symbol, Spacing)), "TemplateArgument"), "D.TemplateArgument")(TParseTree("", false,[], s));
        }
    }
    static string TemplateArgument(GetName g)
    {
        return "D.TemplateArgument";
    }

    static TParseTree Symbol(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing)), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), "D.Symbol")(p);
        }
        else
        {
            if(auto m = tuple(`Symbol`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing)), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), "Symbol"), "D.Symbol")(p);
                memo[tuple(`Symbol`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Symbol(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing)), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), "D.Symbol")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing)), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), "Symbol"), "D.Symbol")(TParseTree("", false,[], s));
        }
    }
    static string Symbol(GetName g)
    {
        return "D.Symbol";
    }

    static TParseTree SymbolTail(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), Spacing)))), "D.SymbolTail")(p);
        }
        else
        {
            if(auto m = tuple(`SymbolTail`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), Spacing)))), "SymbolTail"), "D.SymbolTail")(p);
                memo[tuple(`SymbolTail`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SymbolTail(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), Spacing)))), "D.SymbolTail")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing)), Spacing)))), "SymbolTail"), "D.SymbolTail")(TParseTree("", false,[], s));
        }
    }
    static string SymbolTail(GetName g)
    {
        return "D.SymbolTail";
    }

    static TParseTree TemplateSingleArgument(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing), pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "D.TemplateSingleArgument")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateSingleArgument`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing), pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "TemplateSingleArgument"), "D.TemplateSingleArgument")(p);
                memo[tuple(`TemplateSingleArgument`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateSingleArgument(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing), pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "D.TemplateSingleArgument")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing), pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "TemplateSingleArgument"), "D.TemplateSingleArgument")(TParseTree("", false,[], s));
        }
    }
    static string TemplateSingleArgument(GetName g)
    {
        return "D.TemplateSingleArgument";
    }

    static TParseTree TemplateTypeParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPDefault, Spacing))), "D.TemplateTypeParameter")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateTypeParameter`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPDefault, Spacing))), "TemplateTypeParameter"), "D.TemplateTypeParameter")(p);
                memo[tuple(`TemplateTypeParameter`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateTypeParameter(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPDefault, Spacing))), "D.TemplateTypeParameter")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPDefault, Spacing))), "TemplateTypeParameter"), "D.TemplateTypeParameter")(TParseTree("", false,[], s));
        }
    }
    static string TemplateTypeParameter(GetName g)
    {
        return "D.TemplateTypeParameter";
    }

    static TParseTree TTPSpecialization(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "D.TTPSpecialization")(p);
        }
        else
        {
            if(auto m = tuple(`TTPSpecialization`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "TTPSpecialization"), "D.TTPSpecialization")(p);
                memo[tuple(`TTPSpecialization`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TTPSpecialization(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "D.TTPSpecialization")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "TTPSpecialization"), "D.TTPSpecialization")(TParseTree("", false,[], s));
        }
    }
    static string TTPSpecialization(GetName g)
    {
        return "D.TTPSpecialization";
    }

    static TParseTree TTPDefault(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "D.TTPDefault")(p);
        }
        else
        {
            if(auto m = tuple(`TTPDefault`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "TTPDefault"), "D.TTPDefault")(p);
                memo[tuple(`TTPDefault`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TTPDefault(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "D.TTPDefault")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "TTPDefault"), "D.TTPDefault")(TParseTree("", false,[], s));
        }
    }
    static string TTPDefault(GetName g)
    {
        return "D.TTPDefault";
    }

    static TParseTree TemplateThisParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing)), "D.TemplateThisParameter")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateThisParameter`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing)), "TemplateThisParameter"), "D.TemplateThisParameter")(p);
                memo[tuple(`TemplateThisParameter`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateThisParameter(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing)), "D.TemplateThisParameter")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing)), "TemplateThisParameter"), "D.TemplateThisParameter")(TParseTree("", false,[], s));
        }
    }
    static string TemplateThisParameter(GetName g)
    {
        return "D.TemplateThisParameter";
    }

    static TParseTree TemplateValueParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPDefault, Spacing))), "D.TemplateValueParameter")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateValueParameter`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPDefault, Spacing))), "TemplateValueParameter"), "D.TemplateValueParameter")(p);
                memo[tuple(`TemplateValueParameter`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateValueParameter(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPDefault, Spacing))), "D.TemplateValueParameter")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPDefault, Spacing))), "TemplateValueParameter"), "D.TemplateValueParameter")(TParseTree("", false,[], s));
        }
    }
    static string TemplateValueParameter(GetName g)
    {
        return "D.TemplateValueParameter";
    }

    static TParseTree TVPSpecialization(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), "D.TVPSpecialization")(p);
        }
        else
        {
            if(auto m = tuple(`TVPSpecialization`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), "TVPSpecialization"), "D.TVPSpecialization")(p);
                memo[tuple(`TVPSpecialization`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TVPSpecialization(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), "D.TVPSpecialization")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), "TVPSpecialization"), "D.TVPSpecialization")(TParseTree("", false,[], s));
        }
    }
    static string TVPSpecialization(GetName g)
    {
        return "D.TVPSpecialization";
    }

    static TParseTree TVPDefault(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), "D.TVPDefault")(p);
        }
        else
        {
            if(auto m = tuple(`TVPDefault`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), "TVPDefault"), "D.TVPDefault")(p);
                memo[tuple(`TVPDefault`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TVPDefault(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), "D.TVPDefault")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), "TVPDefault"), "D.TVPDefault")(TParseTree("", false,[], s));
        }
    }
    static string TVPDefault(GetName g)
    {
        return "D.TVPDefault";
    }

    static TParseTree TemplateAliasParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPDefault, Spacing))), "D.TemplateAliasParameter")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateAliasParameter`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPDefault, Spacing))), "TemplateAliasParameter"), "D.TemplateAliasParameter")(p);
                memo[tuple(`TemplateAliasParameter`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateAliasParameter(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPDefault, Spacing))), "D.TemplateAliasParameter")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPDefault, Spacing))), "TemplateAliasParameter"), "D.TemplateAliasParameter")(TParseTree("", false,[], s));
        }
    }
    static string TemplateAliasParameter(GetName g)
    {
        return "D.TemplateAliasParameter";
    }

    static TParseTree TAPSpecialization(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing)), "D.TAPSpecialization")(p);
        }
        else
        {
            if(auto m = tuple(`TAPSpecialization`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing)), "TAPSpecialization"), "D.TAPSpecialization")(p);
                memo[tuple(`TAPSpecialization`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TAPSpecialization(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing)), "D.TAPSpecialization")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing)), "TAPSpecialization"), "D.TAPSpecialization")(TParseTree("", false,[], s));
        }
    }
    static string TAPSpecialization(GetName g)
    {
        return "D.TAPSpecialization";
    }

    static TParseTree TAPDefault(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing)), "D.TAPDefault")(p);
        }
        else
        {
            if(auto m = tuple(`TAPDefault`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing)), "TAPDefault"), "D.TAPDefault")(p);
                memo[tuple(`TAPDefault`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TAPDefault(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing)), "D.TAPDefault")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing)), Spacing)), "TAPDefault"), "D.TAPDefault")(TParseTree("", false,[], s));
        }
    }
    static string TAPDefault(GetName g)
    {
        return "D.TAPDefault";
    }

    static TParseTree TemplateTupleParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), "D.TemplateTupleParameter")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateTupleParameter`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), "TemplateTupleParameter"), "D.TemplateTupleParameter")(p);
                memo[tuple(`TemplateTupleParameter`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateTupleParameter(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), "D.TemplateTupleParameter")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), "TemplateTupleParameter"), "D.TemplateTupleParameter")(TParseTree("", false,[], s));
        }
    }
    static string TemplateTupleParameter(GetName g)
    {
        return "D.TemplateTupleParameter";
    }

    static TParseTree TemplatedConstructor(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.TemplatedConstructor")(p);
        }
        else
        {
            if(auto m = tuple(`TemplatedConstructor`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "TemplatedConstructor"), "D.TemplatedConstructor")(p);
                memo[tuple(`TemplatedConstructor`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplatedConstructor(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.TemplatedConstructor")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "TemplatedConstructor"), "D.TemplatedConstructor")(TParseTree("", false,[], s));
        }
    }
    static string TemplatedConstructor(GetName g)
    {
        return "D.TemplatedConstructor";
    }

    static TParseTree ClassTemplateDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), "D.ClassTemplateDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`ClassTemplateDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), "ClassTemplateDeclaration"), "D.ClassTemplateDeclaration")(p);
                memo[tuple(`ClassTemplateDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ClassTemplateDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), "D.ClassTemplateDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), "ClassTemplateDeclaration"), "D.ClassTemplateDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string ClassTemplateDeclaration(GetName g)
    {
        return "D.ClassTemplateDeclaration";
    }

    static TParseTree StructTemplateDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), "D.StructTemplateDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`StructTemplateDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), "StructTemplateDeclaration"), "D.StructTemplateDeclaration")(p);
                memo[tuple(`StructTemplateDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructTemplateDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), "D.StructTemplateDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), "StructTemplateDeclaration"), "D.StructTemplateDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string StructTemplateDeclaration(GetName g)
    {
        return "D.StructTemplateDeclaration";
    }

    static TParseTree UnionTemplateDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), "D.UnionTemplateDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`UnionTemplateDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), "UnionTemplateDeclaration"), "D.UnionTemplateDeclaration")(p);
                memo[tuple(`UnionTemplateDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnionTemplateDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), "D.UnionTemplateDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), "UnionTemplateDeclaration"), "D.UnionTemplateDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string UnionTemplateDeclaration(GetName g)
    {
        return "D.UnionTemplateDeclaration";
    }

    static TParseTree InterfaceTemplateDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), "D.InterfaceTemplateDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`InterfaceTemplateDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), "InterfaceTemplateDeclaration"), "D.InterfaceTemplateDeclaration")(p);
                memo[tuple(`InterfaceTemplateDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree InterfaceTemplateDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), "D.InterfaceTemplateDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), "InterfaceTemplateDeclaration"), "D.InterfaceTemplateDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string InterfaceTemplateDeclaration(GetName g)
    {
        return "D.InterfaceTemplateDeclaration";
    }

    static TParseTree Constraint(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.Constraint")(p);
        }
        else
        {
            if(auto m = tuple(`Constraint`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Constraint"), "D.Constraint")(p);
                memo[tuple(`Constraint`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Constraint(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.Constraint")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "Constraint"), "D.Constraint")(TParseTree("", false,[], s));
        }
    }
    static string Constraint(GetName g)
    {
        return "D.Constraint";
    }

    static TParseTree TemplateMixinDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.TemplateMixinDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateMixinDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "TemplateMixinDeclaration"), "D.TemplateMixinDeclaration")(p);
                memo[tuple(`TemplateMixinDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateMixinDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "D.TemplateMixinDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "TemplateMixinDeclaration"), "D.TemplateMixinDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string TemplateMixinDeclaration(GetName g)
    {
        return "D.TemplateMixinDeclaration";
    }

    static TParseTree TemplateMixin(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MixinIdentifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.TemplateMixin")(p);
        }
        else
        {
            if(auto m = tuple(`TemplateMixin`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MixinIdentifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "TemplateMixin"), "D.TemplateMixin")(p);
                memo[tuple(`TemplateMixin`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateMixin(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MixinIdentifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.TemplateMixin")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MixinIdentifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "TemplateMixin"), "D.TemplateMixin")(TParseTree("", false,[], s));
        }
    }
    static string TemplateMixin(GetName g)
    {
        return "D.TemplateMixin";
    }

    static TParseTree MixinIdentifier(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "D.MixinIdentifier")(p);
        }
        else
        {
            if(auto m = tuple(`MixinIdentifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "MixinIdentifier"), "D.MixinIdentifier")(p);
                memo[tuple(`MixinIdentifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MixinIdentifier(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "D.MixinIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), "MixinIdentifier"), "D.MixinIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string MixinIdentifier(GetName g)
    {
        return "D.MixinIdentifier";
    }

    static TParseTree TraitsExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TraitsKeyword, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.TraitsExpression")(p);
        }
        else
        {
            if(auto m = tuple(`TraitsExpression`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TraitsKeyword, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "TraitsExpression"), "D.TraitsExpression")(p);
                memo[tuple(`TraitsExpression`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TraitsExpression(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TraitsKeyword, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.TraitsExpression")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TraitsKeyword, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "TraitsExpression"), "D.TraitsExpression")(TParseTree("", false,[], s));
        }
    }
    static string TraitsExpression(GetName g)
    {
        return "D.TraitsExpression";
    }

    static TParseTree TraitsKeyword(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractClass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isArithmetic"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAssociativeArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalClass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFloating"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isIntegral"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isScalar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isUnsigned"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVitualFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVirtualMethod"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isRef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isOut"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isLazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("hasMember"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("identifier"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getMember"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getOverloads"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualFunctions"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualMethods"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("parent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("classInstanceSize"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("allMembers"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("derivedMembers"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isSame"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("compiles"), Spacing)), "D.TraitsKeyword")(p);
        }
        else
        {
            if(auto m = tuple(`TraitsKeyword`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractClass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isArithmetic"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAssociativeArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalClass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFloating"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isIntegral"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isScalar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isUnsigned"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVitualFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVirtualMethod"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isRef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isOut"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isLazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("hasMember"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("identifier"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getMember"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getOverloads"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualFunctions"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualMethods"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("parent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("classInstanceSize"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("allMembers"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("derivedMembers"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isSame"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("compiles"), Spacing)), "TraitsKeyword"), "D.TraitsKeyword")(p);
                memo[tuple(`TraitsKeyword`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TraitsKeyword(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractClass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isArithmetic"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAssociativeArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalClass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFloating"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isIntegral"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isScalar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isUnsigned"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVitualFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVirtualMethod"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isRef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isOut"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isLazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("hasMember"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("identifier"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getMember"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getOverloads"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualFunctions"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualMethods"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("parent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("classInstanceSize"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("allMembers"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("derivedMembers"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isSame"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("compiles"), Spacing)), "D.TraitsKeyword")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractClass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isArithmetic"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAssociativeArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalClass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFloating"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isIntegral"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isScalar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticArray"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isUnsigned"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVitualFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVirtualMethod"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticFunction"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isRef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isOut"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isLazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("hasMember"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("identifier"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getMember"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getOverloads"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualFunctions"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualMethods"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("parent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("classInstanceSize"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("allMembers"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("derivedMembers"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isSame"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("compiles"), Spacing)), "TraitsKeyword"), "D.TraitsKeyword")(TParseTree("", false,[], s));
        }
    }
    static string TraitsKeyword(GetName g)
    {
        return "D.TraitsKeyword";
    }

    static TParseTree TraitsArgument(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "D.TraitsArgument")(p);
        }
        else
        {
            if(auto m = tuple(`TraitsArgument`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "TraitsArgument"), "D.TraitsArgument")(p);
                memo[tuple(`TraitsArgument`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TraitsArgument(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "D.TraitsArgument")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing)), "TraitsArgument"), "D.TraitsArgument")(TParseTree("", false,[], s));
        }
    }
    static string TraitsArgument(GetName g)
    {
        return "D.TraitsArgument";
    }

    static TParseTree UnitTest(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.UnitTest")(p);
        }
        else
        {
            if(auto m = tuple(`UnitTest`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "UnitTest"), "D.UnitTest")(p);
                memo[tuple(`UnitTest`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnitTest(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "D.UnitTest")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), "UnitTest"), "D.UnitTest")(TParseTree("", false,[], s));
        }
    }
    static string UnitTest(GetName g)
    {
        return "D.UnitTest";
    }

    static TParseTree ConditionalDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Declarations, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing)), Spacing)))), "D.ConditionalDeclaration")(p);
        }
        else
        {
            if(auto m = tuple(`ConditionalDeclaration`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Declarations, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing)), Spacing)))), "ConditionalDeclaration"), "D.ConditionalDeclaration")(p);
                memo[tuple(`ConditionalDeclaration`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConditionalDeclaration(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Declarations, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing)), Spacing)))), "D.ConditionalDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Declarations, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing)), Spacing)))), "ConditionalDeclaration"), "D.ConditionalDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string ConditionalDeclaration(GetName g)
    {
        return "D.ConditionalDeclaration";
    }

    static TParseTree CCDeclarationBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.CCDeclarationBlock")(p);
        }
        else
        {
            if(auto m = tuple(`CCDeclarationBlock`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CCDeclarationBlock"), "D.CCDeclarationBlock")(p);
                memo[tuple(`CCDeclarationBlock`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CCDeclarationBlock(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.CCDeclarationBlock")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "CCDeclarationBlock"), "D.CCDeclarationBlock")(TParseTree("", false,[], s));
        }
    }
    static string CCDeclarationBlock(GetName g)
    {
        return "D.CCDeclarationBlock";
    }

    static TParseTree Declarations(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "D.Declarations")(p);
        }
        else
        {
            if(auto m = tuple(`Declarations`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "Declarations"), "D.Declarations")(p);
                memo[tuple(`Declarations`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Declarations(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "D.Declarations")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), "Declarations"), "D.Declarations")(TParseTree("", false,[], s));
        }
    }
    static string Declarations(GetName g)
    {
        return "D.Declarations";
    }

    static TParseTree ConditionalStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), Spacing))), "D.ConditionalStatement")(p);
        }
        else
        {
            if(auto m = tuple(`ConditionalStatement`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), Spacing))), "ConditionalStatement"), "D.ConditionalStatement")(p);
                memo[tuple(`ConditionalStatement`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConditionalStatement(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), Spacing))), "D.ConditionalStatement")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing)), Spacing))), "ConditionalStatement"), "D.ConditionalStatement")(TParseTree("", false,[], s));
        }
    }
    static string ConditionalStatement(GetName g)
    {
        return "D.ConditionalStatement";
    }

    static TParseTree Condition(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VersionCondition, Spacing), pegged.peg.wrapAround!(Spacing, DebugCondition, Spacing), pegged.peg.wrapAround!(Spacing, StaticIfCondition, Spacing)), "D.Condition")(p);
        }
        else
        {
            if(auto m = tuple(`Condition`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VersionCondition, Spacing), pegged.peg.wrapAround!(Spacing, DebugCondition, Spacing), pegged.peg.wrapAround!(Spacing, StaticIfCondition, Spacing)), "Condition"), "D.Condition")(p);
                memo[tuple(`Condition`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Condition(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VersionCondition, Spacing), pegged.peg.wrapAround!(Spacing, DebugCondition, Spacing), pegged.peg.wrapAround!(Spacing, StaticIfCondition, Spacing)), "D.Condition")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VersionCondition, Spacing), pegged.peg.wrapAround!(Spacing, DebugCondition, Spacing), pegged.peg.wrapAround!(Spacing, StaticIfCondition, Spacing)), "Condition"), "D.Condition")(TParseTree("", false,[], s));
        }
    }
    static string Condition(GetName g)
    {
        return "D.Condition";
    }

    static TParseTree VersionCondition(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.VersionCondition")(p);
        }
        else
        {
            if(auto m = tuple(`VersionCondition`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "VersionCondition"), "D.VersionCondition")(p);
                memo[tuple(`VersionCondition`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VersionCondition(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.VersionCondition")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "VersionCondition"), "D.VersionCondition")(TParseTree("", false,[], s));
        }
    }
    static string VersionCondition(GetName g)
    {
        return "D.VersionCondition";
    }

    static TParseTree VersionSpecification(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.VersionSpecification")(p);
        }
        else
        {
            if(auto m = tuple(`VersionSpecification`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "VersionSpecification"), "D.VersionSpecification")(p);
                memo[tuple(`VersionSpecification`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VersionSpecification(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.VersionSpecification")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "VersionSpecification"), "D.VersionSpecification")(TParseTree("", false,[], s));
        }
    }
    static string VersionSpecification(GetName g)
    {
        return "D.VersionSpecification";
    }

    static TParseTree DebugCondition(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing))), "D.DebugCondition")(p);
        }
        else
        {
            if(auto m = tuple(`DebugCondition`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing))), "DebugCondition"), "D.DebugCondition")(p);
                memo[tuple(`DebugCondition`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DebugCondition(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing))), "D.DebugCondition")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing))), "DebugCondition"), "D.DebugCondition")(TParseTree("", false,[], s));
        }
    }
    static string DebugCondition(GetName g)
    {
        return "D.DebugCondition";
    }

    static TParseTree DebugSpecification(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.DebugSpecification")(p);
        }
        else
        {
            if(auto m = tuple(`DebugSpecification`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DebugSpecification"), "D.DebugSpecification")(p);
                memo[tuple(`DebugSpecification`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DebugSpecification(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.DebugSpecification")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DebugSpecification"), "D.DebugSpecification")(TParseTree("", false,[], s));
        }
    }
    static string DebugSpecification(GetName g)
    {
        return "D.DebugSpecification";
    }

    static TParseTree StaticIfCondition(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.StaticIfCondition")(p);
        }
        else
        {
            if(auto m = tuple(`StaticIfCondition`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "StaticIfCondition"), "D.StaticIfCondition")(p);
                memo[tuple(`StaticIfCondition`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StaticIfCondition(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "D.StaticIfCondition")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "StaticIfCondition"), "D.StaticIfCondition")(TParseTree("", false,[], s));
        }
    }
    static string StaticIfCondition(GetName g)
    {
        return "D.StaticIfCondition";
    }

    static TParseTree StaticAssert(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.StaticAssert")(p);
        }
        else
        {
            if(auto m = tuple(`StaticAssert`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "StaticAssert"), "D.StaticAssert")(p);
                memo[tuple(`StaticAssert`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StaticAssert(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "D.StaticAssert")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "StaticAssert"), "D.StaticAssert")(TParseTree("", false,[], s));
        }
    }
    static string StaticAssert(GetName g)
    {
        return "D.StaticAssert";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"))))), "D.Identifier")(p);
        }
        else
        {
            if(auto m = tuple(`Identifier`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"))))), "Identifier"), "D.Identifier")(p);
                memo[tuple(`Identifier`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"))))), "D.Identifier")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"))))), "Identifier"), "D.Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "D.Identifier";
    }

    static TParseTree Keyword(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cdouble"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cfloat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("creal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("idouble"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ifloat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ireal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ucent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("volatile"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__thread"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing)), "D.Keyword")(p);
        }
        else
        {
            if(auto m = tuple(`Keyword`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cdouble"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cfloat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("creal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("idouble"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ifloat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ireal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ucent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("volatile"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__thread"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing)), "Keyword"), "D.Keyword")(p);
                memo[tuple(`Keyword`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Keyword(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cdouble"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cfloat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("creal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("idouble"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ifloat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ireal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ucent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("volatile"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__thread"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing)), "D.Keyword")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cdouble"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cfloat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("creal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("idouble"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ifloat"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ireal"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ucent"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("volatile"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__thread"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing)), "Keyword"), "D.Keyword")(TParseTree("", false,[], s));
        }
    }
    static string Keyword(GetName g)
    {
        return "D.Keyword";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment)), "D.Spacing")(p);
        }
        else
        {
            if(auto m = tuple(`Spacing`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment)), "Spacing"), "D.Spacing")(p);
                memo[tuple(`Spacing`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment)), "D.Spacing")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment)), "Spacing"), "D.Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "D.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(BlockComment, LineComment, NestingBlockComment), "D.Comment")(p);
        }
        else
        {
            if(auto m = tuple(`Comment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(BlockComment, LineComment, NestingBlockComment), "Comment"), "D.Comment")(p);
                memo[tuple(`Comment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(BlockComment, LineComment, NestingBlockComment), "D.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(BlockComment, LineComment, NestingBlockComment), "Comment"), "D.Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "D.Comment";
    }

    static TParseTree BlockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/ *")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("* /")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("* /")))), "D.BlockComment")(p);
        }
        else
        {
            if(auto m = tuple(`BlockComment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/ *")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("* /")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("* /")))), "BlockComment"), "D.BlockComment")(p);
                memo[tuple(`BlockComment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BlockComment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/ *")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("* /")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("* /")))), "D.BlockComment")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/ *")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("* /")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("* /")))), "BlockComment"), "D.BlockComment")(TParseTree("", false,[], s));
        }
    }
    static string BlockComment(GetName g)
    {
        return "D.BlockComment";
    }

    static TParseTree LineComment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "D.LineComment")(p);
        }
        else
        {
            if(auto m = tuple(`LineComment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "LineComment"), "D.LineComment")(p);
                memo[tuple(`LineComment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LineComment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "D.LineComment")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(endOfLine))), "LineComment"), "D.LineComment")(TParseTree("", false,[], s));
        }
    }
    static string LineComment(GetName g)
    {
        return "D.LineComment";
    }

    static TParseTree NestingBlockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/+")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any)), pegged.peg.option!(NestingBlockComment), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("+/")))), "D.NestingBlockComment")(p);
        }
        else
        {
            if(auto m = tuple(`NestingBlockComment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/+")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any)), pegged.peg.option!(NestingBlockComment), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("+/")))), "NestingBlockComment"), "D.NestingBlockComment")(p);
                memo[tuple(`NestingBlockComment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestingBlockComment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/+")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any)), pegged.peg.option!(NestingBlockComment), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("+/")))), "D.NestingBlockComment")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/+")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any)), pegged.peg.option!(NestingBlockComment), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any)), pegged.peg.discard!(pegged.peg.literal!("+/")))), "NestingBlockComment"), "D.NestingBlockComment")(TParseTree("", false,[], s));
        }
    }
    static string NestingBlockComment(GetName g)
    {
        return "D.NestingBlockComment";
    }

    static TParseTree StringLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, WysiwygString, Spacing), pegged.peg.wrapAround!(Spacing, AlternateWysiwygString, Spacing), pegged.peg.wrapAround!(Spacing, DoublequotedString, Spacing), pegged.peg.wrapAround!(Spacing, TokenString, Spacing)), "D.StringLiteral")(p);
        }
        else
        {
            if(auto m = tuple(`StringLiteral`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, WysiwygString, Spacing), pegged.peg.wrapAround!(Spacing, AlternateWysiwygString, Spacing), pegged.peg.wrapAround!(Spacing, DoublequotedString, Spacing), pegged.peg.wrapAround!(Spacing, TokenString, Spacing)), "StringLiteral"), "D.StringLiteral")(p);
                memo[tuple(`StringLiteral`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringLiteral(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, WysiwygString, Spacing), pegged.peg.wrapAround!(Spacing, AlternateWysiwygString, Spacing), pegged.peg.wrapAround!(Spacing, DoublequotedString, Spacing), pegged.peg.wrapAround!(Spacing, TokenString, Spacing)), "D.StringLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, WysiwygString, Spacing), pegged.peg.wrapAround!(Spacing, AlternateWysiwygString, Spacing), pegged.peg.wrapAround!(Spacing, DoublequotedString, Spacing), pegged.peg.wrapAround!(Spacing, TokenString, Spacing)), "StringLiteral"), "D.StringLiteral")(TParseTree("", false,[], s));
        }
    }
    static string StringLiteral(GetName g)
    {
        return "D.StringLiteral";
    }

    static TParseTree WysiwygString(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote, pegged.peg.option!(StringPostfix)), "D.WysiwygString")(p);
        }
        else
        {
            if(auto m = tuple(`WysiwygString`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote, pegged.peg.option!(StringPostfix)), "WysiwygString"), "D.WysiwygString")(p);
                memo[tuple(`WysiwygString`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree WysiwygString(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote, pegged.peg.option!(StringPostfix)), "D.WysiwygString")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), doublequote, pegged.peg.option!(StringPostfix)), "WysiwygString"), "D.WysiwygString")(TParseTree("", false,[], s));
        }
    }
    static string WysiwygString(GetName g)
    {
        return "D.WysiwygString";
    }

    static TParseTree AlternateWysiwygString(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote, pegged.peg.option!(StringPostfix)), "D.AlternateWysiwygString")(p);
        }
        else
        {
            if(auto m = tuple(`AlternateWysiwygString`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote, pegged.peg.option!(StringPostfix)), "AlternateWysiwygString"), "D.AlternateWysiwygString")(p);
                memo[tuple(`AlternateWysiwygString`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AlternateWysiwygString(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote, pegged.peg.option!(StringPostfix)), "D.AlternateWysiwygString")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any)), backquote, pegged.peg.option!(StringPostfix)), "AlternateWysiwygString"), "D.AlternateWysiwygString")(TParseTree("", false,[], s));
        }
    }
    static string AlternateWysiwygString(GetName g)
    {
        return "D.AlternateWysiwygString";
    }

    static TParseTree DoublequotedString(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote, pegged.peg.option!(StringPostfix)), "D.DoublequotedString")(p);
        }
        else
        {
            if(auto m = tuple(`DoublequotedString`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote, pegged.peg.option!(StringPostfix)), "DoublequotedString"), "D.DoublequotedString")(p);
                memo[tuple(`DoublequotedString`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DoublequotedString(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote, pegged.peg.option!(StringPostfix)), "D.DoublequotedString")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(DQChar), doublequote, pegged.peg.option!(StringPostfix)), "DoublequotedString"), "D.DoublequotedString")(TParseTree("", false,[], s));
        }
    }
    static string DoublequotedString(GetName g)
    {
        return "D.DoublequotedString";
    }

    static TParseTree DQChar(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "D.DQChar")(p);
        }
        else
        {
            if(auto m = tuple(`DQChar`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "DQChar"), "D.DQChar")(p);
                memo[tuple(`DQChar`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DQChar(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "D.DQChar")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "DQChar"), "D.DQChar")(TParseTree("", false,[], s));
        }
    }
    static string DQChar(GetName g)
    {
        return "D.DQChar";
    }

    static TParseTree EscapeSequence(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v")), pegged.peg.and!(pegged.peg.literal!("x"), HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit))), "D.EscapeSequence")(p);
        }
        else
        {
            if(auto m = tuple(`EscapeSequence`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v")), pegged.peg.and!(pegged.peg.literal!("x"), HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit))), "EscapeSequence"), "D.EscapeSequence")(p);
                memo[tuple(`EscapeSequence`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EscapeSequence(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v")), pegged.peg.and!(pegged.peg.literal!("x"), HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit))), "D.EscapeSequence")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(backslash, pegged.peg.or!(quote, doublequote, backslash, pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v")), pegged.peg.and!(pegged.peg.literal!("x"), HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit))), "EscapeSequence"), "D.EscapeSequence")(TParseTree("", false,[], s));
        }
    }
    static string EscapeSequence(GetName g)
    {
        return "D.EscapeSequence";
    }

    static TParseTree StringPostfix(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("c"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("w"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("d"), Spacing)), "D.StringPostfix")(p);
        }
        else
        {
            if(auto m = tuple(`StringPostfix`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("c"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("w"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("d"), Spacing)), "StringPostfix"), "D.StringPostfix")(p);
                memo[tuple(`StringPostfix`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringPostfix(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("c"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("w"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("d"), Spacing)), "D.StringPostfix")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("c"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("w"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("d"), Spacing)), "StringPostfix"), "D.StringPostfix")(TParseTree("", false,[], s));
        }
    }
    static string StringPostfix(GetName g)
    {
        return "D.StringPostfix";
    }

    static TParseTree TokenString(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("q{"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("}")), pegged.peg.any)), pegged.peg.literal!("}")), "D.TokenString")(p);
        }
        else
        {
            if(auto m = tuple(`TokenString`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.literal!("q{"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("}")), pegged.peg.any)), pegged.peg.literal!("}")), "TokenString"), "D.TokenString")(p);
                memo[tuple(`TokenString`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TokenString(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("q{"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("}")), pegged.peg.any)), pegged.peg.literal!("}")), "D.TokenString")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.literal!("q{"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("}")), pegged.peg.any)), pegged.peg.literal!("}")), "TokenString"), "D.TokenString")(TParseTree("", false,[], s));
        }
    }
    static string TokenString(GetName g)
    {
        return "D.TokenString";
    }

    static TParseTree CharacterLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(quote, pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.or!(EscapeSequence, pegged.peg.any)), quote), "D.CharacterLiteral")(p);
        }
        else
        {
            if(auto m = tuple(`CharacterLiteral`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(quote, pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.or!(EscapeSequence, pegged.peg.any)), quote), "CharacterLiteral"), "D.CharacterLiteral")(p);
                memo[tuple(`CharacterLiteral`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CharacterLiteral(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(quote, pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.or!(EscapeSequence, pegged.peg.any)), quote), "D.CharacterLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(quote, pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.or!(EscapeSequence, pegged.peg.any)), quote), "CharacterLiteral"), "D.CharacterLiteral")(TParseTree("", false,[], s));
        }
    }
    static string CharacterLiteral(GetName g)
    {
        return "D.CharacterLiteral";
    }

    static TParseTree IntegerLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(DecimalInteger, BinaryInteger, HexadecimalInteger), "D.IntegerLiteral")(p);
        }
        else
        {
            if(auto m = tuple(`IntegerLiteral`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.or!(DecimalInteger, BinaryInteger, HexadecimalInteger), "IntegerLiteral"), "D.IntegerLiteral")(p);
                memo[tuple(`IntegerLiteral`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntegerLiteral(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(DecimalInteger, BinaryInteger, HexadecimalInteger), "D.IntegerLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.or!(DecimalInteger, BinaryInteger, HexadecimalInteger), "IntegerLiteral"), "D.IntegerLiteral")(TParseTree("", false,[], s));
        }
    }
    static string IntegerLiteral(GetName g)
    {
        return "D.IntegerLiteral";
    }

    static TParseTree DecimalInteger(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix)), "D.DecimalInteger")(p);
        }
        else
        {
            if(auto m = tuple(`DecimalInteger`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix)), "DecimalInteger"), "D.DecimalInteger")(p);
                memo[tuple(`DecimalInteger`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DecimalInteger(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix)), "D.DecimalInteger")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix)), "DecimalInteger"), "D.DecimalInteger")(TParseTree("", false,[], s));
        }
    }
    static string DecimalInteger(GetName g)
    {
        return "D.DecimalInteger";
    }

    static TParseTree Integer(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.literal!("_")))), "D.Integer")(p);
        }
        else
        {
            if(auto m = tuple(`Integer`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.literal!("_")))), "Integer"), "D.Integer")(p);
                memo[tuple(`Integer`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Integer(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.literal!("_")))), "D.Integer")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(digit, pegged.peg.literal!("_")))), "Integer"), "D.Integer")(TParseTree("", false,[], s));
        }
    }
    static string Integer(GetName g)
    {
        return "D.Integer";
    }

    static TParseTree IntegerSuffix(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "D.IntegerSuffix")(p);
        }
        else
        {
            if(auto m = tuple(`IntegerSuffix`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "IntegerSuffix"), "D.IntegerSuffix")(p);
                memo[tuple(`IntegerSuffix`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntegerSuffix(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "D.IntegerSuffix")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "IntegerSuffix"), "D.IntegerSuffix")(TParseTree("", false,[], s));
        }
    }
    static string IntegerSuffix(GetName g)
    {
        return "D.IntegerSuffix";
    }

    static TParseTree BinaryInteger(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keywords!("0b", "0B"), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.literal!("_")))), "D.BinaryInteger")(p);
        }
        else
        {
            if(auto m = tuple(`BinaryInteger`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.keywords!("0b", "0B"), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.literal!("_")))), "BinaryInteger"), "D.BinaryInteger")(p);
                memo[tuple(`BinaryInteger`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BinaryInteger(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keywords!("0b", "0B"), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.literal!("_")))), "D.BinaryInteger")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.keywords!("0b", "0B"), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.literal!("_")))), "BinaryInteger"), "D.BinaryInteger")(TParseTree("", false,[], s));
        }
    }
    static string BinaryInteger(GetName g)
    {
        return "D.BinaryInteger";
    }

    static TParseTree HexadecimalInteger(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keywords!("0x", "0X"), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(HexDigit, pegged.peg.literal!("_")))), "D.HexadecimalInteger")(p);
        }
        else
        {
            if(auto m = tuple(`HexadecimalInteger`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.keywords!("0x", "0X"), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(HexDigit, pegged.peg.literal!("_")))), "HexadecimalInteger"), "D.HexadecimalInteger")(p);
                memo[tuple(`HexadecimalInteger`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HexadecimalInteger(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.keywords!("0x", "0X"), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(HexDigit, pegged.peg.literal!("_")))), "D.HexadecimalInteger")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.keywords!("0x", "0X"), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(HexDigit, pegged.peg.literal!("_")))), "HexadecimalInteger"), "D.HexadecimalInteger")(TParseTree("", false,[], s));
        }
    }
    static string HexadecimalInteger(GetName g)
    {
        return "D.HexadecimalInteger";
    }

    static TParseTree HexDigit(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing), "D.HexDigit")(p);
        }
        else
        {
            if(auto m = tuple(`HexDigit`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing), "HexDigit"), "D.HexDigit")(p);
                memo[tuple(`HexDigit`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HexDigit(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing), "D.HexDigit")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing), "HexDigit"), "D.HexDigit")(TParseTree("", false,[], s));
        }
    }
    static string HexDigit(GetName g)
    {
        return "D.HexDigit";
    }

    static TParseTree FloatLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer))), "D.FloatLiteral")(p);
        }
        else
        {
            if(auto m = tuple(`FloatLiteral`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer))), "FloatLiteral"), "D.FloatLiteral")(p);
                memo[tuple(`FloatLiteral`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FloatLiteral(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer))), "D.FloatLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer))), "FloatLiteral"), "D.FloatLiteral")(TParseTree("", false,[], s));
        }
    }
    static string FloatLiteral(GetName g)
    {
        return "D.FloatLiteral";
    }

    static TParseTree Sign(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.option!(pegged.peg.keywords!("-", "+")), "D.Sign")(p);
        }
        else
        {
            if(auto m = tuple(`Sign`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(hooked!(pegged.peg.option!(pegged.peg.keywords!("-", "+")), "Sign"), "D.Sign")(p);
                memo[tuple(`Sign`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Sign(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.option!(pegged.peg.keywords!("-", "+")), "D.Sign")(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(hooked!(pegged.peg.option!(pegged.peg.keywords!("-", "+")), "Sign"), "D.Sign")(TParseTree("", false,[], s));
        }
    }
    static string Sign(GetName g)
    {
        return "D.Sign";
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
        if(__ctfe)
        {
            return D(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            memo = null;
            return D(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "D";
    }

    }
}

alias GenericD!(ParseTree).D D;

