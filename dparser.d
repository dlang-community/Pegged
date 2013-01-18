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

MacroParameterList < :"(" (MacroParameter ("," MacroParameter)*)? :")"

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
#List(Elem) < Elem (',' Elem)*

ImportBindings < Import ":" ImportBind ("," ImportBind)*

ImportBind < Identifier ("=" Identifier)?

MixinDeclaration < "mixin" "(" AssignExpression ")" ";"

# declaration.html
Declaration < AliasDeclaration
             / AliasThisDeclaration
             / Decl

AliasDeclaration < "alias" ( BasicType Declarator
                           / AliasInitializer ("," AliasInitializer)*)

AliasInitializer < Identifier "=" Type

AliasThisDeclaration < "alias" ( Identifier "this"
                               / "this" "=" Identifier)

Decl < BasicType Declarators ";"
      / BasicType Declarator FunctionBody
      / AutoDeclaration
      / StorageClasses Decl

Declarators < DeclaratorInitializer ("," DeclaratorIdentifier ("," DeclaratorIdentifier)*)?

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

StorageClass < UserDefinedAttribute /
              ( "abstract"
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
              / "synchronized")

UserDefinedAttribute < "@(" ArgumentList ")"
                     / "@" Expression  # The online grammar says CallExpression, but this does not exist

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

AutoDeclarationX < Identifier "=" Initializer ("," Identifier "=" Initializer)*

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
                    "(" ForeachType ("," ForeachType)* ";" Aggregate ")"
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

AssocArrayLiteral < "[" KeyValuePair ("," KeyValuePair)* "]"

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
BaseClassList < ":" Identifier ("," Identifier)*

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

NewAnonClassExpression < "new" AllocatorArguments? "class" ClassArguments? Identifier ("," Identifier)* ClassBody

ClassArguments < "(" ArgumentList? ")"

### enum.html

EnumDeclaration < "enum" EnumTag? (":" EnumBaseType)? EnumBody

EnumTag < Identifier

EnumBaseType < Type

EnumBody < ";" / "{" EnumMember ("," EnumMember)* "}"

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
                / ("db" / "ds" / "di" / "dl" / "df" / "dd" / "de") Operand ("," Operand)*
                / Identifier ":" AsmInstruction
                / OpCode
                / OpCode Operand ("," Operand)*

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

BaseInterfaceList < ":" Identifier ("," Identifier)*

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

TemplateParameterList < TemplateParameter ("," TemplateParameter)*

TemplateParameter < TemplateTypeParameter
                   / TemplateValueParameter
                   / TemplateAliasParameter
                   / TemplateTupleParameter
                   / TemplateThisParameter

TemplateInstance < TemplateIdentifier ( "!(" TemplateArgument ("," TemplateArgument)* ")"
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

TemplateMixin < "mixin" TemplateIdentifier (("!(" TemplateArgument ("," TemplateArgument)* ")")? MixinIdentifier?) ";"

MixinIdentifier < Identifier

### traits.html

TraitsExpression < "__traits" "(" TraitsKeyword "," TraitsArgument ("," TraitsArgument)* ")"

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

Spacing <- (space / Comment)*

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
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericD(TParseTree)
{
    struct D
    {
    enum name = "D";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }
    static bool isRule(string s)
    {
        return s.startsWith("D.");
    }
    mixin decimateTree;
    static TParseTree Module(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ModuleDeclaration), pegged.peg.option!(DeclDefs))), "D.Module")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ModuleDeclaration), pegged.peg.option!(DeclDefs))), "D.Module"), "Module")(p);
    }
    static TParseTree Module(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ModuleDeclaration), pegged.peg.option!(DeclDefs))), "D.Module")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(ModuleDeclaration), pegged.peg.option!(DeclDefs))), "D.Module"), "Module")(TParseTree("", false,[], s));
    }
    static string Module(GetName g)
    {
        return "D.Module";
    }

    static TParseTree DeclDefs(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)))), "D.DeclDefs")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)))), "D.DeclDefs"), "DeclDefs")(p);
    }
    static TParseTree DeclDefs(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)))), "D.DeclDefs")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)))), "D.DeclDefs"), "DeclDefs")(TParseTree("", false,[], s));
    }
    static string DeclDefs(GetName g)
    {
        return "D.DeclDefs";
    }

    static TParseTree DeclDef(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AttributeSpecifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EnumDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InterfaceDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AggregateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Constructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Destructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnitTest, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticConstructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticDestructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SharedStaticConstructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SharedStaticDestructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DebugSpecification, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VersionSpecification, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixinDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MacroDeclaration, Spacing))), "D.DeclDef")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AttributeSpecifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EnumDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InterfaceDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AggregateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Constructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Destructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnitTest, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticConstructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticDestructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SharedStaticConstructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SharedStaticDestructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DebugSpecification, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VersionSpecification, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixinDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MacroDeclaration, Spacing))), "D.DeclDef"), "DeclDef")(p);
    }
    static TParseTree DeclDef(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AttributeSpecifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EnumDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InterfaceDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AggregateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Constructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Destructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnitTest, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticConstructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticDestructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SharedStaticConstructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SharedStaticDestructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DebugSpecification, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VersionSpecification, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixinDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MacroDeclaration, Spacing))), "D.DeclDef")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AttributeSpecifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EnumDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InterfaceDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AggregateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Constructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Destructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnitTest, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticConstructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticDestructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SharedStaticConstructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SharedStaticDestructor, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DebugSpecification, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VersionSpecification, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixinDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MacroDeclaration, Spacing))), "D.DeclDef"), "DeclDef")(TParseTree("", false,[], s));
    }
    static string DeclDef(GetName g)
    {
        return "D.DeclDef";
    }

    static TParseTree MacroDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, MacroName, Spacing), pegged.peg.wrapAround!(Spacing, MacroParameterList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MacroLevel, Spacing)), pegged.peg.wrapAround!(Spacing, MacroBeforeBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, MacroAfterBody, Spacing))), "D.MacroDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, MacroName, Spacing), pegged.peg.wrapAround!(Spacing, MacroParameterList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MacroLevel, Spacing)), pegged.peg.wrapAround!(Spacing, MacroBeforeBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, MacroAfterBody, Spacing))), "D.MacroDeclaration"), "MacroDeclaration")(p);
    }
    static TParseTree MacroDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, MacroName, Spacing), pegged.peg.wrapAround!(Spacing, MacroParameterList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MacroLevel, Spacing)), pegged.peg.wrapAround!(Spacing, MacroBeforeBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, MacroAfterBody, Spacing))), "D.MacroDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing), pegged.peg.wrapAround!(Spacing, MacroName, Spacing), pegged.peg.wrapAround!(Spacing, MacroParameterList, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MacroLevel, Spacing)), pegged.peg.wrapAround!(Spacing, MacroBeforeBody, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, MacroAfterBody, Spacing))), "D.MacroDeclaration"), "MacroDeclaration")(TParseTree("", false,[], s));
    }
    static string MacroDeclaration(GetName g)
    {
        return "D.MacroDeclaration";
    }

    static TParseTree MacroName(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroName")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroName"), "MacroName")(p);
    }
    static TParseTree MacroName(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroName")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroName"), "MacroName")(TParseTree("", false,[], s));
    }
    static string MacroName(GetName g)
    {
        return "D.MacroName";
    }

    static TParseTree MacroParameterList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing))), Spacing)))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)))), "D.MacroParameterList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing))), Spacing)))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)))), "D.MacroParameterList"), "MacroParameterList")(p);
    }
    static TParseTree MacroParameterList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing))), Spacing)))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)))), "D.MacroParameterList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, MacroParameter, Spacing))), Spacing)))), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)))), "D.MacroParameterList"), "MacroParameterList")(TParseTree("", false,[], s));
    }
    static string MacroParameterList(GetName g)
    {
        return "D.MacroParameterList";
    }

    static TParseTree MacroParameter(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroParameter")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroParameter"), "MacroParameter")(p);
    }
    static TParseTree MacroParameter(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroParameter")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroParameter"), "MacroParameter")(TParseTree("", false,[], s));
    }
    static string MacroParameter(GetName g)
    {
        return "D.MacroParameter";
    }

    static TParseTree MacroLevel(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroLevel")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroLevel"), "MacroLevel")(p);
    }
    static TParseTree MacroLevel(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroLevel")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing))), "D.MacroLevel"), "MacroLevel")(TParseTree("", false,[], s));
    }
    static string MacroLevel(GetName g)
    {
        return "D.MacroLevel";
    }

    static TParseTree MacroBeforeBody(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(endOfLine, pegged.peg.literal!("}")))), pegged.peg.any)))), pegged.peg.discard!(endOfLine), pegged.peg.discard!(pegged.peg.literal!("}")))), "D.MacroBeforeBody")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(endOfLine, pegged.peg.literal!("}")))), pegged.peg.any)))), pegged.peg.discard!(endOfLine), pegged.peg.discard!(pegged.peg.literal!("}")))), "D.MacroBeforeBody"), "MacroBeforeBody")(p);
    }
    static TParseTree MacroBeforeBody(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(endOfLine, pegged.peg.literal!("}")))), pegged.peg.any)))), pegged.peg.discard!(endOfLine), pegged.peg.discard!(pegged.peg.literal!("}")))), "D.MacroBeforeBody")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("{")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(pegged.peg.and!(endOfLine, pegged.peg.literal!("}")))), pegged.peg.any)))), pegged.peg.discard!(endOfLine), pegged.peg.discard!(pegged.peg.literal!("}")))), "D.MacroBeforeBody"), "MacroBeforeBody")(TParseTree("", false,[], s));
    }
    static string MacroBeforeBody(GetName g)
    {
        return "D.MacroBeforeBody";
    }

    static TParseTree MacroAfterBody(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)))), "D.MacroAfterBody")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)))), "D.MacroAfterBody"), "MacroAfterBody")(p);
    }
    static TParseTree MacroAfterBody(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)))), "D.MacroAfterBody")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.wrapAround!(Spacing, Statement, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)))), "D.MacroAfterBody"), "MacroAfterBody")(TParseTree("", false,[], s));
    }
    static string MacroAfterBody(GetName g)
    {
        return "D.MacroAfterBody";
    }

    static TParseTree ModuleDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ModuleDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ModuleDeclaration"), "ModuleDeclaration")(p);
    }
    static TParseTree ModuleDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ModuleDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ModuleDeclaration"), "ModuleDeclaration")(TParseTree("", false,[], s));
    }
    static string ModuleDeclaration(GetName g)
    {
        return "D.ModuleDeclaration";
    }

    static TParseTree ImportDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ImportDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ImportDeclaration"), "ImportDeclaration")(p);
    }
    static TParseTree ImportDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ImportDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ImportDeclaration"), "ImportDeclaration")(TParseTree("", false,[], s));
    }
    static string ImportDeclaration(GetName g)
    {
        return "D.ImportDeclaration";
    }

    static TParseTree ImportList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportBindings, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing))), Spacing)))), "D.ImportList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportBindings, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing))), Spacing)))), "D.ImportList"), "ImportList")(p);
    }
    static TParseTree ImportList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportBindings, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing))), Spacing)))), "D.ImportList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportBindings, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing))), Spacing)))), "D.ImportList"), "ImportList")(TParseTree("", false,[], s));
    }
    static string ImportList(GetName g)
    {
        return "D.ImportList";
    }

    static TParseTree Import(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing))), "D.Import")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing))), "D.Import"), "Import")(p);
    }
    static TParseTree Import(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing))), "D.Import")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, qualifiedIdentifier, Spacing))), "D.Import"), "Import")(TParseTree("", false,[], s));
    }
    static string Import(GetName g)
    {
        return "D.Import";
    }

    static TParseTree ImportBindings(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ImportBind, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportBind, Spacing))), Spacing)))), "D.ImportBindings")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ImportBind, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportBind, Spacing))), Spacing)))), "D.ImportBindings"), "ImportBindings")(p);
    }
    static TParseTree ImportBindings(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ImportBind, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportBind, Spacing))), Spacing)))), "D.ImportBindings")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Import, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ImportBind, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ImportBind, Spacing))), Spacing)))), "D.ImportBindings"), "ImportBindings")(TParseTree("", false,[], s));
    }
    static string ImportBindings(GetName g)
    {
        return "D.ImportBindings";
    }

    static TParseTree ImportBind(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.ImportBind")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.ImportBind"), "ImportBind")(p);
    }
    static TParseTree ImportBind(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.ImportBind")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.ImportBind"), "ImportBind")(TParseTree("", false,[], s));
    }
    static string ImportBind(GetName g)
    {
        return "D.ImportBind";
    }

    static TParseTree MixinDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.MixinDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.MixinDeclaration"), "MixinDeclaration")(p);
    }
    static TParseTree MixinDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.MixinDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.MixinDeclaration"), "MixinDeclaration")(TParseTree("", false,[], s));
    }
    static string MixinDeclaration(GetName g)
    {
        return "D.MixinDeclaration";
    }

    static TParseTree Declaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasThisDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "D.Declaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasThisDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "D.Declaration"), "Declaration")(p);
    }
    static TParseTree Declaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasThisDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "D.Declaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasThisDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "D.Declaration"), "Declaration")(TParseTree("", false,[], s));
    }
    static string Declaration(GetName g)
    {
        return "D.Declaration";
    }

    static TParseTree AliasDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing))), Spacing)))), Spacing))), "D.AliasDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing))), Spacing)))), Spacing))), "D.AliasDeclaration"), "AliasDeclaration")(p);
    }
    static TParseTree AliasDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing))), Spacing)))), Spacing))), "D.AliasDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AliasInitializer, Spacing))), Spacing)))), Spacing))), "D.AliasDeclaration"), "AliasDeclaration")(TParseTree("", false,[], s));
    }
    static string AliasDeclaration(GetName g)
    {
        return "D.AliasDeclaration";
    }

    static TParseTree AliasInitializer(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.AliasInitializer")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.AliasInitializer"), "AliasInitializer")(p);
    }
    static TParseTree AliasInitializer(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.AliasInitializer")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.AliasInitializer"), "AliasInitializer")(TParseTree("", false,[], s));
    }
    static string AliasInitializer(GetName g)
    {
        return "D.AliasInitializer";
    }

    static TParseTree AliasThisDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing))), "D.AliasThisDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing))), "D.AliasThisDeclaration"), "AliasThisDeclaration")(p);
    }
    static TParseTree AliasThisDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing))), "D.AliasThisDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing))), "D.AliasThisDeclaration"), "AliasThisDeclaration")(TParseTree("", false,[], s));
    }
    static string AliasThisDeclaration(GetName g)
    {
        return "D.AliasThisDeclaration";
    }

    static TParseTree Decl(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarators, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AutoDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "D.Decl")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarators, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AutoDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "D.Decl"), "Decl")(p);
    }
    static TParseTree Decl(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarators, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AutoDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "D.Decl")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarators, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AutoDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, Decl, Spacing))), "D.Decl"), "Decl")(TParseTree("", false,[], s));
    }
    static string Decl(GetName g)
    {
        return "D.Decl";
    }

    static TParseTree Declarators(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclaratorInitializer, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing))), Spacing)))), Spacing)))), "D.Declarators")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclaratorInitializer, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing))), Spacing)))), Spacing)))), "D.Declarators"), "Declarators")(p);
    }
    static TParseTree Declarators(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclaratorInitializer, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing))), Spacing)))), Spacing)))), "D.Declarators")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclaratorInitializer, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, DeclaratorIdentifier, Spacing))), Spacing)))), Spacing)))), "D.Declarators"), "Declarators")(TParseTree("", false,[], s));
    }
    static string Declarators(GetName g)
    {
        return "D.Declarators";
    }

    static TParseTree DeclaratorInitializer(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.DeclaratorInitializer")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.DeclaratorInitializer"), "DeclaratorInitializer")(p);
    }
    static TParseTree DeclaratorInitializer(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.DeclaratorInitializer")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.DeclaratorInitializer"), "DeclaratorInitializer")(TParseTree("", false,[], s));
    }
    static string DeclaratorInitializer(GetName g)
    {
        return "D.DeclaratorInitializer";
    }

    static TParseTree DeclaratorIdentifier(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.DeclaratorIdentifier")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.DeclaratorIdentifier"), "DeclaratorIdentifier")(p);
    }
    static TParseTree DeclaratorIdentifier(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.DeclaratorIdentifier")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.DeclaratorIdentifier"), "DeclaratorIdentifier")(TParseTree("", false,[], s));
    }
    static string DeclaratorIdentifier(GetName g)
    {
        return "D.DeclaratorIdentifier";
    }

    static TParseTree BasicType(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.BasicType")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.BasicType"), "BasicType")(p);
    }
    static TParseTree BasicType(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.BasicType")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.BasicType"), "BasicType")(TParseTree("", false,[], s));
    }
    static string BasicType(GetName g)
    {
        return "D.BasicType";
    }

    static TParseTree BasicTypeX(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing))), "D.BasicTypeX")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing))), "D.BasicTypeX"), "BasicTypeX")(p);
    }
    static TParseTree BasicTypeX(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing))), "D.BasicTypeX")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing))), "D.BasicTypeX"), "BasicTypeX")(TParseTree("", false,[], s));
    }
    static string BasicTypeX(GetName g)
    {
        return "D.BasicTypeX";
    }

    static TParseTree BasicType2(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "D.BasicType2")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "D.BasicType2"), "BasicType2")(p);
    }
    static TParseTree BasicType2(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "D.BasicType2")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "D.BasicType2"), "BasicType2")(TParseTree("", false,[], s));
    }
    static string BasicType2(GetName g)
    {
        return "D.BasicType2";
    }

    static TParseTree Declarator(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "D.Declarator")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "D.Declarator"), "Declarator")(p);
    }
    static TParseTree Declarator(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "D.Declarator")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing))), pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "D.Declarator"), "Declarator")(TParseTree("", false,[], s));
    }
    static string Declarator(GetName g)
    {
        return "D.Declarator";
    }

    static TParseTree DeclaratorSuffixes(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffix, Spacing)))), "D.DeclaratorSuffixes")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffix, Spacing)))), "D.DeclaratorSuffixes"), "DeclaratorSuffixes")(p);
    }
    static TParseTree DeclaratorSuffixes(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffix, Spacing)))), "D.DeclaratorSuffixes")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffix, Spacing)))), "D.DeclaratorSuffixes"), "DeclaratorSuffixes")(TParseTree("", false,[], s));
    }
    static string DeclaratorSuffixes(GetName g)
    {
        return "D.DeclaratorSuffixes";
    }

    static TParseTree DeclaratorSuffix(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttributes, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "D.DeclaratorSuffix")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttributes, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "D.DeclaratorSuffix"), "DeclaratorSuffix")(p);
    }
    static TParseTree DeclaratorSuffix(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttributes, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "D.DeclaratorSuffix")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttributes, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "D.DeclaratorSuffix"), "DeclaratorSuffix")(TParseTree("", false,[], s));
    }
    static string DeclaratorSuffix(GetName g)
    {
        return "D.DeclaratorSuffix";
    }

    static TParseTree IdentifierList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing))), Spacing)))), "D.IdentifierList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing))), Spacing)))), "D.IdentifierList"), "IdentifierList")(p);
    }
    static TParseTree IdentifierList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing))), Spacing)))), "D.IdentifierList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, IdentifierList, Spacing))), Spacing)))), "D.IdentifierList"), "IdentifierList")(TParseTree("", false,[], s));
    }
    static string IdentifierList(GetName g)
    {
        return "D.IdentifierList";
    }

    static TParseTree StorageClasses(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StorageClass, Spacing)))), "D.StorageClasses")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StorageClass, Spacing)))), "D.StorageClasses"), "StorageClasses")(p);
    }
    static TParseTree StorageClasses(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StorageClass, Spacing)))), "D.StorageClasses")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StorageClass, Spacing)))), "D.StorageClasses"), "StorageClasses")(TParseTree("", false,[], s));
    }
    static string StorageClasses(GetName g)
    {
        return "D.StorageClasses";
    }

    static TParseTree StorageClass(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UserDefinedAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Property, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing))), Spacing))), "D.StorageClass")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UserDefinedAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Property, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing))), Spacing))), "D.StorageClass"), "StorageClass")(p);
    }
    static TParseTree StorageClass(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UserDefinedAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Property, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing))), Spacing))), "D.StorageClass")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UserDefinedAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Property, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing))), Spacing))), "D.StorageClass"), "StorageClass")(TParseTree("", false,[], s));
    }
    static string StorageClass(GetName g)
    {
        return "D.StorageClass";
    }

    static TParseTree UserDefinedAttribute(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.UserDefinedAttribute")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.UserDefinedAttribute"), "UserDefinedAttribute")(p);
    }
    static TParseTree UserDefinedAttribute(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.UserDefinedAttribute")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.UserDefinedAttribute"), "UserDefinedAttribute")(TParseTree("", false,[], s));
    }
    static string UserDefinedAttribute(GetName g)
    {
        return "D.UserDefinedAttribute";
    }

    static TParseTree Property(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("property"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("safe"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("trusted"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("system"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("disable"), Spacing))), Spacing))), "D.Property")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("property"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("safe"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("trusted"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("system"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("disable"), Spacing))), Spacing))), "D.Property"), "Property")(p);
    }
    static TParseTree Property(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("property"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("safe"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("trusted"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("system"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("disable"), Spacing))), Spacing))), "D.Property")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("property"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("safe"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("trusted"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("system"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("disable"), Spacing))), Spacing))), "D.Property"), "Property")(TParseTree("", false,[], s));
    }
    static string Property(GetName g)
    {
        return "D.Property";
    }

    static TParseTree Type(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declarator2, Spacing)))), "D.Type")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declarator2, Spacing)))), "D.Type"), "Type")(p);
    }
    static TParseTree Type(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declarator2, Spacing)))), "D.Type")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declarator2, Spacing)))), "D.Type"), "Type")(TParseTree("", false,[], s));
    }
    static string Type(GetName g)
    {
        return "D.Type";
    }

    static TParseTree Declarator2(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "D.Declarator2")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "D.Declarator2"), "Declarator2")(p);
    }
    static TParseTree Declarator2(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "D.Declarator2")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, BasicType2, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Declarator2, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclaratorSuffixes, Spacing)))), "D.Declarator2"), "Declarator2")(TParseTree("", false,[], s));
    }
    static string Declarator2(GetName g)
    {
        return "D.Declarator2";
    }

    static TParseTree Parameters(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Parameters")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Parameters"), "Parameters")(p);
    }
    static TParseTree Parameters(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Parameters")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Parameters"), "Parameters")(TParseTree("", false,[], s));
    }
    static string Parameters(GetName g)
    {
        return "D.Parameters";
    }

    static TParseTree ParameterList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing))), Spacing)))), "D.ParameterList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing))), Spacing)))), "D.ParameterList"), "ParameterList")(p);
    }
    static TParseTree ParameterList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing))), Spacing)))), "D.ParameterList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), pegged.peg.wrapAround!(Spacing, Parameter, Spacing))), Spacing)))), "D.ParameterList"), "ParameterList")(TParseTree("", false,[], s));
    }
    static string ParameterList(GetName g)
    {
        return "D.ParameterList";
    }

    static TParseTree Parameter(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultInitializerExpression, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)))), "D.Parameter")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultInitializerExpression, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)))), "D.Parameter"), "Parameter")(p);
    }
    static TParseTree Parameter(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultInitializerExpression, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)))), "D.Parameter")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, DefaultInitializerExpression, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)))), "D.Parameter"), "Parameter")(TParseTree("", false,[], s));
    }
    static string Parameter(GetName g)
    {
        return "D.Parameter";
    }

    static TParseTree InOut(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InOutX, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)))), "D.InOut")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InOutX, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)))), "D.InOut"), "InOut")(p);
    }
    static TParseTree InOut(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InOutX, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)))), "D.InOut")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InOutX, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, InOut, Spacing)))), "D.InOut"), "InOut")(TParseTree("", false,[], s));
    }
    static string InOut(GetName g)
    {
        return "D.InOut";
    }

    static TParseTree InOutX(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in "), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing))), "D.InOutX")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in "), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing))), "D.InOutX"), "InOutX")(p);
    }
    static TParseTree InOutX(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in "), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing))), "D.InOutX")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in "), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing))), "D.InOutX"), "InOutX")(TParseTree("", false,[], s));
    }
    static string InOutX(GetName g)
    {
        return "D.InOutX";
    }

    static TParseTree FunctionAttributes(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)))), "D.FunctionAttributes")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)))), "D.FunctionAttributes"), "FunctionAttributes")(p);
    }
    static TParseTree FunctionAttributes(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)))), "D.FunctionAttributes")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing)))), "D.FunctionAttributes"), "FunctionAttributes")(TParseTree("", false,[], s));
    }
    static string FunctionAttributes(GetName g)
    {
        return "D.FunctionAttributes";
    }

    static TParseTree FunctionAttribute(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Property, Spacing))), "D.FunctionAttribute")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Property, Spacing))), "D.FunctionAttribute"), "FunctionAttribute")(p);
    }
    static TParseTree FunctionAttribute(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Property, Spacing))), "D.FunctionAttribute")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Property, Spacing))), "D.FunctionAttribute"), "FunctionAttribute")(TParseTree("", false,[], s));
    }
    static string FunctionAttribute(GetName g)
    {
        return "D.FunctionAttribute";
    }

    static TParseTree MemberFunctionAttributes(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttribute, Spacing)))), "D.MemberFunctionAttributes")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttribute, Spacing)))), "D.MemberFunctionAttributes"), "MemberFunctionAttributes")(p);
    }
    static TParseTree MemberFunctionAttributes(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttribute, Spacing)))), "D.MemberFunctionAttributes")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, MemberFunctionAttribute, Spacing)))), "D.MemberFunctionAttributes"), "MemberFunctionAttributes")(TParseTree("", false,[], s));
    }
    static string MemberFunctionAttributes(GetName g)
    {
        return "D.MemberFunctionAttributes";
    }

    static TParseTree MemberFunctionAttribute(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing))), "D.MemberFunctionAttribute")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing))), "D.MemberFunctionAttribute"), "MemberFunctionAttribute")(p);
    }
    static TParseTree MemberFunctionAttribute(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing))), "D.MemberFunctionAttribute")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FunctionAttribute, Spacing))), "D.MemberFunctionAttribute"), "MemberFunctionAttribute")(TParseTree("", false,[], s));
    }
    static string MemberFunctionAttribute(GetName g)
    {
        return "D.MemberFunctionAttribute";
    }

    static TParseTree DefaultInitializerExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing))), "D.DefaultInitializerExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing))), "D.DefaultInitializerExpression"), "DefaultInitializerExpression")(p);
    }
    static TParseTree DefaultInitializerExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing))), "D.DefaultInitializerExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing))), "D.DefaultInitializerExpression"), "DefaultInitializerExpression")(TParseTree("", false,[], s));
    }
    static string DefaultInitializerExpression(GetName g)
    {
        return "D.DefaultInitializerExpression";
    }

    static TParseTree Initializer(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "D.Initializer")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "D.Initializer"), "Initializer")(p);
    }
    static TParseTree Initializer(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "D.Initializer")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "D.Initializer"), "Initializer")(TParseTree("", false,[], s));
    }
    static string Initializer(GetName g)
    {
        return "D.Initializer";
    }

    static TParseTree NonVoidInitializer(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructInitializer, Spacing))), "D.NonVoidInitializer")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructInitializer, Spacing))), "D.NonVoidInitializer"), "NonVoidInitializer")(p);
    }
    static TParseTree NonVoidInitializer(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructInitializer, Spacing))), "D.NonVoidInitializer")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructInitializer, Spacing))), "D.NonVoidInitializer"), "NonVoidInitializer")(TParseTree("", false,[], s));
    }
    static string NonVoidInitializer(GetName g)
    {
        return "D.NonVoidInitializer";
    }

    static TParseTree ArrayInitializer(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArrayMemberInitializations, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.ArrayInitializer")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArrayMemberInitializations, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.ArrayInitializer"), "ArrayInitializer")(p);
    }
    static TParseTree ArrayInitializer(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArrayMemberInitializations, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.ArrayInitializer")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArrayMemberInitializations, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.ArrayInitializer"), "ArrayInitializer")(TParseTree("", false,[], s));
    }
    static string ArrayInitializer(GetName g)
    {
        return "D.ArrayInitializer";
    }

    static TParseTree ArrayMemberInitializations(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing)))), Spacing)))), "D.ArrayMemberInitializations")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing)))), Spacing)))), "D.ArrayMemberInitializations"), "ArrayMemberInitializations")(p);
    }
    static TParseTree ArrayMemberInitializations(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing)))), Spacing)))), "D.ArrayMemberInitializations")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArrayMemberInitialization, Spacing)))), Spacing)))), "D.ArrayMemberInitializations"), "ArrayMemberInitializations")(TParseTree("", false,[], s));
    }
    static string ArrayMemberInitializations(GetName g)
    {
        return "D.ArrayMemberInitializations";
    }

    static TParseTree ArrayMemberInitialization(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "D.ArrayMemberInitialization")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "D.ArrayMemberInitialization"), "ArrayMemberInitialization")(p);
    }
    static TParseTree ArrayMemberInitialization(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "D.ArrayMemberInitialization")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing))), "D.ArrayMemberInitialization"), "ArrayMemberInitialization")(TParseTree("", false,[], s));
    }
    static string ArrayMemberInitialization(GetName g)
    {
        return "D.ArrayMemberInitialization";
    }

    static TParseTree StructInitializer(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructMemberInitializers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.StructInitializer")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructMemberInitializers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.StructInitializer"), "StructInitializer")(p);
    }
    static TParseTree StructInitializer(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructMemberInitializers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.StructInitializer")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, StructMemberInitializers, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.StructInitializer"), "StructInitializer")(TParseTree("", false,[], s));
    }
    static string StructInitializer(GetName g)
    {
        return "D.StructInitializer";
    }

    static TParseTree StructMemberInitializers(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing)))), Spacing)))), "D.StructMemberInitializers")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing)))), Spacing)))), "D.StructMemberInitializers"), "StructMemberInitializers")(p);
    }
    static TParseTree StructMemberInitializers(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing)))), Spacing)))), "D.StructMemberInitializers")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructMemberInitializer, Spacing)))), Spacing)))), "D.StructMemberInitializers"), "StructMemberInitializers")(TParseTree("", false,[], s));
    }
    static string StructMemberInitializers(GetName g)
    {
        return "D.StructMemberInitializers";
    }

    static TParseTree StructMemberInitializer(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)))), "D.StructMemberInitializer")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)))), "D.StructMemberInitializer"), "StructMemberInitializer")(p);
    }
    static TParseTree StructMemberInitializer(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)))), "D.StructMemberInitializer")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, NonVoidInitializer, Spacing)))), "D.StructMemberInitializer"), "StructMemberInitializer")(TParseTree("", false,[], s));
    }
    static string StructMemberInitializer(GetName g)
    {
        return "D.StructMemberInitializer";
    }

    static TParseTree AutoDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, AutoDeclarationX, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.AutoDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, AutoDeclarationX, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.AutoDeclaration"), "AutoDeclaration")(p);
    }
    static TParseTree AutoDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, AutoDeclarationX, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.AutoDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StorageClasses, Spacing), pegged.peg.wrapAround!(Spacing, AutoDeclarationX, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.AutoDeclaration"), "AutoDeclaration")(TParseTree("", false,[], s));
    }
    static string AutoDeclaration(GetName g)
    {
        return "D.AutoDeclaration";
    }

    static TParseTree AutoDeclarationX(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.AutoDeclarationX")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.AutoDeclarationX"), "AutoDeclarationX")(p);
    }
    static TParseTree AutoDeclarationX(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.AutoDeclarationX")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Initializer, Spacing))), Spacing)))), "D.AutoDeclarationX"), "AutoDeclarationX")(TParseTree("", false,[], s));
    }
    static string AutoDeclarationX(GetName g)
    {
        return "D.AutoDeclarationX";
    }

    static TParseTree Typeof(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Typeof")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Typeof"), "Typeof")(p);
    }
    static TParseTree Typeof(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Typeof")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Typeof"), "Typeof")(TParseTree("", false,[], s));
    }
    static string Typeof(GetName g)
    {
        return "D.Typeof";
    }

    static TParseTree VoidInitializer(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing))), "D.VoidInitializer")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing))), "D.VoidInitializer"), "VoidInitializer")(p);
    }
    static TParseTree VoidInitializer(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing))), "D.VoidInitializer")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing))), "D.VoidInitializer"), "VoidInitializer")(TParseTree("", false,[], s));
    }
    static string VoidInitializer(GetName g)
    {
        return "D.VoidInitializer";
    }

    static TParseTree Statement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.Statement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.Statement"), "Statement")(p);
    }
    static TParseTree Statement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.Statement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.Statement"), "Statement")(TParseTree("", false,[], s));
    }
    static string Statement(GetName g)
    {
        return "D.Statement";
    }

    static TParseTree NoScopeNonEmptyStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.NoScopeNonEmptyStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.NoScopeNonEmptyStatement"), "NoScopeNonEmptyStatement")(p);
    }
    static TParseTree NoScopeNonEmptyStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.NoScopeNonEmptyStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.NoScopeNonEmptyStatement"), "NoScopeNonEmptyStatement")(TParseTree("", false,[], s));
    }
    static string NoScopeNonEmptyStatement(GetName g)
    {
        return "D.NoScopeNonEmptyStatement";
    }

    static TParseTree NoScopeStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.NoScopeStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.NoScopeStatement"), "NoScopeStatement")(p);
    }
    static TParseTree NoScopeStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.NoScopeStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.NoScopeStatement"), "NoScopeStatement")(TParseTree("", false,[], s));
    }
    static string NoScopeStatement(GetName g)
    {
        return "D.NoScopeStatement";
    }

    static TParseTree NonEmptyOrScopeBlockStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.NonEmptyOrScopeBlockStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.NonEmptyOrScopeBlockStatement"), "NonEmptyOrScopeBlockStatement")(p);
    }
    static TParseTree NonEmptyOrScopeBlockStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.NonEmptyOrScopeBlockStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.NonEmptyOrScopeBlockStatement"), "NonEmptyOrScopeBlockStatement")(TParseTree("", false,[], s));
    }
    static string NonEmptyOrScopeBlockStatement(GetName g)
    {
        return "D.NonEmptyOrScopeBlockStatement";
    }

    static TParseTree NonEmptyStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CaseStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CaseRangeStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DefaultStatement, Spacing))), "D.NonEmptyStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CaseStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CaseRangeStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DefaultStatement, Spacing))), "D.NonEmptyStatement"), "NonEmptyStatement")(p);
    }
    static TParseTree NonEmptyStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CaseStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CaseRangeStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DefaultStatement, Spacing))), "D.NonEmptyStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CaseStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CaseRangeStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DefaultStatement, Spacing))), "D.NonEmptyStatement"), "NonEmptyStatement")(TParseTree("", false,[], s));
    }
    static string NonEmptyStatement(GetName g)
    {
        return "D.NonEmptyStatement";
    }

    static TParseTree NonEmptyStatementNoCaseNoDefault(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LabeledStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclarationStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IfStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DoStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SwitchStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FinalSwitchStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, GotoStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WithStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SynchronizedStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TryStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeGuardStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ThrowStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PragmaStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForeachRangeStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing))), "D.NonEmptyStatementNoCaseNoDefault")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LabeledStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclarationStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IfStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DoStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SwitchStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FinalSwitchStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, GotoStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WithStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SynchronizedStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TryStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeGuardStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ThrowStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PragmaStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForeachRangeStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing))), "D.NonEmptyStatementNoCaseNoDefault"), "NonEmptyStatementNoCaseNoDefault")(p);
    }
    static TParseTree NonEmptyStatementNoCaseNoDefault(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LabeledStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclarationStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IfStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DoStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SwitchStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FinalSwitchStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, GotoStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WithStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SynchronizedStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TryStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeGuardStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ThrowStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PragmaStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForeachRangeStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing))), "D.NonEmptyStatementNoCaseNoDefault")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LabeledStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ExpressionStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclarationStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IfStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WhileStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DoStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForeachStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SwitchStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FinalSwitchStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ContinueStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BreakStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ReturnStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, GotoStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WithStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SynchronizedStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TryStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeGuardStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ThrowStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PragmaStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ForeachRangeStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticAssert, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateMixin, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportDeclaration, Spacing))), "D.NonEmptyStatementNoCaseNoDefault"), "NonEmptyStatementNoCaseNoDefault")(TParseTree("", false,[], s));
    }
    static string NonEmptyStatementNoCaseNoDefault(GetName g)
    {
        return "D.NonEmptyStatementNoCaseNoDefault";
    }

    static TParseTree ScopeStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.ScopeStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.ScopeStatement"), "ScopeStatement")(p);
    }
    static TParseTree ScopeStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.ScopeStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.ScopeStatement"), "ScopeStatement")(TParseTree("", false,[], s));
    }
    static string ScopeStatement(GetName g)
    {
        return "D.ScopeStatement";
    }

    static TParseTree ScopeBlockStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ScopeBlockStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ScopeBlockStatement"), "ScopeBlockStatement")(p);
    }
    static TParseTree ScopeBlockStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ScopeBlockStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ScopeBlockStatement"), "ScopeBlockStatement")(TParseTree("", false,[], s));
    }
    static string ScopeBlockStatement(GetName g)
    {
        return "D.ScopeBlockStatement";
    }

    static TParseTree LabeledStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing))), "D.LabeledStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing))), "D.LabeledStatement"), "LabeledStatement")(p);
    }
    static TParseTree LabeledStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing))), "D.LabeledStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing))), "D.LabeledStatement"), "LabeledStatement")(TParseTree("", false,[], s));
    }
    static string LabeledStatement(GetName g)
    {
        return "D.LabeledStatement";
    }

    static TParseTree BlockStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.BlockStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.BlockStatement"), "BlockStatement")(p);
    }
    static TParseTree BlockStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.BlockStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StatementList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.BlockStatement"), "BlockStatement")(TParseTree("", false,[], s));
    }
    static string BlockStatement(GetName g)
    {
        return "D.BlockStatement";
    }

    static TParseTree StatementList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)))), "D.StatementList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)))), "D.StatementList"), "StatementList")(p);
    }
    static TParseTree StatementList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)))), "D.StatementList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Statement, Spacing)))), "D.StatementList"), "StatementList")(TParseTree("", false,[], s));
    }
    static string StatementList(GetName g)
    {
        return "D.StatementList";
    }

    static TParseTree ExpressionStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ExpressionStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ExpressionStatement"), "ExpressionStatement")(p);
    }
    static TParseTree ExpressionStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ExpressionStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ExpressionStatement"), "ExpressionStatement")(TParseTree("", false,[], s));
    }
    static string ExpressionStatement(GetName g)
    {
        return "D.ExpressionStatement";
    }

    static TParseTree DeclarationStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing))), "D.DeclarationStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing))), "D.DeclarationStatement"), "DeclarationStatement")(p);
    }
    static TParseTree DeclarationStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing))), "D.DeclarationStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing))), "D.DeclarationStatement"), "DeclarationStatement")(TParseTree("", false,[], s));
    }
    static string DeclarationStatement(GetName g)
    {
        return "D.DeclarationStatement";
    }

    static TParseTree IfStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IfCondition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ThenStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, ElseStatement, Spacing))), Spacing)))), "D.IfStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IfCondition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ThenStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, ElseStatement, Spacing))), Spacing)))), "D.IfStatement"), "IfStatement")(p);
    }
    static TParseTree IfStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IfCondition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ThenStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, ElseStatement, Spacing))), Spacing)))), "D.IfStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IfCondition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ThenStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, ElseStatement, Spacing))), Spacing)))), "D.IfStatement"), "IfStatement")(TParseTree("", false,[], s));
    }
    static string IfStatement(GetName g)
    {
        return "D.IfStatement";
    }

    static TParseTree IfCondition(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.IfCondition")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.IfCondition"), "IfCondition")(p);
    }
    static TParseTree IfCondition(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.IfCondition")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.IfCondition"), "IfCondition")(TParseTree("", false,[], s));
    }
    static string IfCondition(GetName g)
    {
        return "D.IfCondition";
    }

    static TParseTree ThenStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ThenStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ThenStatement"), "ThenStatement")(p);
    }
    static TParseTree ThenStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ThenStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ThenStatement"), "ThenStatement")(TParseTree("", false,[], s));
    }
    static string ThenStatement(GetName g)
    {
        return "D.ThenStatement";
    }

    static TParseTree ElseStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ElseStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ElseStatement"), "ElseStatement")(p);
    }
    static TParseTree ElseStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ElseStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ElseStatement"), "ElseStatement")(TParseTree("", false,[], s));
    }
    static string ElseStatement(GetName g)
    {
        return "D.ElseStatement";
    }

    static TParseTree WhileStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.WhileStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.WhileStatement"), "WhileStatement")(p);
    }
    static TParseTree WhileStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.WhileStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.WhileStatement"), "WhileStatement")(TParseTree("", false,[], s));
    }
    static string WhileStatement(GetName g)
    {
        return "D.WhileStatement";
    }

    static TParseTree DoStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.DoStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.DoStatement"), "DoStatement")(p);
    }
    static TParseTree DoStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.DoStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.DoStatement"), "DoStatement")(TParseTree("", false,[], s));
    }
    static string DoStatement(GetName g)
    {
        return "D.DoStatement";
    }

    static TParseTree ForStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Test, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ForStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Test, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ForStatement"), "ForStatement")(p);
    }
    static TParseTree ForStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Test, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ForStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Initialize, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Test, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Increment, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.ForStatement"), "ForStatement")(TParseTree("", false,[], s));
    }
    static string ForStatement(GetName g)
    {
        return "D.ForStatement";
    }

    static TParseTree Initialize(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.Initialize")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.Initialize"), "Initialize")(p);
    }
    static TParseTree Initialize(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.Initialize")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.Initialize"), "Initialize")(TParseTree("", false,[], s));
    }
    static string Initialize(GetName g)
    {
        return "D.Initialize";
    }

    static TParseTree Test(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Test")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Test"), "Test")(p);
    }
    static TParseTree Test(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Test")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Test"), "Test")(TParseTree("", false,[], s));
    }
    static string Test(GetName g)
    {
        return "D.Test";
    }

    static TParseTree Increment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Increment")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Increment"), "Increment")(p);
    }
    static TParseTree Increment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Increment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Increment"), "Increment")(TParseTree("", false,[], s));
    }
    static string Increment(GetName g)
    {
        return "D.Increment";
    }

    static TParseTree ForeachStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Aggregate, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.ForeachStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Aggregate, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.ForeachStatement"), "ForeachStatement")(p);
    }
    static TParseTree ForeachStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Aggregate, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.ForeachStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Aggregate, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.ForeachStatement"), "ForeachStatement")(TParseTree("", false,[], s));
    }
    static string ForeachStatement(GetName g)
    {
        return "D.ForeachStatement";
    }

    static TParseTree ForeachType(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.ForeachType")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.ForeachType"), "ForeachType")(p);
    }
    static TParseTree ForeachType(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.ForeachType")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.ForeachType"), "ForeachType")(TParseTree("", false,[], s));
    }
    static string ForeachType(GetName g)
    {
        return "D.ForeachType";
    }

    static TParseTree Aggregate(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Aggregate")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Aggregate"), "Aggregate")(p);
    }
    static TParseTree Aggregate(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Aggregate")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), "D.Aggregate"), "Aggregate")(TParseTree("", false,[], s));
    }
    static string Aggregate(GetName g)
    {
        return "D.Aggregate";
    }

    static TParseTree ForeachRangeStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ForeachRangeStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ForeachRangeStatement"), "ForeachRangeStatement")(p);
    }
    static TParseTree ForeachRangeStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ForeachRangeStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ForeachType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ForeachRangeStatement"), "ForeachRangeStatement")(TParseTree("", false,[], s));
    }
    static string ForeachRangeStatement(GetName g)
    {
        return "D.ForeachRangeStatement";
    }

    static TParseTree SwitchStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.SwitchStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.SwitchStatement"), "SwitchStatement")(p);
    }
    static TParseTree SwitchStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.SwitchStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.SwitchStatement"), "SwitchStatement")(TParseTree("", false,[], s));
    }
    static string SwitchStatement(GetName g)
    {
        return "D.SwitchStatement";
    }

    static TParseTree CaseStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.CaseStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.CaseStatement"), "CaseStatement")(p);
    }
    static TParseTree CaseStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.CaseStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.CaseStatement"), "CaseStatement")(TParseTree("", false,[], s));
    }
    static string CaseStatement(GetName g)
    {
        return "D.CaseStatement";
    }

    static TParseTree CaseRangeStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.CaseRangeStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.CaseRangeStatement"), "CaseRangeStatement")(p);
    }
    static TParseTree CaseRangeStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.CaseRangeStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.CaseRangeStatement"), "CaseRangeStatement")(TParseTree("", false,[], s));
    }
    static string CaseRangeStatement(GetName g)
    {
        return "D.CaseRangeStatement";
    }

    static TParseTree DefaultStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.DefaultStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.DefaultStatement"), "DefaultStatement")(p);
    }
    static TParseTree DefaultStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.DefaultStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatementList, Spacing))), "D.DefaultStatement"), "DefaultStatement")(TParseTree("", false,[], s));
    }
    static string DefaultStatement(GetName g)
    {
        return "D.DefaultStatement";
    }

    static TParseTree ScopeStatementList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StatementListNoCaseNoDefault, Spacing))), "D.ScopeStatementList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StatementListNoCaseNoDefault, Spacing))), "D.ScopeStatementList"), "ScopeStatementList")(p);
    }
    static TParseTree ScopeStatementList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StatementListNoCaseNoDefault, Spacing))), "D.ScopeStatementList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StatementListNoCaseNoDefault, Spacing))), "D.ScopeStatementList"), "ScopeStatementList")(TParseTree("", false,[], s));
    }
    static string ScopeStatementList(GetName g)
    {
        return "D.ScopeStatementList";
    }

    static TParseTree StatementListNoCaseNoDefault(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StatementNoCaseNoDefault, Spacing)))), "D.StatementListNoCaseNoDefault")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StatementNoCaseNoDefault, Spacing)))), "D.StatementListNoCaseNoDefault"), "StatementListNoCaseNoDefault")(p);
    }
    static TParseTree StatementListNoCaseNoDefault(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StatementNoCaseNoDefault, Spacing)))), "D.StatementListNoCaseNoDefault")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StatementNoCaseNoDefault, Spacing)))), "D.StatementListNoCaseNoDefault"), "StatementListNoCaseNoDefault")(TParseTree("", false,[], s));
    }
    static string StatementListNoCaseNoDefault(GetName g)
    {
        return "D.StatementListNoCaseNoDefault";
    }

    static TParseTree StatementNoCaseNoDefault(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.StatementNoCaseNoDefault")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.StatementNoCaseNoDefault"), "StatementNoCaseNoDefault")(p);
    }
    static TParseTree StatementNoCaseNoDefault(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.StatementNoCaseNoDefault")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NonEmptyStatementNoCaseNoDefault, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ScopeBlockStatement, Spacing))), "D.StatementNoCaseNoDefault"), "StatementNoCaseNoDefault")(TParseTree("", false,[], s));
    }
    static string StatementNoCaseNoDefault(GetName g)
    {
        return "D.StatementNoCaseNoDefault";
    }

    static TParseTree FinalSwitchStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.FinalSwitchStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.FinalSwitchStatement"), "FinalSwitchStatement")(p);
    }
    static TParseTree FinalSwitchStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.FinalSwitchStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.FinalSwitchStatement"), "FinalSwitchStatement")(TParseTree("", false,[], s));
    }
    static string FinalSwitchStatement(GetName g)
    {
        return "D.FinalSwitchStatement";
    }

    static TParseTree ContinueStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ContinueStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ContinueStatement"), "ContinueStatement")(p);
    }
    static TParseTree ContinueStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ContinueStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ContinueStatement"), "ContinueStatement")(TParseTree("", false,[], s));
    }
    static string ContinueStatement(GetName g)
    {
        return "D.ContinueStatement";
    }

    static TParseTree BreakStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.BreakStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.BreakStatement"), "BreakStatement")(p);
    }
    static TParseTree BreakStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.BreakStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.BreakStatement"), "BreakStatement")(TParseTree("", false,[], s));
    }
    static string BreakStatement(GetName g)
    {
        return "D.BreakStatement";
    }

    static TParseTree ReturnStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ReturnStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ReturnStatement"), "ReturnStatement")(p);
    }
    static TParseTree ReturnStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ReturnStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ReturnStatement"), "ReturnStatement")(TParseTree("", false,[], s));
    }
    static string ReturnStatement(GetName g)
    {
        return "D.ReturnStatement";
    }

    static TParseTree GotoStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing))), "D.GotoStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing))), "D.GotoStatement"), "GotoStatement")(p);
    }
    static TParseTree GotoStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing))), "D.GotoStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing))), "D.GotoStatement"), "GotoStatement")(TParseTree("", false,[], s));
    }
    static string GotoStatement(GetName g)
    {
        return "D.GotoStatement";
    }

    static TParseTree WithStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Symbol, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.WithStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Symbol, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.WithStatement"), "WithStatement")(p);
    }
    static TParseTree WithStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Symbol, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.WithStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Symbol, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.WithStatement"), "WithStatement")(TParseTree("", false,[], s));
    }
    static string WithStatement(GetName g)
    {
        return "D.WithStatement";
    }

    static TParseTree SynchronizedStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.SynchronizedStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.SynchronizedStatement"), "SynchronizedStatement")(p);
    }
    static TParseTree SynchronizedStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.SynchronizedStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing))), "D.SynchronizedStatement"), "SynchronizedStatement")(TParseTree("", false,[], s));
    }
    static string SynchronizedStatement(GetName g)
    {
        return "D.SynchronizedStatement";
    }

    static TParseTree TryStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FinallyStatement, Spacing)))), "D.TryStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FinallyStatement, Spacing)))), "D.TryStatement"), "TryStatement")(p);
    }
    static TParseTree TryStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FinallyStatement, Spacing)))), "D.TryStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing), pegged.peg.wrapAround!(Spacing, ScopeStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FinallyStatement, Spacing)))), "D.TryStatement"), "TryStatement")(TParseTree("", false,[], s));
    }
    static string TryStatement(GetName g)
    {
        return "D.TryStatement";
    }

    static TParseTree Catches(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LastCatch, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Catch, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)))), "D.Catches")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LastCatch, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Catch, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)))), "D.Catches"), "Catches")(p);
    }
    static TParseTree Catches(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LastCatch, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Catch, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)))), "D.Catches")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LastCatch, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Catch, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Catches, Spacing)))), "D.Catches"), "Catches")(TParseTree("", false,[], s));
    }
    static string Catches(GetName g)
    {
        return "D.Catches";
    }

    static TParseTree LastCatch(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.LastCatch")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.LastCatch"), "LastCatch")(p);
    }
    static TParseTree LastCatch(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.LastCatch")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.LastCatch"), "LastCatch")(TParseTree("", false,[], s));
    }
    static string LastCatch(GetName g)
    {
        return "D.LastCatch";
    }

    static TParseTree Catch(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CatchParameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.Catch")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CatchParameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.Catch"), "Catch")(p);
    }
    static TParseTree Catch(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CatchParameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.Catch")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CatchParameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.Catch"), "Catch")(TParseTree("", false,[], s));
    }
    static string Catch(GetName g)
    {
        return "D.Catch";
    }

    static TParseTree CatchParameter(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.CatchParameter")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.CatchParameter"), "CatchParameter")(p);
    }
    static TParseTree CatchParameter(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.CatchParameter")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.CatchParameter"), "CatchParameter")(TParseTree("", false,[], s));
    }
    static string CatchParameter(GetName g)
    {
        return "D.CatchParameter";
    }

    static TParseTree FinallyStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.FinallyStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.FinallyStatement"), "FinallyStatement")(p);
    }
    static TParseTree FinallyStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.FinallyStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), "D.FinallyStatement"), "FinallyStatement")(TParseTree("", false,[], s));
    }
    static string FinallyStatement(GetName g)
    {
        return "D.FinallyStatement";
    }

    static TParseTree ThrowStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ThrowStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ThrowStatement"), "ThrowStatement")(p);
    }
    static TParseTree ThrowStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ThrowStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.ThrowStatement"), "ThrowStatement")(TParseTree("", false,[], s));
    }
    static string ThrowStatement(GetName g)
    {
        return "D.ThrowStatement";
    }

    static TParseTree ScopeGuardStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(exit)"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(success)"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(failure)"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyOrScopeBlockStatement, Spacing))), "D.ScopeGuardStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(exit)"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(success)"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(failure)"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyOrScopeBlockStatement, Spacing))), "D.ScopeGuardStatement"), "ScopeGuardStatement")(p);
    }
    static TParseTree ScopeGuardStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(exit)"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(success)"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(failure)"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyOrScopeBlockStatement, Spacing))), "D.ScopeGuardStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(exit)"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(success)"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope(failure)"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, NonEmptyOrScopeBlockStatement, Spacing))), "D.ScopeGuardStatement"), "ScopeGuardStatement")(TParseTree("", false,[], s));
    }
    static string ScopeGuardStatement(GetName g)
    {
        return "D.ScopeGuardStatement";
    }

    static TParseTree AsmStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.AsmStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.AsmStatement"), "AsmStatement")(p);
    }
    static TParseTree AsmStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.AsmStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.AsmStatement"), "AsmStatement")(TParseTree("", false,[], s));
    }
    static string AsmStatement(GetName g)
    {
        return "D.AsmStatement";
    }

    static TParseTree AsmInstructionList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)))), "D.AsmInstructionList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)))), "D.AsmInstructionList"), "AsmInstructionList")(p);
    }
    static TParseTree AsmInstructionList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)))), "D.AsmInstructionList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AsmInstructionList, Spacing)))), "D.AsmInstructionList"), "AsmInstructionList")(TParseTree("", false,[], s));
    }
    static string AsmInstructionList(GetName g)
    {
        return "D.AsmInstructionList";
    }

    static TParseTree PragmaStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing))), "D.PragmaStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing))), "D.PragmaStatement"), "PragmaStatement")(p);
    }
    static TParseTree PragmaStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing))), "D.PragmaStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeStatement, Spacing))), "D.PragmaStatement"), "PragmaStatement")(TParseTree("", false,[], s));
    }
    static string PragmaStatement(GetName g)
    {
        return "D.PragmaStatement";
    }

    static TParseTree MixinStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.MixinStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.MixinStatement"), "MixinStatement")(p);
    }
    static TParseTree MixinStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.MixinStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.MixinStatement"), "MixinStatement")(TParseTree("", false,[], s));
    }
    static string MixinStatement(GetName g)
    {
        return "D.MixinStatement";
    }

    static TParseTree Expression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.Expression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.Expression"), "Expression")(p);
    }
    static TParseTree Expression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.Expression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.Expression"), "Expression")(TParseTree("", false,[], s));
    }
    static string Expression(GetName g)
    {
        return "D.Expression";
    }

    static TParseTree AssignExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Op, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.AssignExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Op, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.AssignExpression"), "AssignExpression")(p);
    }
    static TParseTree AssignExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Op, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.AssignExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Op, Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.AssignExpression"), "AssignExpression")(TParseTree("", false,[], s));
    }
    static string AssignExpression(GetName g)
    {
        return "D.AssignExpression";
    }

    static TParseTree Op(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing))), "D.Op")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing))), "D.Op"), "Op")(p);
    }
    static TParseTree Op(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing))), "D.Op")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing))), "D.Op"), "Op")(TParseTree("", false,[], s));
    }
    static string Op(GetName g)
    {
        return "D.Op";
    }

    static TParseTree ConditionalExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing)))), "D.ConditionalExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing)))), "D.ConditionalExpression"), "ConditionalExpression")(p);
    }
    static TParseTree ConditionalExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing)))), "D.ConditionalExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing)))), "D.ConditionalExpression"), "ConditionalExpression")(TParseTree("", false,[], s));
    }
    static string ConditionalExpression(GetName g)
    {
        return "D.ConditionalExpression";
    }

    static TParseTree OrOrExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing))), Spacing)))), "D.OrOrExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing))), Spacing)))), "D.OrOrExpression"), "OrOrExpression")(p);
    }
    static TParseTree OrOrExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing))), Spacing)))), "D.OrOrExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, OrOrExpression, Spacing))), Spacing)))), "D.OrOrExpression"), "OrOrExpression")(TParseTree("", false,[], s));
    }
    static string OrOrExpression(GetName g)
    {
        return "D.OrOrExpression";
    }

    static TParseTree AndAndExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CmpExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpression, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing))), Spacing)))), "D.AndAndExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CmpExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpression, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing))), Spacing)))), "D.AndAndExpression"), "AndAndExpression")(p);
    }
    static TParseTree AndAndExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CmpExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpression, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing))), Spacing)))), "D.AndAndExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CmpExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpression, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AndAndExpression, Spacing))), Spacing)))), "D.AndAndExpression"), "AndAndExpression")(TParseTree("", false,[], s));
    }
    static string AndAndExpression(GetName g)
    {
        return "D.AndAndExpression";
    }

    static TParseTree OrExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, XorExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing))), Spacing)))), "D.OrExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, XorExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing))), Spacing)))), "D.OrExpression"), "OrExpression")(p);
    }
    static TParseTree OrExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, XorExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing))), Spacing)))), "D.OrExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, XorExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, OrExpression, Spacing))), Spacing)))), "D.OrExpression"), "OrExpression")(TParseTree("", false,[], s));
    }
    static string OrExpression(GetName g)
    {
        return "D.OrExpression";
    }

    static TParseTree XorExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, XorExpression, Spacing))), Spacing)))), "D.XorExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, XorExpression, Spacing))), Spacing)))), "D.XorExpression"), "XorExpression")(p);
    }
    static TParseTree XorExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, XorExpression, Spacing))), Spacing)))), "D.XorExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, XorExpression, Spacing))), Spacing)))), "D.XorExpression"), "XorExpression")(TParseTree("", false,[], s));
    }
    static string XorExpression(GetName g)
    {
        return "D.XorExpression";
    }

    static TParseTree AndExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AndExpression, Spacing))), Spacing)))), "D.AndExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AndExpression, Spacing))), Spacing)))), "D.AndExpression"), "AndExpression")(p);
    }
    static TParseTree AndExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AndExpression, Spacing))), Spacing)))), "D.AndExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AndExpression, Spacing))), Spacing)))), "D.AndExpression"), "AndExpression")(TParseTree("", false,[], s));
    }
    static string AndExpression(GetName g)
    {
        return "D.AndExpression";
    }

    static TParseTree CmpExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IdentityExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.CmpExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IdentityExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.CmpExpression"), "CmpExpression")(p);
    }
    static TParseTree CmpExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IdentityExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.CmpExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EqualExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IdentityExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, RelExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.CmpExpression"), "CmpExpression")(TParseTree("", false,[], s));
    }
    static string CmpExpression(GetName g)
    {
        return "D.CmpExpression";
    }

    static TParseTree EqualExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.EqualExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.EqualExpression"), "EqualExpression")(p);
    }
    static TParseTree EqualExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.EqualExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.EqualExpression"), "EqualExpression")(TParseTree("", false,[], s));
    }
    static string EqualExpression(GetName g)
    {
        return "D.EqualExpression";
    }

    static TParseTree IdentityExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!is"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.IdentityExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!is"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.IdentityExpression"), "IdentityExpression")(p);
    }
    static TParseTree IdentityExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!is"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.IdentityExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!is"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.IdentityExpression"), "IdentityExpression")(TParseTree("", false,[], s));
    }
    static string IdentityExpression(GetName g)
    {
        return "D.IdentityExpression";
    }

    static TParseTree RelExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelOp, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.RelExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelOp, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.RelExpression"), "RelExpression")(p);
    }
    static TParseTree RelExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelOp, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.RelExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.wrapAround!(Spacing, RelOp, Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), "D.RelExpression"), "RelExpression")(TParseTree("", false,[], s));
    }
    static string RelExpression(GetName g)
    {
        return "D.RelExpression";
    }

    static TParseTree RelOp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing))), "D.RelOp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing))), "D.RelOp"), "RelOp")(p);
    }
    static TParseTree RelOp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing))), "D.RelOp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing))), "D.RelOp"), "RelOp")(TParseTree("", false,[], s));
    }
    static string RelOp(GetName g)
    {
        return "D.RelOp";
    }

    static TParseTree InExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!in"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), Spacing)))), "D.InExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!in"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), Spacing)))), "D.InExpression"), "InExpression")(p);
    }
    static TParseTree InExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!in"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), Spacing)))), "D.InExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!in"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, ShiftExpression, Spacing))), Spacing)))), "D.InExpression"), "InExpression")(TParseTree("", false,[], s));
    }
    static string InExpression(GetName g)
    {
        return "D.InExpression";
    }

    static TParseTree ShiftExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing))), Spacing)))), "D.ShiftExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing))), Spacing)))), "D.ShiftExpression"), "ShiftExpression")(p);
    }
    static TParseTree ShiftExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing))), Spacing)))), "D.ShiftExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing))), Spacing)))), "D.ShiftExpression"), "ShiftExpression")(TParseTree("", false,[], s));
    }
    static string ShiftExpression(GetName g)
    {
        return "D.ShiftExpression";
    }

    static TParseTree AddExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CatExpression, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, MulExpression, Spacing))), Spacing)))), "D.AddExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CatExpression, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, MulExpression, Spacing))), Spacing)))), "D.AddExpression"), "AddExpression")(p);
    }
    static TParseTree AddExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CatExpression, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, MulExpression, Spacing))), Spacing)))), "D.AddExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CatExpression, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, MulExpression, Spacing))), Spacing)))), "D.AddExpression"), "AddExpression")(TParseTree("", false,[], s));
    }
    static string AddExpression(GetName g)
    {
        return "D.AddExpression";
    }

    static TParseTree CatExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing))), Spacing)))), "D.CatExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing))), Spacing)))), "D.CatExpression"), "CatExpression")(p);
    }
    static TParseTree CatExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing))), Spacing)))), "D.CatExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, AddExpression, Spacing))), Spacing)))), "D.CatExpression"), "CatExpression")(TParseTree("", false,[], s));
    }
    static string CatExpression(GetName g)
    {
        return "D.CatExpression";
    }

    static TParseTree MulExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), Spacing)))), "D.MulExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), Spacing)))), "D.MulExpression"), "MulExpression")(p);
    }
    static TParseTree MulExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), Spacing)))), "D.MulExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), Spacing)))), "D.MulExpression"), "MulExpression")(TParseTree("", false,[], s));
    }
    static string MulExpression(GetName g)
    {
        return "D.MulExpression";
    }

    static TParseTree UnaryExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ComplementExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeleteExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PowExpression, Spacing))), "D.UnaryExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ComplementExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeleteExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PowExpression, Spacing))), "D.UnaryExpression"), "UnaryExpression")(p);
    }
    static TParseTree UnaryExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ComplementExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeleteExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PowExpression, Spacing))), "D.UnaryExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ComplementExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeleteExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PowExpression, Spacing))), "D.UnaryExpression"), "UnaryExpression")(TParseTree("", false,[], s));
    }
    static string UnaryExpression(GetName g)
    {
        return "D.UnaryExpression";
    }

    static TParseTree UnaryOp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing))), "D.UnaryOp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing))), "D.UnaryOp"), "UnaryOp")(p);
    }
    static TParseTree UnaryOp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing))), "D.UnaryOp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing))), "D.UnaryOp"), "UnaryOp")(TParseTree("", false,[], s));
    }
    static string UnaryOp(GetName g)
    {
        return "D.UnaryOp";
    }

    static TParseTree ComplementExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.ComplementExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.ComplementExpression"), "ComplementExpression")(p);
    }
    static TParseTree ComplementExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.ComplementExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.ComplementExpression"), "ComplementExpression")(TParseTree("", false,[], s));
    }
    static string ComplementExpression(GetName g)
    {
        return "D.ComplementExpression";
    }

    static TParseTree NewExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NewAnonClassExpression, Spacing))), "D.NewExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NewAnonClassExpression, Spacing))), "D.NewExpression"), "NewExpression")(p);
    }
    static TParseTree NewExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NewAnonClassExpression, Spacing))), "D.NewExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, NewAnonClassExpression, Spacing))), "D.NewExpression"), "NewExpression")(TParseTree("", false,[], s));
    }
    static string NewExpression(GetName g)
    {
        return "D.NewExpression";
    }

    static TParseTree AllocatorArguments(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.AllocatorArguments")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.AllocatorArguments"), "AllocatorArguments")(p);
    }
    static TParseTree AllocatorArguments(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.AllocatorArguments")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.AllocatorArguments"), "AllocatorArguments")(TParseTree("", false,[], s));
    }
    static string AllocatorArguments(GetName g)
    {
        return "D.AllocatorArguments";
    }

    static TParseTree ArgumentList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.ArgumentList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.ArgumentList"), "ArgumentList")(p);
    }
    static TParseTree ArgumentList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.ArgumentList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.ArgumentList"), "ArgumentList")(TParseTree("", false,[], s));
    }
    static string ArgumentList(GetName g)
    {
        return "D.ArgumentList";
    }

    static TParseTree DeleteExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.DeleteExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.DeleteExpression"), "DeleteExpression")(p);
    }
    static TParseTree DeleteExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.DeleteExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.DeleteExpression"), "DeleteExpression")(TParseTree("", false,[], s));
    }
    static string DeleteExpression(GetName g)
    {
        return "D.DeleteExpression";
    }

    static TParseTree CastExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastEqual, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.CastExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastEqual, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.CastExpression"), "CastExpression")(p);
    }
    static TParseTree CastExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastEqual, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.CastExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CastEqual, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), "D.CastExpression"), "CastExpression")(TParseTree("", false,[], s));
    }
    static string CastExpression(GetName g)
    {
        return "D.CastExpression";
    }

    static TParseTree CastEqual(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing))), "D.CastEqual")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing))), "D.CastEqual"), "CastEqual")(p);
    }
    static TParseTree CastEqual(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing))), "D.CastEqual")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing))), "D.CastEqual"), "CastEqual")(TParseTree("", false,[], s));
    }
    static string CastEqual(GetName g)
    {
        return "D.CastEqual";
    }

    static TParseTree PowExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostfixExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), Spacing)))), "D.PowExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostfixExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), Spacing)))), "D.PowExpression"), "PowExpression")(p);
    }
    static TParseTree PowExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostfixExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), Spacing)))), "D.PowExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PostfixExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^^"), Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpression, Spacing))), Spacing)))), "D.PowExpression"), "PowExpression")(TParseTree("", false,[], s));
    }
    static string PowExpression(GetName g)
    {
        return "D.PowExpression";
    }

    static TParseTree PostfixExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IndexExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SliceExpression, Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.PostfixExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IndexExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SliceExpression, Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.PostfixExpression"), "PostfixExpression")(p);
    }
    static TParseTree PostfixExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IndexExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SliceExpression, Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.PostfixExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PrimaryExpression, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IndexExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, SliceExpression, Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, NewExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.PostfixExpression"), "PostfixExpression")(TParseTree("", false,[], s));
    }
    static string PostfixExpression(GetName g)
    {
        return "D.PostfixExpression";
    }

    static TParseTree IndexExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.IndexExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.IndexExpression"), "IndexExpression")(p);
    }
    static TParseTree IndexExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.IndexExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.IndexExpression"), "IndexExpression")(TParseTree("", false,[], s));
    }
    static string IndexExpression(GetName g)
    {
        return "D.IndexExpression";
    }

    static TParseTree SliceExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.SliceExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.SliceExpression"), "SliceExpression")(p);
    }
    static TParseTree SliceExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.SliceExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(".."), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.SliceExpression"), "SliceExpression")(TParseTree("", false,[], s));
    }
    static string SliceExpression(GetName g)
    {
        return "D.SliceExpression";
    }

    static TParseTree PrimaryExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StringLiterals, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssocArrayLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Lambda, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FunctionLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssertExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeidExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IsExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TraitsExpression, Spacing))), "D.PrimaryExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StringLiterals, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssocArrayLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Lambda, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FunctionLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssertExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeidExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IsExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TraitsExpression, Spacing))), "D.PrimaryExpression"), "PrimaryExpression")(p);
    }
    static TParseTree PrimaryExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StringLiterals, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssocArrayLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Lambda, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FunctionLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssertExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeidExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IsExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TraitsExpression, Spacing))), "D.PrimaryExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StringLiterals, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ArrayLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssocArrayLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Lambda, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FunctionLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssertExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MixinExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ImportExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Typeof, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TypeidExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IsExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TraitsExpression, Spacing))), "D.PrimaryExpression"), "PrimaryExpression")(TParseTree("", false,[], s));
    }
    static string PrimaryExpression(GetName g)
    {
        return "D.PrimaryExpression";
    }

    static TParseTree StringLiterals(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)))), "D.StringLiterals")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)))), "D.StringLiterals"), "StringLiterals")(p);
    }
    static TParseTree StringLiterals(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)))), "D.StringLiterals")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)))), "D.StringLiterals"), "StringLiterals")(TParseTree("", false,[], s));
    }
    static string StringLiterals(GetName g)
    {
        return "D.StringLiterals";
    }

    static TParseTree ArrayLiteral(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.ArrayLiteral")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.ArrayLiteral"), "ArrayLiteral")(p);
    }
    static TParseTree ArrayLiteral(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.ArrayLiteral")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.ArrayLiteral"), "ArrayLiteral")(TParseTree("", false,[], s));
    }
    static string ArrayLiteral(GetName g)
    {
        return "D.ArrayLiteral";
    }

    static TParseTree AssocArrayLiteral(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.AssocArrayLiteral")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.AssocArrayLiteral"), "AssocArrayLiteral")(p);
    }
    static TParseTree AssocArrayLiteral(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.AssocArrayLiteral")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, KeyValuePair, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "D.AssocArrayLiteral"), "AssocArrayLiteral")(TParseTree("", false,[], s));
    }
    static string AssocArrayLiteral(GetName g)
    {
        return "D.AssocArrayLiteral";
    }

    static TParseTree KeyValuePair(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.KeyValuePair")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.KeyValuePair"), "KeyValuePair")(p);
    }
    static TParseTree KeyValuePair(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.KeyValuePair")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.KeyValuePair"), "KeyValuePair")(TParseTree("", false,[], s));
    }
    static string KeyValuePair(GetName g)
    {
        return "D.KeyValuePair";
    }

    static TParseTree Lambda(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.Lambda")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.Lambda"), "Lambda")(p);
    }
    static TParseTree Lambda(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.Lambda")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=>"), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), "D.Lambda"), "Lambda")(TParseTree("", false,[], s));
    }
    static string Lambda(GetName g)
    {
        return "D.Lambda";
    }

    static TParseTree FunctionLiteral(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Type, Spacing)))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.FunctionLiteral")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Type, Spacing)))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.FunctionLiteral"), "FunctionLiteral")(p);
    }
    static TParseTree FunctionLiteral(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Type, Spacing)))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.FunctionLiteral")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Type, Spacing)))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterAttributes, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.FunctionLiteral"), "FunctionLiteral")(TParseTree("", false,[], s));
    }
    static string FunctionLiteral(GetName g)
    {
        return "D.FunctionLiteral";
    }

    static TParseTree ParameterAttributes(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "D.ParameterAttributes")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "D.ParameterAttributes"), "ParameterAttributes")(p);
    }
    static TParseTree ParameterAttributes(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "D.ParameterAttributes")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FunctionAttributes, Spacing)))), "D.ParameterAttributes"), "ParameterAttributes")(TParseTree("", false,[], s));
    }
    static string ParameterAttributes(GetName g)
    {
        return "D.ParameterAttributes";
    }

    static TParseTree AssertExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.AssertExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.AssertExpression"), "AssertExpression")(p);
    }
    static TParseTree AssertExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.AssertExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.AssertExpression"), "AssertExpression")(TParseTree("", false,[], s));
    }
    static string AssertExpression(GetName g)
    {
        return "D.AssertExpression";
    }

    static TParseTree MixinExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.MixinExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.MixinExpression"), "MixinExpression")(p);
    }
    static TParseTree MixinExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.MixinExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.MixinExpression"), "MixinExpression")(TParseTree("", false,[], s));
    }
    static string MixinExpression(GetName g)
    {
        return "D.MixinExpression";
    }

    static TParseTree ImportExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ImportExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ImportExpression"), "ImportExpression")(p);
    }
    static TParseTree ImportExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ImportExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ImportExpression"), "ImportExpression")(TParseTree("", false,[], s));
    }
    static string ImportExpression(GetName g)
    {
        return "D.ImportExpression";
    }

    static TParseTree TypeidExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.TypeidExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.TypeidExpression"), "TypeidExpression")(p);
    }
    static TParseTree TypeidExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.TypeidExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expression, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.TypeidExpression"), "TypeidExpression")(TParseTree("", false,[], s));
    }
    static string TypeidExpression(GetName g)
    {
        return "D.TypeidExpression";
    }

    static TParseTree IsExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing))), Spacing)))), Spacing)))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.IsExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing))), Spacing)))), Spacing)))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.IsExpression"), "IsExpression")(p);
    }
    static TParseTree IsExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing))), Spacing)))), Spacing)))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.IsExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, TypeSpecialization, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing))), Spacing)))), Spacing)))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.IsExpression"), "IsExpression")(TParseTree("", false,[], s));
    }
    static string IsExpression(GetName g)
    {
        return "D.IsExpression";
    }

    static TParseTree TypeSpecialization(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing))), "D.TypeSpecialization")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing))), "D.TypeSpecialization"), "TypeSpecialization")(p);
    }
    static TParseTree TypeSpecialization(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing))), "D.TypeSpecialization")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing))), "D.TypeSpecialization"), "TypeSpecialization")(TParseTree("", false,[], s));
    }
    static string TypeSpecialization(GetName g)
    {
        return "D.TypeSpecialization";
    }

    static TParseTree AttributeSpecifier(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationBlock, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing))), "D.AttributeSpecifier")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationBlock, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing))), "D.AttributeSpecifier"), "AttributeSpecifier")(p);
    }
    static TParseTree AttributeSpecifier(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationBlock, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing))), "D.AttributeSpecifier")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, DeclarationBlock, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing))), "D.AttributeSpecifier"), "AttributeSpecifier")(TParseTree("", false,[], s));
    }
    static string AttributeSpecifier(GetName g)
    {
        return "D.AttributeSpecifier";
    }

    static TParseTree Attribute(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LinkageAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AlignAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ProtectionAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@disable"), Spacing))), "D.Attribute")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LinkageAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AlignAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ProtectionAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@disable"), Spacing))), "D.Attribute"), "Attribute")(p);
    }
    static TParseTree Attribute(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LinkageAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AlignAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ProtectionAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@disable"), Spacing))), "D.Attribute")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, LinkageAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AlignAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Pragma, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ProtectionAttribute, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("@disable"), Spacing))), "D.Attribute"), "Attribute")(TParseTree("", false,[], s));
    }
    static string Attribute(GetName g)
    {
        return "D.Attribute";
    }

    static TParseTree DeclarationBlock(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.DeclarationBlock")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.DeclarationBlock"), "DeclarationBlock")(p);
    }
    static TParseTree DeclarationBlock(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.DeclarationBlock")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.DeclarationBlock"), "DeclarationBlock")(TParseTree("", false,[], s));
    }
    static string DeclarationBlock(GetName g)
    {
        return "D.DeclarationBlock";
    }

    static TParseTree LinkageAttribute(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, LinkageType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.LinkageAttribute")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, LinkageType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.LinkageAttribute"), "LinkageAttribute")(p);
    }
    static TParseTree LinkageAttribute(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, LinkageType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.LinkageAttribute")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, LinkageType, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.LinkageAttribute"), "LinkageAttribute")(TParseTree("", false,[], s));
    }
    static string LinkageAttribute(GetName g)
    {
        return "D.LinkageAttribute";
    }

    static TParseTree LinkageType(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("D"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Windows"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Pascal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("System"), Spacing))), "D.LinkageType")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("D"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Windows"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Pascal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("System"), Spacing))), "D.LinkageType"), "LinkageType")(p);
    }
    static TParseTree LinkageType(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("D"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Windows"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Pascal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("System"), Spacing))), "D.LinkageType")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C++"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("C"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("D"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Windows"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("Pascal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("System"), Spacing))), "D.LinkageType"), "LinkageType")(TParseTree("", false,[], s));
    }
    static string LinkageType(GetName g)
    {
        return "D.LinkageType";
    }

    static TParseTree AlignAttribute(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.AlignAttribute")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.AlignAttribute"), "AlignAttribute")(p);
    }
    static TParseTree AlignAttribute(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.AlignAttribute")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.AlignAttribute"), "AlignAttribute")(TParseTree("", false,[], s));
    }
    static string AlignAttribute(GetName g)
    {
        return "D.AlignAttribute";
    }

    static TParseTree ProtectionAttribute(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing))), "D.ProtectionAttribute")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing))), "D.ProtectionAttribute"), "ProtectionAttribute")(p);
    }
    static TParseTree ProtectionAttribute(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing))), "D.ProtectionAttribute")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing))), "D.ProtectionAttribute"), "ProtectionAttribute")(TParseTree("", false,[], s));
    }
    static string ProtectionAttribute(GetName g)
    {
        return "D.ProtectionAttribute";
    }

    static TParseTree ClassDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassTemplateDeclaration, Spacing))), "D.ClassDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassTemplateDeclaration, Spacing))), "D.ClassDeclaration"), "ClassDeclaration")(p);
    }
    static TParseTree ClassDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassTemplateDeclaration, Spacing))), "D.ClassDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassTemplateDeclaration, Spacing))), "D.ClassDeclaration"), "ClassDeclaration")(TParseTree("", false,[], s));
    }
    static string ClassDeclaration(GetName g)
    {
        return "D.ClassDeclaration";
    }

    static TParseTree BaseClassList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.BaseClassList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.BaseClassList"), "BaseClassList")(p);
    }
    static TParseTree BaseClassList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.BaseClassList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.BaseClassList"), "BaseClassList")(TParseTree("", false,[], s));
    }
    static string BaseClassList(GetName g)
    {
        return "D.BaseClassList";
    }

    static TParseTree ClassBody(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.ClassBody")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.ClassBody"), "ClassBody")(p);
    }
    static TParseTree ClassBody(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.ClassBody")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.ClassBody"), "ClassBody")(TParseTree("", false,[], s));
    }
    static string ClassBody(GetName g)
    {
        return "D.ClassBody";
    }

    static TParseTree ClassBodyDeclarations(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)))), "D.ClassBodyDeclarations")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)))), "D.ClassBodyDeclarations"), "ClassBodyDeclarations")(p);
    }
    static TParseTree ClassBodyDeclarations(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)))), "D.ClassBodyDeclarations")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassBodyDeclarations, Spacing)))), "D.ClassBodyDeclarations"), "ClassBodyDeclarations")(TParseTree("", false,[], s));
    }
    static string ClassBodyDeclarations(GetName g)
    {
        return "D.ClassBodyDeclarations";
    }

    static TParseTree ClassBodyDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Invariant, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing))), "D.ClassBodyDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Invariant, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing))), "D.ClassBodyDeclaration"), "ClassBodyDeclaration")(p);
    }
    static TParseTree ClassBodyDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Invariant, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing))), "D.ClassBodyDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Invariant, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing))), "D.ClassBodyDeclaration"), "ClassBodyDeclaration")(TParseTree("", false,[], s));
    }
    static string ClassBodyDeclaration(GetName g)
    {
        return "D.ClassBodyDeclaration";
    }

    static TParseTree Constructor(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplatedConstructor, Spacing))), "D.Constructor")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplatedConstructor, Spacing))), "D.Constructor"), "Constructor")(p);
    }
    static TParseTree Constructor(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplatedConstructor, Spacing))), "D.Constructor")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplatedConstructor, Spacing))), "D.Constructor"), "Constructor")(TParseTree("", false,[], s));
    }
    static string Constructor(GetName g)
    {
        return "D.Constructor";
    }

    static TParseTree Destructor(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.Destructor")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.Destructor"), "Destructor")(p);
    }
    static TParseTree Destructor(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.Destructor")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.Destructor"), "Destructor")(TParseTree("", false,[], s));
    }
    static string Destructor(GetName g)
    {
        return "D.Destructor";
    }

    static TParseTree StaticConstructor(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StaticConstructor")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StaticConstructor"), "StaticConstructor")(p);
    }
    static TParseTree StaticConstructor(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StaticConstructor")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StaticConstructor"), "StaticConstructor")(TParseTree("", false,[], s));
    }
    static string StaticConstructor(GetName g)
    {
        return "D.StaticConstructor";
    }

    static TParseTree StaticDestructor(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StaticDestructor")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StaticDestructor"), "StaticDestructor")(p);
    }
    static TParseTree StaticDestructor(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StaticDestructor")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StaticDestructor"), "StaticDestructor")(TParseTree("", false,[], s));
    }
    static string StaticDestructor(GetName g)
    {
        return "D.StaticDestructor";
    }

    static TParseTree SharedStaticConstructor(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.SharedStaticConstructor")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.SharedStaticConstructor"), "SharedStaticConstructor")(p);
    }
    static TParseTree SharedStaticConstructor(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.SharedStaticConstructor")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.SharedStaticConstructor"), "SharedStaticConstructor")(TParseTree("", false,[], s));
    }
    static string SharedStaticConstructor(GetName g)
    {
        return "D.SharedStaticConstructor";
    }

    static TParseTree SharedStaticDestructor(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.SharedStaticDestructor")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.SharedStaticDestructor"), "SharedStaticDestructor")(p);
    }
    static TParseTree SharedStaticDestructor(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.SharedStaticDestructor")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.SharedStaticDestructor"), "SharedStaticDestructor")(TParseTree("", false,[], s));
    }
    static string SharedStaticDestructor(GetName g)
    {
        return "D.SharedStaticDestructor";
    }

    static TParseTree Invariant(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.Invariant")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.Invariant"), "Invariant")(p);
    }
    static TParseTree Invariant(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.Invariant")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.Invariant"), "Invariant")(TParseTree("", false,[], s));
    }
    static string Invariant(GetName g)
    {
        return "D.Invariant";
    }

    static TParseTree ClassAllocator(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.ClassAllocator")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.ClassAllocator"), "ClassAllocator")(p);
    }
    static TParseTree ClassAllocator(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.ClassAllocator")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.ClassAllocator"), "ClassAllocator")(TParseTree("", false,[], s));
    }
    static string ClassAllocator(GetName g)
    {
        return "D.ClassAllocator";
    }

    static TParseTree ClassDeallocator(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.ClassDeallocator")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.ClassDeallocator"), "ClassDeallocator")(p);
    }
    static TParseTree ClassDeallocator(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.ClassDeallocator")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.ClassDeallocator"), "ClassDeallocator")(TParseTree("", false,[], s));
    }
    static string ClassDeallocator(GetName g)
    {
        return "D.ClassDeallocator";
    }

    static TParseTree AliasThis(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.AliasThis")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.AliasThis"), "AliasThis")(p);
    }
    static TParseTree AliasThis(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.AliasThis")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.AliasThis"), "AliasThis")(TParseTree("", false,[], s));
    }
    static string AliasThis(GetName g)
    {
        return "D.AliasThis";
    }

    static TParseTree NewAnonClassExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing))), "D.NewAnonClassExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing))), "D.NewAnonClassExpression"), "NewAnonClassExpression")(p);
    }
    static TParseTree NewAnonClassExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing))), "D.NewAnonClassExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, AllocatorArguments, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ClassArguments, Spacing)), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing))), "D.NewAnonClassExpression"), "NewAnonClassExpression")(TParseTree("", false,[], s));
    }
    static string NewAnonClassExpression(GetName g)
    {
        return "D.NewAnonClassExpression";
    }

    static TParseTree ClassArguments(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ClassArguments")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ClassArguments"), "ClassArguments")(p);
    }
    static TParseTree ClassArguments(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ClassArguments")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.ClassArguments"), "ClassArguments")(TParseTree("", false,[], s));
    }
    static string ClassArguments(GetName g)
    {
        return "D.ClassArguments";
    }

    static TParseTree EnumDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EnumTag, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, EnumBaseType, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, EnumBody, Spacing))), "D.EnumDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EnumTag, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, EnumBaseType, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, EnumBody, Spacing))), "D.EnumDeclaration"), "EnumDeclaration")(p);
    }
    static TParseTree EnumDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EnumTag, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, EnumBaseType, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, EnumBody, Spacing))), "D.EnumDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, EnumTag, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, EnumBaseType, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, EnumBody, Spacing))), "D.EnumDeclaration"), "EnumDeclaration")(TParseTree("", false,[], s));
    }
    static string EnumDeclaration(GetName g)
    {
        return "D.EnumDeclaration";
    }

    static TParseTree EnumTag(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.EnumTag")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.EnumTag"), "EnumTag")(p);
    }
    static TParseTree EnumTag(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.EnumTag")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.EnumTag"), "EnumTag")(TParseTree("", false,[], s));
    }
    static string EnumTag(GetName g)
    {
        return "D.EnumTag";
    }

    static TParseTree EnumBaseType(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.EnumBaseType")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.EnumBaseType"), "EnumBaseType")(p);
    }
    static TParseTree EnumBaseType(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.EnumBaseType")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.EnumBaseType"), "EnumBaseType")(TParseTree("", false,[], s));
    }
    static string EnumBaseType(GetName g)
    {
        return "D.EnumBaseType";
    }

    static TParseTree EnumBody(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, EnumMember, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, EnumMember, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.EnumBody")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, EnumMember, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, EnumMember, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.EnumBody"), "EnumBody")(p);
    }
    static TParseTree EnumBody(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, EnumMember, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, EnumMember, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.EnumBody")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, EnumMember, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, EnumMember, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.EnumBody"), "EnumBody")(TParseTree("", false,[], s));
    }
    static string EnumBody(GetName g)
    {
        return "D.EnumBody";
    }

    static TParseTree EnumMember(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.EnumMember")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.EnumMember"), "EnumMember")(p);
    }
    static TParseTree EnumMember(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.EnumMember")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)))), "D.EnumMember"), "EnumMember")(TParseTree("", false,[], s));
    }
    static string EnumMember(GetName g)
    {
        return "D.EnumMember";
    }

    static TParseTree FunctionBody(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing))), "D.FunctionBody")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing))), "D.FunctionBody"), "FunctionBody")(p);
    }
    static TParseTree FunctionBody(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing))), "D.FunctionBody")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OutStatement, Spacing), pegged.peg.wrapAround!(Spacing, InStatement, Spacing), pegged.peg.wrapAround!(Spacing, BodyStatement, Spacing))), "D.FunctionBody"), "FunctionBody")(TParseTree("", false,[], s));
    }
    static string FunctionBody(GetName g)
    {
        return "D.FunctionBody";
    }

    static TParseTree InStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.InStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.InStatement"), "InStatement")(p);
    }
    static TParseTree InStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.InStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.InStatement"), "InStatement")(TParseTree("", false,[], s));
    }
    static string InStatement(GetName g)
    {
        return "D.InStatement";
    }

    static TParseTree OutStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.OutStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.OutStatement"), "OutStatement")(p);
    }
    static TParseTree OutStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.OutStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.OutStatement"), "OutStatement")(TParseTree("", false,[], s));
    }
    static string OutStatement(GetName g)
    {
        return "D.OutStatement";
    }

    static TParseTree BodyStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.BodyStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.BodyStatement"), "BodyStatement")(p);
    }
    static TParseTree BodyStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.BodyStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing), pegged.peg.wrapAround!(Spacing, BlockStatement, Spacing))), "D.BodyStatement"), "BodyStatement")(TParseTree("", false,[], s));
    }
    static string BodyStatement(GetName g)
    {
        return "D.BodyStatement";
    }

    static TParseTree AsmInstruction(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, IntegerExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("even"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("naked"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("db"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ds"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("di"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dl"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("df"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dd"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("de"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing))), Spacing)))), "D.AsmInstruction")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, IntegerExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("even"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("naked"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("db"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ds"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("di"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dl"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("df"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dd"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("de"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing))), Spacing)))), "D.AsmInstruction"), "AsmInstruction")(p);
    }
    static TParseTree AsmInstruction(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, IntegerExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("even"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("naked"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("db"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ds"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("di"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dl"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("df"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dd"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("de"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing))), Spacing)))), "D.AsmInstruction")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing), pegged.peg.wrapAround!(Spacing, IntegerExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("even"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("naked"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("db"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ds"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("di"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dl"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("df"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dd"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("de"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmInstruction, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OpCode, Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Operand, Spacing))), Spacing)))), "D.AsmInstruction"), "AsmInstruction")(TParseTree("", false,[], s));
    }
    static string AsmInstruction(GetName g)
    {
        return "D.AsmInstruction";
    }

    static TParseTree IntegerExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.IntegerExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.IntegerExpression"), "IntegerExpression")(p);
    }
    static TParseTree IntegerExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.IntegerExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.IntegerExpression"), "IntegerExpression")(TParseTree("", false,[], s));
    }
    static string IntegerExpression(GetName g)
    {
        return "D.IntegerExpression";
    }

    static TParseTree Operand(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmExp, Spacing))), "D.Operand")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmExp, Spacing))), "D.Operand"), "Operand")(p);
    }
    static TParseTree Operand(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmExp, Spacing))), "D.Operand")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmExp, Spacing))), "D.Operand"), "Operand")(TParseTree("", false,[], s));
    }
    static string Operand(GetName g)
    {
        return "D.Operand";
    }

    static TParseTree AsmExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing))), Spacing)))), "D.AsmExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing))), Spacing)))), "D.AsmExp"), "AsmExp")(p);
    }
    static TParseTree AsmExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing))), Spacing)))), "D.AsmExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing))), Spacing)))), "D.AsmExp"), "AsmExp")(TParseTree("", false,[], s));
    }
    static string AsmExp(GetName g)
    {
        return "D.AsmExp";
    }

    static TParseTree AsmLogOrExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing))), Spacing)))), "D.AsmLogOrExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing))), Spacing)))), "D.AsmLogOrExp"), "AsmLogOrExp")(p);
    }
    static TParseTree AsmLogOrExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing))), Spacing)))), "D.AsmLogOrExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.wrapAround!(Spacing, AsmLogAndExp, Spacing))), Spacing)))), "D.AsmLogOrExp"), "AsmLogOrExp")(TParseTree("", false,[], s));
    }
    static string AsmLogOrExp(GetName g)
    {
        return "D.AsmLogOrExp";
    }

    static TParseTree AsmLogAndExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing))), Spacing)))), "D.AsmLogAndExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing))), Spacing)))), "D.AsmLogAndExp"), "AsmLogAndExp")(p);
    }
    static TParseTree AsmLogAndExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing))), Spacing)))), "D.AsmLogAndExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmOrExp, Spacing))), Spacing)))), "D.AsmLogAndExp"), "AsmLogAndExp")(TParseTree("", false,[], s));
    }
    static string AsmLogAndExp(GetName g)
    {
        return "D.AsmLogAndExp";
    }

    static TParseTree AsmOrExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing))), Spacing)))), "D.AsmOrExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing))), Spacing)))), "D.AsmOrExp"), "AsmOrExp")(p);
    }
    static TParseTree AsmOrExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing))), Spacing)))), "D.AsmOrExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("|"), Spacing), pegged.peg.wrapAround!(Spacing, AsmXorExp, Spacing))), Spacing)))), "D.AsmOrExp"), "AsmOrExp")(TParseTree("", false,[], s));
    }
    static string AsmOrExp(GetName g)
    {
        return "D.AsmOrExp";
    }

    static TParseTree AsmXorExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing))), Spacing)))), "D.AsmXorExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing))), Spacing)))), "D.AsmXorExp"), "AsmXorExp")(p);
    }
    static TParseTree AsmXorExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing))), Spacing)))), "D.AsmXorExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing), pegged.peg.wrapAround!(Spacing, AsmAndExp, Spacing))), Spacing)))), "D.AsmXorExp"), "AsmXorExp")(TParseTree("", false,[], s));
    }
    static string AsmXorExp(GetName g)
    {
        return "D.AsmXorExp";
    }

    static TParseTree AsmAndExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing))), Spacing)))), "D.AsmAndExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing))), Spacing)))), "D.AsmAndExp"), "AsmAndExp")(p);
    }
    static TParseTree AsmAndExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing))), Spacing)))), "D.AsmAndExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&"), Spacing), pegged.peg.wrapAround!(Spacing, AsmEqualExp, Spacing))), Spacing)))), "D.AsmAndExp"), "AsmAndExp")(TParseTree("", false,[], s));
    }
    static string AsmAndExp(GetName g)
    {
        return "D.AsmAndExp";
    }

    static TParseTree AsmEqualExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing))), Spacing)))), "D.AsmEqualExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing))), Spacing)))), "D.AsmEqualExp"), "AsmEqualExp")(p);
    }
    static TParseTree AsmEqualExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing))), Spacing)))), "D.AsmEqualExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmRelExp, Spacing))), Spacing)))), "D.AsmEqualExp"), "AsmEqualExp")(TParseTree("", false,[], s));
    }
    static string AsmEqualExp(GetName g)
    {
        return "D.AsmEqualExp";
    }

    static TParseTree AsmRelExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing))), Spacing)))), "D.AsmRelExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing))), Spacing)))), "D.AsmRelExp"), "AsmRelExp")(p);
    }
    static TParseTree AsmRelExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing))), Spacing)))), "D.AsmRelExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmShiftExp, Spacing))), Spacing)))), "D.AsmRelExp"), "AsmRelExp")(TParseTree("", false,[], s));
    }
    static string AsmRelExp(GetName g)
    {
        return "D.AsmRelExp";
    }

    static TParseTree AsmShiftExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing))), Spacing)))), "D.AsmShiftExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing))), Spacing)))), "D.AsmShiftExp"), "AsmShiftExp")(p);
    }
    static TParseTree AsmShiftExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing))), Spacing)))), "D.AsmShiftExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>>"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<<"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">>"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmAddExp, Spacing))), Spacing)))), "D.AsmShiftExp"), "AsmShiftExp")(TParseTree("", false,[], s));
    }
    static string AsmShiftExp(GetName g)
    {
        return "D.AsmShiftExp";
    }

    static TParseTree AsmAddExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing))), Spacing)))), "D.AsmAddExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing))), Spacing)))), "D.AsmAddExp"), "AsmAddExp")(p);
    }
    static TParseTree AsmAddExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing))), Spacing)))), "D.AsmAddExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmMulExp, Spacing))), Spacing)))), "D.AsmAddExp"), "AsmAddExp")(TParseTree("", false,[], s));
    }
    static string AsmAddExp(GetName g)
    {
        return "D.AsmAddExp";
    }

    static TParseTree AsmMulExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing))), Spacing)))), "D.AsmMulExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing))), Spacing)))), "D.AsmMulExp"), "AsmMulExp")(p);
    }
    static TParseTree AsmMulExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing))), Spacing)))), "D.AsmMulExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmBrExp, Spacing))), Spacing)))), "D.AsmMulExp"), "AsmMulExp")(TParseTree("", false,[], s));
    }
    static string AsmMulExp(GetName g)
    {
        return "D.AsmMulExp";
    }

    static TParseTree AsmBrExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing)))), "D.AsmBrExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing)))), "D.AsmBrExp"), "AsmBrExp")(p);
    }
    static TParseTree AsmBrExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing)))), "D.AsmBrExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), Spacing)))), "D.AsmBrExp"), "AsmBrExp")(TParseTree("", false,[], s));
    }
    static string AsmBrExp(GetName g)
    {
        return "D.AsmBrExp";
    }

    static TParseTree AsmUnaExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmTypePrefix, Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("offsetof"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("seg"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmPrimaryExp, Spacing))), "D.AsmUnaExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmTypePrefix, Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("offsetof"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("seg"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmPrimaryExp, Spacing))), "D.AsmUnaExp"), "AsmUnaExp")(p);
    }
    static TParseTree AsmUnaExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmTypePrefix, Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("offsetof"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("seg"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmPrimaryExp, Spacing))), "D.AsmUnaExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmTypePrefix, Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("offsetof"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("seg"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, AsmUnaExp, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsmPrimaryExp, Spacing))), "D.AsmUnaExp"), "AsmUnaExp")(TParseTree("", false,[], s));
    }
    static string AsmUnaExp(GetName g)
    {
        return "D.AsmUnaExp";
    }

    static TParseTree AsmPrimaryExp(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LOCAL_SIZE"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Register, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing))), "D.AsmPrimaryExp")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LOCAL_SIZE"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Register, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing))), "D.AsmPrimaryExp"), "AsmPrimaryExp")(p);
    }
    static TParseTree AsmPrimaryExp(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LOCAL_SIZE"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Register, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing))), "D.AsmPrimaryExp")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LOCAL_SIZE"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("$"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Register, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing))), "D.AsmPrimaryExp"), "AsmPrimaryExp")(TParseTree("", false,[], s));
    }
    static string AsmPrimaryExp(GetName g)
    {
        return "D.AsmPrimaryExp";
    }

    static TParseTree DotIdentifier(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing))), Spacing)))), "D.DotIdentifier")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing))), Spacing)))), "D.DotIdentifier"), "DotIdentifier")(p);
    }
    static TParseTree DotIdentifier(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing))), Spacing)))), "D.DotIdentifier")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, DotIdentifier, Spacing))), Spacing)))), "D.DotIdentifier"), "DotIdentifier")(TParseTree("", false,[], s));
    }
    static string DotIdentifier(GetName g)
    {
        return "D.DotIdentifier";
    }

    static TParseTree AsmTypePrefix(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("near"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("far"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("word"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dword"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("qword"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ptr"), Spacing))), "D.AsmTypePrefix")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("near"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("far"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("word"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dword"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("qword"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ptr"), Spacing))), "D.AsmTypePrefix"), "AsmTypePrefix")(p);
    }
    static TParseTree AsmTypePrefix(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("near"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("far"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("word"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dword"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("qword"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ptr"), Spacing))), "D.AsmTypePrefix")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("near"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("far"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("word"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dword"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("qword"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ptr"), Spacing))), "D.AsmTypePrefix"), "AsmTypePrefix")(TParseTree("", false,[], s));
    }
    static string AsmTypePrefix(GetName g)
    {
        return "D.AsmTypePrefix";
    }

    static TParseTree Register(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.Register")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.Register"), "Register")(p);
    }
    static TParseTree Register(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.Register")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.Register"), "Register")(TParseTree("", false,[], s));
    }
    static string Register(GetName g)
    {
        return "D.Register";
    }

    static TParseTree OpCode(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.OpCode")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.OpCode"), "OpCode")(p);
    }
    static TParseTree OpCode(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.OpCode")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.OpCode"), "OpCode")(TParseTree("", false,[], s));
    }
    static string OpCode(GetName g)
    {
        return "D.OpCode";
    }

    static TParseTree InterfaceDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InterfaceTemplateDeclaration, Spacing))), "D.InterfaceDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InterfaceTemplateDeclaration, Spacing))), "D.InterfaceDeclaration"), "InterfaceDeclaration")(p);
    }
    static TParseTree InterfaceDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InterfaceTemplateDeclaration, Spacing))), "D.InterfaceDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, InterfaceTemplateDeclaration, Spacing))), "D.InterfaceDeclaration"), "InterfaceDeclaration")(TParseTree("", false,[], s));
    }
    static string InterfaceDeclaration(GetName g)
    {
        return "D.InterfaceDeclaration";
    }

    static TParseTree BaseInterfaceList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.BaseInterfaceList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.BaseInterfaceList"), "BaseInterfaceList")(p);
    }
    static TParseTree BaseInterfaceList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.BaseInterfaceList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing)))), "D.BaseInterfaceList"), "BaseInterfaceList")(TParseTree("", false,[], s));
    }
    static string BaseInterfaceList(GetName g)
    {
        return "D.BaseInterfaceList";
    }

    static TParseTree InterfaceBody(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.InterfaceBody")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.InterfaceBody"), "InterfaceBody")(p);
    }
    static TParseTree InterfaceBody(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.InterfaceBody")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.InterfaceBody"), "InterfaceBody")(TParseTree("", false,[], s));
    }
    static string InterfaceBody(GetName g)
    {
        return "D.InterfaceBody";
    }

    static TParseTree Pragma(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Pragma")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Pragma"), "Pragma")(p);
    }
    static TParseTree Pragma(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Pragma")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, ArgumentList, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Pragma"), "Pragma")(TParseTree("", false,[], s));
    }
    static string Pragma(GetName g)
    {
        return "D.Pragma";
    }

    static TParseTree AggregateDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructTemplateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionTemplateDeclaration, Spacing))), "D.AggregateDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructTemplateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionTemplateDeclaration, Spacing))), "D.AggregateDeclaration"), "AggregateDeclaration")(p);
    }
    static TParseTree AggregateDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructTemplateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionTemplateDeclaration, Spacing))), "D.AggregateDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBody, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructTemplateDeclaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnionTemplateDeclaration, Spacing))), "D.AggregateDeclaration"), "AggregateDeclaration")(TParseTree("", false,[], s));
    }
    static string AggregateDeclaration(GetName g)
    {
        return "D.AggregateDeclaration";
    }

    static TParseTree StructBody(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.StructBody")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.StructBody"), "StructBody")(p);
    }
    static TParseTree StructBody(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.StructBody")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.StructBody"), "StructBody")(TParseTree("", false,[], s));
    }
    static string StructBody(GetName g)
    {
        return "D.StructBody";
    }

    static TParseTree StructBodyDeclarations(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)))), "D.StructBodyDeclarations")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)))), "D.StructBodyDeclarations"), "StructBodyDeclarations")(p);
    }
    static TParseTree StructBodyDeclarations(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)))), "D.StructBodyDeclarations")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructBodyDeclaration, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, StructBodyDeclarations, Spacing)))), "D.StructBodyDeclarations"), "StructBodyDeclarations")(TParseTree("", false,[], s));
    }
    static string StructBodyDeclarations(GetName g)
    {
        return "D.StructBodyDeclarations";
    }

    static TParseTree StructBodyDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructAllocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeallocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructPostblit, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasThis, Spacing))), "D.StructBodyDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructAllocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeallocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructPostblit, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasThis, Spacing))), "D.StructBodyDeclaration"), "StructBodyDeclaration")(p);
    }
    static TParseTree StructBodyDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructAllocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeallocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructPostblit, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasThis, Spacing))), "D.StructBodyDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DeclDef, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructAllocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructDeallocator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StructPostblit, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AliasThis, Spacing))), "D.StructBodyDeclaration"), "StructBodyDeclaration")(TParseTree("", false,[], s));
    }
    static string StructBodyDeclaration(GetName g)
    {
        return "D.StructBodyDeclaration";
    }

    static TParseTree StructAllocator(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing))), "D.StructAllocator")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing))), "D.StructAllocator"), "StructAllocator")(p);
    }
    static TParseTree StructAllocator(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing))), "D.StructAllocator")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassAllocator, Spacing))), "D.StructAllocator"), "StructAllocator")(TParseTree("", false,[], s));
    }
    static string StructAllocator(GetName g)
    {
        return "D.StructAllocator";
    }

    static TParseTree StructDeallocator(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing))), "D.StructDeallocator")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing))), "D.StructDeallocator"), "StructDeallocator")(p);
    }
    static TParseTree StructDeallocator(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing))), "D.StructDeallocator")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ClassDeallocator, Spacing))), "D.StructDeallocator"), "StructDeallocator")(TParseTree("", false,[], s));
    }
    static string StructDeallocator(GetName g)
    {
        return "D.StructDeallocator";
    }

    static TParseTree StructPostblit(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this(this)"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StructPostblit")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this(this)"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StructPostblit"), "StructPostblit")(p);
    }
    static TParseTree StructPostblit(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this(this)"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StructPostblit")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this(this)"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.StructPostblit"), "StructPostblit")(TParseTree("", false,[], s));
    }
    static string StructPostblit(GetName g)
    {
        return "D.StructPostblit";
    }

    static TParseTree TemplateDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "D.TemplateDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "D.TemplateDeclaration"), "TemplateDeclaration")(p);
    }
    static TParseTree TemplateDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "D.TemplateDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)))), "D.TemplateDeclaration"), "TemplateDeclaration")(TParseTree("", false,[], s));
    }
    static string TemplateDeclaration(GetName g)
    {
        return "D.TemplateDeclaration";
    }

    static TParseTree TemplateIdentifier(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.TemplateIdentifier")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.TemplateIdentifier"), "TemplateIdentifier")(p);
    }
    static TParseTree TemplateIdentifier(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.TemplateIdentifier")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.TemplateIdentifier"), "TemplateIdentifier")(TParseTree("", false,[], s));
    }
    static string TemplateIdentifier(GetName g)
    {
        return "D.TemplateIdentifier";
    }

    static TParseTree TemplateParameterList(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing))), Spacing)))), "D.TemplateParameterList")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing))), Spacing)))), "D.TemplateParameterList"), "TemplateParameterList")(p);
    }
    static TParseTree TemplateParameterList(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing))), Spacing)))), "D.TemplateParameterList")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameter, Spacing))), Spacing)))), "D.TemplateParameterList"), "TemplateParameterList")(TParseTree("", false,[], s));
    }
    static string TemplateParameterList(GetName g)
    {
        return "D.TemplateParameterList";
    }

    static TParseTree TemplateParameter(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateValueParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateAliasParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateTupleParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateThisParameter, Spacing))), "D.TemplateParameter")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateValueParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateAliasParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateTupleParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateThisParameter, Spacing))), "D.TemplateParameter"), "TemplateParameter")(p);
    }
    static TParseTree TemplateParameter(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateValueParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateAliasParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateTupleParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateThisParameter, Spacing))), "D.TemplateParameter")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateValueParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateAliasParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateTupleParameter, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateThisParameter, Spacing))), "D.TemplateParameter"), "TemplateParameter")(TParseTree("", false,[], s));
    }
    static string TemplateParameter(GetName g)
    {
        return "D.TemplateParameter";
    }

    static TParseTree TemplateInstance(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateSingleArgument, Spacing))), Spacing))), "D.TemplateInstance")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateSingleArgument, Spacing))), Spacing))), "D.TemplateInstance"), "TemplateInstance")(p);
    }
    static TParseTree TemplateInstance(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateSingleArgument, Spacing))), Spacing))), "D.TemplateInstance")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateSingleArgument, Spacing))), Spacing))), "D.TemplateInstance"), "TemplateInstance")(TParseTree("", false,[], s));
    }
    static string TemplateInstance(GetName g)
    {
        return "D.TemplateInstance";
    }

    static TParseTree TemplateArgument(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Symbol, Spacing))), "D.TemplateArgument")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Symbol, Spacing))), "D.TemplateArgument"), "TemplateArgument")(p);
    }
    static TParseTree TemplateArgument(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Symbol, Spacing))), "D.TemplateArgument")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Symbol, Spacing))), "D.TemplateArgument"), "TemplateArgument")(TParseTree("", false,[], s));
    }
    static string TemplateArgument(GetName g)
    {
        return "D.TemplateArgument";
    }

    static TParseTree Symbol(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing)), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), "D.Symbol")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing)), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), "D.Symbol"), "Symbol")(p);
    }
    static TParseTree Symbol(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing)), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), "D.Symbol")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing)), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), "D.Symbol"), "Symbol")(TParseTree("", false,[], s));
    }
    static string Symbol(GetName g)
    {
        return "D.Symbol";
    }

    static TParseTree SymbolTail(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), Spacing)))), "D.SymbolTail")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), Spacing)))), "D.SymbolTail"), "SymbolTail")(p);
    }
    static TParseTree SymbolTail(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), Spacing)))), "D.SymbolTail")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TemplateInstance, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), Spacing))), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, SymbolTail, Spacing))), Spacing)))), "D.SymbolTail"), "SymbolTail")(TParseTree("", false,[], s));
    }
    static string SymbolTail(GetName g)
    {
        return "D.SymbolTail";
    }

    static TParseTree TemplateSingleArgument(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.TemplateSingleArgument")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.TemplateSingleArgument"), "TemplateSingleArgument")(p);
    }
    static TParseTree TemplateSingleArgument(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.TemplateSingleArgument")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicTypeX, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CharacterLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FloatLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.TemplateSingleArgument"), "TemplateSingleArgument")(TParseTree("", false,[], s));
    }
    static string TemplateSingleArgument(GetName g)
    {
        return "D.TemplateSingleArgument";
    }

    static TParseTree TemplateTypeParameter(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPDefault, Spacing)))), "D.TemplateTypeParameter")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPDefault, Spacing)))), "D.TemplateTypeParameter"), "TemplateTypeParameter")(p);
    }
    static TParseTree TemplateTypeParameter(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPDefault, Spacing)))), "D.TemplateTypeParameter")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TTPDefault, Spacing)))), "D.TemplateTypeParameter"), "TemplateTypeParameter")(TParseTree("", false,[], s));
    }
    static string TemplateTypeParameter(GetName g)
    {
        return "D.TemplateTypeParameter";
    }

    static TParseTree TTPSpecialization(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TTPSpecialization")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TTPSpecialization"), "TTPSpecialization")(p);
    }
    static TParseTree TTPSpecialization(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TTPSpecialization")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TTPSpecialization"), "TTPSpecialization")(TParseTree("", false,[], s));
    }
    static string TTPSpecialization(GetName g)
    {
        return "D.TTPSpecialization";
    }

    static TParseTree TTPDefault(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TTPDefault")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TTPDefault"), "TTPDefault")(p);
    }
    static TParseTree TTPDefault(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TTPDefault")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TTPDefault"), "TTPDefault")(TParseTree("", false,[], s));
    }
    static string TTPDefault(GetName g)
    {
        return "D.TTPDefault";
    }

    static TParseTree TemplateThisParameter(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing))), "D.TemplateThisParameter")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing))), "D.TemplateThisParameter"), "TemplateThisParameter")(p);
    }
    static TParseTree TemplateThisParameter(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing))), "D.TemplateThisParameter")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateTypeParameter, Spacing))), "D.TemplateThisParameter"), "TemplateThisParameter")(TParseTree("", false,[], s));
    }
    static string TemplateThisParameter(GetName g)
    {
        return "D.TemplateThisParameter";
    }

    static TParseTree TemplateValueParameter(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPDefault, Spacing)))), "D.TemplateValueParameter")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPDefault, Spacing)))), "D.TemplateValueParameter"), "TemplateValueParameter")(p);
    }
    static TParseTree TemplateValueParameter(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPDefault, Spacing)))), "D.TemplateValueParameter")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TVPDefault, Spacing)))), "D.TemplateValueParameter"), "TemplateValueParameter")(TParseTree("", false,[], s));
    }
    static string TemplateValueParameter(GetName g)
    {
        return "D.TemplateValueParameter";
    }

    static TParseTree TVPSpecialization(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), "D.TVPSpecialization")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), "D.TVPSpecialization"), "TVPSpecialization")(p);
    }
    static TParseTree TVPSpecialization(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), "D.TVPSpecialization")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), "D.TVPSpecialization"), "TVPSpecialization")(TParseTree("", false,[], s));
    }
    static string TVPSpecialization(GetName g)
    {
        return "D.TVPSpecialization";
    }

    static TParseTree TVPDefault(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing))), "D.TVPDefault")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing))), "D.TVPDefault"), "TVPDefault")(p);
    }
    static TParseTree TVPDefault(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing))), "D.TVPDefault")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing))), "D.TVPDefault"), "TVPDefault")(TParseTree("", false,[], s));
    }
    static string TVPDefault(GetName g)
    {
        return "D.TVPDefault";
    }

    static TParseTree TemplateAliasParameter(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPDefault, Spacing)))), "D.TemplateAliasParameter")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPDefault, Spacing)))), "D.TemplateAliasParameter"), "TemplateAliasParameter")(p);
    }
    static TParseTree TemplateAliasParameter(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPDefault, Spacing)))), "D.TemplateAliasParameter")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, BasicType, Spacing), pegged.peg.wrapAround!(Spacing, Declarator, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPSpecialization, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TAPDefault, Spacing)))), "D.TemplateAliasParameter"), "TemplateAliasParameter")(TParseTree("", false,[], s));
    }
    static string TemplateAliasParameter(GetName g)
    {
        return "D.TemplateAliasParameter";
    }

    static TParseTree TAPSpecialization(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing))), "D.TAPSpecialization")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing))), "D.TAPSpecialization"), "TAPSpecialization")(p);
    }
    static TParseTree TAPSpecialization(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing))), "D.TAPSpecialization")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing))), "D.TAPSpecialization"), "TAPSpecialization")(TParseTree("", false,[], s));
    }
    static string TAPSpecialization(GetName g)
    {
        return "D.TAPSpecialization";
    }

    static TParseTree TAPDefault(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing))), "D.TAPDefault")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing))), "D.TAPDefault"), "TAPDefault")(p);
    }
    static TParseTree TAPDefault(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing))), "D.TAPDefault")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, ConditionalExpression, Spacing))), Spacing))), "D.TAPDefault"), "TAPDefault")(TParseTree("", false,[], s));
    }
    static string TAPDefault(GetName g)
    {
        return "D.TAPDefault";
    }

    static TParseTree TemplateTupleParameter(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing))), "D.TemplateTupleParameter")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing))), "D.TemplateTupleParameter"), "TemplateTupleParameter")(p);
    }
    static TParseTree TemplateTupleParameter(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing))), "D.TemplateTupleParameter")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing))), "D.TemplateTupleParameter"), "TemplateTupleParameter")(TParseTree("", false,[], s));
    }
    static string TemplateTupleParameter(GetName g)
    {
        return "D.TemplateTupleParameter";
    }

    static TParseTree TemplatedConstructor(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.TemplatedConstructor")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.TemplatedConstructor"), "TemplatedConstructor")(p);
    }
    static TParseTree TemplatedConstructor(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.TemplatedConstructor")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, Parameters, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.TemplatedConstructor"), "TemplatedConstructor")(TParseTree("", false,[], s));
    }
    static string TemplatedConstructor(GetName g)
    {
        return "D.TemplatedConstructor";
    }

    static TParseTree ClassTemplateDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing))), "D.ClassTemplateDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing))), "D.ClassTemplateDeclaration"), "ClassTemplateDeclaration")(p);
    }
    static TParseTree ClassTemplateDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing))), "D.ClassTemplateDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseClassList, Spacing)), pegged.peg.wrapAround!(Spacing, ClassBody, Spacing))), "D.ClassTemplateDeclaration"), "ClassTemplateDeclaration")(TParseTree("", false,[], s));
    }
    static string ClassTemplateDeclaration(GetName g)
    {
        return "D.ClassTemplateDeclaration";
    }

    static TParseTree StructTemplateDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing))), "D.StructTemplateDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing))), "D.StructTemplateDeclaration"), "StructTemplateDeclaration")(p);
    }
    static TParseTree StructTemplateDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing))), "D.StructTemplateDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing))), "D.StructTemplateDeclaration"), "StructTemplateDeclaration")(TParseTree("", false,[], s));
    }
    static string StructTemplateDeclaration(GetName g)
    {
        return "D.StructTemplateDeclaration";
    }

    static TParseTree UnionTemplateDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing))), "D.UnionTemplateDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing))), "D.UnionTemplateDeclaration"), "UnionTemplateDeclaration")(p);
    }
    static TParseTree UnionTemplateDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing))), "D.UnionTemplateDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, StructBody, Spacing))), "D.UnionTemplateDeclaration"), "UnionTemplateDeclaration")(TParseTree("", false,[], s));
    }
    static string UnionTemplateDeclaration(GetName g)
    {
        return "D.UnionTemplateDeclaration";
    }

    static TParseTree InterfaceTemplateDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing))), "D.InterfaceTemplateDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing))), "D.InterfaceTemplateDeclaration"), "InterfaceTemplateDeclaration")(p);
    }
    static TParseTree InterfaceTemplateDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing))), "D.InterfaceTemplateDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, BaseInterfaceList, Spacing)), pegged.peg.wrapAround!(Spacing, InterfaceBody, Spacing))), "D.InterfaceTemplateDeclaration"), "InterfaceTemplateDeclaration")(TParseTree("", false,[], s));
    }
    static string InterfaceTemplateDeclaration(GetName g)
    {
        return "D.InterfaceTemplateDeclaration";
    }

    static TParseTree Constraint(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Constraint")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Constraint"), "Constraint")(p);
    }
    static TParseTree Constraint(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Constraint")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Expression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.Constraint"), "Constraint")(TParseTree("", false,[], s));
    }
    static string Constraint(GetName g)
    {
        return "D.Constraint";
    }

    static TParseTree TemplateMixinDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.TemplateMixinDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.TemplateMixinDeclaration"), "TemplateMixinDeclaration")(p);
    }
    static TParseTree TemplateMixinDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.TemplateMixinDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateParameterList, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Constraint, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, DeclDefs, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.TemplateMixinDeclaration"), "TemplateMixinDeclaration")(TParseTree("", false,[], s));
    }
    static string TemplateMixinDeclaration(GetName g)
    {
        return "D.TemplateMixinDeclaration";
    }

    static TParseTree TemplateMixin(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MixinIdentifier, Spacing)))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.TemplateMixin")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MixinIdentifier, Spacing)))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.TemplateMixin"), "TemplateMixin")(p);
    }
    static TParseTree TemplateMixin(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MixinIdentifier, Spacing)))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.TemplateMixin")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing), pegged.peg.wrapAround!(Spacing, TemplateIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!("), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TemplateArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, MixinIdentifier, Spacing)))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.TemplateMixin"), "TemplateMixin")(TParseTree("", false,[], s));
    }
    static string TemplateMixin(GetName g)
    {
        return "D.TemplateMixin";
    }

    static TParseTree MixinIdentifier(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.MixinIdentifier")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.MixinIdentifier"), "MixinIdentifier")(p);
    }
    static TParseTree MixinIdentifier(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.MixinIdentifier")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "D.MixinIdentifier"), "MixinIdentifier")(TParseTree("", false,[], s));
    }
    static string MixinIdentifier(GetName g)
    {
        return "D.MixinIdentifier";
    }

    static TParseTree TraitsExpression(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TraitsKeyword, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.TraitsExpression")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TraitsKeyword, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.TraitsExpression"), "TraitsExpression")(p);
    }
    static TParseTree TraitsExpression(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TraitsKeyword, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.TraitsExpression")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, TraitsKeyword, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, TraitsArgument, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.TraitsExpression"), "TraitsExpression")(TParseTree("", false,[], s));
    }
    static string TraitsExpression(GetName g)
    {
        return "D.TraitsExpression";
    }

    static TParseTree TraitsKeyword(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractClass"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isArithmetic"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAssociativeArray"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalClass"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFloating"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isIntegral"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isScalar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticArray"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isUnsigned"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVitualFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVirtualMethod"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isRef"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isOut"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isLazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("hasMember"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("identifier"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getMember"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getOverloads"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualFunctions"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualMethods"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("parent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("classInstanceSize"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("allMembers"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("derivedMembers"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isSame"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("compiles"), Spacing))), "D.TraitsKeyword")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractClass"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isArithmetic"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAssociativeArray"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalClass"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFloating"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isIntegral"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isScalar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticArray"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isUnsigned"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVitualFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVirtualMethod"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isRef"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isOut"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isLazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("hasMember"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("identifier"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getMember"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getOverloads"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualFunctions"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualMethods"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("parent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("classInstanceSize"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("allMembers"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("derivedMembers"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isSame"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("compiles"), Spacing))), "D.TraitsKeyword"), "TraitsKeyword")(p);
    }
    static TParseTree TraitsKeyword(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractClass"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isArithmetic"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAssociativeArray"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalClass"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFloating"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isIntegral"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isScalar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticArray"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isUnsigned"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVitualFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVirtualMethod"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isRef"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isOut"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isLazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("hasMember"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("identifier"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getMember"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getOverloads"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualFunctions"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualMethods"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("parent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("classInstanceSize"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("allMembers"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("derivedMembers"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isSame"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("compiles"), Spacing))), "D.TraitsKeyword")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractClass"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isArithmetic"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAssociativeArray"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalClass"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFloating"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isIntegral"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isScalar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticArray"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isUnsigned"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVitualFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isVirtualMethod"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isAbstractFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isFinalFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isStaticFunction"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isRef"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isOut"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isLazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("hasMember"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("identifier"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getMember"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getOverloads"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualFunctions"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("getVirtualMethods"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("parent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("classInstanceSize"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("allMembers"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("derivedMembers"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("isSame"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("compiles"), Spacing))), "D.TraitsKeyword"), "TraitsKeyword")(TParseTree("", false,[], s));
    }
    static string TraitsKeyword(GetName g)
    {
        return "D.TraitsKeyword";
    }

    static TParseTree TraitsArgument(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TraitsArgument")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TraitsArgument"), "TraitsArgument")(p);
    }
    static TParseTree TraitsArgument(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TraitsArgument")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Type, Spacing))), "D.TraitsArgument"), "TraitsArgument")(TParseTree("", false,[], s));
    }
    static string TraitsArgument(GetName g)
    {
        return "D.TraitsArgument";
    }

    static TParseTree UnitTest(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.UnitTest")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.UnitTest"), "UnitTest")(p);
    }
    static TParseTree UnitTest(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.UnitTest")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing), pegged.peg.wrapAround!(Spacing, FunctionBody, Spacing))), "D.UnitTest"), "UnitTest")(TParseTree("", false,[], s));
    }
    static string UnitTest(GetName g)
    {
        return "D.UnitTest";
    }

    static TParseTree ConditionalDeclaration(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Declarations, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing))), Spacing)))), "D.ConditionalDeclaration")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Declarations, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing))), Spacing)))), "D.ConditionalDeclaration"), "ConditionalDeclaration")(p);
    }
    static TParseTree ConditionalDeclaration(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Declarations, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing))), Spacing)))), "D.ConditionalDeclaration")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Declarations, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, CCDeclarationBlock, Spacing))), Spacing)))), "D.ConditionalDeclaration"), "ConditionalDeclaration")(TParseTree("", false,[], s));
    }
    static string ConditionalDeclaration(GetName g)
    {
        return "D.ConditionalDeclaration";
    }

    static TParseTree CCDeclarationBlock(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.CCDeclarationBlock")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.CCDeclarationBlock"), "CCDeclarationBlock")(p);
    }
    static TParseTree CCDeclarationBlock(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.CCDeclarationBlock")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "D.CCDeclarationBlock"), "CCDeclarationBlock")(TParseTree("", false,[], s));
    }
    static string CCDeclarationBlock(GetName g)
    {
        return "D.CCDeclarationBlock";
    }

    static TParseTree Declarations(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)))), "D.Declarations")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)))), "D.Declarations"), "Declarations")(p);
    }
    static TParseTree Declarations(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)))), "D.Declarations")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Declaration, Spacing)))), "D.Declarations"), "Declarations")(TParseTree("", false,[], s));
    }
    static string Declarations(GetName g)
    {
        return "D.Declarations";
    }

    static TParseTree ConditionalStatement(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), Spacing)))), "D.ConditionalStatement")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), Spacing)))), "D.ConditionalStatement"), "ConditionalStatement")(p);
    }
    static TParseTree ConditionalStatement(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), Spacing)))), "D.ConditionalStatement")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Condition, Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.wrapAround!(Spacing, NoScopeNonEmptyStatement, Spacing))), Spacing)))), "D.ConditionalStatement"), "ConditionalStatement")(TParseTree("", false,[], s));
    }
    static string ConditionalStatement(GetName g)
    {
        return "D.ConditionalStatement";
    }

    static TParseTree Condition(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VersionCondition, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DebugCondition, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticIfCondition, Spacing))), "D.Condition")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VersionCondition, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DebugCondition, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticIfCondition, Spacing))), "D.Condition"), "Condition")(p);
    }
    static TParseTree Condition(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VersionCondition, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DebugCondition, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticIfCondition, Spacing))), "D.Condition")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, VersionCondition, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DebugCondition, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, StaticIfCondition, Spacing))), "D.Condition"), "Condition")(TParseTree("", false,[], s));
    }
    static string Condition(GetName g)
    {
        return "D.Condition";
    }

    static TParseTree VersionCondition(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.VersionCondition")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.VersionCondition"), "VersionCondition")(p);
    }
    static TParseTree VersionCondition(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.VersionCondition")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.VersionCondition"), "VersionCondition")(TParseTree("", false,[], s));
    }
    static string VersionCondition(GetName g)
    {
        return "D.VersionCondition";
    }

    static TParseTree VersionSpecification(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.VersionSpecification")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.VersionSpecification"), "VersionSpecification")(p);
    }
    static TParseTree VersionSpecification(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.VersionSpecification")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.VersionSpecification"), "VersionSpecification")(TParseTree("", false,[], s));
    }
    static string VersionSpecification(GetName g)
    {
        return "D.VersionSpecification";
    }

    static TParseTree DebugCondition(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.DebugCondition")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.DebugCondition"), "DebugCondition")(p);
    }
    static TParseTree DebugCondition(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.DebugCondition")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)))), "D.DebugCondition"), "DebugCondition")(TParseTree("", false,[], s));
    }
    static string DebugCondition(GetName g)
    {
        return "D.DebugCondition";
    }

    static TParseTree DebugSpecification(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.DebugSpecification")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.DebugSpecification"), "DebugSpecification")(p);
    }
    static TParseTree DebugSpecification(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.DebugSpecification")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing))), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.DebugSpecification"), "DebugSpecification")(TParseTree("", false,[], s));
    }
    static string DebugSpecification(GetName g)
    {
        return "D.DebugSpecification";
    }

    static TParseTree StaticIfCondition(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.StaticIfCondition")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.StaticIfCondition"), "StaticIfCondition")(p);
    }
    static TParseTree StaticIfCondition(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.StaticIfCondition")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "D.StaticIfCondition"), "StaticIfCondition")(TParseTree("", false,[], s));
    }
    static string StaticIfCondition(GetName g)
    {
        return "D.StaticIfCondition";
    }

    static TParseTree StaticAssert(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.StaticAssert")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.StaticAssert"), "StaticAssert")(p);
    }
    static TParseTree StaticAssert(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.StaticAssert")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AssignExpression, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing))), "D.StaticAssert"), "StaticAssert")(TParseTree("", false,[], s));
    }
    static string StaticAssert(GetName g)
    {
        return "D.StaticAssert";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")))))), "D.Identifier")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")))))), "D.Identifier"), "Identifier")(p);
    }
    static TParseTree Identifier(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")))))), "D.Identifier")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(Keyword), pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")))))), "D.Identifier"), "Identifier")(TParseTree("", false,[], s));
    }
    static string Identifier(GetName g)
    {
        return "D.Identifier";
    }

    static TParseTree Keyword(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cdouble"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cfloat"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("creal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("idouble"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ifloat"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ireal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ucent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("volatile"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__thread"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing))), "D.Keyword")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cdouble"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cfloat"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("creal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("idouble"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ifloat"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ireal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ucent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("volatile"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__thread"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing))), "D.Keyword"), "Keyword")(p);
    }
    static TParseTree Keyword(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cdouble"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cfloat"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("creal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("idouble"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ifloat"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ireal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ucent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("volatile"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__thread"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing))), "D.Keyword")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("abstract"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("align"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("asm"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("assert"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("body"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("byte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("case"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("catch"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cdouble"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cfloat"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("char"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("class"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("creal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("debug"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("default"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delegate"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("delete"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("deprecated"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("double"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("do"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("enum"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("export"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("extern"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("finally"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("final"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("float"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach_reverse"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("foreach"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("function"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("goto"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("idouble"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ifloat"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("immutable"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("inout"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("interface"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("invariant"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("in"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ireal"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("is"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("lazy"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("long"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("macro"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("mixin"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("module"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("new"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("nothrow"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("out"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("override"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("package"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pragma"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("private"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("protected"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("public"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("pure"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("real"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ref"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("scope"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("shared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("short"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("static"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("super"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("switch"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("synchronized"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("template"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("this"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("throw"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("try"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeid"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typeof"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ubyte"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ucent"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ulong"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("union"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("unittest"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("ushort"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("version"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("volatile"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("wchar"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("with"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__FILE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__LINE__"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__gshared"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__thread"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__traits"), Spacing))), "D.Keyword"), "Keyword")(TParseTree("", false,[], s));
    }
    static string Keyword(GetName g)
    {
        return "D.Keyword";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(space), pegged.peg.and!(Comment))))), "D.Spacing")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(space), pegged.peg.and!(Comment))))), "D.Spacing"), "Spacing")(p);
    }
    static TParseTree Spacing(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(space), pegged.peg.and!(Comment))))), "D.Spacing")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(space), pegged.peg.and!(Comment))))), "D.Spacing"), "Spacing")(TParseTree("", false,[], s));
    }
    static string Spacing(GetName g)
    {
        return "D.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(BlockComment), pegged.peg.and!(LineComment), pegged.peg.and!(NestingBlockComment)), "D.Comment")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(BlockComment), pegged.peg.and!(LineComment), pegged.peg.and!(NestingBlockComment)), "D.Comment"), "Comment")(p);
    }
    static TParseTree Comment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(BlockComment), pegged.peg.and!(LineComment), pegged.peg.and!(NestingBlockComment)), "D.Comment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(BlockComment), pegged.peg.and!(LineComment), pegged.peg.and!(NestingBlockComment)), "D.Comment"), "Comment")(TParseTree("", false,[], s));
    }
    static string Comment(GetName g)
    {
        return "D.Comment";
    }

    static TParseTree BlockComment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/ *")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("* /")), pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!("* /"))))), "D.BlockComment")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/ *")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("* /")), pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!("* /"))))), "D.BlockComment"), "BlockComment")(p);
    }
    static TParseTree BlockComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/ *")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("* /")), pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!("* /"))))), "D.BlockComment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/ *")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("* /")), pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!("* /"))))), "D.BlockComment"), "BlockComment")(TParseTree("", false,[], s));
    }
    static string BlockComment(GetName g)
    {
        return "D.BlockComment";
    }

    static TParseTree LineComment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any))), pegged.peg.discard!(endOfLine)))), "D.LineComment")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any))), pegged.peg.discard!(endOfLine)))), "D.LineComment"), "LineComment")(p);
    }
    static TParseTree LineComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any))), pegged.peg.discard!(endOfLine)))), "D.LineComment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("//")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any))), pegged.peg.discard!(endOfLine)))), "D.LineComment"), "LineComment")(TParseTree("", false,[], s));
    }
    static string LineComment(GetName g)
    {
        return "D.LineComment";
    }

    static TParseTree NestingBlockComment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/+")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any))), pegged.peg.option!(NestingBlockComment), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!("+/"))))), "D.NestingBlockComment")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/+")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any))), pegged.peg.option!(NestingBlockComment), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!("+/"))))), "D.NestingBlockComment"), "NestingBlockComment")(p);
    }
    static TParseTree NestingBlockComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/+")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any))), pegged.peg.option!(NestingBlockComment), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!("+/"))))), "D.NestingBlockComment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("/+")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any))), pegged.peg.option!(NestingBlockComment), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/+", "+/")), pegged.peg.any))), pegged.peg.discard!(pegged.peg.literal!("+/"))))), "D.NestingBlockComment"), "NestingBlockComment")(TParseTree("", false,[], s));
    }
    static string NestingBlockComment(GetName g)
    {
        return "D.NestingBlockComment";
    }

    static TParseTree StringLiteral(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WysiwygString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AlternateWysiwygString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DoublequotedString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TokenString, Spacing))), "D.StringLiteral")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WysiwygString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AlternateWysiwygString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DoublequotedString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TokenString, Spacing))), "D.StringLiteral"), "StringLiteral")(p);
    }
    static TParseTree StringLiteral(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WysiwygString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AlternateWysiwygString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DoublequotedString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TokenString, Spacing))), "D.StringLiteral")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, WysiwygString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AlternateWysiwygString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DoublequotedString, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, TokenString, Spacing))), "D.StringLiteral"), "StringLiteral")(TParseTree("", false,[], s));
    }
    static string StringLiteral(GetName g)
    {
        return "D.StringLiteral";
    }

    static TParseTree WysiwygString(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), doublequote, pegged.peg.option!(StringPostfix))), "D.WysiwygString")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), doublequote, pegged.peg.option!(StringPostfix))), "D.WysiwygString"), "WysiwygString")(p);
    }
    static TParseTree WysiwygString(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), doublequote, pegged.peg.option!(StringPostfix))), "D.WysiwygString")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("r"), doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), doublequote, pegged.peg.option!(StringPostfix))), "D.WysiwygString"), "WysiwygString")(TParseTree("", false,[], s));
    }
    static string WysiwygString(GetName g)
    {
        return "D.WysiwygString";
    }

    static TParseTree AlternateWysiwygString(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any))), backquote, pegged.peg.option!(StringPostfix))), "D.AlternateWysiwygString")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any))), backquote, pegged.peg.option!(StringPostfix))), "D.AlternateWysiwygString"), "AlternateWysiwygString")(p);
    }
    static TParseTree AlternateWysiwygString(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any))), backquote, pegged.peg.option!(StringPostfix))), "D.AlternateWysiwygString")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(backquote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.any))), backquote, pegged.peg.option!(StringPostfix))), "D.AlternateWysiwygString"), "AlternateWysiwygString")(TParseTree("", false,[], s));
    }
    static string AlternateWysiwygString(GetName g)
    {
        return "D.AlternateWysiwygString";
    }

    static TParseTree DoublequotedString(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(DQChar))), doublequote, pegged.peg.option!(StringPostfix))), "D.DoublequotedString")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(DQChar))), doublequote, pegged.peg.option!(StringPostfix))), "D.DoublequotedString"), "DoublequotedString")(p);
    }
    static TParseTree DoublequotedString(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(DQChar))), doublequote, pegged.peg.option!(StringPostfix))), "D.DoublequotedString")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(DQChar))), doublequote, pegged.peg.option!(StringPostfix))), "D.DoublequotedString"), "DoublequotedString")(TParseTree("", false,[], s));
    }
    static string DoublequotedString(GetName g)
    {
        return "D.DoublequotedString";
    }

    static TParseTree DQChar(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(EscapeSequence), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "D.DQChar")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(EscapeSequence), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "D.DQChar"), "DQChar")(p);
    }
    static TParseTree DQChar(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(EscapeSequence), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "D.DQChar")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(EscapeSequence), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "D.DQChar"), "DQChar")(TParseTree("", false,[], s));
    }
    static string DQChar(GetName g)
    {
        return "D.DQChar";
    }

    static TParseTree EscapeSequence(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.and!(quote), pegged.peg.and!(doublequote), pegged.peg.and!(backslash), pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"))), pegged.peg.and!(pegged.peg.literal!("x"), HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit)))), "D.EscapeSequence")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.and!(quote), pegged.peg.and!(doublequote), pegged.peg.and!(backslash), pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"))), pegged.peg.and!(pegged.peg.literal!("x"), HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit)))), "D.EscapeSequence"), "EscapeSequence")(p);
    }
    static TParseTree EscapeSequence(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.and!(quote), pegged.peg.and!(doublequote), pegged.peg.and!(backslash), pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"))), pegged.peg.and!(pegged.peg.literal!("x"), HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit)))), "D.EscapeSequence")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(backslash, pegged.peg.or!(pegged.peg.and!(quote), pegged.peg.and!(doublequote), pegged.peg.and!(backslash), pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("a"), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"))), pegged.peg.and!(pegged.peg.literal!("x"), HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("u"), HexDigit, HexDigit, HexDigit, HexDigit), pegged.peg.and!(pegged.peg.literal!("U"), HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit, HexDigit)))), "D.EscapeSequence"), "EscapeSequence")(TParseTree("", false,[], s));
    }
    static string EscapeSequence(GetName g)
    {
        return "D.EscapeSequence";
    }

    static TParseTree StringPostfix(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("c"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("w"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("d"), Spacing))), "D.StringPostfix")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("c"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("w"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("d"), Spacing))), "D.StringPostfix"), "StringPostfix")(p);
    }
    static TParseTree StringPostfix(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("c"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("w"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("d"), Spacing))), "D.StringPostfix")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("c"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("w"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("d"), Spacing))), "D.StringPostfix"), "StringPostfix")(TParseTree("", false,[], s));
    }
    static string StringPostfix(GetName g)
    {
        return "D.StringPostfix";
    }

    static TParseTree TokenString(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("q{"), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("}")), pegged.peg.any))), pegged.peg.literal!("}"))), "D.TokenString")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("q{"), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("}")), pegged.peg.any))), pegged.peg.literal!("}"))), "D.TokenString"), "TokenString")(p);
    }
    static TParseTree TokenString(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("q{"), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("}")), pegged.peg.any))), pegged.peg.literal!("}"))), "D.TokenString")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("q{"), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("}")), pegged.peg.any))), pegged.peg.literal!("}"))), "D.TokenString"), "TokenString")(TParseTree("", false,[], s));
    }
    static string TokenString(GetName g)
    {
        return "D.TokenString";
    }

    static TParseTree CharacterLiteral(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.or!(pegged.peg.and!(EscapeSequence), pegged.peg.and!(pegged.peg.any)))), quote)), "D.CharacterLiteral")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.or!(pegged.peg.and!(EscapeSequence), pegged.peg.and!(pegged.peg.any)))), quote)), "D.CharacterLiteral"), "CharacterLiteral")(p);
    }
    static TParseTree CharacterLiteral(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.or!(pegged.peg.and!(EscapeSequence), pegged.peg.and!(pegged.peg.any)))), quote)), "D.CharacterLiteral")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(quote, pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.or!(pegged.peg.and!(EscapeSequence), pegged.peg.and!(pegged.peg.any)))), quote)), "D.CharacterLiteral"), "CharacterLiteral")(TParseTree("", false,[], s));
    }
    static string CharacterLiteral(GetName g)
    {
        return "D.CharacterLiteral";
    }

    static TParseTree IntegerLiteral(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(DecimalInteger), pegged.peg.and!(BinaryInteger), pegged.peg.and!(HexadecimalInteger)), "D.IntegerLiteral")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(DecimalInteger), pegged.peg.and!(BinaryInteger), pegged.peg.and!(HexadecimalInteger)), "D.IntegerLiteral"), "IntegerLiteral")(p);
    }
    static TParseTree IntegerLiteral(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(DecimalInteger), pegged.peg.and!(BinaryInteger), pegged.peg.and!(HexadecimalInteger)), "D.IntegerLiteral")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(DecimalInteger), pegged.peg.and!(BinaryInteger), pegged.peg.and!(HexadecimalInteger)), "D.IntegerLiteral"), "IntegerLiteral")(TParseTree("", false,[], s));
    }
    static string IntegerLiteral(GetName g)
    {
        return "D.IntegerLiteral";
    }

    static TParseTree DecimalInteger(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix))), "D.DecimalInteger")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix))), "D.DecimalInteger"), "DecimalInteger")(p);
    }
    static TParseTree DecimalInteger(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix))), "D.DecimalInteger")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(Integer, pegged.peg.option!(IntegerSuffix))), "D.DecimalInteger"), "DecimalInteger")(TParseTree("", false,[], s));
    }
    static string DecimalInteger(GetName g)
    {
        return "D.DecimalInteger";
    }

    static TParseTree Integer(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(digit), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.Integer")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(digit), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.Integer"), "Integer")(p);
    }
    static TParseTree Integer(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(digit), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.Integer")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(digit, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(digit), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.Integer"), "Integer")(TParseTree("", false,[], s));
    }
    static string Integer(GetName g)
    {
        return "D.Integer";
    }

    static TParseTree IntegerSuffix(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "D.IntegerSuffix")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "D.IntegerSuffix"), "IntegerSuffix")(p);
    }
    static TParseTree IntegerSuffix(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "D.IntegerSuffix")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.keywords!("Lu", "LU", "uL", "UL", "L", "u", "U"), "D.IntegerSuffix"), "IntegerSuffix")(TParseTree("", false,[], s));
    }
    static string IntegerSuffix(GetName g)
    {
        return "D.IntegerSuffix";
    }

    static TParseTree BinaryInteger(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("0b", "0B"), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1"))), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.BinaryInteger")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("0b", "0B"), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1"))), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.BinaryInteger"), "BinaryInteger")(p);
    }
    static TParseTree BinaryInteger(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("0b", "0B"), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1"))), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.BinaryInteger")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("0b", "0B"), pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.or!(pegged.peg.literal!("0"), pegged.peg.literal!("1"))), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.BinaryInteger"), "BinaryInteger")(TParseTree("", false,[], s));
    }
    static string BinaryInteger(GetName g)
    {
        return "D.BinaryInteger";
    }

    static TParseTree HexadecimalInteger(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("0x", "0X"), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(HexDigit), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.HexadecimalInteger")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("0x", "0X"), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(HexDigit), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.HexadecimalInteger"), "HexadecimalInteger")(p);
    }
    static TParseTree HexadecimalInteger(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("0x", "0X"), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(HexDigit), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.HexadecimalInteger")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("0x", "0X"), HexDigit, pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(HexDigit), pegged.peg.and!(pegged.peg.literal!("_")))))), "D.HexadecimalInteger"), "HexadecimalInteger")(TParseTree("", false,[], s));
    }
    static string HexadecimalInteger(GetName g)
    {
        return "D.HexadecimalInteger";
    }

    static TParseTree HexDigit(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing))), "D.HexDigit")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing))), "D.HexDigit"), "HexDigit")(p);
    }
    static TParseTree HexDigit(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing))), "D.HexDigit")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), Spacing))), "D.HexDigit"), "HexDigit")(TParseTree("", false,[], s));
    }
    static string HexDigit(GetName g)
    {
        return "D.HexDigit";
    }

    static TParseTree FloatLiteral(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer))))), "D.FloatLiteral")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer))))), "D.FloatLiteral"), "FloatLiteral")(p);
    }
    static TParseTree FloatLiteral(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer))))), "D.FloatLiteral")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.or!(pegged.peg.and!(pegged.peg.keywords!("e", "E"), pegged.peg.option!(Sign), Integer))))), "D.FloatLiteral"), "FloatLiteral")(TParseTree("", false,[], s));
    }
    static string FloatLiteral(GetName g)
    {
        return "D.FloatLiteral";
    }

    static TParseTree Sign(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.keywords!("-", "+")))), "D.Sign")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.keywords!("-", "+")))), "D.Sign"), "Sign")(p);
    }
    static TParseTree Sign(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.keywords!("-", "+")))), "D.Sign")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.option!(pegged.peg.keywords!("-", "+")))), "D.Sign"), "Sign")(TParseTree("", false,[], s));
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
        return D(TParseTree(``, false, [], input, 0, 0));
}
    static string opCall(GetName g)
    {
        return "D";
    }

    }
}

alias GenericD!(ParseTree).D D;

