module pegged.performancetest.cursive.generator;

import pegged.grammar;

void main()
{
    asModule("pegged.performancetest.cursive.parser2",
             "parser2",


             "Cursive:
              Program <- Spacing Module Spacing (Declaration Spacing)* Spacing eoi
              LineComment <- :(slash slash (&(endOfLine / eoi) (endOfLine / eoi) / .*))
               BlockComment <- :(slash '*' (&('*' slash) '*' slash / .*))

               Boolean <- 'true' / 'false'
               Null <- 'null'

               IntegerSuffix <- slash ('i8' / 'u8' / 'i16' / 'u16' / 'i32' / 'u32' / 'i64' / 'u64')
               RealSuffix <- slash ('f32' / 'f64')

               Sign <- '+' / '-'

               Decimal <~ [0-9]+
               Binary <~ '0' ('b' / 'B') [0-1]+
               Octal <~ '0' ('o' / 'O') [0-7]+
               Hexadecimal <~ '0' ('x' / 'X') [0-9a-fA-F]+

               Size <- Binary / Octal / Hexadecimal / Decimal
               Integer <- Sign? (Binary / Octal / Hexadecimal / Decimal) IntegerSuffix
               Real <- Sign? Decimal '.' Decimal (('e' / 'E') Sign? Decimal)? RealSuffix

               EscapeSequence <- backslash (quote / doublequote / backslash / 'a' / 'b' / 'f' / 'n' / 'r' / 't' / 'v' / '0')
               UnicodeSequence <- backslash ('u' / 'U') '+'? [0-9a-fA-F]+

               Character <~ :quote (EscapeSequence / UnicodeSequence / !quote .) :quote
               String <~ :doublequote (EscapeSequence / UnicodeSequence / !doublequote .)* :doublequote

               Literal <- Boolean / Null / Real / Integer / Character / String

               Qualifiedidentifier <- identifier (Spacing '.' Spacing identifier)*

               FixedIntegralType <- 'int8' / 'uint8' / 'int16' / 'uint16' / 'int32' / 'uint32' / 'int64' / 'uint64'
               IntegralType <- FixedIntegralType / 'int' / 'uint'
               FloatingPointType <- 'float32' / 'float64'
               CoreType <- 'object' / 'bool' / IntegralType / FloatingPointType

               TypeReference <- Qualifiedidentifier (Spacing GenericParameters)?
               TypeSpecification <- (CoreType / TypeReference) Spacing NullableType?
               GenericParameters <- '<' Spacing Type (Spacing ',' Spacing Type)* Spacing '>'

               FunctionType <- CallingConvention? Spacing ParameterTypes Spacing NullableType?
               ParameterTypes <- '(' Spacing (Type (Spacing ',' Spacing Type)*)? ')'

               PointerType <- '*'
               ArrayType <- '[' Spacing ']'
               VectorType <- '[' Spacing Size Spacing ']'
               NullableType <- '?'

               TupleType <- '{' Spacing Type (Spacing ',' Spacing Type)* Spacing '}'

               TypeModifiers <- (PointerType / ArrayType / VectorType)+ Spacing NullableType?
               Type <- TupleType / TypeSpecification (Spacing (TypeModifiers / FunctionType))?
               InferredType <- 'var' / Type

               ReturnType <- 'void' / Type
               CallingConvention <- 'cdecl' / 'stdcall'

               Module <- 'module' Spacing Qualifiedidentifier Spacing ';'

               Import <- 'import' Spacing Qualifiedidentifier (Spacing ',' Spacing Qualifiedidentifier)* ';'

               Declaration <- Import / Class / Interface / Struct / Enum / Data / ModuleConstructor / ModuleFinalizer / Field / ModuleFunction / Operator

               Visibility <- 'public' / 'internal' / 'private' / 'protected' (('and' / 'or') 'internal')?

               InheritanceList <- ':' Spacing TypeReference (Spacing ',' Spacing TypeReference)*

               Class <- Visibility? Spacing ClassModifiers Spacing 'class' Spacing identifier Spacing InheritanceList? '{' Spacing (ClassMember Spacing)* Spacing '}'
               ClassModifiers <- ('open' / 'abstract')? Spacing ('pure' / 'impure')? Spacing ('safe' / 'unsafe')?
               ClassMember <- ClassConstructor / ClassFinalizer / Constructor / Finalizer / Field / Property / Method / Operator

               Interface <- Visibility? Spacing 'interface' Spacing identifier Spacing InheritanceList? Spacing '{' Spacing (InterfaceMember Spacing)* Spacing '}'
               InterfaceMember <- Property / Method / Operator

               Struct <- Visibility? Spacing StructModifiers Spacing 'struct' Spacing identifier Spacing InheritanceList? Spacing '{' Spacing (StructMember Spacing)* Spacing '}'
               StructModifiers <- ('pure' / 'impure')? Spacing ('safe' / 'unsafe')?
               StructMember <- StructConstructor / StructFinalizer / Constructor / Field / Property / Function / Operator

               Enum <- Visibility? Spacing 'enum' Spacing identifier Spacing EnumBase Spacing '{' Spacing (EnumMember Spacing)* Spacing '}'
               EnumBase <- 'of' Spacing FixedIntegralType
               EnumMember <- identifier (Spacing ',' Spacing identifier)* Spacing '=' Spacing Size Spacing ';'

               Data <- Visibility? Spacing 'data' Spacing DataSemantics Spacing identifier Spacing '{' Spacing (DataMember Spacing)* Spacing '}'
               DataSemantics <- 'byref' / 'byval'
               DataMember <- DataField / DataCase / Property / Function / Operator
               DataField <- Type Spacing identifier (Spacing ',' Spacing identifier)* Spacing ';'
               DataCase <- 'case' Spacing identifier '{' Spacing (DataField Spacing)* Spacing '}'

               StaticConstructorModifiers <- 'thread'? Spacing ('pure' / 'impure')? Spacing ('safe' / 'unsafe')?
               StaticConstructorBody <- BlockStatement

               ModuleConstructor <- StaticConstructorModifiers Spacing 'module' Spacing '(' Spacing ')' Spacing StaticConstructorBody
               ModuleFinalizer <- StaticConstructorModifiers Spacing '~' Spacing 'module' Spacing EmptyParameterList Spacing StaticConstructorBody

               ClassConstructor <- StaticConstructorModifiers Spacing 'class' Spacing '(' Spacing ')' Spacing StaticConstructorBody
               ClassFinalizer <- StaticConstructorModifiers Spacing '~' Spacing 'class' Spacing EmptyParameterList Spacing StaticConstructorBody

               StructConstructor <- StaticConstructorModifiers Spacing 'struct' Spacing '(' Spacing ')' Spacing StaticConstructorBody
               StructFinalizer <- StaticConstructorModifiers Spacing '~' Spacing 'struct' Spacing EmptyParameterList Spacing StaticConstructorBody

               Field <- Visibility? Spacing FieldModifiers Spacing Type Spacing identifier (Spacing ',' Spacing identifier)* Spacing ';'
               FieldModifiers <- 'const' / ('thread' / 'static')? Spacing 'final'?

               ModuleFunction <- RawFunction / ExternFunction / Function

               RawFunction <- Visibility? Spacing 'raw' Spacing ReturnType Spacing identifier Spacing ParameterList Spacing RawFunctionBody
               RawFunctionBody <- BlockStatement

               ExternFunction <- Visibility? Spacing 'extern' Spacing ExternFunctionLocation Spacing ReturnType Spacing identifier Spacing ParameterList Spacing ';'
               ExternFunctionLocation <- '(' Spacing String Spacing ',' Spacing String Spacing ')'

               Function <- Visibility? Spacing FunctionModifiers Spacing ReturnType Spacing identifier Spacing ParameterList Spacing FunctionBody
               FunctionModifiers <- ('pure' / 'impure')? Spacing ('safe' / 'unsafe')?
               FunctionBody <- BlockStatement

               EmptyParameterList <- '(' Spacing ')'
               ParameterList <- '(' Spacing (Parameter (Spacing ',' Spacing Parameter)*)? Spacing ')'
               Parameter <- ParameterModifiers Spacing Type Spacing identifier
               ParameterModifiers <- ('ref' / 'out' / 'params')?

               Operator <- Visibility? Spacing Type Spacing 'operator' Spacing CustomOperator Spacing ParameterList Spacing OperatorBody
               CustomOperator <- '+' / '-' / '*' / slash / '%' / '&' / '|' / '^'
               OperatorBody <- BlockStatement

               Property <- Visibility? Spacing PropertyModifiers Spacing Type Spacing identifier Spacing '{' Spacing PropertyBody Spacing '}'
               PropertyModifiers <- ('pure' / 'impure')? Spacing ('safe' / 'unsafe')? Spacing (('virtual' / 'abstract') / 'override'? Spacing 'final'?)
               PropertyBody <- PropertyGetter Spacing PropertySetter / PropertyGetter / PropertySetter
               PropertyGetter <- AccessorModifiers Spacing 'get' Spacing AccessorBody
               PropertySetter <- AccessorModifiers Spacing 'set' Spacing AccessorBody
               AccessorModifiers <- ('pure' / 'impure')? Spacing ('safe' / 'unsafe')?
               AccessorBody <- BlockStatement / ';'

               Method <- Visibility? Spacing MethodModifiers Spacing ReturnType Spacing identifier Spacing ParameterList Spacing MethodBody
               MethodModifiers <- ('pure' / 'impure')? Spacing ('safe' / 'unsafe')? Spacing (('virtual' / 'abstract') / 'override'? Spacing 'final'?)
               MethodBody <- BlockStatement / ';'

               Constructor <- Visibility? Spacing 'this' Spacing ParameterList Spacing FunctionBody
               Finalizer <- '~' Spacing 'this' Spacing EmptyParameterList Spacing FunctionBody

               BlockStatement <- '{' Spacing '}'

               ");
}
