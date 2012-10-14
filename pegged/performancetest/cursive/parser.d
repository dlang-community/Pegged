/**
This module was automatically generated from the following grammar:

Cursive:
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



*/
module pegged.performancetest.cursive.parser;

public import pegged.peg;
struct Cursive
{
    import std.typecons:Tuple, tuple;
    static ParseTree[Tuple!(string, uint)] memo;
    static bool[string] names;
    static this()
    {
        names = [`Program`:true, `LineComment`:true, `BlockComment`:true, `Boolean`:true,
                 `Null`:true, `IntegerSuffix`:true, `RealSuffix`:true, `Sign`:true,
                 `Decimal`:true, `Binary`:true, `Octal`:true, `Hexadecimal`:true,
                 `Size`:true, `Integer`:true, `Real`:true, `EscapeSequence`:true,
                 `UnicodeSequence`:true, `Character`:true, `String`:true, `Literal`:true,
                 `Qualifiedidentifier`:true, `FixedIntegralType`:true, `IntegralType`:true, `FloatingPointType`:true,
                 `CoreType`:true, `TypeReference`:true, `TypeSpecification`:true, `GenericParameters`:true,
                 `FunctionType`:true, `ParameterTypes`:true, `PointerType`:true, `ArrayType`:true,
                 `VectorType`:true, `NullableType`:true, `TupleType`:true, `TypeModifiers`:true,
                 `Type`:true, `InferredType`:true, `ReturnType`:true, `CallingConvention`:true,
                 `Module`:true, `Import`:true, `Declaration`:true, `Visibility`:true,
                 `InheritanceList`:true, `Class`:true, `ClassModifiers`:true, `ClassMember`:true,
                 `Interface`:true, `InterfaceMember`:true, `Struct`:true, `StructModifiers`:true,
                 `StructMember`:true, `Enum`:true, `EnumBase`:true, `EnumMember`:true,
                 `Data`:true, `DataSemantics`:true, `DataMember`:true, `DataField`:true,
                 `DataCase`:true, `StaticConstructorModifiers`:true, `StaticConstructorBody`:true, `ModuleConstructor`:true,
                 `ModuleFinalizer`:true, `ClassConstructor`:true, `ClassFinalizer`:true, `StructConstructor`:true,
                 `StructFinalizer`:true, `Field`:true, `FieldModifiers`:true, `ModuleFunction`:true,
                 `RawFunction`:true, `RawFunctionBody`:true, `ExternFunction`:true, `ExternFunctionLocation`:true,
                 `Function`:true, `FunctionModifiers`:true, `FunctionBody`:true, `EmptyParameterList`:true,
                 `ParameterList`:true, `Parameter`:true, `ParameterModifiers`:true, `Operator`:true,
                 `CustomOperator`:true, `OperatorBody`:true, `Property`:true, `PropertyModifiers`:true,
                 `PropertyBody`:true, `PropertyGetter`:true, `PropertySetter`:true, `AccessorModifiers`:true,
                 `AccessorBody`:true, `Method`:true, `MethodModifiers`:true, `MethodBody`:true,
                 `Constructor`:true, `Finalizer`:true, `BlockStatement`:true];
    }
    static ParseTree decimateTree(ParseTree p)
    {
        if(p.children.length == 0) return p;

        ParseTree[] filterChildren(ParseTree pt)
        {
            ParseTree[] result;
            foreach(child; pt.children)
            {
                if (child.name in names) // keep nodes that belongs to the current grammar
                {
                    child.children = filterChildren(child);
                    result ~= child;
                }
                else if (child.name == "keep") // 'keep' node are never discarded. They have only one child, the node to keep
                {
                    result ~= child.children[0];
                }
                else // discard this node, but see if its children contain nodes to keep
                {
                    result ~= filterChildren(child);
                }
            }
            return result;
        }
        p.children = filterChildren(p);
        return p;
    }

    alias spacing Spacing;

    static ParseTree Program(ParseTree p)
    {
        if(auto m = tuple("Program",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Spacing, Module, Spacing, zeroOrMore!(and!(Declaration, Spacing)), Spacing, eoi), "Program")(p);
            memo[tuple("Program",p.end)] = result;
            return result;
        }
    }

    static ParseTree Program(string s)
    {
        memo = null;
        return Program(ParseTree("", false,[], s));
    }

    static ParseTree LineComment(ParseTree p)
    {
        if(auto m = tuple("LineComment",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(discard!(and!(slash, slash, or!(and!(posLookahead!(or!(and!(endOfLine), and!(eoi))), or!(and!(endOfLine), and!(eoi))), and!(zeroOrMore!(pegged.peg.any)))))), "LineComment")(p);
            memo[tuple("LineComment",p.end)] = result;
            return result;
        }
    }

    static ParseTree LineComment(string s)
    {
        memo = null;
        return LineComment(ParseTree("", false,[], s));
    }

    static ParseTree BlockComment(ParseTree p)
    {
        if(auto m = tuple("BlockComment",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(discard!(and!(slash, literal!("*"), or!(and!(posLookahead!(and!(literal!("*"), slash)), literal!("*"), slash), and!(zeroOrMore!(pegged.peg.any)))))), "BlockComment")(p);
            memo[tuple("BlockComment",p.end)] = result;
            return result;
        }
    }

    static ParseTree BlockComment(string s)
    {
        memo = null;
        return BlockComment(ParseTree("", false,[], s));
    }

    static ParseTree Boolean(ParseTree p)
    {
        if(auto m = tuple("Boolean",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("true")), and!(literal!("false"))), "Boolean")(p);
            memo[tuple("Boolean",p.end)] = result;
            return result;
        }
    }

    static ParseTree Boolean(string s)
    {
        memo = null;
        return Boolean(ParseTree("", false,[], s));
    }

    static ParseTree Null(ParseTree p)
    {
        if(auto m = tuple("Null",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("null")), "Null")(p);
            memo[tuple("Null",p.end)] = result;
            return result;
        }
    }

    static ParseTree Null(string s)
    {
        memo = null;
        return Null(ParseTree("", false,[], s));
    }

    static ParseTree IntegerSuffix(ParseTree p)
    {
        if(auto m = tuple("IntegerSuffix",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(slash, or!(and!(literal!("i8")), and!(literal!("u8")), and!(literal!("i16")), and!(literal!("u16")), and!(literal!("i32")), and!(literal!("u32")), and!(literal!("i64")), and!(literal!("u64")))), "IntegerSuffix")(p);
            memo[tuple("IntegerSuffix",p.end)] = result;
            return result;
        }
    }

    static ParseTree IntegerSuffix(string s)
    {
        memo = null;
        return IntegerSuffix(ParseTree("", false,[], s));
    }

    static ParseTree RealSuffix(ParseTree p)
    {
        if(auto m = tuple("RealSuffix",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(slash, or!(and!(literal!("f32")), and!(literal!("f64")))), "RealSuffix")(p);
            memo[tuple("RealSuffix",p.end)] = result;
            return result;
        }
    }

    static ParseTree RealSuffix(string s)
    {
        memo = null;
        return RealSuffix(ParseTree("", false,[], s));
    }

    static ParseTree Sign(ParseTree p)
    {
        if(auto m = tuple("Sign",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("+")), and!(literal!("-"))), "Sign")(p);
            memo[tuple("Sign",p.end)] = result;
            return result;
        }
    }

    static ParseTree Sign(string s)
    {
        memo = null;
        return Sign(ParseTree("", false,[], s));
    }

    static ParseTree Decimal(ParseTree p)
    {
        if(auto m = tuple("Decimal",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(fuse!(and!(oneOrMore!(charRange!('0', '9')))), "Decimal")(p);
            memo[tuple("Decimal",p.end)] = result;
            return result;
        }
    }

    static ParseTree Decimal(string s)
    {
        memo = null;
        return Decimal(ParseTree("", false,[], s));
    }

    static ParseTree Binary(ParseTree p)
    {
        if(auto m = tuple("Binary",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(fuse!(and!(literal!("0"), or!(and!(literal!("b")), and!(literal!("B"))), oneOrMore!(charRange!('0', '1')))), "Binary")(p);
            memo[tuple("Binary",p.end)] = result;
            return result;
        }
    }

    static ParseTree Binary(string s)
    {
        memo = null;
        return Binary(ParseTree("", false,[], s));
    }

    static ParseTree Octal(ParseTree p)
    {
        if(auto m = tuple("Octal",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(fuse!(and!(literal!("0"), or!(and!(literal!("o")), and!(literal!("O"))), oneOrMore!(charRange!('0', '7')))), "Octal")(p);
            memo[tuple("Octal",p.end)] = result;
            return result;
        }
    }

    static ParseTree Octal(string s)
    {
        memo = null;
        return Octal(ParseTree("", false,[], s));
    }

    static ParseTree Hexadecimal(ParseTree p)
    {
        if(auto m = tuple("Hexadecimal",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(fuse!(and!(literal!("0"), or!(and!(literal!("x")), and!(literal!("X"))), oneOrMore!(or!(charRange!('0', '9'), charRange!('a', 'f'), charRange!('A', 'F'))))), "Hexadecimal")(p);
            memo[tuple("Hexadecimal",p.end)] = result;
            return result;
        }
    }

    static ParseTree Hexadecimal(string s)
    {
        memo = null;
        return Hexadecimal(ParseTree("", false,[], s));
    }

    static ParseTree Size(ParseTree p)
    {
        if(auto m = tuple("Size",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(Binary), and!(Octal), and!(Hexadecimal), and!(Decimal)), "Size")(p);
            memo[tuple("Size",p.end)] = result;
            return result;
        }
    }

    static ParseTree Size(string s)
    {
        memo = null;
        return Size(ParseTree("", false,[], s));
    }

    static ParseTree Integer(ParseTree p)
    {
        if(auto m = tuple("Integer",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Sign), or!(and!(Binary), and!(Octal), and!(Hexadecimal), and!(Decimal)), IntegerSuffix), "Integer")(p);
            memo[tuple("Integer",p.end)] = result;
            return result;
        }
    }

    static ParseTree Integer(string s)
    {
        memo = null;
        return Integer(ParseTree("", false,[], s));
    }

    static ParseTree Real(ParseTree p)
    {
        if(auto m = tuple("Real",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Sign), Decimal, literal!("."), Decimal, option!(and!(or!(and!(literal!("e")), and!(literal!("E"))), option!(Sign), Decimal)), RealSuffix), "Real")(p);
            memo[tuple("Real",p.end)] = result;
            return result;
        }
    }

    static ParseTree Real(string s)
    {
        memo = null;
        return Real(ParseTree("", false,[], s));
    }

    static ParseTree EscapeSequence(ParseTree p)
    {
        if(auto m = tuple("EscapeSequence",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(backslash, or!(and!(quote), and!(doublequote), and!(backslash), and!(literal!("a")), and!(literal!("b")), and!(literal!("f")), and!(literal!("n")), and!(literal!("r")), and!(literal!("t")), and!(literal!("v")), and!(literal!("0")))), "EscapeSequence")(p);
            memo[tuple("EscapeSequence",p.end)] = result;
            return result;
        }
    }

    static ParseTree EscapeSequence(string s)
    {
        memo = null;
        return EscapeSequence(ParseTree("", false,[], s));
    }

    static ParseTree UnicodeSequence(ParseTree p)
    {
        if(auto m = tuple("UnicodeSequence",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(backslash, or!(and!(literal!("u")), and!(literal!("U"))), option!(literal!("+")), oneOrMore!(or!(charRange!('0', '9'), charRange!('a', 'f'), charRange!('A', 'F')))), "UnicodeSequence")(p);
            memo[tuple("UnicodeSequence",p.end)] = result;
            return result;
        }
    }

    static ParseTree UnicodeSequence(string s)
    {
        memo = null;
        return UnicodeSequence(ParseTree("", false,[], s));
    }

    static ParseTree Character(ParseTree p)
    {
        if(auto m = tuple("Character",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(fuse!(and!(discard!(quote), or!(and!(EscapeSequence), and!(UnicodeSequence), and!(negLookahead!(quote), pegged.peg.any)), discard!(quote))), "Character")(p);
            memo[tuple("Character",p.end)] = result;
            return result;
        }
    }

    static ParseTree Character(string s)
    {
        memo = null;
        return Character(ParseTree("", false,[], s));
    }

    static ParseTree String(ParseTree p)
    {
        if(auto m = tuple("String",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(fuse!(and!(discard!(doublequote), zeroOrMore!(or!(and!(EscapeSequence), and!(UnicodeSequence), and!(negLookahead!(doublequote), pegged.peg.any))), discard!(doublequote))), "String")(p);
            memo[tuple("String",p.end)] = result;
            return result;
        }
    }

    static ParseTree String(string s)
    {
        memo = null;
        return String(ParseTree("", false,[], s));
    }

    static ParseTree Literal(ParseTree p)
    {
        if(auto m = tuple("Literal",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(Boolean), and!(Null), and!(Real), and!(Integer), and!(Character), and!(String)), "Literal")(p);
            memo[tuple("Literal",p.end)] = result;
            return result;
        }
    }

    static ParseTree Literal(string s)
    {
        memo = null;
        return Literal(ParseTree("", false,[], s));
    }

    static ParseTree Qualifiedidentifier(ParseTree p)
    {
        if(auto m = tuple("Qualifiedidentifier",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(identifier, zeroOrMore!(and!(Spacing, literal!("."), Spacing, identifier))), "Qualifiedidentifier")(p);
            memo[tuple("Qualifiedidentifier",p.end)] = result;
            return result;
        }
    }

    static ParseTree Qualifiedidentifier(string s)
    {
        memo = null;
        return Qualifiedidentifier(ParseTree("", false,[], s));
    }

    static ParseTree FixedIntegralType(ParseTree p)
    {
        if(auto m = tuple("FixedIntegralType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("int8")), and!(literal!("uint8")), and!(literal!("int16")), and!(literal!("uint16")), and!(literal!("int32")), and!(literal!("uint32")), and!(literal!("int64")), and!(literal!("uint64"))), "FixedIntegralType")(p);
            memo[tuple("FixedIntegralType",p.end)] = result;
            return result;
        }
    }

    static ParseTree FixedIntegralType(string s)
    {
        memo = null;
        return FixedIntegralType(ParseTree("", false,[], s));
    }

    static ParseTree IntegralType(ParseTree p)
    {
        if(auto m = tuple("IntegralType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(FixedIntegralType), and!(literal!("int")), and!(literal!("uint"))), "IntegralType")(p);
            memo[tuple("IntegralType",p.end)] = result;
            return result;
        }
    }

    static ParseTree IntegralType(string s)
    {
        memo = null;
        return IntegralType(ParseTree("", false,[], s));
    }

    static ParseTree FloatingPointType(ParseTree p)
    {
        if(auto m = tuple("FloatingPointType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("float32")), and!(literal!("float64"))), "FloatingPointType")(p);
            memo[tuple("FloatingPointType",p.end)] = result;
            return result;
        }
    }

    static ParseTree FloatingPointType(string s)
    {
        memo = null;
        return FloatingPointType(ParseTree("", false,[], s));
    }

    static ParseTree CoreType(ParseTree p)
    {
        if(auto m = tuple("CoreType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("object")), and!(literal!("bool")), and!(IntegralType), and!(FloatingPointType)), "CoreType")(p);
            memo[tuple("CoreType",p.end)] = result;
            return result;
        }
    }

    static ParseTree CoreType(string s)
    {
        memo = null;
        return CoreType(ParseTree("", false,[], s));
    }

    static ParseTree TypeReference(ParseTree p)
    {
        if(auto m = tuple("TypeReference",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Qualifiedidentifier, option!(and!(Spacing, GenericParameters))), "TypeReference")(p);
            memo[tuple("TypeReference",p.end)] = result;
            return result;
        }
    }

    static ParseTree TypeReference(string s)
    {
        memo = null;
        return TypeReference(ParseTree("", false,[], s));
    }

    static ParseTree TypeSpecification(ParseTree p)
    {
        if(auto m = tuple("TypeSpecification",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(or!(and!(CoreType), and!(TypeReference)), Spacing, option!(NullableType)), "TypeSpecification")(p);
            memo[tuple("TypeSpecification",p.end)] = result;
            return result;
        }
    }

    static ParseTree TypeSpecification(string s)
    {
        memo = null;
        return TypeSpecification(ParseTree("", false,[], s));
    }

    static ParseTree GenericParameters(ParseTree p)
    {
        if(auto m = tuple("GenericParameters",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("<"), Spacing, Type, zeroOrMore!(and!(Spacing, literal!(","), Spacing, Type)), Spacing, literal!(">")), "GenericParameters")(p);
            memo[tuple("GenericParameters",p.end)] = result;
            return result;
        }
    }

    static ParseTree GenericParameters(string s)
    {
        memo = null;
        return GenericParameters(ParseTree("", false,[], s));
    }

    static ParseTree FunctionType(ParseTree p)
    {
        if(auto m = tuple("FunctionType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(CallingConvention), Spacing, ParameterTypes, Spacing, option!(NullableType)), "FunctionType")(p);
            memo[tuple("FunctionType",p.end)] = result;
            return result;
        }
    }

    static ParseTree FunctionType(string s)
    {
        memo = null;
        return FunctionType(ParseTree("", false,[], s));
    }

    static ParseTree ParameterTypes(ParseTree p)
    {
        if(auto m = tuple("ParameterTypes",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("("), Spacing, option!(and!(Type, zeroOrMore!(and!(Spacing, literal!(","), Spacing, Type)))), literal!(")")), "ParameterTypes")(p);
            memo[tuple("ParameterTypes",p.end)] = result;
            return result;
        }
    }

    static ParseTree ParameterTypes(string s)
    {
        memo = null;
        return ParameterTypes(ParseTree("", false,[], s));
    }

    static ParseTree PointerType(ParseTree p)
    {
        if(auto m = tuple("PointerType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("*")), "PointerType")(p);
            memo[tuple("PointerType",p.end)] = result;
            return result;
        }
    }

    static ParseTree PointerType(string s)
    {
        memo = null;
        return PointerType(ParseTree("", false,[], s));
    }

    static ParseTree ArrayType(ParseTree p)
    {
        if(auto m = tuple("ArrayType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("["), Spacing, literal!("]")), "ArrayType")(p);
            memo[tuple("ArrayType",p.end)] = result;
            return result;
        }
    }

    static ParseTree ArrayType(string s)
    {
        memo = null;
        return ArrayType(ParseTree("", false,[], s));
    }

    static ParseTree VectorType(ParseTree p)
    {
        if(auto m = tuple("VectorType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("["), Spacing, Size, Spacing, literal!("]")), "VectorType")(p);
            memo[tuple("VectorType",p.end)] = result;
            return result;
        }
    }

    static ParseTree VectorType(string s)
    {
        memo = null;
        return VectorType(ParseTree("", false,[], s));
    }

    static ParseTree NullableType(ParseTree p)
    {
        if(auto m = tuple("NullableType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("?")), "NullableType")(p);
            memo[tuple("NullableType",p.end)] = result;
            return result;
        }
    }

    static ParseTree NullableType(string s)
    {
        memo = null;
        return NullableType(ParseTree("", false,[], s));
    }

    static ParseTree TupleType(ParseTree p)
    {
        if(auto m = tuple("TupleType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("{"), Spacing, Type, zeroOrMore!(and!(Spacing, literal!(","), Spacing, Type)), Spacing, literal!("}")), "TupleType")(p);
            memo[tuple("TupleType",p.end)] = result;
            return result;
        }
    }

    static ParseTree TupleType(string s)
    {
        memo = null;
        return TupleType(ParseTree("", false,[], s));
    }

    static ParseTree TypeModifiers(ParseTree p)
    {
        if(auto m = tuple("TypeModifiers",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(oneOrMore!(or!(and!(PointerType), and!(ArrayType), and!(VectorType))), Spacing, option!(NullableType)), "TypeModifiers")(p);
            memo[tuple("TypeModifiers",p.end)] = result;
            return result;
        }
    }

    static ParseTree TypeModifiers(string s)
    {
        memo = null;
        return TypeModifiers(ParseTree("", false,[], s));
    }

    static ParseTree Type(ParseTree p)
    {
        if(auto m = tuple("Type",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(TupleType), and!(TypeSpecification, option!(and!(Spacing, or!(and!(TypeModifiers), and!(FunctionType)))))), "Type")(p);
            memo[tuple("Type",p.end)] = result;
            return result;
        }
    }

    static ParseTree Type(string s)
    {
        memo = null;
        return Type(ParseTree("", false,[], s));
    }

    static ParseTree InferredType(ParseTree p)
    {
        if(auto m = tuple("InferredType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("var")), and!(Type)), "InferredType")(p);
            memo[tuple("InferredType",p.end)] = result;
            return result;
        }
    }

    static ParseTree InferredType(string s)
    {
        memo = null;
        return InferredType(ParseTree("", false,[], s));
    }

    static ParseTree ReturnType(ParseTree p)
    {
        if(auto m = tuple("ReturnType",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("void")), and!(Type)), "ReturnType")(p);
            memo[tuple("ReturnType",p.end)] = result;
            return result;
        }
    }

    static ParseTree ReturnType(string s)
    {
        memo = null;
        return ReturnType(ParseTree("", false,[], s));
    }

    static ParseTree CallingConvention(ParseTree p)
    {
        if(auto m = tuple("CallingConvention",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("cdecl")), and!(literal!("stdcall"))), "CallingConvention")(p);
            memo[tuple("CallingConvention",p.end)] = result;
            return result;
        }
    }

    static ParseTree CallingConvention(string s)
    {
        memo = null;
        return CallingConvention(ParseTree("", false,[], s));
    }

    static ParseTree Module(ParseTree p)
    {
        if(auto m = tuple("Module",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("module"), Spacing, Qualifiedidentifier, Spacing, literal!(";")), "Module")(p);
            memo[tuple("Module",p.end)] = result;
            return result;
        }
    }

    static ParseTree Module(string s)
    {
        memo = null;
        return Module(ParseTree("", false,[], s));
    }

    static ParseTree Import(ParseTree p)
    {
        if(auto m = tuple("Import",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("import"), Spacing, Qualifiedidentifier, zeroOrMore!(and!(Spacing, literal!(","), Spacing, Qualifiedidentifier)), literal!(";")), "Import")(p);
            memo[tuple("Import",p.end)] = result;
            return result;
        }
    }

    static ParseTree Import(string s)
    {
        memo = null;
        return Import(ParseTree("", false,[], s));
    }

    static ParseTree Declaration(ParseTree p)
    {
        if(auto m = tuple("Declaration",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(Import), and!(Class), and!(Interface), and!(Struct), and!(Enum), and!(Data), and!(ModuleConstructor), and!(ModuleFinalizer), and!(Field), and!(ModuleFunction), and!(Operator)), "Declaration")(p);
            memo[tuple("Declaration",p.end)] = result;
            return result;
        }
    }

    static ParseTree Declaration(string s)
    {
        memo = null;
        return Declaration(ParseTree("", false,[], s));
    }

    static ParseTree Visibility(ParseTree p)
    {
        if(auto m = tuple("Visibility",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("public")), and!(literal!("internal")), and!(literal!("private")), and!(literal!("protected"), option!(and!(or!(and!(literal!("and")), and!(literal!("or"))), literal!("internal"))))), "Visibility")(p);
            memo[tuple("Visibility",p.end)] = result;
            return result;
        }
    }

    static ParseTree Visibility(string s)
    {
        memo = null;
        return Visibility(ParseTree("", false,[], s));
    }

    static ParseTree InheritanceList(ParseTree p)
    {
        if(auto m = tuple("InheritanceList",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!(":"), Spacing, TypeReference, zeroOrMore!(and!(Spacing, literal!(","), Spacing, TypeReference))), "InheritanceList")(p);
            memo[tuple("InheritanceList",p.end)] = result;
            return result;
        }
    }

    static ParseTree InheritanceList(string s)
    {
        memo = null;
        return InheritanceList(ParseTree("", false,[], s));
    }

    static ParseTree Class(ParseTree p)
    {
        if(auto m = tuple("Class",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, ClassModifiers, Spacing, literal!("class"), Spacing, identifier, Spacing, option!(InheritanceList), literal!("{"), Spacing, zeroOrMore!(and!(ClassMember, Spacing)), Spacing, literal!("}")), "Class")(p);
            memo[tuple("Class",p.end)] = result;
            return result;
        }
    }

    static ParseTree Class(string s)
    {
        memo = null;
        return Class(ParseTree("", false,[], s));
    }

    static ParseTree ClassModifiers(ParseTree p)
    {
        if(auto m = tuple("ClassModifiers",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(or!(and!(literal!("open")), and!(literal!("abstract")))), Spacing, option!(or!(and!(literal!("pure")), and!(literal!("impure")))), Spacing, option!(or!(and!(literal!("safe")), and!(literal!("unsafe"))))), "ClassModifiers")(p);
            memo[tuple("ClassModifiers",p.end)] = result;
            return result;
        }
    }

    static ParseTree ClassModifiers(string s)
    {
        memo = null;
        return ClassModifiers(ParseTree("", false,[], s));
    }

    static ParseTree ClassMember(ParseTree p)
    {
        if(auto m = tuple("ClassMember",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(ClassConstructor), and!(ClassFinalizer), and!(Constructor), and!(Finalizer), and!(Field), and!(Property), and!(Method), and!(Operator)), "ClassMember")(p);
            memo[tuple("ClassMember",p.end)] = result;
            return result;
        }
    }

    static ParseTree ClassMember(string s)
    {
        memo = null;
        return ClassMember(ParseTree("", false,[], s));
    }

    static ParseTree Interface(ParseTree p)
    {
        if(auto m = tuple("Interface",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, literal!("interface"), Spacing, identifier, Spacing, option!(InheritanceList), Spacing, literal!("{"), Spacing, zeroOrMore!(and!(InterfaceMember, Spacing)), Spacing, literal!("}")), "Interface")(p);
            memo[tuple("Interface",p.end)] = result;
            return result;
        }
    }

    static ParseTree Interface(string s)
    {
        memo = null;
        return Interface(ParseTree("", false,[], s));
    }

    static ParseTree InterfaceMember(ParseTree p)
    {
        if(auto m = tuple("InterfaceMember",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(Property), and!(Method), and!(Operator)), "InterfaceMember")(p);
            memo[tuple("InterfaceMember",p.end)] = result;
            return result;
        }
    }

    static ParseTree InterfaceMember(string s)
    {
        memo = null;
        return InterfaceMember(ParseTree("", false,[], s));
    }

    static ParseTree Struct(ParseTree p)
    {
        if(auto m = tuple("Struct",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, StructModifiers, Spacing, literal!("struct"), Spacing, identifier, Spacing, option!(InheritanceList), Spacing, literal!("{"), Spacing, zeroOrMore!(and!(StructMember, Spacing)), Spacing, literal!("}")), "Struct")(p);
            memo[tuple("Struct",p.end)] = result;
            return result;
        }
    }

    static ParseTree Struct(string s)
    {
        memo = null;
        return Struct(ParseTree("", false,[], s));
    }

    static ParseTree StructModifiers(ParseTree p)
    {
        if(auto m = tuple("StructModifiers",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(or!(and!(literal!("pure")), and!(literal!("impure")))), Spacing, option!(or!(and!(literal!("safe")), and!(literal!("unsafe"))))), "StructModifiers")(p);
            memo[tuple("StructModifiers",p.end)] = result;
            return result;
        }
    }

    static ParseTree StructModifiers(string s)
    {
        memo = null;
        return StructModifiers(ParseTree("", false,[], s));
    }

    static ParseTree StructMember(ParseTree p)
    {
        if(auto m = tuple("StructMember",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(StructConstructor), and!(StructFinalizer), and!(Constructor), and!(Field), and!(Property), and!(Function), and!(Operator)), "StructMember")(p);
            memo[tuple("StructMember",p.end)] = result;
            return result;
        }
    }

    static ParseTree StructMember(string s)
    {
        memo = null;
        return StructMember(ParseTree("", false,[], s));
    }

    static ParseTree Enum(ParseTree p)
    {
        if(auto m = tuple("Enum",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, literal!("enum"), Spacing, identifier, Spacing, EnumBase, Spacing, literal!("{"), Spacing, zeroOrMore!(and!(EnumMember, Spacing)), Spacing, literal!("}")), "Enum")(p);
            memo[tuple("Enum",p.end)] = result;
            return result;
        }
    }

    static ParseTree Enum(string s)
    {
        memo = null;
        return Enum(ParseTree("", false,[], s));
    }

    static ParseTree EnumBase(ParseTree p)
    {
        if(auto m = tuple("EnumBase",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("of"), Spacing, FixedIntegralType), "EnumBase")(p);
            memo[tuple("EnumBase",p.end)] = result;
            return result;
        }
    }

    static ParseTree EnumBase(string s)
    {
        memo = null;
        return EnumBase(ParseTree("", false,[], s));
    }

    static ParseTree EnumMember(ParseTree p)
    {
        if(auto m = tuple("EnumMember",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(identifier, zeroOrMore!(and!(Spacing, literal!(","), Spacing, identifier)), Spacing, literal!("="), Spacing, Size, Spacing, literal!(";")), "EnumMember")(p);
            memo[tuple("EnumMember",p.end)] = result;
            return result;
        }
    }

    static ParseTree EnumMember(string s)
    {
        memo = null;
        return EnumMember(ParseTree("", false,[], s));
    }

    static ParseTree Data(ParseTree p)
    {
        if(auto m = tuple("Data",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, literal!("data"), Spacing, DataSemantics, Spacing, identifier, Spacing, literal!("{"), Spacing, zeroOrMore!(and!(DataMember, Spacing)), Spacing, literal!("}")), "Data")(p);
            memo[tuple("Data",p.end)] = result;
            return result;
        }
    }

    static ParseTree Data(string s)
    {
        memo = null;
        return Data(ParseTree("", false,[], s));
    }

    static ParseTree DataSemantics(ParseTree p)
    {
        if(auto m = tuple("DataSemantics",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("byref")), and!(literal!("byval"))), "DataSemantics")(p);
            memo[tuple("DataSemantics",p.end)] = result;
            return result;
        }
    }

    static ParseTree DataSemantics(string s)
    {
        memo = null;
        return DataSemantics(ParseTree("", false,[], s));
    }

    static ParseTree DataMember(ParseTree p)
    {
        if(auto m = tuple("DataMember",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(DataField), and!(DataCase), and!(Property), and!(Function), and!(Operator)), "DataMember")(p);
            memo[tuple("DataMember",p.end)] = result;
            return result;
        }
    }

    static ParseTree DataMember(string s)
    {
        memo = null;
        return DataMember(ParseTree("", false,[], s));
    }

    static ParseTree DataField(ParseTree p)
    {
        if(auto m = tuple("DataField",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(Type, Spacing, identifier, zeroOrMore!(and!(Spacing, literal!(","), Spacing, identifier)), Spacing, literal!(";")), "DataField")(p);
            memo[tuple("DataField",p.end)] = result;
            return result;
        }
    }

    static ParseTree DataField(string s)
    {
        memo = null;
        return DataField(ParseTree("", false,[], s));
    }

    static ParseTree DataCase(ParseTree p)
    {
        if(auto m = tuple("DataCase",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("case"), Spacing, identifier, literal!("{"), Spacing, zeroOrMore!(and!(DataField, Spacing)), Spacing, literal!("}")), "DataCase")(p);
            memo[tuple("DataCase",p.end)] = result;
            return result;
        }
    }

    static ParseTree DataCase(string s)
    {
        memo = null;
        return DataCase(ParseTree("", false,[], s));
    }

    static ParseTree StaticConstructorModifiers(ParseTree p)
    {
        if(auto m = tuple("StaticConstructorModifiers",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(literal!("thread")), Spacing, option!(or!(and!(literal!("pure")), and!(literal!("impure")))), Spacing, option!(or!(and!(literal!("safe")), and!(literal!("unsafe"))))), "StaticConstructorModifiers")(p);
            memo[tuple("StaticConstructorModifiers",p.end)] = result;
            return result;
        }
    }

    static ParseTree StaticConstructorModifiers(string s)
    {
        memo = null;
        return StaticConstructorModifiers(ParseTree("", false,[], s));
    }

    static ParseTree StaticConstructorBody(ParseTree p)
    {
        if(auto m = tuple("StaticConstructorBody",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(BlockStatement), "StaticConstructorBody")(p);
            memo[tuple("StaticConstructorBody",p.end)] = result;
            return result;
        }
    }

    static ParseTree StaticConstructorBody(string s)
    {
        memo = null;
        return StaticConstructorBody(ParseTree("", false,[], s));
    }

    static ParseTree ModuleConstructor(ParseTree p)
    {
        if(auto m = tuple("ModuleConstructor",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(StaticConstructorModifiers, Spacing, literal!("module"), Spacing, literal!("("), Spacing, literal!(")"), Spacing, StaticConstructorBody), "ModuleConstructor")(p);
            memo[tuple("ModuleConstructor",p.end)] = result;
            return result;
        }
    }

    static ParseTree ModuleConstructor(string s)
    {
        memo = null;
        return ModuleConstructor(ParseTree("", false,[], s));
    }

    static ParseTree ModuleFinalizer(ParseTree p)
    {
        if(auto m = tuple("ModuleFinalizer",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(StaticConstructorModifiers, Spacing, literal!("~"), Spacing, literal!("module"), Spacing, EmptyParameterList, Spacing, StaticConstructorBody), "ModuleFinalizer")(p);
            memo[tuple("ModuleFinalizer",p.end)] = result;
            return result;
        }
    }

    static ParseTree ModuleFinalizer(string s)
    {
        memo = null;
        return ModuleFinalizer(ParseTree("", false,[], s));
    }

    static ParseTree ClassConstructor(ParseTree p)
    {
        if(auto m = tuple("ClassConstructor",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(StaticConstructorModifiers, Spacing, literal!("class"), Spacing, literal!("("), Spacing, literal!(")"), Spacing, StaticConstructorBody), "ClassConstructor")(p);
            memo[tuple("ClassConstructor",p.end)] = result;
            return result;
        }
    }

    static ParseTree ClassConstructor(string s)
    {
        memo = null;
        return ClassConstructor(ParseTree("", false,[], s));
    }

    static ParseTree ClassFinalizer(ParseTree p)
    {
        if(auto m = tuple("ClassFinalizer",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(StaticConstructorModifiers, Spacing, literal!("~"), Spacing, literal!("class"), Spacing, EmptyParameterList, Spacing, StaticConstructorBody), "ClassFinalizer")(p);
            memo[tuple("ClassFinalizer",p.end)] = result;
            return result;
        }
    }

    static ParseTree ClassFinalizer(string s)
    {
        memo = null;
        return ClassFinalizer(ParseTree("", false,[], s));
    }

    static ParseTree StructConstructor(ParseTree p)
    {
        if(auto m = tuple("StructConstructor",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(StaticConstructorModifiers, Spacing, literal!("struct"), Spacing, literal!("("), Spacing, literal!(")"), Spacing, StaticConstructorBody), "StructConstructor")(p);
            memo[tuple("StructConstructor",p.end)] = result;
            return result;
        }
    }

    static ParseTree StructConstructor(string s)
    {
        memo = null;
        return StructConstructor(ParseTree("", false,[], s));
    }

    static ParseTree StructFinalizer(ParseTree p)
    {
        if(auto m = tuple("StructFinalizer",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(StaticConstructorModifiers, Spacing, literal!("~"), Spacing, literal!("struct"), Spacing, EmptyParameterList, Spacing, StaticConstructorBody), "StructFinalizer")(p);
            memo[tuple("StructFinalizer",p.end)] = result;
            return result;
        }
    }

    static ParseTree StructFinalizer(string s)
    {
        memo = null;
        return StructFinalizer(ParseTree("", false,[], s));
    }

    static ParseTree Field(ParseTree p)
    {
        if(auto m = tuple("Field",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, FieldModifiers, Spacing, Type, Spacing, identifier, zeroOrMore!(and!(Spacing, literal!(","), Spacing, identifier)), Spacing, literal!(";")), "Field")(p);
            memo[tuple("Field",p.end)] = result;
            return result;
        }
    }

    static ParseTree Field(string s)
    {
        memo = null;
        return Field(ParseTree("", false,[], s));
    }

    static ParseTree FieldModifiers(ParseTree p)
    {
        if(auto m = tuple("FieldModifiers",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("const")), and!(option!(or!(and!(literal!("thread")), and!(literal!("static")))), Spacing, option!(literal!("final")))), "FieldModifiers")(p);
            memo[tuple("FieldModifiers",p.end)] = result;
            return result;
        }
    }

    static ParseTree FieldModifiers(string s)
    {
        memo = null;
        return FieldModifiers(ParseTree("", false,[], s));
    }

    static ParseTree ModuleFunction(ParseTree p)
    {
        if(auto m = tuple("ModuleFunction",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(RawFunction), and!(ExternFunction), and!(Function)), "ModuleFunction")(p);
            memo[tuple("ModuleFunction",p.end)] = result;
            return result;
        }
    }

    static ParseTree ModuleFunction(string s)
    {
        memo = null;
        return ModuleFunction(ParseTree("", false,[], s));
    }

    static ParseTree RawFunction(ParseTree p)
    {
        if(auto m = tuple("RawFunction",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, literal!("raw"), Spacing, ReturnType, Spacing, identifier, Spacing, ParameterList, Spacing, RawFunctionBody), "RawFunction")(p);
            memo[tuple("RawFunction",p.end)] = result;
            return result;
        }
    }

    static ParseTree RawFunction(string s)
    {
        memo = null;
        return RawFunction(ParseTree("", false,[], s));
    }

    static ParseTree RawFunctionBody(ParseTree p)
    {
        if(auto m = tuple("RawFunctionBody",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(BlockStatement), "RawFunctionBody")(p);
            memo[tuple("RawFunctionBody",p.end)] = result;
            return result;
        }
    }

    static ParseTree RawFunctionBody(string s)
    {
        memo = null;
        return RawFunctionBody(ParseTree("", false,[], s));
    }

    static ParseTree ExternFunction(ParseTree p)
    {
        if(auto m = tuple("ExternFunction",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, literal!("extern"), Spacing, ExternFunctionLocation, Spacing, ReturnType, Spacing, identifier, Spacing, ParameterList, Spacing, literal!(";")), "ExternFunction")(p);
            memo[tuple("ExternFunction",p.end)] = result;
            return result;
        }
    }

    static ParseTree ExternFunction(string s)
    {
        memo = null;
        return ExternFunction(ParseTree("", false,[], s));
    }

    static ParseTree ExternFunctionLocation(ParseTree p)
    {
        if(auto m = tuple("ExternFunctionLocation",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("("), Spacing, String, Spacing, literal!(","), Spacing, String, Spacing, literal!(")")), "ExternFunctionLocation")(p);
            memo[tuple("ExternFunctionLocation",p.end)] = result;
            return result;
        }
    }

    static ParseTree ExternFunctionLocation(string s)
    {
        memo = null;
        return ExternFunctionLocation(ParseTree("", false,[], s));
    }

    static ParseTree Function(ParseTree p)
    {
        if(auto m = tuple("Function",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, FunctionModifiers, Spacing, ReturnType, Spacing, identifier, Spacing, ParameterList, Spacing, FunctionBody), "Function")(p);
            memo[tuple("Function",p.end)] = result;
            return result;
        }
    }

    static ParseTree Function(string s)
    {
        memo = null;
        return Function(ParseTree("", false,[], s));
    }

    static ParseTree FunctionModifiers(ParseTree p)
    {
        if(auto m = tuple("FunctionModifiers",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(or!(and!(literal!("pure")), and!(literal!("impure")))), Spacing, option!(or!(and!(literal!("safe")), and!(literal!("unsafe"))))), "FunctionModifiers")(p);
            memo[tuple("FunctionModifiers",p.end)] = result;
            return result;
        }
    }

    static ParseTree FunctionModifiers(string s)
    {
        memo = null;
        return FunctionModifiers(ParseTree("", false,[], s));
    }

    static ParseTree FunctionBody(ParseTree p)
    {
        if(auto m = tuple("FunctionBody",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(BlockStatement), "FunctionBody")(p);
            memo[tuple("FunctionBody",p.end)] = result;
            return result;
        }
    }

    static ParseTree FunctionBody(string s)
    {
        memo = null;
        return FunctionBody(ParseTree("", false,[], s));
    }

    static ParseTree EmptyParameterList(ParseTree p)
    {
        if(auto m = tuple("EmptyParameterList",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("("), Spacing, literal!(")")), "EmptyParameterList")(p);
            memo[tuple("EmptyParameterList",p.end)] = result;
            return result;
        }
    }

    static ParseTree EmptyParameterList(string s)
    {
        memo = null;
        return EmptyParameterList(ParseTree("", false,[], s));
    }

    static ParseTree ParameterList(ParseTree p)
    {
        if(auto m = tuple("ParameterList",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("("), Spacing, option!(and!(Parameter, zeroOrMore!(and!(Spacing, literal!(","), Spacing, Parameter)))), Spacing, literal!(")")), "ParameterList")(p);
            memo[tuple("ParameterList",p.end)] = result;
            return result;
        }
    }

    static ParseTree ParameterList(string s)
    {
        memo = null;
        return ParameterList(ParseTree("", false,[], s));
    }

    static ParseTree Parameter(ParseTree p)
    {
        if(auto m = tuple("Parameter",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(ParameterModifiers, Spacing, Type, Spacing, identifier), "Parameter")(p);
            memo[tuple("Parameter",p.end)] = result;
            return result;
        }
    }

    static ParseTree Parameter(string s)
    {
        memo = null;
        return Parameter(ParseTree("", false,[], s));
    }

    static ParseTree ParameterModifiers(ParseTree p)
    {
        if(auto m = tuple("ParameterModifiers",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(or!(and!(literal!("ref")), and!(literal!("out")), and!(literal!("params"))))), "ParameterModifiers")(p);
            memo[tuple("ParameterModifiers",p.end)] = result;
            return result;
        }
    }

    static ParseTree ParameterModifiers(string s)
    {
        memo = null;
        return ParameterModifiers(ParseTree("", false,[], s));
    }

    static ParseTree Operator(ParseTree p)
    {
        if(auto m = tuple("Operator",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, Type, Spacing, literal!("operator"), Spacing, CustomOperator, Spacing, ParameterList, Spacing, OperatorBody), "Operator")(p);
            memo[tuple("Operator",p.end)] = result;
            return result;
        }
    }

    static ParseTree Operator(string s)
    {
        memo = null;
        return Operator(ParseTree("", false,[], s));
    }

    static ParseTree CustomOperator(ParseTree p)
    {
        if(auto m = tuple("CustomOperator",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(literal!("+")), and!(literal!("-")), and!(literal!("*")), and!(slash), and!(literal!("%")), and!(literal!("&")), and!(literal!("|")), and!(literal!("^"))), "CustomOperator")(p);
            memo[tuple("CustomOperator",p.end)] = result;
            return result;
        }
    }

    static ParseTree CustomOperator(string s)
    {
        memo = null;
        return CustomOperator(ParseTree("", false,[], s));
    }

    static ParseTree OperatorBody(ParseTree p)
    {
        if(auto m = tuple("OperatorBody",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(BlockStatement), "OperatorBody")(p);
            memo[tuple("OperatorBody",p.end)] = result;
            return result;
        }
    }

    static ParseTree OperatorBody(string s)
    {
        memo = null;
        return OperatorBody(ParseTree("", false,[], s));
    }

    static ParseTree Property(ParseTree p)
    {
        if(auto m = tuple("Property",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, PropertyModifiers, Spacing, Type, Spacing, identifier, Spacing, literal!("{"), Spacing, PropertyBody, Spacing, literal!("}")), "Property")(p);
            memo[tuple("Property",p.end)] = result;
            return result;
        }
    }

    static ParseTree Property(string s)
    {
        memo = null;
        return Property(ParseTree("", false,[], s));
    }

    static ParseTree PropertyModifiers(ParseTree p)
    {
        if(auto m = tuple("PropertyModifiers",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(or!(and!(literal!("pure")), and!(literal!("impure")))), Spacing, option!(or!(and!(literal!("safe")), and!(literal!("unsafe")))), Spacing, or!(and!(or!(and!(literal!("virtual")), and!(literal!("abstract")))), and!(option!(literal!("override")), Spacing, option!(literal!("final"))))), "PropertyModifiers")(p);
            memo[tuple("PropertyModifiers",p.end)] = result;
            return result;
        }
    }

    static ParseTree PropertyModifiers(string s)
    {
        memo = null;
        return PropertyModifiers(ParseTree("", false,[], s));
    }

    static ParseTree PropertyBody(ParseTree p)
    {
        if(auto m = tuple("PropertyBody",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(PropertyGetter, Spacing, PropertySetter), and!(PropertyGetter), and!(PropertySetter)), "PropertyBody")(p);
            memo[tuple("PropertyBody",p.end)] = result;
            return result;
        }
    }

    static ParseTree PropertyBody(string s)
    {
        memo = null;
        return PropertyBody(ParseTree("", false,[], s));
    }

    static ParseTree PropertyGetter(ParseTree p)
    {
        if(auto m = tuple("PropertyGetter",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(AccessorModifiers, Spacing, literal!("get"), Spacing, AccessorBody), "PropertyGetter")(p);
            memo[tuple("PropertyGetter",p.end)] = result;
            return result;
        }
    }

    static ParseTree PropertyGetter(string s)
    {
        memo = null;
        return PropertyGetter(ParseTree("", false,[], s));
    }

    static ParseTree PropertySetter(ParseTree p)
    {
        if(auto m = tuple("PropertySetter",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(AccessorModifiers, Spacing, literal!("set"), Spacing, AccessorBody), "PropertySetter")(p);
            memo[tuple("PropertySetter",p.end)] = result;
            return result;
        }
    }

    static ParseTree PropertySetter(string s)
    {
        memo = null;
        return PropertySetter(ParseTree("", false,[], s));
    }

    static ParseTree AccessorModifiers(ParseTree p)
    {
        if(auto m = tuple("AccessorModifiers",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(or!(and!(literal!("pure")), and!(literal!("impure")))), Spacing, option!(or!(and!(literal!("safe")), and!(literal!("unsafe"))))), "AccessorModifiers")(p);
            memo[tuple("AccessorModifiers",p.end)] = result;
            return result;
        }
    }

    static ParseTree AccessorModifiers(string s)
    {
        memo = null;
        return AccessorModifiers(ParseTree("", false,[], s));
    }

    static ParseTree AccessorBody(ParseTree p)
    {
        if(auto m = tuple("AccessorBody",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(BlockStatement), and!(literal!(";"))), "AccessorBody")(p);
            memo[tuple("AccessorBody",p.end)] = result;
            return result;
        }
    }

    static ParseTree AccessorBody(string s)
    {
        memo = null;
        return AccessorBody(ParseTree("", false,[], s));
    }

    static ParseTree Method(ParseTree p)
    {
        if(auto m = tuple("Method",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, MethodModifiers, Spacing, ReturnType, Spacing, identifier, Spacing, ParameterList, Spacing, MethodBody), "Method")(p);
            memo[tuple("Method",p.end)] = result;
            return result;
        }
    }

    static ParseTree Method(string s)
    {
        memo = null;
        return Method(ParseTree("", false,[], s));
    }

    static ParseTree MethodModifiers(ParseTree p)
    {
        if(auto m = tuple("MethodModifiers",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(or!(and!(literal!("pure")), and!(literal!("impure")))), Spacing, option!(or!(and!(literal!("safe")), and!(literal!("unsafe")))), Spacing, or!(and!(or!(and!(literal!("virtual")), and!(literal!("abstract")))), and!(option!(literal!("override")), Spacing, option!(literal!("final"))))), "MethodModifiers")(p);
            memo[tuple("MethodModifiers",p.end)] = result;
            return result;
        }
    }

    static ParseTree MethodModifiers(string s)
    {
        memo = null;
        return MethodModifiers(ParseTree("", false,[], s));
    }

    static ParseTree MethodBody(ParseTree p)
    {
        if(auto m = tuple("MethodBody",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(or!(and!(BlockStatement), and!(literal!(";"))), "MethodBody")(p);
            memo[tuple("MethodBody",p.end)] = result;
            return result;
        }
    }

    static ParseTree MethodBody(string s)
    {
        memo = null;
        return MethodBody(ParseTree("", false,[], s));
    }

    static ParseTree Constructor(ParseTree p)
    {
        if(auto m = tuple("Constructor",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(option!(Visibility), Spacing, literal!("this"), Spacing, ParameterList, Spacing, FunctionBody), "Constructor")(p);
            memo[tuple("Constructor",p.end)] = result;
            return result;
        }
    }

    static ParseTree Constructor(string s)
    {
        memo = null;
        return Constructor(ParseTree("", false,[], s));
    }

    static ParseTree Finalizer(ParseTree p)
    {
        if(auto m = tuple("Finalizer",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("~"), Spacing, literal!("this"), Spacing, EmptyParameterList, Spacing, FunctionBody), "Finalizer")(p);
            memo[tuple("Finalizer",p.end)] = result;
            return result;
        }
    }

    static ParseTree Finalizer(string s)
    {
        memo = null;
        return Finalizer(ParseTree("", false,[], s));
    }

    static ParseTree BlockStatement(ParseTree p)
    {
        if(auto m = tuple("BlockStatement",p.end) in memo)
            return *m;
        else
        {
            ParseTree result = named!(and!(literal!("{"), Spacing, literal!("}")), "BlockStatement")(p);
            memo[tuple("BlockStatement",p.end)] = result;
            return result;
        }
    }

    static ParseTree BlockStatement(string s)
    {
        memo = null;
        return BlockStatement(ParseTree("", false,[], s));
    }

    static ParseTree opCall(ParseTree p)
    {
        ParseTree result = decimateTree(Program(p));
        result.children = [result];
        result.name = "Cursive";
        return result;
    }

    static ParseTree opCall(string input)
    {
        memo = null;
        return Cursive(ParseTree(``, false, [], input, 0, 0));
    }
}

