module pegged.examples.dgrammar;

import pegged.grammar;

mixin Grammar!( "Hex <- [0-9a-fA-F]");

mixin Grammar!( "String <- :DoubleQuote ~(Char*) :DoubleQuote"
              , "Char   <~ BackSlash DoubleQuote 
                         / BackSlash BackSlash 
                         / BackSlash [bfnrt] 
                         / BackSlash 'u' Hex Hex Hex Hex
                         / !DoubleQuote _");

mixin Grammar!( "Unsigned   <~ Digit+"
              , "Digit      <- [0-9]");

mixin Grammar!( "Integer <~ Sign? Unsigned"
              , "Sign    <- ('-' / '+')");

mixin Grammar!( "FloatingPoint <~ Floating ( ('e' / 'E' ) Integer )?"
              , "Floating      <~ Integer '.' Unsigned");

mixin Grammar!( `Literal <- "true" 
                          / "false" 
                          / "null"
                          / "__FILE__"
                          / "__LINE__"`);

mixin Grammar!(`BuiltInType   <- "void"
                                / "bool"
                                / "byte" / "short" / "int" / "long"
                                / "ubyte" / "ushort" / "uint" / "ulong"
                                / "float" / "double" / "real"
                                / "char" / "dchar" / "wchar"
                                / "string" / "dstring" / "wstring"
                                / "size_t" / "ptrdiff_t"`);

mixin Grammar!( `Type             <- SimpleType (DynamicArray / StaticArray / Slice / AssociativeArray / Pointer)*`
              , `SimpleType       <- TemplateInstantiation
                                   / BuiltInType
                                   / UserType`
              , `DynamicArray     <- "[]"`
              , `StaticArray      <~ "[" Integer (Spacing "," Spacing Integer)* "]"`
              , `Slice            <~ "[" Integer ".." (Integer / "$") "]"` // not exact: [$-1..$] is a legal slice
              , `AssociativeArray <- "[" KeyType "]"`
              , `KeyType          <- Type`
              , `Pointer          <- "*"`
              , `TemplateInstantiation <- TemplateName "!" TemplateArgumentsList`
              , `TemplateName     <- QualifiedIdentifier`
              , `TemplateArgumentsList <- "(" (Spacing TemplateArgument Spacing (',' Spacing TemplateArgument Spacing )*)? ")"
                                        / TemplateArgument`
              , `TemplateArgument <- Type
                                   / StringLiteral
                                   / FloatingPointLiteral
                                   / IntegerLiteral
                                   / SpecialLiteral
                                   / AliasName` // aliases: Foo!(bar)
              , `StringLiteral <- String`
              , `FloatingPointLiteral <- FloatingPoint`
              , `IntegerLiteral <- Integer`
              , `SpecialLiteral <- Literal`
              , `AliasName <- QualifiedIdentifier`
              , `UserType <- QualifiedIdentifier`
              , `TemplateDeclaration <- TemplateName TemplateParametersList`
              , `TemplateParametersList <- "(" (Spacing TemplateParameter Spacing (',' Spacing TemplateParameter Spacing )*)? ")"`
              , `TemplateParameter <- AliasParameter
                                    / TypeParameter
                                    / TupleParameter
                                    / UserType TemplateTypeSpecialization?`
              , `AliasParameter <- "alias" Spacing ParameterName`
              , `TypeParameter <- BuiltInType Spacing ParameterName`
              , `ParameterName <- QualifiedIdentifier`
              , `TupleParameter <- UserType TupleDots`
              , `TupleDots <- "..."`
              , `TemplateTypeSpecialization <- ((Spacing ":" Spacing Type) / (Spacing "=" Spacing Type))?`);

ParseResult fixType(ParseResult p)
{
    ParseResult[] newChildren;
    ParseResult   root;
    foreach(child; p.children)
        newChildren ~= fixType(child);
    p.children = newChildren;
    
    if (p.name == "Type" && p.children.length >= 2)
    {
        root = newChildren[0];
        foreach(child; newChildren[1..$])
        {
            child.children = root ~ child.children;
            child.capture = root.capture ~ child.capture;
            root = child;
        }
        p.children = [root];
    }
    
    return p;
}
