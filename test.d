/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
//import std.file;
//import std.path;
import std.range;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.grammar;
import pegged.parser;
import pegged.introspection;

/// TODO: modify the way generic rules are written, again.
/// Caution: memoization code
void main()
{
    enum G =`
    Gram:
        Keywords <- "abstract" / "alias" / "align" / "asm" / "assert" / "auto" / "body" / "bool" / "break" / "byte" 
         / "case" / "cast" / "catch" / "cdouble" / "cent" / "cfloat" / "char" / "class" / "const" / "continue" / "creal" / "dchar" 
         / "debug" / "default" / "delegate" / "delete" / "deprecated" / "do"/ "double" / "else" / "enum" / "export" / "extern" 
         / "false" / "final"/ "finally" / "float" / "for"/ "foreach"/ "foreach_reverse" / "function" / "goto" / "idouble" / "if" 
         / "ifloat" / "immutable" / "import" / "in"/ "inout" / "int"/ "interface" / "invariant"/ "ireal" / "is" / "lazy" 
         / "long" / "macro" / "mixin" / "module" / "new" / "nothrow" / "null" / "out" / "override" / "package" / "pragma" 
         / "private" / "protected" / "public" / "pure" / "real" / "ref" / "return" / "scope" / "shared" / "short" / "static" 
         / "struct" / "super" / "switch" / "synchronized" / "template" / "this" / "throw" / "true" / "try" / "typedef" / "typeid" 
         / "typeof" / "ubyte" / "ucent" / "uint" / "ulong" / "union" / "unittest" / "ushort" / "version" / "void" / "volatile" 
         / "wchar" / "while" / "with" / "__FILE__" / "__LINE__" / "__gshared" / "__thread" / "__traits" 
         / . # dot to kill the keywords-specific code 
    `;
    
    enum G2 =`
    Gram2:
        Keywords <- "abstract" / "alias" / "align" / "asm" / "assert" / "auto" / "body" / "bool" / "break" / "byte" 
         / "case" / "cast" / "catch" / "cdouble" / "cent" / "cfloat" / "char" / "class" / "const" / "continue" / "creal" / "dchar" 
         / "debug" / "default" / "delegate" / "delete" / "deprecated" / "do"/ "double" / "else" / "enum" / "export" / "extern" 
         / "false" / "final"/ "finally" / "float" / "for"/ "foreach"/ "foreach_reverse" / "function" / "goto" / "idouble" / "if" 
         / "ifloat" / "immutable" / "import" / "in"/ "inout" / "int"/ "interface" / "invariant"/ "ireal" / "is" / "lazy" 
         / "long" / "macro" / "mixin" / "module" / "new" / "nothrow" / "null" / "out" / "override" / "package" / "pragma" 
         / "private" / "protected" / "public" / "pure" / "real" / "ref" / "return" / "scope" / "shared" / "short" / "static" 
         / "struct" / "super" / "switch" / "synchronized" / "template" / "this" / "throw" / "true" / "try" / "typedef" / "typeid" 
         / "typeof" / "ubyte" / "ucent" / "uint" / "ulong" / "union" / "unittest" / "ushort" / "version" / "void" / "volatile" 
         / "wchar" / "while" / "with" / "__FILE__" / "__LINE__" / "__gshared" / "__thread" / "__traits" 
    `;

    mixin(grammar(G));
    mixin(grammar(G2));
         
         
    auto b = benchmark!(()=>Gram("Hello"),()=>Gram("abstract"),()=>Gram("__traits"),
                        ()=>Gram2("Hello"),()=>Gram2("abstract"),()=>Gram2("__traits"))(10_000);
    float b0 = b[0].to!("msecs",float);
    float b1 = b[1].to!("msecs",float);
    float b2 = b[2].to!("msecs",float);
    float b3 = b[3].to!("msecs",float);
    float b4 = b[4].to!("msecs",float);
    float b5 = b[5].to!("msecs",float);
    auto ratio1 = 100*(b0/b1-1);
    auto ratio2 = 100*(b1/b4-1);
    auto ratio3 = 100*(b2/b5-1);
    
    writeln("On failure: ", ratio1, "% speedup.");
    writeln("On match, first case: ",ratio2, "% speedup.");
    writeln("On match, last case: ", ratio3, "% speedup.");
    /+
    writeln(keywordCode(["abstract" , "alias" , "align" , "asm" , "assert" , "auto" , "body" , "bool" , "break" , "byte" 
         , "case" , "cast" , "catch" , "cdouble" , "cent" , "cfloat" , "char" , "class" , "const" , "continue" , "creal" , "dchar" 
         , "debug" , "default" , "delegate" , "delete" , "deprecated" , "do", "double" , "else" , "enum" , "export" , "extern" 
         , "false" , "final", "finally" , "float" , "for", "foreach", "foreach_reverse" , "function" , "goto" , "idouble" , "if" 
         , "ifloat" , "immutable" , "import" , "in", "inout" , "int", "interface" , "invariant", "ireal" , "is" , "lazy" 
         , "long" , "macro" , "mixin" , "module" , "new" , "nothrow" , "null" , "out" , "override" , "package" , "pragma" 
         , "private" , "protected" , "public" , "pure" , "real" , "ref" , "return" , "scope" , "shared" , "short" , "static" 
         , "struct" , "super" , "switch" , "synchronized" , "template" , "this" , "throw" , "true" , "try" , "typedef" , "typeid" 
         , "typeof" , "ubyte" , "ucent" , "uint" , "ulong" , "union" , "unittest" , "ushort" , "version" , "void" , "volatile" 
         , "wchar" , "while" , "with" , "__FILE__" , "__LINE__" , "__gshared" , "__thread" , "__traits"]));
         +/
    
    
}