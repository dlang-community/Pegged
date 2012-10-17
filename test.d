/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.grammar;

mixin(grammar(`
Parse:
        Line <- (Spaces (Keyword / Other / Number / String / Parens / Symbol) Spaces)*
        Other <~ [a-zA-Z_]+
        Number <~ (digit+ '.' digit*) / ('.' digit+) / digit+
        String < FullString / PartialString
        FullString <~ quote (!quote .)* quote
                    / backquote (!backquote .)* backquote
                    / doublequote (!doublequote .)* doublequote
        PartialString <~ quote (!quote .)*
                       / backquote (!backquote .)*
                       / doublequote (!doublequote .)*
        Symbol <- '~' / '!' / '@' / '#' / '$' / '%' / '^' / '&' / '*' / '/' /
                  '+' / '=' / '<' / '.' / '>' / ',' / ':' / ';' / backslash
        Parens <- '(' / ')' / '{' / '}' / '[' / ']'
        Spaces <~ (' ' / '\n' / '\t')*
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
`));


void main()
{
    writeln(Parse("finally 3.14 @@@ `Hello`"));
}