module pegged.examples.strings;

import pegged.grammar;

mixin(grammar(`
String:
# Example of a rule for double-quoted strings

    String <~ doublequote (!doublequote Char)* doublequote

    Char   <~ backslash ( doublequote  # '\' Escapes
                        / quote
                        / backslash
                        / [bfnrt]
                        / [0-2][0-7][0-7]
                        / [0-7][0-7]?
                        / 'x' Hex Hex
                        / 'u' Hex Hex Hex Hex
                        / 'U' Hex Hex Hex Hex Hex Hex Hex Hex
                        )
             / . # Or any char, really

    Hex     <- [0-9a-fA-F]
`));

unittest
{
    assert(String(`"Hello, World!"`).successful);
    assert(String(`"Hello,

                        World!"`).successful);
    assert(String(`""`).successful);
    assert(String(`"\'\""`).successful);
    assert(String(`"\\"`).successful);
    assert(String(`"\n\t\r"`).successful);
    assert(String(`"\60\61\7\111"`).successful);
    assert(String(`"\x40\x41"`).successful);
    assert(String(`"\u00A0\u00FF"`).successful);
    assert(String(`"\U000000A0\U000000B2"`).successful);

    // Failures
    assert(!String(`"Hello, World!`).successful);
    assert(!String(`Hello, World!"`).successful);
    assert(!String(`Hello, World!`).successful);
}
