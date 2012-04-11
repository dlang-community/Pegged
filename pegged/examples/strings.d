module pegged.examples.strings;

import pegged.grammar;

mixin(grammar(`
# Example of a rule for double-quoted strings

    String <~ DoubleQuote (!DoubleQuote Char)* DoubleQuote
    
    Char   <~ BackSlash ( DoubleQuote  # '\' Escapes
                        / Quote
                        / BackSlash 
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
    assert(String.parse(`"Hello, World!"`).success);
    assert(String.parse(`"Hello, 
                        
                        World!"`).success);
    assert(String.parse(`""`).success);
    assert(String.parse(`"\'\""`).success);
    assert(String.parse(`"\\"`).success);
    assert(String.parse(`"\n\t\r"`).success);
    assert(String.parse(`"\60\61\7\111"`).success);
    assert(String.parse(`"\x40\x41"`).success);
    assert(String.parse(`"\u00A0\u00FF"`).success);
    assert(String.parse(`"\U000000A0\U000000B2"`).success);
}