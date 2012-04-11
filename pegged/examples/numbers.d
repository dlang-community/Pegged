module pegged.examples.numbers;

import pegged.grammar;

/// Numbers
mixin(grammar(`
Numbers:
    Scientific <~ Floating ( ('e' / 'E' ) Integer )?
    Floating   <~ Integer ('.' Unsigned )?
    Unsigned   <~ [0-9]+
    Integer    <~ Sign? Unsigned
    Hexa       <~ [0-9a-fA-F]+ 
    Sign       <- '-' / '+'
`));

unittest
{
    assert(Numbers.parse("123").success);
    assert(Numbers.parse("123.0").success);
    assert(Numbers.parse("123.01").success);
    assert(Numbers.parse("123.").success);
    assert(Numbers.parse("-123").success);
    assert(Numbers.parse("-123.0").success);
    assert(Numbers.parse("+123").success);
    assert(Numbers.parse("-123e+12").success);
    assert(Numbers.parse("+123E-12").success);
    assert(Numbers.parse("123.456e+00").success);
    assert(Numbers.Hexa.parse("DEADBEEF").success);
    assert(Numbers.Hexa.parse("0123BEEF").success);
    assert(Numbers.Hexa.parse("DEAD0123").success);
    
    assert(!Numbers.parse("").success);
    assert(!Numbers.parse("abc").success);
}
