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
    assert(Numbers("123").successful);
    assert(Numbers("123.0").successful);
    assert(Numbers("123.01").successful);
    assert(Numbers("123.").successful);
    assert(Numbers("-123").successful);
    assert(Numbers("-123.0").successful);
    assert(Numbers("+123").successful);
    assert(Numbers("-123e+12").successful);
    assert(Numbers("+123E-12").successful);
    assert(Numbers("123.456e+00").successful);
    assert(Numbers.Hexa(ParseTree("",false,null,"DEADBEEF")).successful);
    assert(Numbers.Hexa(ParseTree("",false,null,"0123BEEF")).successful);
    assert(Numbers.Hexa(ParseTree("",false,null,"DEAD0123")).successful);

    assert(!Numbers("").successful);
    assert(!Numbers("abc").successful);
}
