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
    string[] testNumbers =
    [
        "0", ".0", "0.", "0.0", ".01",
        "-0", "+0", "-0.", "+0.", "-0.0", "+0.0", "-.0",  "+.0",
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
        "0123456789",
        "123", "123.", "123.0", "123.01", "-123", "-123.", "-123.0", "-123.01",
        "-123e+12", "-123e-12", "+123e+12", "+123e-12", "-123E+12", "-123E-12", "+123E+12", "+123E-12",
        "123.456e+00", "123.456E+00", "123.456e-00", "123.456E-00",
        "-123.456e+00", "-123.456E+00", "-123.456e-00", "-123.456E-00",
        "+123.456e+00", "+123.456E+00", "+123.456e-00", "+123.456E-00"
    ];

    foreach(number; testNumbers)
    {
        assert(Numbers(number).matches == [number]); // Everything parsed
    }

    // Failures
    assert(!Numbers("").successful);
    assert(!Numbers("abc").successful);
    assert(!Numbers("+").successful);
    assert(!Numbers("-").successful);
    assert(!Numbers("+.").successful);
    assert(!Numbers("-.").successful);
    assert(!Numbers(".").successful);
    assert(!Numbers("--1").successful);
    assert(!Numbers("++1").successful);
    assert(!Numbers("+-1").successful);
    assert(!Numbers("-+1").successful);
    assert(!Numbers("1e").successful);
    assert(!Numbers("1e+").successful);
    assert(!Numbers("1e-").successful);
    assert(!Numbers("1ee").successful);

    // Hexadecimal numbers
    testNumbers =
    [
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F",
        "0123456789ABCDEF",
        "DEADBEEF", "0123BEEF", "DEAD0123"
    ];

    foreach(number; testNumbers)
    {
        assert(Numbers(number).matches == [number]); // Everything parsed
    }

    // Hexadecimal failures
    assert(!Numbers.Hexa("G").successful);
    assert(!Numbers.Hexa("-1").successful);

}
