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
        "0", "0.0", "0.01",
        "-0", "+0", "-0.0", "+0.0",
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
        "0123456789",
        "123", "123.0", "123.01", "-123", "-123.0", "-123.01",
        "-123e+12", "-123e-12", "+123e+12", "+123e-12", "-123E+12", "-123E-12", "+123E+12", "+123E-12",
        "123.456e+00", "123.456E+00", "123.456e-00", "123.456E-00",
        "-123.456e+00", "-123.456E+00", "-123.456e-00", "-123.456E-00",
        "+123.456e+00", "+123.456E+00", "+123.456e-00", "+123.456E-00"
    ];

    foreach(i,number; testNumbers)
    {
        assert(Numbers(number).matches == [number]); // Shall parse
    }

    // Failures
    testNumbers =
    [
        ".", ".0", "0.", "123..456",
        "", "abc", "+", "-", "+.", "-.",
        "--1", "++1", "+-1", "-+1",
        "1e", "1e+", "1e-","1ee"
    ];

    foreach(i,number; testNumbers)
    {
        assert(Numbers(number).matches != [number]); // None shall parse
    }

    // Hexadecimal numbers
    testNumbers =
    [
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
        "A", "B", "C", "D", "E", "F",
        "a", "b", "c", "d", "e", "f",
        "0123456789ABCDEF",
        "0123456789abcdef",
        "DEADBEEF", "0123BEEF", "DEAD0123",
        "deadbeef", "0123beef", "dead0123",
        "123E", "123e"
    ];

    foreach(number; testNumbers)
    {
        assert(Numbers.decimateTree(Numbers.Hexa(number)).matches == [number]); // Shall parse
    }

    // Hexadecimal failures
    testNumbers =
    [
        "", "G", "g", "-1", "123.456", "123e+100"
    ];

    foreach(number; testNumbers)
    {
        assert(Numbers.decimateTree(Numbers.Hexa(number)).matches != [number]); // None shall parse
    }
}
