/**
This module contains a example grammar rules to parse different kind of numbers literals.
*/
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
    Binary     <~ "0b" [01] [01_]*
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

    foreach(number; testNumbers)
    {
        const parseTree = Numbers(number);
        auto match = parseTree.matches;
        assert(parseTree.successful, "Expected to parse successfully number " ~ number);
        assert(match == [number], "Expected " ~ number ~ " but was " ~ match[0]); // Shall parse
    }

    // Failures
    testNumbers =
    [
        ".", ".0", "0.", "123..456",
        "", "abc", "+", "-", "+.", "-.",
        "--1", "++1", "+-1", "-+1",
        "1e", "1e+", "1e-","1ee"
    ];

    foreach(number; testNumbers)
    {
        assert(Numbers(number).matches != [number], "Number \"" ~ number ~ "\" musn't be parsed"); // None shall parse
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
        const parseTree = Numbers.decimateTree(Numbers.Hexa(number));
        auto match = parseTree.matches;
        assert(parseTree.successful, "Expected to parse successfully number " ~ number);
        assert(match == [number], "Expected " ~ number ~ " but was " ~ match[0]); // Shall parse
    }

    // Hexadecimal failures
    testNumbers =
    [
        "", "G", "g", "-1", "123.456", "123e+100"
    ];

    foreach(number; testNumbers)
    {
        assert(Numbers.decimateTree(Numbers.Hexa(number)).matches != [number],
                "Number \"" ~ number ~ "\" musn't be parsed"); // None shall parse
    }

    // Binary numbers
    testNumbers =
    [
        "0b0", "0b1", "0b0000", "0b0001", "0b11110000", "0b0000_1111", "0b1010_00_11"
    ];

    foreach(number; testNumbers)
    {
        const parseTree = Numbers.decimateTree(Numbers.Binary(number));
        auto match = parseTree.matches;
        assert(parseTree.successful, "Expected to parse successfully number " ~ number);
        assert(match == [number], "Expected " ~ number ~ " but was " ~ match[0]); // Shall parse
    }

    // Hexadecimal failures
    testNumbers =
    [
        "", "G", "g", "-1", "123.456", "123e+100", "0b", "01010", "0b3456"
    ];

    foreach(number; testNumbers)
    {
        assert(Numbers.decimateTree(Numbers.Binary(number)).matches != [number],
                "Number \"" ~ number ~ "\" musn't be parsed"); // None shall parse
    }
}
