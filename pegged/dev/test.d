/// Testing Pegged modifications.
module pegged.dev.test;

//import std.algorithm;
//import std.array;
//import std.conv;
import std.datetime;
//import std.functional;
//import std.range;
import std.stdio;
//import std.typecons;
//import std.typetuple;

import pegged.grammar;

mixin(grammar("
    TOML:
    Element <  Key* :eoi
    Key < ^identifier :'=' Var
    Var < Integer
    Integer <~ '-'? digits
 
    # These two are predefined by Pegged
    spacing <~ blank+
    blank   <- space / endOfLine
    Spacing <: ( spacing / Comment )* # s/spacing/blank/ and it works.  Why?
    Comment <- '#' (!eol .)* :eol # Cribbed directly from the Pegged grammar
    "));

void main()
{
    enum input = `
    int1 = 40
    # It bails here.
    int2 = 51`;
    writeln(TOML(input));

}

