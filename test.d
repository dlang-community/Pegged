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


/// TODO: modify the way generic rules are written, again.
/// Caution: memoization code
import std.stdio;
import pegged.grammar;

mixin(grammar(`
Arithmetic:
    Term     < Factor (Add / Sub)*
    Add      < "+" Factor
    Sub      < "-" Factor
    Factor   < Primary (Mul / Div)*
    Mul      < "*" Primary
    Div      < "/" Primary
    Primary  < Parens / Neg / Number / Variable
    Parens   < "(" Term ")"
    Neg      < "-" Primary
    Number   < ~([0-9]+)
    Variable <- identifier
`));

void main() 
{
  // Parsing at compile-time:
enum parseTree1 = Arithmetic("1 + 2 - (3*x-5)*6");

pragma(msg, parseTree1.matches);
assert(parseTree1.matches == ["1", "+", "2", "-", "(", "3", "*", "x", "-", "5", ")", "*", "6"]);
writeln(parseTree1);

// And at runtime too:
auto parseTree2 = Arithmetic(" 0 + 123 - 456 ");
assert(parseTree2.matches == ["0", "+", "123", "-", "456"]);

}