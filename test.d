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

import pegged.grammar;
import pegged.peg;
enum mp=`
MarkupParser:
Markup <- Spacing (Tag / Text)* :eoi

Text <~ (! '<' .)*

Tag <- StartTag / EndTag

StartTag <- '<' Name Attr* '>'

EndTag < "</" Name '>'

Attr < Name ('=' Value)?

Name <- (alpha / Alpha) (alpha / Alpha / digit / '-' / '_' / ':' / '.')*

Value <- SQValue / DQValue / CharValue

SQValue <~ :"'" (! "'" .)* :"'"

DQValue <~ :'"' (! '"' .)* :'"'

CharValue <~ (!(Spacing / '>') .)*
`;



void main() {

mixin(grammar(mp));

}