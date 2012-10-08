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

mixin( grammar!(Memoization.yes)( `
Test:
  Div <- Block( 'div' )
  Block( Tag ) <- Open1( Tag )
                  ( Block( Tag ) / ( !(HtmlTag( "/" Tag )) .))*
                  HtmlTag( "/" Tag )

  HtmlTag( Tag ) <- "<" Tag ">"

  Open1( Tag )  <- "<" Tag ">"
  Open2( Tag )  <- HtmlTag( Tag )
`));

void main() 
{
    writeln(Test("<div></div>"));

}