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
  Div <- HtmlBlockTag( 'div' )
  HtmlBlockTag( Tag ) <- HtmlTagOpen( Tag )
                        ( HtmlBlockTag( Tag ) /
                          AllIfNot( HtmlTagClose( Tag ) ) )*
                        HtmlTagClose( Tag )

  HtmlTag( Contents ) <- Lt Spnl ^Contents Spnl Gt
  HtmlTagClose( Tag ) <- HtmlTag( ^slash ^Tag )

  # The version of HtmTagOpen that uses the HtmlTag rule fails under memoization;
  # both versions work under no memoization
  HtmlTagOpen( Tag )  <- Lt Spnl ^Tag Spnl ( HtmlAttribute Spnl )* Spnl Gt
  #HtmlTagOpen( Tag )  <- HtmlTag( ^Tag Spnl HtmlAttribute* )

  HtmlAttributeValue <~ (Quoted / (!"/" !">" Nonspacechar)+)
  HtmlAttributeName <~ (AlphanumericAscii / "-")+
  HtmlAttribute <- HtmlAttributeName Spnl (^"=" Spnl HtmlAttributeValue)? Spnl

  Lt <- "<"
  Gt <- ">"

  Quoted <-    ^doublequote FuseAllUntil(doublequote) ^doublequote
             / ^quote FuseAllUntil(quote) ^quote
  BlankLine <~     Spaces Newline
  AlphanumericAscii <~ [A-Za-z0-9]
  Nonspacechar <~  !Spacechar !Newline .
  Spacechar <~     " " / "\t"
  Newline <~       "\n" / "\r" "\n"?
  Spaces <~        Spacechar*
  Spnl <~          Spaces (Newline Spaces)?

  AllIfNot(Predicate) <- (!Predicate .)
  AllUntil(Predicate) <- AllIfNot(Predicate)*
  FuseAllUntil(Predicate) <~ AllUntil(Predicate)
`));

void main() {
  auto tree = Test(
`<div id="bar">
  foo <br/> bar
</div>

`);

  writeln( tree );
  writeln( tree.matches );
}