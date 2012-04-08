module pegged.examples.csv;

import pegged.grammar;

mixin(grammar(`
CSV:

CsvFile         <- Line* LastLine
Line            <- RawString (:',' RawString)* :EOL
LastLine        <- RawString (:',' RawString)* EOI
RawString       <- S* ( SimpleField 
                      / QuotedField 
                      / Eps
                      ) S*
SimpleField     <- (!EOI !EOL !S !',' !DoubleQuote .)+
QuotedField     <- DoubleQuote EscapedField DoubleQuote
EscapedField    <- SubField (DoubleQuote DoubleQuote SubField)*
SubField        <- (!DoubleQuote !EOI .)+

S               <: ' ' / '  '
`));

unittest
{
    assert(CSV.parse("1,2,3").success);
    assert(CSV.parse("1,2,3,").success);
    assert(CSV.parse("1,2,,3").success);
    assert(CSV.parse("1,,,,,").success);
    assert(CSV.parse(",1").success);
    assert(CSV.parse("").success);
    auto p1 = CSV.parse(
"1, 2 , 3
 4, 5,  6,,
 7");
    
    
    assert(p1.children[0].children.length == 3); // 3 lines
    assert(p1.children[0].children[0].capture == ["1","2","3"]);
    assert(p1.children[0].children[1].capture == ["4","5","6","",""]);
    assert(p1.children[0].children[2].capture == ["7"]); 
    
    assert(CSV.parse("1,
                     2,
                     3,
                     4,
                     ").success);
    
    assert(!CSV.parse("1 2 3").success);
}