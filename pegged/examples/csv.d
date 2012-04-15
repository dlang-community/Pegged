module pegged.examples.csv;

import pegged.grammar;

mixin(grammar(`
CSV:

CsvFile         <- Line* LastLine
Line            <- Item (:',' Item)* :EOL
LastLine        <- Item (:',' Item)* EOI
Item            <~ S* ( SimpleField 
                      / QuotedField 
                      / Eps
                      ) S*
SimpleField     <- (!EOI !EOL !S !',' !DoubleQuote .)+
QuotedField     <- DoubleQuote EscapedField DoubleQuote
EscapedField    <- SubField (DoubleQuote DoubleQuote SubField)*
SubField        <- (!DoubleQuote !EOI .)+

S               <: ' ' / '\t'
`));

unittest
{
    assert(CSV.parse("1,2,3").success);
    assert(CSV.parse("a,b,c").success);
    assert(CSV.parse("a1,b2,c3, 3.14159").success);
    assert(CSV.parse(`a1,b2,c3, 3.14159, άλφα, "abc", "abc""def"`).success);
    
    assert(CSV.parse("1,2,3,").success);
    assert(CSV.parse("1,2,,3").success);
    assert(CSV.parse("1,,,,,").success);
    assert(CSV.parse(",1").success);
    assert(CSV.parse("").success);
    auto p1 = CSV.parse(
"1, 2 , 3
 4, 5,  6,,
 7");
    
    assert(p1.children.length == 3); // 3 lines
    assert(p1.children[0].capture == ["1"d,"2"d,"3"d]);
    assert(p1.children[1].capture == ["4"d,"5"d,"6"d,""d,""d]);
    assert(p1.children[2].capture == ["7"d]); 

    assert(CSV.parse("1,
                     2,
                     3,
                     4,
                     ").success);
    
    assert(!CSV.parse("1 2 3").success);
}