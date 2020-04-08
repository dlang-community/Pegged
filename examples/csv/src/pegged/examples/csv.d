module pegged.examples.csv;

import pegged.grammar;

version(unittest)
{
    import std.stdio;
}

mixin(grammar(`
CSV:

CsvFile         <- Line* LastLine
Line            <- :S* Item (:Sep Item)* :endOfLine
LastLine        <- :S* Item (:Sep Item)* eoi
Item            <- SimpleField
                / QuotedField
                / EmptyField
SimpleField     <~ (!eoi !endOfLine !S !',' !doublequote .)+ :S*
QuotedField     <~ doublequote EscapedField doublequote :S*
EscapedField    <~ SubField (doublequote doublequote SubField)*
SubField        <~ (!doublequote !eoi .)+
EmptyField      <- eps :S*

Sep             <- ',' :S*
S               <- (' ' / '\t')
`));

unittest
{
    assert(CSV("a,b,c").successful);
    assert(CSV("a1,b2,c3, 3.14159").successful);
    assert(CSV(`a1,,,b2,c3, 3.14159, άλφα, "abc", "abc""def"`).successful);

    assert(CSV("1,2,3,").successful);
    assert(CSV("1,2,,3").successful);
    assert(CSV("1,,,,,").successful);
    assert(CSV(",1").successful);
    assert(CSV("").successful);
    auto p1 = CSV(
"1, 2 , 3
 4, 5,  6,,
 7");

    assert(p1.children[0].children.length == 3); // 3 lines
    assert(p1.children[0].children[0].matches == ["1", "2", "3"]);
    assert(p1.children[0].children[1].matches == ["4","5","6","",""]);
    assert(p1.children[0].children[2].matches == ["7"]);

    assert(CSV("1,
                     2,
                     3,
                     4,
                     ").successful);

    assert(!CSV("1 2 3").successful);

}
