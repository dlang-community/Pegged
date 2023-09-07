module pegged.examples.csv;

import pegged.grammar;

@safe:

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
    const csv1 = CSV("a,b,c");
    assert(csv1.successful);
    assert(csv1.toString == q"EOS
CSV[0, 5]["a", "b", "c"]
 +-CSV.CsvFile[0, 5]["a", "b", "c"]
    +-CSV.LastLine[0, 5]["a", "b", "c"]
       +-CSV.Item[0, 1]["a"]
       |  +-CSV.SimpleField[0, 1]["a"]
       +-CSV.Item[2, 3]["b"]
       |  +-CSV.SimpleField[2, 3]["b"]
       +-CSV.Item[4, 5]["c"]
          +-CSV.SimpleField[4, 5]["c"]
EOS");

    const csv2 = CSV("a1,b2,c3, 3.14159");
    assert(csv2.successful);
    assert(csv2.toString == q"EOS
CSV[0, 17]["a1", "b2", "c3", "3.14159"]
 +-CSV.CsvFile[0, 17]["a1", "b2", "c3", "3.14159"]
    +-CSV.LastLine[0, 17]["a1", "b2", "c3", "3.14159"]
       +-CSV.Item[0, 2]["a1"]
       |  +-CSV.SimpleField[0, 2]["a1"]
       +-CSV.Item[3, 5]["b2"]
       |  +-CSV.SimpleField[3, 5]["b2"]
       +-CSV.Item[6, 8]["c3"]
       |  +-CSV.SimpleField[6, 8]["c3"]
       +-CSV.Item[10, 17]["3.14159"]
          +-CSV.SimpleField[10, 17]["3.14159"]
EOS");

    const csv3 = CSV(`a1,,,b2,c3, 3.14159, άλφα, "abc", "abc""def"`);
    assert(csv3.successful);
    assert(csv3.toString == q"EOS
CSV[0, 48]["a1", "", "", "b2", "c3", "3.14159", "άλφα", "\"abc\"", "\"abc\"\"def\""]
 +-CSV.CsvFile[0, 48]["a1", "", "", "b2", "c3", "3.14159", "άλφα", "\"abc\"", "\"abc\"\"def\""]
    +-CSV.LastLine[0, 48]["a1", "", "", "b2", "c3", "3.14159", "άλφα", "\"abc\"", "\"abc\"\"def\""]
       +-CSV.Item[0, 2]["a1"]
       |  +-CSV.SimpleField[0, 2]["a1"]
       +-CSV.Item[3, 3][""]
       |  +-CSV.EmptyField[3, 3][""]
       +-CSV.Item[4, 4][""]
       |  +-CSV.EmptyField[4, 4][""]
       +-CSV.Item[5, 7]["b2"]
       |  +-CSV.SimpleField[5, 7]["b2"]
       +-CSV.Item[8, 10]["c3"]
       |  +-CSV.SimpleField[8, 10]["c3"]
       +-CSV.Item[12, 19]["3.14159"]
       |  +-CSV.SimpleField[12, 19]["3.14159"]
       +-CSV.Item[21, 29]["άλφα"]
       |  +-CSV.SimpleField[21, 29]["άλφα"]
       +-CSV.Item[31, 36]["\"abc\""]
       |  +-CSV.QuotedField[31, 36]["\"abc\""]
       +-CSV.Item[38, 48]["\"abc\"\"def\""]
          +-CSV.QuotedField[38, 48]["\"abc\"\"def\""]
EOS");

    const csv4 = CSV("1,2,3,");
    assert(csv4.successful);
    assert(csv4.toString == q"EOS
CSV[0, 6]["1", "2", "3", ""]
 +-CSV.CsvFile[0, 6]["1", "2", "3", ""]
    +-CSV.LastLine[0, 6]["1", "2", "3", ""]
       +-CSV.Item[0, 1]["1"]
       |  +-CSV.SimpleField[0, 1]["1"]
       +-CSV.Item[2, 3]["2"]
       |  +-CSV.SimpleField[2, 3]["2"]
       +-CSV.Item[4, 5]["3"]
       |  +-CSV.SimpleField[4, 5]["3"]
       +-CSV.Item[6, 6][""]
          +-CSV.EmptyField[6, 6][""]
EOS");

    const csv5 = CSV("1,2,,3");
    assert(csv5.successful);
    assert(csv5.toString == q"EOS
CSV[0, 6]["1", "2", "", "3"]
 +-CSV.CsvFile[0, 6]["1", "2", "", "3"]
    +-CSV.LastLine[0, 6]["1", "2", "", "3"]
       +-CSV.Item[0, 1]["1"]
       |  +-CSV.SimpleField[0, 1]["1"]
       +-CSV.Item[2, 3]["2"]
       |  +-CSV.SimpleField[2, 3]["2"]
       +-CSV.Item[4, 4][""]
       |  +-CSV.EmptyField[4, 4][""]
       +-CSV.Item[5, 6]["3"]
          +-CSV.SimpleField[5, 6]["3"]
EOS");

    const csv6 = CSV("1,,,,,");
    assert(csv6.successful);
    assert(csv6.toString == q"EOS
CSV[0, 6]["1", "", "", "", "", ""]
 +-CSV.CsvFile[0, 6]["1", "", "", "", "", ""]
    +-CSV.LastLine[0, 6]["1", "", "", "", "", ""]
       +-CSV.Item[0, 1]["1"]
       |  +-CSV.SimpleField[0, 1]["1"]
       +-CSV.Item[2, 2][""]
       |  +-CSV.EmptyField[2, 2][""]
       +-CSV.Item[3, 3][""]
       |  +-CSV.EmptyField[3, 3][""]
       +-CSV.Item[4, 4][""]
       |  +-CSV.EmptyField[4, 4][""]
       +-CSV.Item[5, 5][""]
       |  +-CSV.EmptyField[5, 5][""]
       +-CSV.Item[6, 6][""]
          +-CSV.EmptyField[6, 6][""]
EOS");

    const csv7 = CSV(",1");
    assert(csv7.successful);
    assert(csv7.toString == q"EOS
CSV[0, 2]["", "1"]
 +-CSV.CsvFile[0, 2]["", "1"]
    +-CSV.LastLine[0, 2]["", "1"]
       +-CSV.Item[0, 0][""]
       |  +-CSV.EmptyField[0, 0][""]
       +-CSV.Item[1, 2]["1"]
          +-CSV.SimpleField[1, 2]["1"]
EOS");

    const csv8 = CSV("");
    assert(csv8.successful);
    assert(csv8.toString == q"EOS
CSV[0, 0][""]
 +-CSV.CsvFile[0, 0][""]
    +-CSV.LastLine[0, 0][""]
       +-CSV.Item[0, 0][""]
          +-CSV.EmptyField[0, 0][""]
EOS");

    auto p1 = CSV(
"1, 2 , 3
 4, 5,  6,,
 7");

    assert(p1[0].children.length == 3); // 3 lines
    assert(p1[0][0].matches == ["1", "2", "3"]);
    assert(p1[0][1].matches == ["4","5","6","",""]);
    assert(p1[0][2].matches == ["7"]);
    assert(p1.toString == q"EOS
CSV[0, 23]["1", "2", "3", "4", "5", "6", "", "", "7"]
 +-CSV.CsvFile[0, 23]["1", "2", "3", "4", "5", "6", "", "", "7"]
    +-CSV.Line[0, 9]["1", "2", "3"]
    |  +-CSV.Item[0, 1]["1"]
    |  |  +-CSV.SimpleField[0, 1]["1"]
    |  +-CSV.Item[3, 5]["2"]
    |  |  +-CSV.SimpleField[3, 5]["2"]
    |  +-CSV.Item[7, 8]["3"]
    |     +-CSV.SimpleField[7, 8]["3"]
    +-CSV.Line[9, 21]["4", "5", "6", "", ""]
    |  +-CSV.Item[10, 11]["4"]
    |  |  +-CSV.SimpleField[10, 11]["4"]
    |  +-CSV.Item[13, 14]["5"]
    |  |  +-CSV.SimpleField[13, 14]["5"]
    |  +-CSV.Item[17, 18]["6"]
    |  |  +-CSV.SimpleField[17, 18]["6"]
    |  +-CSV.Item[19, 19][""]
    |  |  +-CSV.EmptyField[19, 19][""]
    |  +-CSV.Item[20, 20][""]
    |     +-CSV.EmptyField[20, 20][""]
    +-CSV.LastLine[21, 23]["7"]
       +-CSV.Item[22, 23]["7"]
          +-CSV.SimpleField[22, 23]["7"]
EOS");

    const csv9 = CSV("1,
                      2,
                      3,
                      4,
                      ");
    assert(csv9.successful);
    assert((csv9.toString == q"EOS
CSV[0, 100]["1", "", "2", "", "3", "", "4", "", ""]
 +-CSV.CsvFile[0, 100]["1", "", "2", "", "3", "", "4", "", ""]
    +-CSV.Line[0, 3]["1", ""]
    |  +-CSV.Item[0, 1]["1"]
    |  |  +-CSV.SimpleField[0, 1]["1"]
    |  +-CSV.Item[2, 2][""]
    |     +-CSV.EmptyField[2, 2][""]
    +-CSV.Line[3, 28]["2", ""]
    |  +-CSV.Item[25, 26]["2"]
    |  |  +-CSV.SimpleField[25, 26]["2"]
    |  +-CSV.Item[27, 27][""]
    |     +-CSV.EmptyField[27, 27][""]
    +-CSV.Line[28, 53]["3", ""]
    |  +-CSV.Item[50, 51]["3"]
    |  |  +-CSV.SimpleField[50, 51]["3"]
    |  +-CSV.Item[52, 52][""]
    |     +-CSV.EmptyField[52, 52][""]
    +-CSV.Line[53, 78]["4", ""]
    |  +-CSV.Item[75, 76]["4"]
    |  |  +-CSV.SimpleField[75, 76]["4"]
    |  +-CSV.Item[77, 77][""]
    |     +-CSV.EmptyField[77, 77][""]
    +-CSV.LastLine[78, 100][""]
       +-CSV.Item[100, 100][""]
          +-CSV.EmptyField[100, 100][""]
EOS"));

    const csv10 = CSV("1 2 3");
    assert(!csv10.successful);
    assert(csv10.toString == q"EOS
CSV (failure)
 +-CSV.CsvFile (failure)
    +-zeroOrMore!(CSV.Line)[0, 0][]
    +-CSV.LastLine (failure)
       +-CSV.Item[0, 2]["1"]
       |  +-CSV.SimpleField[0, 2]["1"]
       +-eoi Failure at line 0, col 2, after "1 " expected "end of input", but got "2 3"
EOS");
}
