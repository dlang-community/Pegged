module pegged.examples.csv;

enum CSVGrammar = "
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

S               <: ' ' / '\t'
";


