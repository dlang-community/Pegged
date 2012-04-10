Predefined Parsers
==================

**Pegged** comes with a bunch of predefined parsers, that I added once a pattern kept coming back in my own code:


Expressed in PEG syntax (they are internally implemented as expression templates), they are:

```
letter     <- [a-z]
Letter     <- [A-Z]
Alpha      <- letter / Letter / '_'
Digit      <- [0-9]
Alphanum   <- Alpha / Digit

Identifier <~ Alpha Alphanum*
QualifiedIdentifier <~ Identifier ('.' Identifier)*

Space   <- " "
Blank   <- Space / '\t' / '\v' / '\b' / '\a'
LF      <- '\n'
CR      <- '\r'
CRLF    <- "\r\n"
EOL     <- CRLF / LF / CR
Spacing <: (Blank / EOL)*  # Drops captured spaces

DoubleQuote <- '"'
Quote       <- "'"
BackQuote   <- "`"
Slash       <- "/"
BackSlash   <- '\\'

Line  <~  (!EOL .)* (EOL/EOI)
Lines <- Line+
```


In `pegged.examples.numbers` and `pegged.examples.json`, some other useful rules are described, respectively for integral / floating point numbers and for double-quoted strings. I may put them in a special module, with the previous definitions.

