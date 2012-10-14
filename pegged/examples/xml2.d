module pegged.examples.xml2;

import pegged.grammar;

enum XMLgrammar = `
XML:

Document <- prolog element Misc*

Char <- .

# RestrictedChar <- [\u0001-\uD7FF\uE000-\uFFFD]
#\U00010000-\U0010FFFF]

S <: ~('\x20' / '\x09' / '\x0D' / '\x0A')+

NameStartChar <- ":" / [A-Z] / "_" / [a-z] / [\xC0-\xD6\xD8-\xF6]

# \xF8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]
# \U00010000-\U000EFFFF]

NameChar <- NameStartChar / "-" / "." / [0-9] / '\xB7'
# / [\u0300-\u036F] / [\x203F-\x2040]

Name <~ NameStartChar (NameChar)*

Names <- Name (' ' Name)*

Nmtoken <~ (NameChar)+

nmtokens <- Nmtoken (' ' Nmtoken)*

EntityValue <- doublequote (!('%' / '&' / doublequote) Char / PEReference / Reference)* doublequote
             / quote  (!('%' / '&' / quote) Char / PEReference / Reference)* quote

AttValue <- doublequote (!('%' / '&' / doublequote) Char / Reference)* doublequote
             / quote  (!('%' / '&' / quote) Char / Reference)* quote

SystemLiteral <~ doublequote (!doublequote Char)* doublequote
               / quote (!quote Char)* quote

PubidLiteral <~ doublequote PubidChar* doublequote
              / quote (!quote PubidChar)* quote

PubidChar <- [\x20\x0D\x0A] / [a-zA-Z0-9] / [-'()+,./:=?;!*#@$_%]

CharData <~ (!('<' / '&' / "]]>" ) Char)*

Comment <- "<!--" ~(!'-' Char / '-' !'-' Char)* "-->"

PI <- "<?" PITarget (S (!"?>" Char)*)? "?>"

PITarget <- !([xX][mM][lL]) Name

CDSect <- CDStart CData CDEnd

CDStart <- "<![CDATA["

CData <- (!"]]>" Char)*

CDEnd <- "]]>"

prolog <- XMLDecl Misc* (doctypedecl Misc*)?

XMLDecl <- "<?xml" VersionInfo EncodingDecl? SDDecl? S? "?>"

VersionInfo <- S "version" Eq (quote VersionNum quote / doublequote VersionNum doublequote)

Eq <- S? '=' S?

VersionNum <- '1.0' / '1.1'

Misc <- Comment / PI / S

doctypedecl <- "<!DOCTYPE" S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'

DeclSep <- PEReference / S

intSubset <- (markupdecl / DeclSep)*

markupdecl <- elementdecl / AttlistDecl / EntityDecl / NotationDecl / PI / Comment

extSubset <- TextDecl? extSubsetDecl
extSubsetDecl <- (markupdecl / conditionalSect / DeclSep)*


SDDecl <- S 'standalone' Eq ( doublequote ("yes"/"no") doublequote
                            / quote       ("yes"/"no") quote)

element <- EmptyElemTag / STag content ETag

STag <- "<" Name (S Attribute)* S? ">"
Attribute <- Name Eq AttValue

ETag <- "</" Name S? ">"

content <- CharData? ((element / Reference / CDSect / PI / Comment) CharData?)*

EmptyElemTag <- "<" (S Attribute)* S? "/>"

elementdecl <- "<!ELEMENT" S Name S contentspec S? ">"
contentspec <- "EMPTY" / "ANY" / Mixed / children

children <- (choice / seq) ('?' / '*' / '+')?
cp <- (Name / choice / seq) ('?' / '*' / '+')?
choice <- '(' S? cp ( S? '|' S? cp )+ S? ')'
seq <-    '(' S? cp ( S? ',' S? cp )* S? ')'

Mixed <- '(' S? "#PCDATA" (S? '|' S? Name)* S? ")*"
       / '(' S? "#PCDATA" S? ")"

AttlistDecl <- "<!ATTLIST" S Name AttDef* S? ">"
AttDef <- S Name S AttType S DefaultDecl

AttType <- StringType / TokenizedType / EnumeratedType
StringType <- "CDATA"
TokenizedType <- "IDREFS" / "IDREF" / "ID"
               / "ENTITIES" / "ENTITY"
               / "NMTOKENS" / "NMTOKEN"

EnumeratedType <- NotationType / Enumeration
NotationType <- "NOTATION" S "(" S? Name (S? '|' S? Name)* S? ')'

Enumeration <- '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'

DefaultDecl <- "#REQUIRED" / "#IMPLIED"
             / (("#FIXED" S)? AttValue)

conditionalSect <- includeSect / ignoreSect

includeSect <- "<![" S? "INCLUDE" S? "[" extSubsetDecl "]]>"

ignoreSect <- "<![" S? "IGNORE"   S? "[" ignoreSectContents* "]]>"

ignoreSectContents <- Ignore ("<![" ignoreSectContents "]]>" Ignore)*

Ignore <- (!("<![" / "]]>") Char)*

CharRef <- "&#"  [0-9]+       ";"
         / "&#x" [0-9a-fA-F]+ ";"

Reference <- EntityRef / CharRef

EntityRef <- '&' Name ';'

PEReference <- '%' Name ';'

EntityDecl <- GEDecl / PEDecl

GEDecl <- "<!ENTITY" S Name S EntityDef S? '>'
PEDecl <- "<!ENTITY" S '%' S Name S PEDef S? '>'

EntityDef <- EntityValue / (ExternalID NDataDecl?)

PEDef <- EntityValue / ExternalID

ExternalID <- "SYSTEM" S SystemLiteral
            / "PUBLIC" S PubidLiteral S SystemLiteral

NDataDecl <- S "NDATA" S Name

TextDecl <- "<?xml" VersionInfo? EncodingDecl S? "?>"

extParsedEnt <- (TextDecl? content)

EncodingDecl <- S "encoding" Eq ( doublequote EncName doublequote
                                / quote EncName quote)
EncName <~ [A-Za-z] ([A-Za-z0-9._] / '-')*

NotationDecl <- "<!NOTATION" S Name S (ExternalID / PublicID) S? ">"
PublicID <- "PUBLIC" S PubidLiteral

`;

mixin(grammar(XMLgrammar));

enum example1 =
`<?xml version="1.1" encoding="ISO-8859-1"?>
<!-- Edited by XMLSpy® -->
<CATALOG>
    <CD>
        <TITLE>Empire Burlesque</TITLE>
        <ARTIST>Bob Dylan</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>Columbia</COMPANY>
        <PRICE>10.90</PRICE>
        <YEAR>1985</YEAR>
    </CD>
    <CD>
        <TITLE>Hide your heart</TITLE>
        <ARTIST>Bonnie Tyler</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>CBS Records</COMPANY>
        <PRICE>9.90</PRICE>
        <YEAR>1988</YEAR>
    </CD>
    <CD>
        <TITLE>Greatest Hits</TITLE>
        <ARTIST>Dolly Parton</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>RCA</COMPANY>
        <PRICE>9.90</PRICE>
        <YEAR>1982</YEAR>
    </CD>
    <CD>
        <TITLE>Still got the blues</TITLE>
        <ARTIST>Gary Moore</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Virgin records</COMPANY>
        <PRICE>10.20</PRICE>
        <YEAR>1990</YEAR>
    </CD>
    <CD>
        <TITLE>Eros</TITLE>
        <ARTIST>Eros Ramazzotti</ARTIST>
        <COUNTRY>EU</COUNTRY>
        <COMPANY>BMG</COMPANY>
        <PRICE>9.90</PRICE>
        <YEAR>1997</YEAR>
    </CD>
    <CD>
        <TITLE>One night only</TITLE>
        <ARTIST>Bee Gees</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Polydor</COMPANY>
        <PRICE>10.90</PRICE>
        <YEAR>1998</YEAR>
    </CD>
    <CD>
        <TITLE>Sylvias Mother</TITLE>
        <ARTIST>Dr.Hook</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>CBS</COMPANY>
        <PRICE>8.10</PRICE>
        <YEAR>1973</YEAR>
    </CD>
    <CD>
        <TITLE>Maggie May</TITLE>
        <ARTIST>Rod Stewart</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Pickwick</COMPANY>
        <PRICE>8.50</PRICE>
        <YEAR>1990</YEAR>
    </CD>
    <CD>
        <TITLE>Romanza</TITLE>
        <ARTIST>Andrea Bocelli</ARTIST>
        <COUNTRY>EU</COUNTRY>
        <COMPANY>Polydor</COMPANY>
        <PRICE>10.80</PRICE>
        <YEAR>1996</YEAR>
    </CD>
    <CD>
        <TITLE>When a man loves a woman</TITLE>
        <ARTIST>Percy Sledge</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>Atlantic</COMPANY>
        <PRICE>8.70</PRICE>
        <YEAR>1987</YEAR>
    </CD>
    <CD>
        <TITLE>Black angel</TITLE>
        <ARTIST>Savage Rose</ARTIST>
        <COUNTRY>EU</COUNTRY>
        <COMPANY>Mega</COMPANY>
        <PRICE>10.90</PRICE>
        <YEAR>1995</YEAR>
    </CD>
    <CD>
        <TITLE>1999 Grammy Nominees</TITLE>
        <ARTIST>Many</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>Grammy</COMPANY>
        <PRICE>10.20</PRICE>
        <YEAR>1999</YEAR>
    </CD>
    <CD>
        <TITLE>For the good times</TITLE>
        <ARTIST>Kenny Rogers</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Mucik Master</COMPANY>
        <PRICE>8.70</PRICE>
        <YEAR>1995</YEAR>
    </CD>
    <CD>
        <TITLE>Big Willie style</TITLE>
        <ARTIST>Will Smith</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>Columbia</COMPANY>
        <PRICE>9.90</PRICE>
        <YEAR>1997</YEAR>
    </CD>
    <CD>
        <TITLE>Tupelo Honey</TITLE>
        <ARTIST>Van Morrison</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Polydor</COMPANY>
        <PRICE>8.20</PRICE>
        <YEAR>1971</YEAR>
    </CD>
    <CD>
        <TITLE>Soulsville</TITLE>
        <ARTIST>Jorn Hoel</ARTIST>
        <COUNTRY>Norway</COUNTRY>
        <COMPANY>WEA</COMPANY>
        <PRICE>7.90</PRICE>
        <YEAR>1996</YEAR>
    </CD>
    <CD>
        <TITLE>The very best of</TITLE>
        <ARTIST>Cat Stevens</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Island</COMPANY>
        <PRICE>8.90</PRICE>
        <YEAR>1990</YEAR>
    </CD>
    <CD>
        <TITLE>Stop</TITLE>
        <ARTIST>Sam Brown</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>A and M</COMPANY>
        <PRICE>8.90</PRICE>
        <YEAR>1988</YEAR>
    </CD>
    <CD>
        <TITLE>Bridge of Spies</TITLE>
        <ARTIST>T'Pau</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Siren</COMPANY>
        <PRICE>7.90</PRICE>
        <YEAR>1987</YEAR>
    </CD>
    <CD>
        <TITLE>Private Dancer</TITLE>
        <ARTIST>Tina Turner</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>Capitol</COMPANY>
        <PRICE>8.90</PRICE>
        <YEAR>1983</YEAR>
    </CD>
    <CD>
        <TITLE>Midt om natten</TITLE>
        <ARTIST>Kim Larsen</ARTIST>
        <COUNTRY>EU</COUNTRY>
        <COMPANY>Medley</COMPANY>
        <PRICE>7.80</PRICE>
        <YEAR>1983</YEAR>
    </CD>
    <CD>
        <TITLE>Pavarotti Gala Concert</TITLE>
        <ARTIST>Luciano Pavarotti</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>DECCA</COMPANY>
        <PRICE>9.90</PRICE>
        <YEAR>1991</YEAR>
    </CD>
    <CD>
        <TITLE>The dock of the bay</TITLE>
        <ARTIST>Otis Redding</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>Atlantic</COMPANY>
        <PRICE>7.90</PRICE>
        <YEAR>1987</YEAR>
    </CD>
    <CD>
        <TITLE>Picture book</TITLE>
        <ARTIST>Simply Red</ARTIST>
        <COUNTRY>EU</COUNTRY>
        <COMPANY>Elektra</COMPANY>
        <PRICE>7.20</PRICE>
        <YEAR>1985</YEAR>
    </CD>
    <CD>
        <TITLE>Red</TITLE>
        <ARTIST>The Communards</ARTIST>
        <COUNTRY>UK</COUNTRY>
        <COMPANY>London</COMPANY>
        <PRICE>7.80</PRICE>
        <YEAR>1987</YEAR>
    </CD>
    <CD>
        <TITLE>Unchain my heart</TITLE>
        <ARTIST>Joe Cocker</ARTIST>
        <COUNTRY>USA</COUNTRY>
        <COMPANY>EMI</COMPANY>
        <PRICE>8.20</PRICE>
        <YEAR>1987</YEAR>
    </CD>
</CATALOG>
`;

enum example2 =
`<?xml version="1.0"?>
<catalog>
   <book id="bk101">
      <author>Gambardella, Matthew</author>
      <title>XML Developer's Guide</title>
      <genre>Computer</genre>
      <price>44.95</price>
      <publish_date>2000-10-01</publish_date>
      <description>An in-depth look at creating applications
      with XML.</description>
   </book>
   <book id="bk102">
      <author>Ralls, Kim</author>
      <title>Midnight Rain</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2000-12-16</publish_date>
      <description>A former architect battles corporate zombies,
      an evil sorceress, and her own childhood to become queen
      of the world.</description>
   </book>
   <book id="bk103">
      <author>Corets, Eva</author>
      <title>Maeve Ascendant</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2000-11-17</publish_date>
      <description>After the collapse of a nanotechnology
      society in England, the young survivors lay the
      foundation for a new society.</description>
   </book>
   <book id="bk104">
      <author>Corets, Eva</author>
      <title>Oberon's Legacy</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2001-03-10</publish_date>
      <description>In post-apocalypse England, the mysterious
      agent known only as Oberon helps to create a new life
      for the inhabitants of London. Sequel to Maeve
      Ascendant.</description>
   </book>
   <book id="bk105">
      <author>Corets, Eva</author>
      <title>The Sundered Grail</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2001-09-10</publish_date>
      <description>The two daughters of Maeve, half-sisters,
      battle one another for control of England. Sequel to
      Oberon's Legacy.</description>
   </book>
   <book id="bk106">
      <author>Randall, Cynthia</author>
      <title>Lover Birds</title>
      <genre>Romance</genre>
      <price>4.95</price>
      <publish_date>2000-09-02</publish_date>
      <description>When Carla meets Paul at an ornithology
      conference, tempers fly as feathers get ruffled.</description>
   </book>
   <book id="bk107">
      <author>Thurman, Paula</author>
      <title>Splish Splash</title>
      <genre>Romance</genre>
      <price>4.95</price>
      <publish_date>2000-11-02</publish_date>
      <description>A deep sea diver finds true love twenty
      thousand leagues beneath the sea.</description>
   </book>
   <book id="bk108">
      <author>Knorr, Stefan</author>
      <title>Creepy Crawlies</title>
      <genre>Horror</genre>
      <price>4.95</price>
      <publish_date>2000-12-06</publish_date>
      <description>An anthology of horror stories about roaches,
      centipedes, scorpions  and other insects.</description>
   </book>
   <book id="bk109">
      <author>Kress, Peter</author>
      <title>Paradox Lost</title>
      <genre>Science Fiction</genre>
      <price>6.95</price>
      <publish_date>2000-11-02</publish_date>
      <description>After an inadvertant trip through a Heisenberg
      Uncertainty Device, James Salway discovers the problems
      of being quantum.</description>
   </book>
   <book id="bk110">
      <author>O'Brien, Tim</author>
      <title>Microsoft .NET: The Programming Bible</title>
      <genre>Computer</genre>
      <price>36.95</price>
      <publish_date>2000-12-09</publish_date>
      <description>Microsoft's .NET initiative is explored in
      detail in this deep programmer's reference.</description>
   </book>
   <book id="bk111">
      <author>O'Brien, Tim</author>
      <title>MSXML3: A Comprehensive Guide</title>
      <genre>Computer</genre>
      <price>36.95</price>
      <publish_date>2000-12-01</publish_date>
      <description>The Microsoft MSXML3 parser is covered in
      detail, with attention to XML DOM interfaces, XSLT processing,
      SAX and more.</description>
   </book>
   <book id="bk112">
      <author>Galos, Mike</author>
      <title>Visual Studio 7: A Comprehensive Guide</title>
      <genre>Computer</genre>
      <price>49.95</price>
      <publish_date>2001-04-16</publish_date>
      <description>Microsoft Visual Studio 7 is explored in depth,
      looking at how Visual Basic, Visual C++, C#, and ASP+ are
      integrated into a comprehensive development
      environment.</description>
   </book>
</catalog>
`;

unittest
{
    assert(XML(example1).successful);
    assert(XML(example2).successful);
}
