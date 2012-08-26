module pegged.examples.xml2;

enum XMLgrammar = `
XML:

Document <- prolog element Misc*

Char <- .

# RestrictedChar <- [\u0001-\uD7FF\uE000-\uFFFD] 
#\U00010000-\U0010FFFF]

S <~ ('\x20' / '\x09' / '\x0D' / '\x0A')+

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

VersionNum <- "1.1"

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
