module pegged.examples.markdown;

enum MarkdownGrammar= `
# Markdown grammar.
# Taken from the PEG grammar at
# https://github.com/jgm/peg-markdown
# And simplified somewhat.

Markdown:

Doc <- BOM?  TitleBlock? (%Block)*

#### Pandoc Extension #### Partially implemented
TitleBlock <- :"%" TitleText
              :"%" Date
              :"%" Authors
TitleText <~ Line (:Spacechar Line)*
Date      <- ;Line
Authors   <- Author ((:";" (:Newline :Spacechar)? / :Newline :Spacechar) Author)*
Author    <- (!";" !Newline %Inlines)+

Block <- BlankLine*
         ( BlockQuote
         / FootnoteDefinition #### Pandoc Extension
         / CodeBlock
         / Verbatim
         / Note
         / Reference
         / HorizontalRule
         / Heading
         / Table #### Pandoc Extension
         / DefinitionList #### Pandoc Extension
         / OrderedList
         / BulletList
         / HtmlBlock
         / StyleBlock
         / Para
         / %Inlines )

Para <- :NonindentSpace %Inlines BlankLine+

#Plain <~ %Inlines

Heading <- SetextHeading / AtxHeading

SetextHeading <- SetextHeading1 / SetextHeading2

SetextHeading1 <~ &(Line SetextBottom1)
                  ( !Endline Inline )+ Sp? :Newline
                  :SetextBottom1

SetextHeading2 <~ &(Line SetextBottom2)
                  ( !Endline Inline)+ Sp? :Newline
                  :SetextBottom2

SetextBottom1 <~ "===" "="* Newline

SetextBottom2 <~ "---" "-"* Newline

AtxHeading <- AtxStart ~(Sp? AtxInline+ Sp? ("#"* Sp)?  Newline)

AtxInline <- !Newline !(Sp? "#"* Sp Newline) Inline

AtxStart <- ( "######" / "#####" / "####" / "###" / "##" / "#" )

#### Pandoc Extension ####
# A semantic function must find the columns, based on the dashes
Table <- SimpleTable # For further extension (multiline tables and grid tables)

SimpleTable <- TableHeaders
               TableLine+
               :(BlankLine / "-"+ Newline BlankLine)
               TableCaption?

TableHeaders <- ;Line?
                ~("-"+) ~(Spacechar+) ~("-"+) (~(Spacechar+) ~("-"+))* :Newline

# ;Line makes all the inlines disappear. Is that wanted or not?
TableLine <- !(BlankLine / "-"+ Newline BlankLine) ;Line

TableCaption <- :"Table:" ;Line
              / :":" ;Line

BlockQuote <- ( ">" " "? Line ( !">" !BlankLine Line )* BlankLine* )+

NonblankIndentedLine <~ !BlankLine IndentedLine

VerbatimChunk <- BlankLine* NonblankIndentedLine+

Verbatim <~ VerbatimChunk+

HorizontalRule <- NonindentSpace
                 ( "*" Sp "*" Sp "*" (Sp "*")*
                 / "-" Sp "-" Sp "-" (Sp "-")*
                 / "_" Sp "_" Sp "_" (Sp "_")*)
                 Sp Newline BlankLine+

BulletList <- &Bullet (%BulletListTight / %BulletListLoose)

BulletListTight <- (%BulletListItemTight)+ :BlankLine* !Bullet

BulletListItemTight <- Bullet ListBlock
                       (!BlankLine ListContinuationBlock)*
                       #!ListContinuationBlock

BulletListLoose <- (%BulletListItem :BlankLine*)+

BulletListItem <- Bullet ListBlock ListContinuationBlock*

Bullet <: !HorizontalRule NonindentSpace ("+" / "*" / "-") Spacechar+

OrderedList <- &Enumerator (OrderedListTight / OrderedListLoose)

OrderedListTight <- (%OrderedListItemTight)+ :BlankLine* !Enumerator

OrderedListItemTight <- Enumerator ListBlock
                        (!BlankLine ListContinuationBlock)*
                        #!ListContinuationBlock # Is it necessary?

OrderedListLoose <- (%OrderedListItem :BlankLine*)+

OrderedListItem <- Enumerator ListBlock ListContinuationBlock*

Enumerator <: NonindentSpace ~[0-9]+ "." Spacechar+

ListBlock <- !BlankLine %Inlines ListBlockLine*

ListContinuationBlock <- BlankLine* (Indent ListBlock)+

ListBlockLine <- !BlankLine !( Indent? (Bullet / Enumerator)) !HorizontalRule Indent? %Inlines

DefinitionList <- Term :(BlankLine?) Definition+

Term <- (!Newline .)+ :Newline

Definition <- ( Spacechar Spacechar :(":"/"~") Spacechar Spacechar
              / Spacechar :(":"/"~") Spacechar Spacechar Spacechar
              / :(":"/"~") Spacechar Spacechar Spacechar Spacechar)
              Inlines :Newline
              IndentedLine*

# Parsers for different kinds of block-level HTML content.
# This is repetitive due to constraints of PEG grammar.

HtmlBlockOpen(Type) <- :"<" :Spnl Type :Spnl HtmlAttribute* :">" :(Spnl*)
HtmlBlockClose(Type) <- :"<" :Spnl :"/" Type :Spnl :">" :(Spnl*)
HtmlBlockT(Type) <- ;HtmlBlockOpen(Type)
                    (%HtmlBlockInTags / NonHtml)*
                    ;HtmlBlockClose(Type)
# Hack. This should use a HtmlBlockClose(every possibility)
NonHtml <- (!("<" Spnl "/") Inline)*

HtmlBlockInTags <- HtmlBlockT("address" / "ADDRESS")
                 / HtmlBlockT("blockquote" / "BLOCKQUOTE")
                 / HtmlBlockT("center" / "CENTER")
                 / HtmlBlockT("dir" / "DIR")
                 / HtmlBlockT("div" / "DIV")
                 / HtmlBlockT("dl" / "DL")
                 / HtmlBlockT("fieldset" / "FIELDSET")
                 / HtmlBlockT("form" / "FORM")
                 / HtmlBlockT("h1" / "H1")
                 / HtmlBlockT("h2" / "H2")
                 / HtmlBlockT("h3" / "H3")
                 / HtmlBlockT("h4" / "H4")
                 / HtmlBlockT("h5" / "H5")
                 / HtmlBlockT("h6" / "H6")
                 / HtmlBlockT("menu" / "MENU")
                 / HtmlBlockT("noframes" / "NOFRAMES")
                 / HtmlBlockT("noscript" / "NOSCRIPT")
                 / HtmlBlockT("ol" / "OL")
                 / HtmlBlockT("p" / "P")
                 / HtmlBlockT("pre" / "PRE")
                 / HtmlBlockT("table" / "TABLE")
                 / HtmlBlockT("ul" / "UL")
                 / HtmlBlockT("dd" / "DD")
                 / HtmlBlockT("dt" / "DT")
                 / HtmlBlockT("frameset" / "FRAMESET")
                 / HtmlBlockT("li" / "LI")
                 / HtmlBlockT("tbody" / "TBODY")
                 / HtmlBlockT("td" / "TD")
                 / HtmlBlockT("tfoot" / "TFOOT")
                 / HtmlBlockT("th" / "TH")
                 / HtmlBlockT("thead" / "THEAD")
                 / HtmlBlockT("tr" / "TR")
                 / HtmlBlockT("script" / "SCRIPT")

HtmlBlock <- (%HtmlBlockInTags / HtmlComment / HtmlBlockSelfClosing) BlankLine+

HtmlBlockSelfClosing <- "<" Spnl HtmlBlockType Spnl HtmlAttribute* "/" Spnl ">"

HtmlBlockType <- "address" / "blockquote" / "center" / "dir" / "div" / "dl" / "fieldset" / "form" / "h1" / "h2" / "h3" /
                "h4" / "h5" / "h6" / "hr" / "isindex" / "menu" / "noframes" / "noscript" / "ol" / "p" / "pre" / "table" /
                "ul" / "dd" / "dt" / "frameset" / "li" / "tbody" / "td" / "tfoot" / "th" / "thead" / "tr" / "script" /
                "ADDRESS" / "BLOCKQUOTE" / "CENTER" / "DIR" / "DIV" / "DL" / "FIELDSET" / "FORM" / "H1" / "H2" / "H3" /
                "H4" / "H5" / "H6" / "HR" / "ISINDEX" / "MENU" / "NOFRAMES" / "NOSCRIPT" / "OL" / "P" / "PRE" / "TABLE" /
                "UL" / "DD" / "DT" / "FRAMESET" / "LI" / "TBODY" / "TD" / "TFOOT" / "TH" / "THEAD" / "TR" / "SCRIPT"

StyleOpen <- "<" Spnl ("style" / "STYLE") Spnl HtmlAttribute* ">"
StyleClose <- "<" Spnl "/" ("style" / "STYLE") Spnl ">"
InStyleTags <- StyleOpen (!StyleClose .)* StyleClose
StyleBlock <- InStyleTags BlankLine*

Inlines <- (!Endline %Inline )+ Endline?

Inline <- Str
        / Endline
        / UlOrStarLine
        / Space
        / Strong
        / Emph
        / Strikeout          #### Pandoc Extension
        / Superscript        #### Pandoc Extension
        / Subscript          #### Pandoc Extension
        / Math               #### Pandoc Extension
        / FootnoteReference  #### Pandoc Extension
        / Image
        / Link
        / NoteReference
        / InlineNote
        / Code
        / RawHtml
        / Entity
        / EscapedChar
        / Smart
        / Symbol

Space <~ Spacechar+

Str <~ NormalChar+ StrChunk*

StrChunk <~ (NormalChar / "_"+ &Alphanumeric)+ / AposChunk

AposChunk <- quote &Alphanumeric

EscapedChar <- backslash (backquote / backslash / [-/_*{}[\]()#+.!><])

Entity <- HexEntity / DecEntity / CharEntity

Endline <~ LineBreak / TerminalEndline / NormalEndline

NormalEndline <- Sp Newline !BlankLine !">" !AtxStart
                 !(Line ("<-<-<-" "<-"* / "---" "-"*) Newline)

TerminalEndline <- Sp Newline eoi

LineBreak <~ "  " NormalEndline

Symbol <~ SpecialChar

UlOrStarLine <~ UlLine / StarLine
StarLine <- "****" "*"* / Spacechar "*"+ &Spacechar
UlLine   <- "____" "_"* / Spacechar "_"+ &Spacechar

Emph <~ EmphStar / EmphUl

OneStarOpen  <- !StarLine "*" !Spacechar !Newline
OneStarClose <- !Spacechar !Newline Inline :"*"

EmphStar <- :OneStarOpen
            ( !OneStarClose Inline )*
            OneStarClose

OneUlOpen  <- !UlLine "_" !Spacechar !Newline
OneUlClose <- !Spacechar !Newline Inline :"_" !Alphanumeric

EmphUl <- :OneUlOpen
          ( !OneUlClose Inline )*
          OneUlClose

Strong <~ StrongStar / StrongUl

TwoStarOpen <-  !StarLine "**" !Spacechar !Newline
TwoStarClose <- !Spacechar !Newline Inline :"**"

StrongStar <- :TwoStarOpen
              ( !TwoStarClose Inline )*
              TwoStarClose

TwoUlOpen <- !UlLine "__" !Spacechar !Newline
TwoUlClose <- !Spacechar !Newline Inline :"__" !Alphanumeric

StrongUl <- :TwoUlOpen
            ( !TwoUlClose Inline )*
            :TwoUlClose

#### Pandoc Extension ####
Strikeout <- :"~~" Inline :"~~"

#### Pandoc Extension ####
Superscript <- :"^" Inline :"^"

#### Pandoc Extension ####
Subscript <- :"~" Inline :"~"

#### Pandoc Extension ####
Math <- :"$" !Spacechar (!(Spacechar "$") .)* :"$"

Image <- "!" ( ExplicitLink / ReferenceLink )

Link <-  ExplicitLink / ReferenceLink / AutoLink

ReferenceLink <- ReferenceLinkDouble / ReferenceLinkSingle

ReferenceLinkDouble <-  Label Spnl !"[]" Label

ReferenceLinkSingle <-  Label (Spnl "[]")?

ExplicitLink <-  Label Spnl :"(" Sp Source Spnl Title? Sp :")"

Source  <- HeaderIdentifier #### Pandoc extension ####
         / :"<" SourceContents :">"
         / SourceContents

HeaderIdentifier <~ :"#" [a-z][-_.a-z0-9]*

SourceContents <~ ( ( !"(" !")" !">" Nonspacechar )+ / :"(" SourceContents :")")*

Title <~ (TitleSingle / TitleDouble)

TitleSingle <- :quote ( !( quote Sp ( ")" / Newline ) ) . )*  :quote

TitleDouble <- :doublequote ( !( doublequote Sp ( ")" / Newline ) ) . )* :doublequote

AutoLink <- AutoLinkUrl / AutoLinkEmail

AutoLinkUrl <- :"<" ~([A-Za-z]+ "://" ( !Newline !">" . )+) :">"

AutoLinkEmail <- :"<" ( "mailto:" )? ~([-A-Za-z0-9+_./!%~$]+ "@" ( !Newline !">" . )+) :">"

Reference <- NonindentSpace !"[]" Label ":" Spnl RefSrc RefTitle BlankLine+

Label <~ :"[" (!"]" Inline )* :"]"

RefSrc <- Nonspacechar+

RefTitle <- RefTitleSingle / RefTitleDouble / RefTitleParens / EmptyTitle

EmptyTitle <- eps

RefTitleSingle <- Spnl quote ( !( quote Sp Newline / Newline ) . )* quote

RefTitleDouble <- Spnl doublequote ( !(doublequote Sp Newline / Newline) . )* doublequote

RefTitleParens <- Spnl "(" ( !(")" Sp Newline / Newline) .)* ")"

References <- ( Reference / SkipBlock )*

Ticks1 <- backquote !backquote
Ticks2 <- backquote backquote !backquote
Ticks3 <- backquote backquote backquote !backquote
Ticks4 <- backquote backquote backquote backquote !backquote
Ticks5 <- backquote backquote backquote backquote backquote !backquote

Tildes <- "~~~" "~"*

### Standard extension. Covers both Github Markdown and Pandoc Markdown
CodeBlock <- ( :Ticks5 CodeOptions? :Newline ~(!Ticks5 .)+ :Ticks5 :Newline
             / :Ticks4 CodeOptions? :Newline ~(!Ticks4 .)+ :Ticks4 :Newline
             / :Ticks3 CodeOptions? :Newline ~(!Ticks3 .)+ :Ticks3 :Newline
             / :Tildes CodeOptions? :Newline ~(!Tildes .)+ :Tildes :Newline)

Code <- ( :Ticks1 ~(!Ticks1 .)+ :Ticks1 CodeOptions?
        / :Ticks2 ~(!Ticks2 .)+ :Ticks2 CodeOptions?)

CodeOptions <- :"{" :Sp (;Option :Sp)* :Sp :"}"
             / ;Option

Option <~ "."? identifier (:"=" (digit+ / identifier))?

#### Pandoc Extension #### Partially implemented (multiline footnotes)
FootnoteReference <- :"[^" FootnoteName :"]" !":"
FootnoteDefinition <- :"[^" FootnoteName :"]:" Line (BlankLine / Indent Line)*
FootnoteName <- (digit+ / identifier)

RawHtml <- HtmlComment / HtmlBlockT("script" / "SCRIPT") / HtmlTag

BlankLine <~ Sp Newline

Quoted <- doublequote (!doublequote .)* doublequote / quote (!quote .)* quote
HtmlAttribute <- (AlphanumericAscii / "-")+ Spnl ("=" Spnl (Quoted / (!">" Nonspacechar)+))? Spnl
HtmlComment <- "<!--" (!"-->" .)* "-->"
HtmlTag <- "<" Spnl "/"? AlphanumericAscii+ Spnl HtmlAttribute* "/"? Spnl ">"

Spacechar <- " " / "\t"
Nonspacechar <- !Spacechar !Newline .
Newline <- endOfLine
Sp <- Spacechar*
Spnl <- Sp (Newline Sp)?

SpecialChar <- "*" / "_" / backquote / "&" / "[" / "]" / "(" / ")" / "<" / "!" / "#" / backslash / quote / doublequote / ExtendedSpecialChar
NormalChar <-    !( SpecialChar / Spacechar / Newline ) .
NonAlphanumeric <- !Alphanumeric . #[\001-\057] / [\072-\100] / [\133-\140] / [\173-\177]
Alphanumeric <- [0-9A-Za-z] / "\200" / "\201" / "\202" / "\203" / "\204"
               / "\205" / "\206" / "\207" / "\210" / "\211" / "\212"
               / "\213" / "\214" / "\215" / "\216" / "\217" / "\220"
               / "\221" / "\222" / "\223" / "\224" / "\225" / "\226"
               / "\227" / "\230" / "\231" / "\232" / "\233" / "\234"
               / "\235" / "\236" / "\237" / "\240" / "\241" / "\242"
               / "\243" / "\244" / "\245" / "\246" / "\247" / "\250"
               / "\251" / "\252" / "\253" / "\254" / "\255" / "\256"
               / "\257" / "\260" / "\261" / "\262" / "\263" / "\264"
               / "\265" / "\266" / "\267" / "\270" / "\271" / "\272"
               / "\273" / "\274" / "\275" / "\276" / "\277" / "\300"
               / "\301" / "\302" / "\303" / "\304" / "\305" / "\306"
               / "\307" / "\310" / "\311" / "\312" / "\313" / "\314"
               / "\315" / "\316" / "\317" / "\320" / "\321" / "\322"
               / "\323" / "\324" / "\325" / "\326" / "\327" / "\330"
               / "\331" / "\332" / "\333" / "\334" / "\335" / "\336"
               / "\337" / "\340" / "\341" / "\342" / "\343" / "\344"
               / "\345" / "\346" / "\347" / "\350" / "\351" / "\352"
               / "\353" / "\354" / "\355" / "\356" / "\357" / "\360"
               / "\361" / "\362" / "\363" / "\364" / "\365" / "\366"
               / "\367" / "\370" / "\371" / "\372" / "\373" / "\374"
               / "\375" / "\376" / "\377"

AlphanumericAscii <- [A-Za-z0-9]
Digit <- [0-9]
BOM <- "\357\273\277"

HexEntity <-  "&" "#" [Xx] [0-9a-fA-F]+
DecEntity <-  "&" "#" [0-9]+
CharEntity <- "&" [A-Za-z0-9]+

NonindentSpace <: ("   " / "  " / " ")?
Indent <- "\t" / "    "
IndentedLine <- :Indent Line
OptionallyIndentedLine <- Indent? Line

Line <~ (!Newline .)* :Newline
      / .+ :eoi

SkipBlock <- HtmlBlock
           / ( !"#" !SetextBottom1 !SetextBottom2 !BlankLine Line )+ BlankLine*
           / BlankLine+
           / Line

ExtendedSpecialChar <- "." / "-" / quote / doublequote / "^"

Smart <- Ellipsis / Dash / SingleQuoted / DoubleQuoted / Apostrophe

Apostrophe <- quote

Ellipsis <- "..." / ". . ."

Dash <- EmDash / EnDash

EnDash <- "-" &Digit

EmDash <- "---" / "--"

SingleQuoteStart <- quote !(Spacechar / Newline)

SingleQuoteEnd <- quote !Alphanumeric

SingleQuoted <- SingleQuoteStart ( !SingleQuoteEnd Inline )+  SingleQuoteEnd

DoubleQuoteStart <- doublequote

DoubleQuoteEnd <- doublequote

DoubleQuoted <-  DoubleQuoteStart ( !DoubleQuoteEnd Inline )+ DoubleQuoteEnd

NoteReference <- RawNoteReference

RawNoteReference <~ :"[^"  ( !Newline !"]" . )+  :"]" !":"

Note <- :NonindentSpace RawNoteReference :":" :Sp
        RawNoteBlock
        ( &Indent RawNoteBlock  )*

InlineNote <- :"^[" ( !"]" Inline)+ :"]"

Notes <- (Note / SkipBlock)*

RawNoteBlock <- ( !BlankLine OptionallyIndentedLine )+ BlankLine*
`;
