module pegged.examples.markdown;

enum MarkdownGrammar= `
# Markdown grammar.
# Taken from the PEG grammar at
# https://github.com/jgm/peg-markdown
# And simplified somewhat.

Markdown:

Doc <- BOM?  Block*

Block <-     BlankLine*
            ( BlockQuote
            / Verbatim
            / Note
            / Reference
            / HorizontalRule
            / Heading
            / OrderedList
            / BulletList
            / HtmlBlock
            / StyleBlock
            / Para
            / Inlines )

Para <~      NonindentSpace Inlines BlankLine+

#Plain <~     Inlines

AtxInline <- !Newline !(Sp? '#'* Sp Newline) Inline

AtxStart <-  ( "######" / "#####" / "####" / "###" / "##" / "#" )

AtxHeading <- AtxStart Sp?  ~(AtxInline+) (Sp? '#'* Sp)?  Newline

SetextHeading <- SetextHeading1 / SetextHeading2

SetextBottom1 <- "<-<-<-" '<-'* Newline

SetextBottom2 <- "---" '-'* Newline

SetextHeading1 <-  &(Line SetextBottom1)
                   ( !Endline Inline )+ Sp? Newline
                  SetextBottom1

SetextHeading2 <-  &(Line SetextBottom2)
                   ( !Endline Inline)+ Sp? Newline
                  SetextBottom2

Heading <- SetextHeading / AtxHeading

BlockQuote <- BlockQuoteRaw

BlockQuoteRaw <- ( '>' ' '? Line ( !'>' !BlankLine Line )* BlankLine* )+

NonblankIndentedLine <~ !BlankLine IndentedLine

VerbatimChunk <-  BlankLine* NonblankIndentedLine+

Verbatim <-      VerbatimChunk+

HorizontalRule <- NonindentSpace
                 ( '*' Sp '*' Sp '*' (Sp '*')*
                 / '-' Sp '-' Sp '-' (Sp '-')*
                 / '_' Sp '_' Sp '_' (Sp '_')*)
                 Sp Newline BlankLine+

Bullet <- !HorizontalRule NonindentSpace ('+' / '*' / '-') Spacechar+

BulletList <- &Bullet (ListTight / ListLoose)

ListTight <-  ListItemTight+ BlankLine* !(Bullet / Enumerator)

ListLoose <-  (ListItem BlankLine*)+

ListItem <- ( Bullet / Enumerator )
            ListBlock
            ListContinuationBlock*
            
ListItemTight <- ( Bullet / Enumerator ) ListBlock
                ( !BlankLine ListContinuationBlock )*
                !ListContinuationBlock # Is it necessary?

ListBlock <- !BlankLine Line ListBlockLine*

ListContinuationBlock <- BlankLine* ( Indent ListBlock )+

Enumerator <- NonindentSpace [0-9]+ '.' Spacechar+

OrderedList <- &Enumerator (ListTight / ListLoose)

ListBlockLine <- !BlankLine
                !( Indent? (Bullet / Enumerator) )
                !HorizontalRule
                OptionallyIndentedLine

# Parsers for different kinds of block-level HTML content.
# This is repetitive due to constraints of PEG grammar.

HtmlBlockOpenAddress <- '<' Spnl ("address" / "ADDRESS") Spnl HtmlAttribute* '>'
HtmlBlockCloseAddress <- '<' Spnl '/' ("address" / "ADDRESS") Spnl '>'
HtmlBlockAddress <- HtmlBlockOpenAddress (HtmlBlockAddress / !HtmlBlockCloseAddress .)* HtmlBlockCloseAddress

HtmlBlockOpenBlockquote <- '<' Spnl ("blockquote" / "BLOCKQUOTE") Spnl HtmlAttribute* '>'
HtmlBlockCloseBlockquote <- '<' Spnl '/' ("blockquote" / "BLOCKQUOTE") Spnl '>'
HtmlBlockBlockquote <- HtmlBlockOpenBlockquote (HtmlBlockBlockquote / !HtmlBlockCloseBlockquote .)* HtmlBlockCloseBlockquote

HtmlBlockOpenCenter <- '<' Spnl ("center" / "CENTER") Spnl HtmlAttribute* '>'
HtmlBlockCloseCenter <- '<' Spnl '/' ("center" / "CENTER") Spnl '>'
HtmlBlockCenter <- HtmlBlockOpenCenter (HtmlBlockCenter / !HtmlBlockCloseCenter .)* HtmlBlockCloseCenter

HtmlBlockOpenDir <- '<' Spnl ("dir" / "DIR") Spnl HtmlAttribute* '>'
HtmlBlockCloseDir <- '<' Spnl '/' ("dir" / "DIR") Spnl '>'
HtmlBlockDir <- HtmlBlockOpenDir (HtmlBlockDir / !HtmlBlockCloseDir .)* HtmlBlockCloseDir

HtmlBlockOpenDiv <- '<' Spnl ("div" / "DIV") Spnl HtmlAttribute* '>'
HtmlBlockCloseDiv <- '<' Spnl '/' ("div" / "DIV") Spnl '>'
HtmlBlockDiv <- HtmlBlockOpenDiv (HtmlBlockDiv / !HtmlBlockCloseDiv .)* HtmlBlockCloseDiv

HtmlBlockOpenDl <- '<' Spnl ("dl" / "DL") Spnl HtmlAttribute* '>'
HtmlBlockCloseDl <- '<' Spnl '/' ("dl" / "DL") Spnl '>'
HtmlBlockDl <- HtmlBlockOpenDl (HtmlBlockDl / !HtmlBlockCloseDl .)* HtmlBlockCloseDl

HtmlBlockOpenFieldset <- '<' Spnl ("fieldset" / "FIELDSET") Spnl HtmlAttribute* '>'
HtmlBlockCloseFieldset <- '<' Spnl '/' ("fieldset" / "FIELDSET") Spnl '>'
HtmlBlockFieldset <- HtmlBlockOpenFieldset (HtmlBlockFieldset / !HtmlBlockCloseFieldset .)* HtmlBlockCloseFieldset

HtmlBlockOpenForm <- '<' Spnl ("form" / "FORM") Spnl HtmlAttribute* '>'
HtmlBlockCloseForm <- '<' Spnl '/' ("form" / "FORM") Spnl '>'
HtmlBlockForm <- HtmlBlockOpenForm (HtmlBlockForm / !HtmlBlockCloseForm .)* HtmlBlockCloseForm

HtmlBlockOpenH1 <- '<' Spnl ("h1" / "H1") Spnl HtmlAttribute* '>'
HtmlBlockCloseH1 <- '<' Spnl '/' ("h1" / "H1") Spnl '>'
HtmlBlockH1 <- HtmlBlockOpenH1 (HtmlBlockH1 / !HtmlBlockCloseH1 .)* HtmlBlockCloseH1

HtmlBlockOpenH2 <- '<' Spnl ("h2" / "H2") Spnl HtmlAttribute* '>'
HtmlBlockCloseH2 <- '<' Spnl '/' ("h2" / "H2") Spnl '>'
HtmlBlockH2 <- HtmlBlockOpenH2 (HtmlBlockH2 / !HtmlBlockCloseH2 .)* HtmlBlockCloseH2

HtmlBlockOpenH3 <- '<' Spnl ("h3" / "H3") Spnl HtmlAttribute* '>'
HtmlBlockCloseH3 <- '<' Spnl '/' ("h3" / "H3") Spnl '>'
HtmlBlockH3 <- HtmlBlockOpenH3 (HtmlBlockH3 / !HtmlBlockCloseH3 .)* HtmlBlockCloseH3

HtmlBlockOpenH4 <- '<' Spnl ("h4" / "H4") Spnl HtmlAttribute* '>'
HtmlBlockCloseH4 <- '<' Spnl '/' ("h4" / "H4") Spnl '>'
HtmlBlockH4 <- HtmlBlockOpenH4 (HtmlBlockH4 / !HtmlBlockCloseH4 .)* HtmlBlockCloseH4

HtmlBlockOpenH5 <- '<' Spnl ("h5" / "H5") Spnl HtmlAttribute* '>'
HtmlBlockCloseH5 <- '<' Spnl '/' ("h5" / "H5") Spnl '>'
HtmlBlockH5 <- HtmlBlockOpenH5 (HtmlBlockH5 / !HtmlBlockCloseH5 .)* HtmlBlockCloseH5

HtmlBlockOpenH6 <- '<' Spnl ("h6" / "H6") Spnl HtmlAttribute* '>'
HtmlBlockCloseH6 <- '<' Spnl '/' ("h6" / "H6") Spnl '>'
HtmlBlockH6 <- HtmlBlockOpenH6 (HtmlBlockH6 / !HtmlBlockCloseH6 .)* HtmlBlockCloseH6

HtmlBlockOpenMenu <- '<' Spnl ("menu" / "MENU") Spnl HtmlAttribute* '>'
HtmlBlockCloseMenu <- '<' Spnl '/' ("menu" / "MENU") Spnl '>'
HtmlBlockMenu <- HtmlBlockOpenMenu (HtmlBlockMenu / !HtmlBlockCloseMenu .)* HtmlBlockCloseMenu

HtmlBlockOpenNoframes <- '<' Spnl ("noframes" / "NOFRAMES") Spnl HtmlAttribute* '>'
HtmlBlockCloseNoframes <- '<' Spnl '/' ("noframes" / "NOFRAMES") Spnl '>'
HtmlBlockNoframes <- HtmlBlockOpenNoframes (HtmlBlockNoframes / !HtmlBlockCloseNoframes .)* HtmlBlockCloseNoframes

HtmlBlockOpenNoscript <- '<' Spnl ("noscript" / "NOSCRIPT") Spnl HtmlAttribute* '>'
HtmlBlockCloseNoscript <- '<' Spnl '/' ("noscript" / "NOSCRIPT") Spnl '>'
HtmlBlockNoscript <- HtmlBlockOpenNoscript (HtmlBlockNoscript / !HtmlBlockCloseNoscript .)* HtmlBlockCloseNoscript

HtmlBlockOpenOl <- '<' Spnl ("ol" / "OL") Spnl HtmlAttribute* '>'
HtmlBlockCloseOl <- '<' Spnl '/' ("ol" / "OL") Spnl '>'
HtmlBlockOl <- HtmlBlockOpenOl (HtmlBlockOl / !HtmlBlockCloseOl .)* HtmlBlockCloseOl

HtmlBlockOpenP <- '<' Spnl ("p" / "P") Spnl HtmlAttribute* '>'
HtmlBlockCloseP <- '<' Spnl '/' ("p" / "P") Spnl '>'
HtmlBlockP <- HtmlBlockOpenP (HtmlBlockP / !HtmlBlockCloseP .)* HtmlBlockCloseP

HtmlBlockOpenPre <- '<' Spnl ("pre" / "PRE") Spnl HtmlAttribute* '>'
HtmlBlockClosePre <- '<' Spnl '/' ("pre" / "PRE") Spnl '>'
HtmlBlockPre <- HtmlBlockOpenPre (HtmlBlockPre / !HtmlBlockClosePre .)* HtmlBlockClosePre

HtmlBlockOpenTable <- '<' Spnl ("table" / "TABLE") Spnl HtmlAttribute* '>'
HtmlBlockCloseTable <- '<' Spnl '/' ("table" / "TABLE") Spnl '>'
HtmlBlockTable <- HtmlBlockOpenTable (HtmlBlockTable / !HtmlBlockCloseTable .)* HtmlBlockCloseTable

HtmlBlockOpenUl <- '<' Spnl ("ul" / "UL") Spnl HtmlAttribute* '>'
HtmlBlockCloseUl <- '<' Spnl '/' ("ul" / "UL") Spnl '>'
HtmlBlockUl <- HtmlBlockOpenUl (HtmlBlockUl / !HtmlBlockCloseUl .)* HtmlBlockCloseUl

HtmlBlockOpenDd <- '<' Spnl ("dd" / "DD") Spnl HtmlAttribute* '>'
HtmlBlockCloseDd <- '<' Spnl '/' ("dd" / "DD") Spnl '>'
HtmlBlockDd <- HtmlBlockOpenDd (HtmlBlockDd / !HtmlBlockCloseDd .)* HtmlBlockCloseDd

HtmlBlockOpenDt <- '<' Spnl ("dt" / "DT") Spnl HtmlAttribute* '>'
HtmlBlockCloseDt <- '<' Spnl '/' ("dt" / "DT") Spnl '>'
HtmlBlockDt <- HtmlBlockOpenDt (HtmlBlockDt / !HtmlBlockCloseDt .)* HtmlBlockCloseDt

HtmlBlockOpenFrameset <- '<' Spnl ("frameset" / "FRAMESET") Spnl HtmlAttribute* '>'
HtmlBlockCloseFrameset <- '<' Spnl '/' ("frameset" / "FRAMESET") Spnl '>'
HtmlBlockFrameset <- HtmlBlockOpenFrameset (HtmlBlockFrameset / !HtmlBlockCloseFrameset .)* HtmlBlockCloseFrameset

HtmlBlockOpenLi <- '<' Spnl ("li" / "LI") Spnl HtmlAttribute* '>'
HtmlBlockCloseLi <- '<' Spnl '/' ("li" / "LI") Spnl '>'
HtmlBlockLi <- HtmlBlockOpenLi (HtmlBlockLi / !HtmlBlockCloseLi .)* HtmlBlockCloseLi

HtmlBlockOpenTbody <- '<' Spnl ("tbody" / "TBODY") Spnl HtmlAttribute* '>'
HtmlBlockCloseTbody <- '<' Spnl '/' ("tbody" / "TBODY") Spnl '>'
HtmlBlockTbody <- HtmlBlockOpenTbody (HtmlBlockTbody / !HtmlBlockCloseTbody .)* HtmlBlockCloseTbody

HtmlBlockOpenTd <- '<' Spnl ("td" / "TD") Spnl HtmlAttribute* '>'
HtmlBlockCloseTd <- '<' Spnl '/' ("td" / "TD") Spnl '>'
HtmlBlockTd <- HtmlBlockOpenTd (HtmlBlockTd / !HtmlBlockCloseTd .)* HtmlBlockCloseTd

HtmlBlockOpenTfoot <- '<' Spnl ("tfoot" / "TFOOT") Spnl HtmlAttribute* '>'
HtmlBlockCloseTfoot <- '<' Spnl '/' ("tfoot" / "TFOOT") Spnl '>'
HtmlBlockTfoot <- HtmlBlockOpenTfoot (HtmlBlockTfoot / !HtmlBlockCloseTfoot .)* HtmlBlockCloseTfoot

HtmlBlockOpenTh <- '<' Spnl ("th" / "TH") Spnl HtmlAttribute* '>'
HtmlBlockCloseTh <- '<' Spnl '/' ("th" / "TH") Spnl '>'
HtmlBlockTh <- HtmlBlockOpenTh (HtmlBlockTh / !HtmlBlockCloseTh .)* HtmlBlockCloseTh

HtmlBlockOpenThead <- '<' Spnl ("thead" / "THEAD") Spnl HtmlAttribute* '>'
HtmlBlockCloseThead <- '<' Spnl '/' ("thead" / "THEAD") Spnl '>'
HtmlBlockThead <- HtmlBlockOpenThead (HtmlBlockThead / !HtmlBlockCloseThead .)* HtmlBlockCloseThead

HtmlBlockOpenTr <- '<' Spnl ("tr" / "TR") Spnl HtmlAttribute* '>'
HtmlBlockCloseTr <- '<' Spnl '/' ("tr" / "TR") Spnl '>'
HtmlBlockTr <- HtmlBlockOpenTr (HtmlBlockTr / !HtmlBlockCloseTr .)* HtmlBlockCloseTr

HtmlBlockOpenScript <- '<' Spnl ("script" / "SCRIPT") Spnl HtmlAttribute* '>'
HtmlBlockCloseScript <- '<' Spnl '/' ("script" / "SCRIPT") Spnl '>'
HtmlBlockScript <- HtmlBlockOpenScript (!HtmlBlockCloseScript .)* HtmlBlockCloseScript


HtmlBlockInTags <- HtmlBlockAddress
                / HtmlBlockBlockquote
                / HtmlBlockCenter
                / HtmlBlockDir
                / HtmlBlockDiv
                / HtmlBlockDl
                / HtmlBlockFieldset
                / HtmlBlockForm
                / HtmlBlockH1
                / HtmlBlockH2
                / HtmlBlockH3
                / HtmlBlockH4
                / HtmlBlockH5
                / HtmlBlockH6
                / HtmlBlockMenu
                / HtmlBlockNoframes
                / HtmlBlockNoscript
                / HtmlBlockOl
                / HtmlBlockP
                / HtmlBlockPre
                / HtmlBlockTable
                / HtmlBlockUl
                / HtmlBlockDd
                / HtmlBlockDt
                / HtmlBlockFrameset
                / HtmlBlockLi
                / HtmlBlockTbody
                / HtmlBlockTd
                / HtmlBlockTfoot
                / HtmlBlockTh
                / HtmlBlockThead
                / HtmlBlockTr
                / HtmlBlockScript

HtmlBlock <- ( HtmlBlockInTags / HtmlComment / HtmlBlockSelfClosing ) BlankLine+

HtmlBlockSelfClosing <- '<' Spnl HtmlBlockType Spnl HtmlAttribute* '/' Spnl '>'

HtmlBlockType <- "address" / "blockquote" / "center" / "dir" / "div" / "dl" / "fieldset" / "form" / "h1" / "h2" / "h3" /
                "h4" / "h5" / "h6" / "hr" / "isindex" / "menu" / "noframes" / "noscript" / "ol" / "p" / "pre" / "table" /
                "ul" / "dd" / "dt" / "frameset" / "li" / "tbody" / "td" / "tfoot" / "th" / "thead" / "tr" / "script" /
                "ADDRESS" / "BLOCKQUOTE" / "CENTER" / "DIR" / "DIV" / "DL" / "FIELDSET" / "FORM" / "H1" / "H2" / "H3" /
                "H4" / "H5" / "H6" / "HR" / "ISINDEX" / "MENU" / "NOFRAMES" / "NOSCRIPT" / "OL" / "P" / "PRE" / "TABLE" /
                "UL" / "DD" / "DT" / "FRAMESET" / "LI" / "TBODY" / "TD" / "TFOOT" / "TH" / "THEAD" / "TR" / "SCRIPT"

StyleOpen <-     '<' Spnl ("style" / "STYLE") Spnl HtmlAttribute* '>'
StyleClose <-    '<' Spnl '/' ("style" / "STYLE") Spnl '>'
InStyleTags <-   StyleOpen (!StyleClose .)* StyleClose
StyleBlock <-   InStyleTags BlankLine*

Inlines  <-  ( !Endline Inline )+ Endline?

Inline  <- Str
        / Endline
        / UlOrStarLine
        / Space
        / Strong
        / Emph
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

Str <~  NormalChar+ StrChunk*

StrChunk <~ (NormalChar / '_'+ &Alphanumeric)+ / AposChunk

AposChunk <- Quote &Alphanumeric

EscapedChar <-   BackSlash (BackQuote / BackSlash / [-/_*{}[\]()#+.!><])

Entity <- HexEntity / DecEntity / CharEntity

Endline <-   LineBreak / TerminalEndline / NormalEndline

NormalEndline <-   Sp Newline !BlankLine !'>' !AtxStart
                  !(Line ("<-<-<-" '<-'* / "---" '-'*) Newline)

TerminalEndline <- Sp Newline EOI

LineBreak <- "  " NormalEndline

Symbol <- SpecialChar

UlOrStarLine <- UlLine / StarLine
StarLine <- "****" '*'* / Spacechar '*'+ &Spacechar
UlLine   <- "____" '_'* / Spacechar '_'+ &Spacechar

Emph <- EmphStar / EmphUl

OneStarOpen  <-  !StarLine '*' !Spacechar !Newline
OneStarClose <-  !Spacechar !Newline Inline '*'

EmphStar <-  OneStarOpen
            ( !OneStarClose Inline )*
            OneStarClose

OneUlOpen  <-  !UlLine '_' !Spacechar !Newline
OneUlClose <-  !Spacechar !Newline Inline '_' !Alphanumeric

EmphUl <-    OneUlOpen
            ( !OneUlClose Inline )*
            OneUlClose

Strong <- StrongStar / StrongUl

TwoStarOpen <-   !StarLine "**" !Spacechar !Newline
TwoStarClose <-  !Spacechar !Newline Inline "**"

StrongStar <-    TwoStarOpen
                ( !TwoStarClose Inline )*
                TwoStarClose
                
TwoUlOpen <-     !UlLine "__" !Spacechar !Newline
TwoUlClose <-    !Spacechar !Newline Inline "__" !Alphanumeric

StrongUl <-  TwoUlOpen
            ( !TwoUlClose Inline )*
            TwoUlClose

Image <- '!' ( ExplicitLink / ReferenceLink )

Link <-  ExplicitLink / ReferenceLink / AutoLink

ReferenceLink <- ReferenceLinkDouble / ReferenceLinkSingle

ReferenceLinkDouble <-  Label Spnl !"[]" Label

ReferenceLinkSingle <-  Label (Spnl "[]")?

ExplicitLink <-  Label Spnl '(' Sp Source Spnl Title Sp ')'

Source  <- '<' SourceContents '>' 
         / SourceContents

SourceContents <- ( ( !'(' !')' !'>' Nonspacechar )+ / '(' SourceContents ')')*
                / Eps

Title <- TitleSingle / TitleDouble / Eps

TitleSingle <- Quote ( !( Quote Sp ( ')' / Newline ) ) . )*  Quote

TitleDouble <- DoubleQuote ( !( DoubleQuote Sp ( ')' / Newline ) ) . )* DoubleQuote

AutoLink <- AutoLinkUrl / AutoLinkEmail

AutoLinkUrl <-   '<' [A-Za-z]+ "://" ( !Newline !'>' . )+ '>'

AutoLinkEmail <- '<' ( "mailto:" )? [-A-Za-z0-9+_./!%~$]+ '@' ( !Newline !'>' . )+ '>'

Reference <- NonindentSpace !"[]" Label ':' Spnl RefSrc RefTitle BlankLine+

Label <- '[' (!']' Inline  )* ']'

RefSrc <- Nonspacechar+

RefTitle <- RefTitleSingle / RefTitleDouble / RefTitleParens / EmptyTitle

EmptyTitle <- Eps

RefTitleSingle <- Spnl Quote ( !( Quote Sp Newline / Newline ) . )* Quote

RefTitleDouble <- Spnl DoubleQuote ( !(DoubleQuote Sp Newline / Newline) . )* DoubleQuote

RefTitleParens <- Spnl '(' ( !(')' Sp Newline / Newline) . )*  ')'

References <- ( Reference / SkipBlock )*

Ticks1 <- BackQuote !BackQuote
Ticks2 <- BackQuote BackQuote !BackQuote
Ticks3 <- BackQuote BackQuote BackQuote !BackQuote
Ticks4 <- BackQuote BackQuote BackQuote BackQuote !BackQuote
Ticks5 <- BackQuote BackQuote BackQuote BackQuote BackQuote !BackQuote

Code <~ ( :Ticks1 Sp ( ( !BackQuote Nonspacechar )+ / !Ticks1 BackQuote+ / !( Sp Ticks1 ) ( Spacechar / Newline !BlankLine ) )+  Sp :Ticks1
       / :Ticks2 Sp  ( ( !BackQuote Nonspacechar )+ / !Ticks2 BackQuote+ / !( Sp Ticks2 ) ( Spacechar / Newline !BlankLine ) )+  Sp :Ticks2
       / :Ticks3 Sp  ( ( !BackQuote Nonspacechar )+ / !Ticks3 BackQuote+ / !( Sp Ticks3 ) ( Spacechar / Newline !BlankLine ) )+  Sp :Ticks3
       / :Ticks4 Sp  ( ( !BackQuote Nonspacechar )+ / !Ticks4 BackQuote+ / !( Sp Ticks4 ) ( Spacechar / Newline !BlankLine ) )+  Sp :Ticks4
       / :Ticks5 Sp  ( ( !BackQuote Nonspacechar )+ / !Ticks5 BackQuote+ / !( Sp Ticks5 ) ( Spacechar / Newline !BlankLine ) )+  Sp :Ticks5
       )

RawHtml <- HtmlComment / HtmlBlockScript / HtmlTag

BlankLine <~ Sp Newline

Quoted <-        DoubleQuote (!DoubleQuote .)* DoubleQuote / Quote (!Quote .)* Quote
HtmlAttribute <- (AlphanumericAscii / '-')+ Spnl ('<-' Spnl (Quoted / (!'>' Nonspacechar)+))? Spnl
HtmlComment <-   "<!--" (!"-->" .)* "-->"
HtmlTag <-       '<' Spnl '/'? AlphanumericAscii+ Spnl HtmlAttribute* '/'? Spnl '>'

Spacechar <-     ' ' / '\t'
Nonspacechar <-  !Spacechar !Newline .
Newline <-       '\n' / '\r' '\n'?
Sp <-            Spacechar*
Spnl <-          Sp (Newline Sp)?

SpecialChar <-   '*' / '_' / BackQuote / '&' / '[' / ']' / '(' / ')' / '<' / '!' / '#' / BackSlash / Quote / DoubleQuote / ExtendedSpecialChar
NormalChar <-    !( SpecialChar / Spacechar / Newline ) .
NonAlphanumeric <- !Alphanumeric . #[\001-\057] / [\072-\100] / [\133-\140] / [\173-\177]
Alphanumeric <- [0-9A-Za-z] / '\200' / '\201' / '\202' / '\203' / '\204' / '\205' / '\206' / '\207' / '\210' / '\211' / '\212' / '\213' / '\214' / '\215' / '\216' / '\217' / '\220' / '\221' / '\222' / '\223' / '\224' / '\225' / '\226' / '\227' / '\230' / '\231' / '\232' / '\233' / '\234' / '\235' / '\236' / '\237' / '\240' / '\241' / '\242' / '\243' / '\244' / '\245' / '\246' / '\247' / '\250' / '\251' / '\252' / '\253' / '\254' / '\255' / '\256' / '\257' / '\260' / '\261' / '\262' / '\263' / '\264' / '\265' / '\266' / '\267' / '\270' / '\271' / '\272' / '\273' / '\274' / '\275' / '\276' / '\277' / '\300' / '\301' / '\302' / '\303' / '\304' / '\305' / '\306' / '\307' / '\310' / '\311' / '\312' / '\313' / '\314' / '\315' / '\316' / '\317' / '\320' / '\321' / '\322' / '\323' / '\324' / '\325' / '\326' / '\327' / '\330' / '\331' / '\332' / '\333' / '\334' / '\335' / '\336' / '\337' / '\340' / '\341' / '\342' / '\343' / '\344' / '\345' / '\346' / '\347' / '\350' / '\351' / '\352' / '\353' / '\354' / '\355' / '\356' / '\357' / '\360' / '\361' / '\362' / '\363' / '\364' / '\365' / '\366' / '\367' / '\370' / '\371' / '\372' / '\373' / '\374' / '\375' / '\376' / '\377'
AlphanumericAscii <- [A-Za-z0-9]
Digit <- [0-9]
BOM <- "\357\273\277"

HexEntity <-     '&' '#' [Xx] [0-9a-fA-F]+
DecEntity <-     '&' '#' [0-9]+
CharEntity <-    '&' [A-Za-z0-9]+

NonindentSpace <-    "   " / "  " / " " / Eps
Indent <-            "\t" / "    "
IndentedLine <-      :Indent Line
OptionallyIndentedLine <- Indent? Line

Line <~  (!'\r' !'\n' .)* Newline 
         / .+ EOI 

SkipBlock <- HtmlBlock
          / ( !'#' !SetextBottom1 !SetextBottom2 !BlankLine Line )+ BlankLine*
          / BlankLine+
          / Line
          
ExtendedSpecialChar <- '.' / '-' / Quote / DoubleQuote / '^'

Smart <- Ellipsis / Dash / SingleQuoted / DoubleQuoted / Apostrophe

Apostrophe <- Quote

Ellipsis <- "..." / ". . ."

Dash <- EmDash / EnDash

EnDash <- '-' &Digit

EmDash <- "---" / "--"

SingleQuoteStart <- Quote !(Spacechar / Newline)

SingleQuoteEnd <- Quote !Alphanumeric

SingleQuoted <- SingleQuoteStart ( !SingleQuoteEnd Inline )+  SingleQuoteEnd

DoubleQuoteStart <- DoubleQuote

DoubleQuoteEnd <- DoubleQuote

DoubleQuoted <-  DoubleQuoteStart ( !DoubleQuoteEnd Inline )+ DoubleQuoteEnd
                
NoteReference <- RawNoteReference

RawNoteReference <- "[^"  ( !Newline !']' . )+  ']'

Note <- NonindentSpace RawNoteReference ':' Sp
        RawNoteBlock 
        ( &Indent RawNoteBlock  )*

InlineNote <-    "^[" ( !']' Inline  )+ ']'

Notes <-  ( Note / SkipBlock )*
                
RawNoteBlock <-  ( !BlankLine OptionallyIndentedLine )+ BlankLine*
`;