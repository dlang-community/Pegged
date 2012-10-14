Predefined Parsers
==================

**Pegged** comes with a bunch of predefined parsers, that I added once a pattern kept coming back in my own code:

Expressed in PEG syntax (they are internally implemented as expression templates), they are:

```
# Chars
alpha       <- [a-z]
Alpha       <- [A-Z]
digit       <- [0-9]
digits      <~ digit+
hexdigit    <- [0-9a-fA-F]

# End of Input
endOfInput  <- !.   # alias: eoi

# whitespace
endofLine   <- '\r\n' / '\n' / '\r'  # alias: eol
space       <- ' ' / '\t'
blank       <- space / endOfLine
spacing     <~ blank+

# Special chars
quote       <- "'"
doublequote <- '"'
backquote   <- "`"
slash       <- "/"
backslash   <- '\\'

# Identifiers
identifier  <~ [a-zA-Z_] [a-zA-Z_0-9]*
qualifiedIdentifier <~ identifier ('.' identifier)*
```

In `pegged.examples.numbers`, `pegged.examples.strings`, some other useful rules are described, respectively for integral / floating point numbers and for double-quoted strings.

Public Imports
--------------

`pegged.grammar` imports `pegged.peg` publicly, to allow the user to mix grammars in her own code. This means the predefined rules are directly visible in your own modules. The good news is, you can invoke them directly in your grammars.

```
MyRule <- identifier backslash digit digit
```

The bad news is, they can collide with Phobos names. Use the usual D solutions to disimbiguate in this case. To avoid collisions, `pegged.grammar` will produce code calling qualified names (it calls `drop` as `pegged.peg.drop` to avoid collision with `std.range.drop` and so on).

