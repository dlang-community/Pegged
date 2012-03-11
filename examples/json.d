module pegged.examples.json;

import pegged.grammar;

/// JSON
mixin(grammar(
   `JSON   <- :'{' (Pair (:',' Pair)*)? :'}'
    Pair   <- String :':' Value
    Array  <- :'[' (Value (:',' Value)* )? :']'
    Value  <- String 
            / Number 
            / JSON 
            / Array 
            / True 
            / False 
            / Null
    True   <- "true"
    False  <- "false"
    Null   <- "null"
    String <~ :DoubleQuote>Char*>:DoubleQuote
    Char   <~ BackSlash>DoubleQuote 
            / BackSlash>BackSlash 
            / BackSlash>[bfnrt] 
            / BackSlash>'u'>Hex>Hex>Hex>Hex
            / (!DoubleQuote>.)
    Number <~ '0'
            / [1-9]>Digit*>('.'>Digit*)
    Digit  <- [0-9]
    Hex    <- [0-9A-Fa-f]`
));

