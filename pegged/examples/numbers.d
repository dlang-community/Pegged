module pegged.examples.numbers;

import pegged.grammar;

/// Numbers
mixin(grammar(
   `Number   <- Floating ( ('e' / 'E' ) Integer )?
    Floating <- Integer ('.' Unsigned )?
    Unsigned <- [0-9]+
    Integer  <- Sign Unsigned
    Hexa     <- [0-9a-f]+ / [0-9A-F]+ 
    Sign     <- ('-' / '+')?`
));

