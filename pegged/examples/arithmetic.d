module pegged.examples.arithmetic;

import pegged.grammar;

// Arithmetic Expressions
mixin Grammar!( "Expr     <- Factor AddExpr*"
              , "AddExpr  <- ('+'/'-') Factor"
              , "Factor   <- Primary MulExpr*"
              , "MulExpr  <- ('*'/'/') Primary"
              , "Primary  <- Parens / Number / Variable / '-' Primary"
              
              , "Parens   <- '(' Expr ')'"
              , "Number   <~ [0-9]+"
              , "Variable <- Identifier");
