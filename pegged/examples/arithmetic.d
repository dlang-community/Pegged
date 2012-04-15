module pegged.examples.arithmetic;

import pegged.grammar;

// Arithmetic Expressions
mixin(grammar(
   "Expr     <  Factor AddExpr*
    AddExpr  <  ^('+'/'-') Factor
    Factor   <  Primary MulExpr*
    MulExpr  <  ^('*'/'/') Primary
    Primary  <  Parens / Number / Variable / ^'-' Primary

    Parens   <  '(' Expr ')'
    Number   <~ [0-9]+
    Variable <- Identifier"
));


unittest 
{
    import std.stdio;
    enum example1 = "1 + 2 - (3 * 4 - 5)*6";
    auto p1 = Expr.parse(example1);
    
    assert(p1.success);
    assert(p1.capture == ["1"d, "+"d, "2"d, "-"d, "("d, "3"d, "*"d, "4"d, "-"d, "5"d, ")"d, "*"d, "6"d]);
    assert(p1.pos.index == example1.length); // consumed the entire input
    assert(p1.pos.line == 0);
    assert(p1.pos.col == example1.length);
        
    assert(p1.parseTree.ruleName == "Expr");
    assert(p1.parseTree.children.length == 3);
    assert(p1.parseTree.children[0].ruleName == "Factor");
    assert(p1.parseTree.children[1].ruleName == "AddExpr");
    assert(p1.parseTree.children[2].ruleName == "AddExpr");
    
    enum example2 = "0 123";
    auto p2 = Expr.parse(example2);
    
    assert(p2.success); // succeeds, but
    assert(p2.capture == ["0"d]);
    assert(p2.pos.index == 2); // consumed only 2 chars ('0' and ' ')
}
    
