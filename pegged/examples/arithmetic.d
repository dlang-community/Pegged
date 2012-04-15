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
    assert(p1.capture == ["1", "+", "2", "-", "(", "3", "*", "4", "-", "5", ")", "*", "6"]);
    assert(p1.pos.index == example1.length); // consumed the entire input
    assert(p1.pos.line == 0);
    assert(p1.pos.col == example1.length);
        
    assert(p1.parseTree.name == "Expr");
    assert(p1.parseTree.children.length == 3);
    assert(p1.parseTree.children[0].name == "Factor");
    assert(p1.parseTree.children[1].name == "AddExpr");
    assert(p1.parseTree.children[2].name == "AddExpr");
    
    enum example2 = "0 123";
    auto p2 = Expr.parse(example2);
    
    assert(p2.success); // succeeds, but
    assert(p2.capture == ["0"]);
    assert(p2.pos.index == 2); // consumed only 2 chars ('0' and ' ')
}
    
