module pegged.examples.arithmetic;

import pegged.grammar;

mixin(grammar(`
Arithmetic:
    Term    < Factor (Add / Sub)*
    Add     < "+" Factor
    Sub     < "-" Factor
    Factor  < Primary (Mul / Div)*
    Mul     < "*" Primary
    Div     < "/" Primary
    Primary < Parens / Neg / Number
    Parens  < :"(" Term :")"
    Neg     < "-" Primary
    Number  < ~([0-9]+)
`));

float interpreter(string expr)
{
    auto p = Arithmetic(expr);
    
    //writeln(p);
    
    float value(ParseTree p)
    {
        switch (p.name) 
        {
            case "Arithmetic":
                return value(p.children[0]);
            case "Term":
                float v = 0.0;
                foreach(child; p.children) v += value(child);
                return v;
            case "Add":
                return value(p.children[0]);
            case "Sub":
                return -value(p.children[0]);
            case "Factor":
                float v = 1.0;
                foreach(child; p.children) v *= value(child);
                return v;
            case "Mul":
                return value(p.children[0]);
            case "Div":
                return 1.0/value(p.children[0]);
            case "Primary":
                return value(p.children[0]);
            case "Parens":
                return value(p.children[0]);
            case "Neg":
                return -value(p.children[0]);
            case "Number":
                return to!float(p.matches[0]);
            default:
                return float.nan;
        }
    }
    
    return value(p);
}

unittest
{
    assert(interpreter("1") == 1.0);
    assert(interpreter("-1") == -1.0);
    assert(interpreter("1+1") == 2.0);
    assert(interpreter("1-1") == 0.0);
    
    assert(interpreter("1+1+1") == 3.0);
    assert(interpreter("1-1-1") == -1.0);
    assert(interpreter("1+1-1") == 1.0);
    assert(interpreter("1-1+1") == 1.0);
    assert(interpreter("-1+1+1") == 1.0);
    
    assert(interpreter("(-1+1)+1") == 1.0);
    assert(interpreter("-1+(1+1)") == 1.0);
    assert(interpreter("(-1+1+1)") == 1.0);
    assert(interpreter("1-(1-1)") == 1.0);
    
    assert(interpreter("1*1") == 1.0);
    assert(interpreter("1/1") == 1.0);
    assert(interpreter("-1*1") == -1.0);
    assert(interpreter("-1/1") == -1.0);
    
    assert(interpreter("1+2*3") == 7.0);
    assert(interpreter("1-2*3") == -5.0);
    assert(interpreter("-1-2*-3") == 5.0);
    assert(interpreter("-1+2*-3") == -7.0);

    assert(interpreter("1/2/(1/2)") == 1.0);
    assert(interpreter("1/2/1/2") == .25);
    assert(interpreter("1-2*3-2*3") == -11.0);

    assert(interpreter("2*3*3-3*3+3*4") == 21.0);
    assert(interpreter("2*3*3-3*(3+3*4)") == -27.0);
}