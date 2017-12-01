import pegged.grammar;

mixin(grammar(`
Arithmetic:
    Term     < Factor (Add / Sub)*
    Add      < "+" Factor
    Sub      < "-" Factor
    Factor   < Primary (Mul / Div)*
    Mul      < "*" Primary
    Div      < "/" Primary
    Primary  < Parens / Neg / Number / Variable
    Parens   < :"(" Term :")"
    Neg      < "-" Primary
    Number   < ~([0-9]+)
    Variable <- identifier
`));

void main()
{
    enum parseTree = Arithmetic("1 + 2 - (3 * 2 - 5) * 6");

    float value(ParseTree p)
    {
        switch (p.name)
        {
            case "Arithmetic", "Arithmetic.Primary", "Arithmetic.Parens", "Arithmetic.Add", "Arithmetic.Mul":
                return value(p.children[0]);
            case "Arithmetic.Sub", "Arithmetic.Neg":
                return -value(p.children[0]);
            case "Arithmetic.Term":
                float v = 0.0;
                foreach(child; p.children) v += value(child);
                return v;
            case "Arithmetic.Factor":
                float v = 1.0;
                foreach(child; p.children) v *= value(child);
                return v;
            case "Arithmetic.Div":
                return 1.0/value(p.children[0]);
            case "Arithmetic.Number":
                import std.conv: to;
                return to!float(p.matches[0]);
            default:
                return float.nan;
        }
    }

    import std.stdio;
    enum answer = value(parseTree);
    writeln(answer);
}

// rdmd -I../../.. arithmetic.d 