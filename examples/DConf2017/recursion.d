import pegged.peg;  // Predefined parser combinators (and, or, literal).

// Right-recursion:
// R <- 'n+' R / 'n'
ParseTree R(ParseTree p)
{
    return 
        or!(and!(literal!("n+"), R), literal!("n"))(p);
}

void main()
{
    ParseTree p = { input : "n+n" };
    auto result = R(p);
    import std.stdio;
    writeln(result);
}

// rdmd -I../../.. ../../../libpegged.a recursion.d
