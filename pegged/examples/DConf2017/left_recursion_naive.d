/* This causes infinite recursion and terminates due to stack space exhaustion. */

import pegged.peg;  // Predefined parser combinators (and, or, literal).

// Left-recursion:
// R <- R '+n' / 'n'
ParseTree R(ParseTree p)
{
    return or!(and!(R, literal!("+n")), literal!("n"))(p);
}

void main()
{
    ParseTree p = { input : "n+n" };
    auto result = R(p);
    import std.stdio;
    writeln(result);
}

// rdmd -I../../.. ../../../libpegged.a left_recursion_naive.d
