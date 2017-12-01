import pegged.peg;  // Predefined parser combinators (and, or, literal).

// Left-recursion:
// R <- R '+n' / 'n'
ParseTree R(ParseTree p)
{
    static ParseTree[size_t /*position*/] prev;
    if (auto s = p.end in prev)               // We are recursing. Don't evaluate R anew
        return *s;                            //     return the memoized result instead.
    ParseTree current = fail(p);              // R_{-1}.
    prev[p.end] = current;                    // Stop on next recursive call. 
    while (true)                              // Controlled loop, try R with increased
    {                                         //      recursion bound.
        ParseTree result = or!(and!(R, literal!("+n")), literal!("n"))(p);
        if (result.end > current.end)         // The match length is growing, continue.
        {
            prev[p.end] = result;             // Memoize R_{n-1} for when we recurse.
            current = result;
        }
        else                                  // Optimum bound exceeded, current is
        {                                     //       the best match.
            prev.remove(p.end);               // Clean up.
            return current;                   // Done.
        }
    }
}

void main()
{
    ParseTree p = { input : "n+n" };
    auto result = R(p);
    import std.stdio;
    writeln(result);
}

// rdmd -I../../.. ../../../libpegged.a left_recursion_proper.d
