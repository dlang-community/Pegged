module pegged.tester.main;

import std.stdio;

// Import the modules with unit tests.
import pegged.grammar,
       pegged.peg;

int main()
{
    // This main function implicitly triggers unit tests
    // of all loaded modules. This is its sole purpose.
    version (unittest)
    {
        writeln("All unit tests passed!");
        return 0;
    }
    else
    {
        // The build system doesn't actually let this
        // happen, but just in case someone compiles it
        // in an awkward way...
        writeln("Not compiled with unit tests; no tests run.");
        return 1;
    }
}
