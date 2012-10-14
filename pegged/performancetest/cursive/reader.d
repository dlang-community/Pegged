module pegged.performancetest.cursive.reader;

import std.datetime;
import pegged.performancetest.cursive.parser;

void main(string[] args)
{
    import std.stdio, std.file;

    string input = readText(args[1]);

    int N = 100;
    auto time = benchmark!( () => Cursive(input) )(N);

    writeln("Parser 1: ", time[0].to!("msecs",int)/N, " ms to parse test.crs on average.");
}
