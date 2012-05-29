import std.stdio : writeln;
import std.file : readText, write;
import std.conv;
import pegged.grammar;

int main (string[] args)
{
	if (args.length < 2 || args.length > 3) {
		writeln("Usage: ", args[0], " <input> [output]");
		return 1;
	}

	dstring header = "import pegged.peg; public import pegged.peg : ParseTree;";

	if (args.length == 3) {
		write(args[2], cast (void[]) (header ~ grammar(readText(args[1]).to!dstring)));
	}
	else {
		writeln(header ~ grammar(readText(args[1]).to!dstring));
	}

	return 0;
}
