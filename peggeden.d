import std.stdio : writeln;
import std.file : readText, write;
import std.getopt;
import std.conv;
import pegged.grammar;

int main (string[] args)
{
	if (args.length < 2 || args.length > 3) {
		writeln("Usage: ", args[0], " <input> [output]");
		return 1;
	}

	bool          check         = false;
	ReduceFurther reduceFurther = ReduceFurther.No;
	dstring       header        = "import pegged.peg; public import pegged.peg : ParseTree;";

	getopt(args, config.noPassThrough,
		"check|c",  &check,
		"reduce|r", &reduceFurther,
		"header|h", &header);

	if (check) {
		auto d   = checkGrammar(readText(args[1]).to!dstring);
		bool any = false;

		writeln("");

		foreach (rule, loop; d.infiniteLoops) {
			final switch (loop) {
				case InfiniteLoop.NoLoop:
					continue;

				case InfiniteLoop.MightConsumeNothing:
					writeln(rule, " might consume nothing.");
					break;

				case InfiniteLoop.PossibleInfiniteLoop:
					writeln(rule, " has a possible infinite loop.");
					break;

				case InfiniteLoop.Undecided:
					writeln(rule, " might have some issues.");
					break;
			}

			any = true;
		}

		if (!any) {
			writeln("There are no infinite loops.");
		}

		writeln("");

		any = false;
		foreach (rule, recursion; d.leftRecursions) {
			final switch (recursion) {
				case LeftRecursion.NoLeftRecursion:
					continue;

				case LeftRecursion.DirectLeftRecursion:
					writeln(rule, " has a direct left recursion");
					break;

				case LeftRecursion.IndirectLeftRecursion:
					writeln(rule, "has an indirect left recursion");
					break;

				case LeftRecursion.HiddenLeftRecursion:
					writeln(rule, " has a hidden left recursion");
					break;

				case LeftRecursion.Undecided:
					writeln(rule, " might have some issues");
					break;
			}

			any = true;
		}

		if (!any) {
			writeln("There's no left recursion.");
		}

		return 0;
	}

	if (args.length == 3) {
		write(args[2], cast (void[]) (header ~ grammar(readText(args[1]).to!dstring)).to!string);
	}
	else {
		writeln((header ~ grammar(readText(args[1]).to!dstring)).to!string);
	}

	return 0;
}
