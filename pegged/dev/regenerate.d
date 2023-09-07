/**
 * Recreate the Pegged parser from the grammar.
 *
 * When called without arguments: generate sources.
 * Otherwise: validate that sources were generated.
 */
module pegged.dev.regenerate;

import pegged.grammar;
import pegged.examples.peggedgrammar;
import pegged.examples.testergrammar;

enum Task {generate, validate}

void main(string[] args)
{
    const task = args.length == 1 ? Task.generate : Task.validate;
    doModule!(Memoization.no) (task, "pegged.parser",              "../parser",              PEGGEDgrammar);
    doModule!(Memoization.yes)(task, "pegged.tester.testerparser", "../tester/testerparser", testerGrammar);
}

void doModule(Memoization withMemo = Memoization.yes)(Task task, string moduleName, string fileName, string grammarString, string optHeader = "")
{
    final switch (task) with (Task)
    {
        case generate:
            asModule!(withMemo)(moduleName, fileName, grammarString, optHeader);
            break;
        case validate:
        {
            import std.file : remove;
            import std.algorithm : equal;
            import std.exception : enforce;
            import std.stdio : File;

            const tempName = "tempfile";
            const tempNameExt = tempName ~ ".d";
            const fileNameExt = fileName ~ ".d";
            const chunk = 1024;
            scope (exit) { remove(tempNameExt); }
            asModule!(withMemo)(moduleName, tempName, grammarString, optHeader);
            enforce(equal(File(fileNameExt).byChunk(chunk), File(tempNameExt).byChunk(chunk)),
                          `"` ~ fileNameExt ~ `" deviates from its grammar. Consult pegged/dev/README.md.`);
            break;
        }
    }
}
