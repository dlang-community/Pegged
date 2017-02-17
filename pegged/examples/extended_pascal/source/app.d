import std.stdio;
import std.file;
import std.getopt;
import std.path;
import epparser;

bool html, text;

int main(string[] args)
{
    auto helpInformation = getopt(
        args,
        "text", "Generate text output (default)", &text,
        "html", "Generate HTML5 output", &html);    // enum

    if (helpInformation.helpWanted || args.length < 2)
    {
        defaultGetoptPrinter("An Extended Pascal parser.\n" ~
                             "Usage: " ~ args[0] ~ " [options] <example.pas>\n" ~
                             "Options:",
            helpInformation.options);
        return 1;
    }

    auto parseTree = EP(readText(args[1]));

    if (html) {
        import pegged.tohtml;
        toHTML(parseTree, stripExtension(args[1]));
    }
    if (text || !html) {
        parseTree.toString.toFile(stripExtension(args[1]) ~ ".txt");
    }

    return parseTree.successful;
}
