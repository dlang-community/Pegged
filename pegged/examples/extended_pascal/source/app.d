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


    version (tracer)
    {
        import std.experimental.logger;
        import std.algorithm : startsWith;
        sharedLog = new TraceLogger("TraceLog.txt");
        bool cond (string ruleName, const ref ParseTree p)
        {
            static startTrace = false;
            //if (ruleName.startsWith("EP.FunctionDeclaration"))
            if (p.begin > 4000)
                startTrace = true;
            return startTrace && ruleName.startsWith("EP");
        }
        // Various ways of turning logging on and of:
        //setTraceConditionFunction(&cond);
        setTraceConditionFunction(function(string ruleName, const ref ParseTree p) {return ruleName.startsWith("EP");});
        //traceAll;
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
