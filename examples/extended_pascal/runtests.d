// rdmd runtests

void main()
{
    import std.process: execute;

    const txt = execute(["dub", "run", "--", "example.pas"]);
    assert (txt.status == 0, txt.output);
    compareFiles("example.txt", "output/example.txt");

    const html = execute(["dub", "run", "--", "--html", "example.pas"]);
    assert (html.status == 0, html.output);
    compareFiles("example.html", "output/example.html");
}

void compareFiles(string a, string b)
{
    import std.exception: enforce;
    import std.stdio: File;
    import std.algorithm: equal;

    const chunk = 1024;
    enforce(equal(File(a).byChunk(chunk), File(b).byChunk(chunk)),
            `"` ~ a ~ `" differs from "` ~ b ~ `"`);
}
