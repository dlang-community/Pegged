module pegged.tohtml;

import std.stdio;
import std.conv;
import pegged.peg;

void toHTML(const ref ParseTree p, File file)
{
    file.write(`
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Pegged produced parse tree</title>
<style>
a.tooltip {
    position: relative;
    font-family: monospace;
 }
/* hide tooltip */
a.tooltip span {
    display: none;
}
/* show and style tooltip */
a.tooltip:hover span {
    /* show tooltip */
    display: block;
    /* position relative to container div.tooltip */
    position: absolute;
    top: -1.6em;
    left: -0.6em;
    background: #ffffff;
    padding: 0.5em;
    color: #000000;
    border: 0.1em solid #b7ddf2;
    border-radius: 0.5em;
    white-space: nowrap;
    z-index : 1;
}
details, div {
    margin-left:25px;
}
</style>
</head>
<body>
    `);

    string treeToHTML(const ref ParseTree p)
    {
        import std.algorithm.comparison;
        import std.algorithm.searching;
        string summary = p.name ~ " " ~ to!string([p.begin, p.end]);
        auto firstNewLine = p.input[p.begin .. p.end].countUntil('\n');
        if (p.begin != p.end)
            summary ~= ` <a class="tooltip">` ~ p.input[p.begin .. firstNewLine >= 0 ? p.begin + firstNewLine : p.end] ~
                       "<span><pre>" ~ p.input[p.begin .. p.end] ~ "</pre></span></a>";
        if (p.children.length == 0)
            return "<div>" ~ summary ~ "</div>\n";
        string result = "<details><summary>" ~ summary ~ "</summary>\n";
        foreach (child; p.children)
            result ~= treeToHTML(child);
        return result ~ "</details>\n";
    }

    file.write(treeToHTML(p));

    file.write(`
</body>
</html>
    `);
}

void toHTML(const ref ParseTree p, string filename)
{
    import std.algorithm.searching;
    if (filename.endsWith(".html", ".htm") == 0)
        filename ~= ".html";
    toHTML(p, File(filename, "w"));
}
