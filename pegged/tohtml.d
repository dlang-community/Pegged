module pegged.tohtml;

import std.stdio;
import std.conv;
import std.algorithm.searching;
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
code.tooltip {
    position: relative;
    font-family: monospace;
    white-space: pre;
 }
/* hide tooltip */
code.tooltip span {
    display: none;
}
/* show and style tooltip */
code.tooltip:hover span {
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
    z-index: 1;
}
details {
    margin-left:25px;
    white-space: nowrap;
}
details.leaf summary::-webkit-details-marker {
    opacity: 0;
}
</style>
</head>
<body>
    `);

    string treeToHTML(const ref ParseTree p)
    {
        auto firstNewLine = p.input[p.begin .. p.end].countUntil('\n');
        string summary = p.name ~ " " ~ to!string([p.begin, p.end]) ~
            ` <code class="tooltip">` ~ p.input[p.begin .. firstNewLine >= 0 ? p.begin + firstNewLine : p.end] ~
            "<span><pre>" ~ p.input[p.begin .. p.end] ~ "</pre></span></code>";
        string result = "<details" ~ (p.children.length == 0 ? ` class="leaf"` : "") ~ "><summary>" ~ summary ~ "</summary>\n";
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
    if (filename.endsWith(".html", ".htm") == 0)
        filename ~= ".html";
    toHTML(p, File(filename, "w"));
}
