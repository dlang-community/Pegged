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
code {
    position: relative;
    font-family: monospace;
    white-space: pre;
}
code.failure {
    color: red;
}
/* hide tooltip */
code span {
    display: none;
}
/* show and style tooltip */
code:hover span {
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

    void treeToHTML(const ref ParseTree p)
    {
        file.write("<details");
        if (p.children.length == 0)
            file.write(` class="leaf"`);
        file.write("><summary>", p.name, " ", to!string([p.begin, p.end]));

        if (p.children.length == 0 && !p.successful)
        {   // Failure leaf.
            Position pos = position(p);
            auto left = pos.index < 10 ? p.input[0 .. pos.index] : p.input[pos.index-10 .. pos.index];
            auto right = pos.index + 10 < p.input.length ? p.input[pos.index .. pos.index + 10] : p.input[pos.index .. $];
            file.write(" failure at line ", to!string(pos.line), ", col ", to!string(pos.col), ", ");
            if (left.length > 0)
                file.write("after <code>", left.stringified, "</code> ");
            file.write("expected <code>");
            if (p.matches.length > 0)
                file.write(p.matches[$-1].stringified);
            else
                file.write("NO MATCH");
            file.write(`</code>, but got <code class="failure">`, right.stringified, "</code>\n");
        }
        else
        {
            auto firstNewLine = p.input[p.begin .. p.end].countUntil('\n');
            file.write(" <code");
            if (!p.successful)
                file.write(` class="failure"`);
            file.write(">", p.input[p.begin .. firstNewLine >= 0 ? p.begin + firstNewLine : p.end],
                        "<span><pre>", p.input[p.begin .. p.end], "</pre></span></code>");
        }

        file.write("</summary>\n");
        foreach (child; p.children)
            treeToHTML(child);
        file.write("</details>\n");
    }

    treeToHTML(p);

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
