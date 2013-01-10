/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.grammar;
import pegged.examples.markdown;
//import md;

void main()
{
    asModule("md", "md", MarkdownGrammar);

    writeln(Markdown.decimateTree(Markdown.HtmlBlockInTags(
q{<table>
    <tr>
        <td>*one*</td>
        <td>[a link](http://google.com)</td>
    </tr>
</table>
    })));

}

