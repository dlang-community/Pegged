Using the Parse Tree
====================

**Pegged** parse trees are described here: [[Parse Trees]].

As you could see, there are a deliberately simple structure. There are no multiple types to encode AST nodes and whatnots. Since D has wonderful compile-time evaluation capabilities and code can be mixed-in, **Pegged** has all the power it needs: parse trees are structs, they are easy to use at compile-time (classes are a bit trickier for now, so I chose structs): you can iterate on them, search for specific nodes, delete nodes or simplify them, etc.

See [[Semantic Actions]], [[User-Defined Parsers]] and [[Generating Code]] for more on this.

```
auto p = Arithmetic(input);
writeln(p.matches);
foreach(ref child; p.children)
    doSomething(child);
```

Since parse trees are run-of-the-mill N-ary trees, I didn't add tree-walking algorithms in **Pegged**, but I may do so to group useful tree functions with the related structure:

- searching for all nodes with a particular name
- getting the leaves (parse trees with no children)
- filtering a tree: cutting the nodes that do not obey a particular predicate.
- modifying a tree.

I'll see. You can find tree-related algorithms in another Github project of mine: https://github.com/PhilippeSigaud/dranges (see for example https://github.com/PhilippeSigaud/dranges/blob/master/treerange.d)


Transforming Wiki Syntax into LaTeX Code
----------------------------------------

Once you have a parse tree, an external function can be called to transform the tree into user-defined input. Say for example we want a parser to transform a wiki-like syntax into LaTeX code:

* From `===Title===` to `\section{Title}`

* From `==Title==` to `\subsection{Title}`

* From `_text_` to `\emph{text}`

* From `* Text1 (LF) * Text2 (LF) * Text3(LF)` to `begin{itemize}\item Text1 \item Text2 \item Text3 \end{itemize}`

And so on... First, let's define a small grammar for such a wiki syntax:

```d
Wiki:
    Document <  Element*
    Element  <  Section
            / Subsection
            / Emph
            / List
            / Text

    Section     <  :SEC    Title :SEC
    Subsection  <  :SUBSEC Title :SUBSEC
    Title       <~ (!(SUBSEC / SEC) .)+
    Emph        <  :EMPH Text :EMPH
    List        <  ListElement+
    ListElement <- :LIST :Spacing (!endOfLine .)+ endOfLine
    Text        <~ (!MARKUP .)+

    MARKUP <  SUBSEC / SEC / EMPH / LIST
    SEC    <  "==="
    SUBSEC <  "=="
    EMPH   <  "_"
    LIST   <  "*"
```

A wiki document is a bunch of elements, which can be any of (sections, subsection, ...). I put the markup elements in a different part of the grammar instead of inlining them to allow for an easier extension: if you decide that lists can be marked by `+` and `-` also, just change `LIST` to:

```
LIST <- "*" / "+" / "-"
```

The previous grammar uses some **pegged**-specific PEG extensions showed here: [[Extended PEG Syntax]], to simplify the output. For example, the short `< ` arrow is just to have **Pegged** create a space-consuming sequence: it will silently consume and drop any blank char before, in-between and after rules.

I'll use the following input:

```d
enum input = "
=== This is the Title===

A _very_ important introductory text.

== A nice small section ==

And some text.

== Another section ==

And a list:

* A
* B
";
```

Used on `input`, `Wiki` gives the following parse tree (just do `writeln(Wiki(input));`:

```
Wiki  [0, 154]["This is the Title", "A", "very", "important introductory text.", "A nice small section", "And some text.", "Another section", "And a list:", "A", "\n", "B", "\n"]
 +-Wiki.Document  [0, 154]["This is the Title", "A", "very", "important introductory text.", "A nice small section", "And some text.", "Another section", "And a list:", "A", "\n", "B", "\n"]
    +-Wiki.Element  [1, 27]["This is the Title"]
    |  +-Wiki.Section  [1, 27]["This is the Title"]
    |     +-Wiki.Title  [5, 22]["This is the Title"]
    +-Wiki.Element  [27, 29]["A"]
    |  +-Wiki.Text  [27, 28]["A"]
    +-Wiki.Element  [29, 36]["very"]
    |  +-Wiki.Emph  [29, 36]["very"]
    |     +-Wiki.Text  [30, 34]["very"]
    +-Wiki.Element  [36, 66]["important introductory text."]
    |  +-Wiki.Text  [36, 64]["important introductory text."]
    +-Wiki.Element  [66, 94]["A nice small section"]
    |  +-Wiki.Subsection  [66, 94]["A nice small section"]
    |     +-Wiki.Title  [69, 89]["A nice small section"]
    +-Wiki.Element  [94, 110]["And some text."]
    |  +-Wiki.Text  [94, 108]["And some text."]
    +-Wiki.Element  [110, 133]["Another section"]
    |  +-Wiki.Subsection  [110, 133]["Another section"]
    |     +-Wiki.Title  [113, 128]["Another section"]
    +-Wiki.Element  [133, 146]["And a list:"]
    |  +-Wiki.Text  [133, 144]["And a list:"]
    +-Wiki.Element  [146, 154]["A", "\n", "B", "\n"]
       +-Wiki.List  [146, 154]["A", "\n", "B", "\n"]
          +-Wiki.ListElement  [146, 150]["A", "\n"]
          +-Wiki.ListElement  [150, 154]["B", "\n"]
```

Now, to generate the wanted LaTeX code, we will switch on the nodes names and use a D inner function to recurse on the tree elements:

```d
string toLaTeX(ParseTree p)
{
    string parseToCode(ParseTree p)
    {
        switch(p.name)
        {
            case "Wiki":
                return parseToCode(p.children[0]); // The grammar result has only child: the start rule's parse tree
            case "Wiki.Document":
                string result = "\\documentclass{article}\n\\begin{document}\n";
                foreach(child; p.children) // child is a ParseTree
                    result ~= parseToCode(child);
                return result ~ "\n\\end{document}\n";
            case "Wiki.Element":
                return parseToCode(p.children[0]); // one child only
            case "Wiki.Section":
                return "\n\\section{"
                       ~ p.matches[0] // the first match contains the title
                       ~ "}\n";
            case "Wiki.Subsection":
                return "\n\\subsection{"
                       ~ p.matches[0] // the first match contains the title
                       ~ "}\n";
            case "Wiki.Emph":
                return " \\emph{"
                       ~ p.matches[0] // the first match contains the text to emphasize
                       ~ "} ";
            case "Wiki.List":
                string result = "\n\\begin{itemize}\n";
                foreach(child; p.children)
                    result ~= parseToCode(child);
                return result ~ "\\end{itemize}\n";
            case "Wiki.ListElement":
                return "\\item " ~ p.matches[0] ~ "\n";
            case "Wiki.Text":
                return p.matches[0];
            default:
                return "";
        }
    }

    return parseToCode(p);
}
```

Using this, `input` is transformed into LaTeX:

```latex
\documentclass{article}
\begin{document}
\section{This is the Title}

A \emph{very} important introductory text.

\subsection{A nice small section }

And some text.

\subsection{Another section }

And a list:

\begin{itemize}
\item A
\item B
\end{itemize}
\end{document}
```

Which is ready to be compiled by LaTeX.

It's all there is to it:

1) Write a grammar

2) Write a tree-to-code function and call it on the grammar output.

3) Success!

In fact, I keep finding myself writing this kind of code. I might be seeing a pattern emerge, there.

More Than Just Pushing Strings
------------------------------

Of course, the tree-walking function can do much more than concatenating strings. It could for example increment a counter each time a certain kind of node is found, transform the tree on-the-fly, make some verifications and comparison between nodes... If [[Semantic Actions]] are examples of *internal* actions (from `ParseTree` to `ParseTree`, used internally during the parsing process), the functions presented here are *external* actions. You can use them to construct any D value from the parse tree.

At Compile-Time?
----------------

What's mightily cool with D is its powerful evaluation capabilities at compile-time. In fact *everything* I presented previously can be done at compile time:

```d
enum p = Wiki(input); // CT-parsing
enum code = toLaTeX(p); // CT-treewalking and transformation
```

I admit that for the previous example, it's in the vast 'fun-but-not-really-useful' category. But just think for a moment: given a flat input, **Pegged** creates a structured entity (the parse tree) at compile-time and offered a transformation on it, at CT also. Which means it can be used to create D code... which can be fluidly mixed in D code. See [[Generating Code]] for more on that.

* * * *

Next lesson: [[Tree Decimation]]

* * * *

[[Pegged Tutorial]]
