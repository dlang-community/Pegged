Using the Parse Tree
====================

**Pegged** parse trees are described here: [[Parse Trees]].

As you could see, there are a deliberately simple structure. There are not multiple types to encode AST nodes and whatnots. Since D has wonderful compile-time evaluation capabilities and code can be mixed-in, **Pegged** has all the power it needs: parse trees are structs, they are easy to use at compile-time (classes are a bit trickier for now, so I chose structs): you can iterate on them, search for specific nodes, delete nodes or simplify them, etc.

See [[Semantic Actions]], [[User-Defined Parsers]] and [[Generating Code]] for more on this.

An `Output` contains a `ParseTree` member called `parseTree`. Since `Output` uses `alias this`, the parse tree is directly accessible:

```
auto p = Expr.parse(input);
writeln(p.capture); // really p.parseTree.capture
foreach(ref child; p.children) // in truth, we are iterating on p.parseTree.children
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
Document <- Element*
Element  <- Section 
          / Subsection
          / Emph
          / List
          / Text

Section     <- :SEC    Title :SEC
Subsection  <- :SUBSEC Title :SUBSEC
Title       <~ (!(SUBSEC / SEC)>.)*
Emph        <- :EMPH Text :EMPH
List        <- ListElement+
ListElement <- :LIST (!EOL>.)*>EOL
Text        <~ (!MARKUP>.)*

MARKUP <- SUBSEC / SEC / EMPH / LIST
SEC    <- "==="
SUBSEC <- "=="
EMPH   <- "_"
LIST   <- "*"
```

A wiki document is a bunch of elements, which can be any of (sections, subsection, ...). I put the markup elements in a different part of the grammar instead of inlining them to allow for an easier extension: if you decide that list can be marked by `+` and `-` also, just change `LIST` to:

```
LIST <- "*" / "+" / "-"
```

The previous grammar uses some **pegged**-specific PEG extensions showed here: [[Extended PEG Syntax]], to simplify the output.

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

Used on `input`, `Document` gives the following parse tree (just do `writeln(Document.parse(input));`:

```
Document: ["\n", "This is the Title", "\n\nA ", "very", " important introductory text.\n\n", "A nice small section ", "\n\nAnd some text.\n\n", "Another section ", "\n\nAnd a list:\n\n", "A", "\n", "B", "\n"]
  Element: ["\n"]
      Text: ["\n"]
  Element: ["This is the Title"]
      Section: ["This is the Title"]
          Title: ["This is the Title"]
  Element: ["\n\nA "]
      Text: ["\n\nA "]
  Element: ["very"]
      Emph: ["very"]
          Text: ["very"]
  Element: [" important introductory text.\n\n"]
      Text: [" important introductory text.\n\n"]
  Element: ["A nice small section "]
      Subsection: ["A nice small section "]
          Title: ["A nice small section "]
  Element: ["\n\nAnd some text.\n\n"]
      Text: ["\n\nAnd some text.\n\n"]
  Element: ["Another section "]
      Subsection: ["Another section "]
          Title: ["Another section "]
  Element: ["\n\nAnd a list:\n\n"]
      Text: ["\n\nAnd a list:\n\n"]
  Element: ["A", "\n", "B", "\n"]
      List: ["A", "\n", "B", "\n"]
          ListElement: ["A", "\n"]
          ListElement: ["B", "\n"]
```

Now, to generate the wanted LaTeX code, we will switch on the nodes names and use a D inner function to recurse on the tree elements:

```d
string toLaTeX(Output o)
{
    string parseToCode(ParseTree p)
    {
        switch(p.ruleName)
        {
            case "Document":
                string result = "\\documentclass{article}\n\\begin{document}";
                foreach(child; p.children) // child is a ParseTree
                    result ~= parseToCode(child);
                return result ~ "\n\\end{document}\n";
            case "Element":
                return parseToCode(p.children[0]); // one child only
            case "Section":
                return "\\section{" 
                       ~ p.capture[0] // the capture contains the title
                       ~ "}";
            case "Subsection":
                return "\\subsection{" 
                       ~ p.capture[0] // the capture contains the title
                       ~ "}"; 
            case "Emph":
                return "\\emph{" 
                       ~ p.capture[0] // the capture contains the text to emphasize
                       ~ "}";
            case "List":
                string result = "\\begin{itemize}\n";
                foreach(child; p.children)
                    result ~= parseToCode(child);
                return result ~ "\\end{itemize}";
            case "ListElement":
                return "\\item " ~ p.capture[0] ~ "\n";
            case "Text":
                return p.capture[0];
            default:
                return "";
        }
    }

    return parseToCode(o.parseTree);
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

This is all there is to it:

1) Write a grammar

2) Write a tree-to-code function and call it on the grammar output.

3) Success!

In fact, I keep finding myself writing this kind of code. I might be seeing a pattern emerge, there.

More Than Just Pushing Strings
------------------------------

Of course, the tree-walking function can do much more than concatenating strings. It could for example increment a counter each time a certain kind of node is found.

If [[Semantic Actions]] are examples of *internal* actions (from `Output` to `Output`, used internally during the parsing process), the functions presented here are *external* actions. You can use them to construct any D value from the parse tree. 

At Compile-Time?
----------------

What's mightily cool with D is its powerful evaluation capabilities at compile-time. In fact *everything* I presented previously can be done at compile time:

```d
enum p = Document.parse(input); // CT-parsing
enum code = toLaTeX(p); // CT-treewalking and transformation
```

I admit that for the previous example, it's in the vast 'fun-but-not-really-useful' category. But just think for a moment: given a flat input, **Pegged** create a structured entity (the parse tree) at compile-time and offered a transformation on it, at CT also. Which means it can be used to create D code... which can be fluidly mixed in D code. See [[Generating Code]] for more on that.

* * * *

Next lesson: [[Tree Decimation]]

* * * *

[[Pegged Tutorial]]