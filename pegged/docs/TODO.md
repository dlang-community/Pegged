TODO
====


New parsers
-----------

- Extracting the useful parsers from `pegged.examples.json` (strings) and `pegged.examples.numbers` (numbers) and put them in a new `pegged.predefined` module.

- Add predefined parametrized rules in `pegged.predefined`: `List(Elem, Sep)`, `But(Pred)`, `Until(Pred)`, `Sequence(Begin, Elem, Sep, End) <- Begin List(!End Elem, Sep) End` and arrays/strings/parenthesis lists. Also comments (line comments and nested comments)

Infrastructure
--------------

- (DONE) Add a BeginPos and EndPos member to any parse result (line/col)

- Make rule names enumerated results to use `final switch` on them

- (DONE) Use indices internally

- Autosimplification of the parse tree: for any parse result with only one child, cut one level

- Grammar optimization: inlining, for example

- parse `wstring`s and `dstring`s

- (PARTIALLY DONE) Add unit-tests. I tend to write docs and unittests as I go, D making that so easy. **Pegged** is an exception, which I'll kill with extreme prejudice.

- Directly reading a grammar from a file.

- (DONE, sort of) A better error reporting would be good. Maybe with threading some error stack.

- Better rules to control the parse tree: right now, the 'fuse' (~) and 'discard' (:) rules are there. Maybe something to tell the grammar to simplify a branch or using a policy for the parse tree construction and the level of simplification before presenting it to the user.

- (DONE) A way to indicate rule-level space sensitivity.

- (DONE, but differently) Right now, grammars are 'open' in that you cannot defined multiple rules with the same name in the same module. Use D modules to, well, do modularization of your code. But I intend to put different levels of 'openness'. See [[Four Levels of Encapsulation]]. You can use qualified identifiers for the rules names, though.

Documentation
-------------

- Explain how to change the **Pegged** grammar

- Explain the D grammar

- (DONE) Explain how to do compile-time parsing and tree-walking

- (DONE) Example on how to generate code/string

