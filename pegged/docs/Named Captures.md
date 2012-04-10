Named Captures
==============

An expression's parse tree can be named and stored in a special structure inside any `Input`/`Output`, accessible as `namedCaptures`. The synax is `e=name`:

```d
mixin(grammar("
Email <- QualifiedIdentifier = name '@' QualifiedIdentifier = domain
"));
```

`namedCaptures` then acts as an associative array:

```d
enum p = Email.parse("john.doe@example.org");

assert(p.namedCaptures["name"] == "john.doe");
assert(p.namedCaptures["domain"] == "example.org");
```

`namedCaptures` is (by design) a set: if there is more than one rule with the same name, they will overwrite each other in `namedCaptures`. It's not yet a stack, I plan to add the `=` (just '=' by itself) operator to push anonymous parse trees and maybe a `@` operator to pop and compare. However, see [[Semantic Actions]] to do the same thing and much more.

Also, there is a more general way to name the parse trees: the rules themselves. After all, that's what's PEG are all about.

```d
mixin(grammar("
Email <- Name '@' Domain
Name <- QualifiedIdentifier
Domain <- QualifiedIdentifier
"));

auto p = Email.parse("john.doe@example.org");

writeln(p);
/**
Email: ["john.doe", "example.org"]
    Name: ["john.doe"]
    Domain: ["example.org"]*/
```

The parse tree contains named nodes, storing the rules that were used to parse the input. In the previous example, `Email` has two children, a `Name` node and a `Domain` node.

So, my advice: for simple, regex-like cases, use named captures and the `=name` operator. But if you need more than a handful of names or need to use a particular name more than one time, use standard PEG rules. That's what they are for.


* * * *

Next lesson: [[Semantic Actions]]

* * * *

[[Pegged Tutorial]]