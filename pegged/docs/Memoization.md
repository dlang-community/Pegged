Memoization
===========

**Pegged** uses memoization to remember previous parse attempts: every time a rule is called at a particular
input position, the parser looks if it was already called here. If yes, the previous result is used. If not,
the parser uses the rule and then stores the resulting parse tree (be it successful or not: it's useful to remember
past failures too!).

As Bryan Ford showed in the original article, this makes a PEG linear in input length, at the cost of a higher demand
on memory. Tests carried out during the memoization implementation for **Pegged** showed a speed increase up to 40% while
using memoization and *never* showed a speed decrease (as could be feared from the frequent access to the underlying
associative array). No test was carried out on huge inputs (10.000 lines-of-text files, for example).

There is a catch, though: testing different memoization implementations, the most efficient one was found to be D's built-in
associative arrays, which is what is used by **Pegged** right now. But AAs do not work at compile-time.
So right now, memoization is disabled by default to allow **Pegged** parsers to function at compile-time.

If you are sure the generated parser will be used only at runtime, use the `Memoization.yes` option while generating the grammar:

```d
mixin(grammar!(Memoization.yes)(myGrammar));
```

This option also exists for `asModule`:

```d
asModule!(Memoization.yes)("mymod", myGrammar);
```

Future Extensions
-----------------

A possible future extension is to have the memoization option selected on whether the parsing is attempted at compile-time or runtime.
This means inserting a `if (__ctfe)` branch everywhere. Its influence on parsing speed remains to be tested.


Next step: [[Grammars as D Modules]]

* * * *

[[Pegged Tutorial]]
