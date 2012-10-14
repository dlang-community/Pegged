Here is the parse tree resulting from the main page grammar:

The grammar is the same as `pegged.examples.arithmetic`.

```
// writeln(Arithmetic("1 + 2 - (3*x-5)*6"));
Arithmetic  [0, 17]["1", "+", "2", "-", "(", "3", "*", "x", "-", "5", ")", "*", "6"]
 +-Arithmetic.Term  [0, 17]["1", "+", "2", "-", "(", "3", "*", "x", "-", "5", ")", "*", "6"]
    +-Arithmetic.Factor  [0, 2]["1"]
    |  +-Arithmetic.Primary  [0, 1]["1"]
    |     +-Arithmetic.Number  [0, 1]["1"]
    +-Arithmetic.Add  [2, 6]["+", "2"]
    |  +-Arithmetic.Factor  [4, 6]["2"]
    |     +-Arithmetic.Primary  [4, 5]["2"]
    |        +-Arithmetic.Number  [4, 5]["2"]
    +-Arithmetic.Sub  [6, 17]["-", "(", "3", "*", "x", "-", "5", ")", "*", "6"]
       +-Arithmetic.Factor  [8, 17]["(", "3", "*", "x", "-", "5", ")", "*", "6"]
          +-Arithmetic.Primary  [8, 15]["(", "3", "*", "x", "-", "5", ")"]
          |  +-Arithmetic.Parens  [8, 15]["(", "3", "*", "x", "-", "5", ")"]
          |     +-Arithmetic.Term  [9, 14]["3", "*", "x", "-", "5"]
          |        +-Arithmetic.Factor  [9, 12]["3", "*", "x"]
          |        |  +-Arithmetic.Primary  [9, 10]["3"]
          |        |  |  +-Arithmetic.Number  [9, 10]["3"]
          |        |  +-Arithmetic.Mul  [10, 12]["*", "x"]
          |        |     +-Arithmetic.Primary  [11, 12]["x"]
          |        |        +-Arithmetic.Variable  [11, 12]["x"]
          |        +-Arithmetic.Sub  [12, 14]["-", "5"]
          |           +-Arithmetic.Factor  [13, 14]["5"]
          |              +-Arithmetic.Primary  [13, 14]["5"]
          |                 +-Arithmetic.Number  [13, 14]["5"]
          +-Arithmetic.Mul  [15, 17]["*", "6"]
             +-Arithmetic.Primary  [16, 17]["6"]
                +-Arithmetic.Number  [16, 17]["6"]
```

As you can see, even with a simple grammar, a parse tree can become a big beast quite rapidly. See [[Extended PEG Syntax]] to learn how to tell **Pegged** to drop branches from the tree.
