Here is the parse tree resulting from the main page grammar:

The grammar is the same than the one in `pegged.examples.arithmetic`.

```
// Expr.parse("1 + 2 - (3*x-5)*6");
Parse output: success
named captures: []
position: [index: 17, line: 0, col: 17]
parse tree:
Arithmetic.Expr: [[index: 0, line: 0, col: 0] - [index: 17, line: 0, col: 17]]["1", "+", "2", "-", "(", "3", "*", "x", "-", "5", ")", "*", "6"]
  Arithmetic.Factor: [[index: 0, line: 0, col: 0] - [index: 2, line: 0, col: 2]]["1"]
      Arithmetic.Primary: [[index: 0, line: 0, col: 0] - [index: 1, line: 0, col: 1]]["1"]
          Arithmetic.Number: [[index: 0, line: 0, col: 0] - [index: 1, line: 0, col: 1]]["1"]
  Arithmetic.AddExpr: [[index: 2, line: 0, col: 2] - [index: 6, line: 0, col: 6]]["+", "2"]
      Arithmetic.Lit!(+): [[index: 2, line: 0, col: 2] - [index: 3, line: 0, col: 3]]["+"]
      Arithmetic.Factor: [[index: 4, line: 0, col: 4] - [index: 6, line: 0, col: 6]]["2"]
          Arithmetic.Primary: [[index: 4, line: 0, col: 4] - [index: 5, line: 0, col: 5]]["2"]
              Arithmetic.Number: [[index: 4, line: 0, col: 4] - [index: 5, line: 0, col: 5]]["2"]
  Arithmetic.AddExpr: [[index: 6, line: 0, col: 6] - [index: 17, line: 0, col: 17]]["-", "(", "3", "*", "x", "-", "5", ")", "*", "6"]
      Arithmetic.Lit!(-): [[index: 6, line: 0, col: 6] - [index: 7, line: 0, col: 7]]["-"]
      Arithmetic.Factor: [[index: 8, line: 0, col: 8] - [index: 17, line: 0, col: 17]]["(", "3", "*", "x", "-", "5", ")", "*", "6"]
          Arithmetic.Primary: [[index: 8, line: 0, col: 8] - [index: 15, line: 0, col: 15]]["(", "3", "*", "x", "-", "5", ")"]
              Arithmetic.Expr: [[index: 9, line: 0, col: 9] - [index: 14, line: 0, col: 14]]["3", "*", "x", "-", "5"]
                  Arithmetic.Factor: [[index: 9, line: 0, col: 9] - [index: 12, line: 0, col: 12]]["3", "*", "x"]
                      Arithmetic.Primary: [[index: 9, line: 0, col: 9] - [index: 10, line: 0, col: 10]]["3"]
                          Arithmetic.Number: [[index: 9, line: 0, col: 9] - [index: 10, line: 0, col: 10]]["3"]
                      Arithmetic.MulExpr: [[index: 10, line: 0, col: 10] - [index: 12, line: 0, col: 12]]["*", "x"]
                          Arithmetic.Lit!(*): [[index: 10, line: 0, col: 10] - [index: 11, line: 0, col: 11]]["*"]
                          Arithmetic.Primary: [[index: 11, line: 0, col: 11] - [index: 12, line: 0, col: 12]]["x"]
                              Arithmetic.Variable: [[index: 11, line: 0, col: 11] - [index: 12, line: 0, col: 12]]["x"]
                  Arithmetic.AddExpr: [[index: 12, line: 0, col: 12] - [index: 14, line: 0, col: 14]]["-", "5"]
                      Arithmetic.Lit!(-): [[index: 12, line: 0, col: 12] - [index: 13, line: 0, col: 13]]["-"]
                      Arithmetic.Factor: [[index: 13, line: 0, col: 13] - [index: 14, line: 0, col: 14]]["5"]
                          Arithmetic.Primary: [[index: 13, line: 0, col: 13] - [index: 14, line: 0, col: 14]]["5"]
                              Arithmetic.Number: [[index: 13, line: 0, col: 13] - [index: 14, line: 0, col: 14]]["5"]
          Arithmetic.MulExpr: [[index: 15, line: 0, col: 15] - [index: 17, line: 0, col: 17]]["*", "6"]
              Arithmetic.Lit!(*): [[index: 15, line: 0, col: 15] - [index: 16, line: 0, col: 16]]["*"]
              Arithmetic.Primary: [[index: 16, line: 0, col: 16] - [index: 17, line: 0, col: 17]]["6"]
                  Arithmetic.Number: [[index: 16, line: 0, col: 16] - [index: 17, line: 0, col: 17]]["6"]
```

As you can see, even with a simple grammar, a parse tree can become a big beast quite rapidly. See [[Extended PEG Syntax]] to learn how to tell **Pegged** to drop branches from the tree.