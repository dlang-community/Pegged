Here is the parse tree resulting from the main page grammar:

The grammar is the same than the one in `pegged.examples.arithmetic`.

```d
// Expr.parse("1 + 2 - (3*x-5)*6");
"Parse output: success
named captures: []
position: [index: 17, line: 0, col: 17]
parse tree:
Expr: [[index: 0, line: 0, col: 0] - [index: 17, line: 0, col: 17]]["1", "+", "2", "-", "(", "3", "*", "x", "-", "5", ")", "*", "6"]
  Factor: [[index: 0, line: 0, col: 0] - [index: 2, line: 0, col: 2]]["1"]
      Primary: [[index: 0, line: 0, col: 0] - [index: 1, line: 0, col: 1]]["1"]
          Number: [[index: 0, line: 0, col: 0] - [index: 1, line: 0, col: 1]]["1"]
  AddExpr: [[index: 2, line: 0, col: 2] - [index: 6, line: 0, col: 6]]["+", "2"]
      Factor: [[index: 4, line: 0, col: 4] - [index: 6, line: 0, col: 6]]["2"]
          Primary: [[index: 4, line: 0, col: 4] - [index: 5, line: 0, col: 5]]["2"]
              Number: [[index: 4, line: 0, col: 4] - [index: 5, line: 0, col: 5]]["2"]
  AddExpr: [[index: 6, line: 0, col: 6] - [index: 17, line: 0, col: 17]]["-", "(", "3", "*", "x", "-", "5", ")", "*", "6"]
      Factor: [[index: 8, line: 0, col: 8] - [index: 17, line: 0, col: 17]]["(", "3", "*", "x", "-", "5", ")", "*", "6"]
          Primary: [[index: 8, line: 0, col: 8] - [index: 15, line: 0, col: 15]]["(", "3", "*", "x", "-", "5", ")"]
              Parens: [[index: 8, line: 0, col: 8] - [index: 15, line: 0, col: 15]]["(", "3", "*", "x", "-", "5", ")"]
                  Expr: [[index: 9, line: 0, col: 9] - [index: 14, line: 0, col: 14]]["3", "*", "x", "-", "5"]
                      Factor: [[index: 9, line: 0, col: 9] - [index: 12, line: 0, col: 12]]["3", "*", "x"]
                          Primary: [[index: 9, line: 0, col: 9] - [index: 10, line: 0, col: 10]]["3"]
                              Number: [[index: 9, line: 0, col: 9] - [index: 10, line: 0, col: 10]]["3"]
                          MulExpr: [[index: 10, line: 0, col: 10] - [index: 12, line: 0, col: 12]]["*", "x"]
                              Primary: [[index: 11, line: 0, col: 11] - [index: 12, line: 0, col: 12]]["x"]
                                  Variable: [[index: 11, line: 0, col: 11] - [index: 12, line: 0, col: 12]]["x"]
                      AddExpr: [[index: 12, line: 0, col: 12] - [index: 14, line: 0, col: 14]]["-", "5"]
                          Factor: [[index: 13, line: 0, col: 13] - [index: 14, line: 0, col: 14]]["5"]
                              Primary: [[index: 13, line: 0, col: 13] - [index: 14, line: 0, col: 14]]["5"]
                                  Number: [[index: 13, line: 0, col: 13] - [index: 14, line: 0, col: 14]]["5"]
          MulExpr: [[index: 15, line: 0, col: 15] - [index: 17, line: 0, col: 17]]["*", "6"]
              Primary: [[index: 16, line: 0, col: 16] - [index: 17, line: 0, col: 17]]["6"]
                  Number: [[index: 16, line: 0, col: 16] - [index: 17, line: 0, col: 17]]["6"]
"
```

As you can see, even with a simple grammar, a parse tree can become a big beast quite rapidly. See [[Extended PEG Syntax]] to learn how to tell **Pegged** to drop branches from the tree.