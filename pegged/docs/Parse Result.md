Here is the parse tree resulting from the main page grammar:

The grammar is the same as `pegged.examples.arithmetic`.

```
// writeln(Arithmetic("1 + 2 - (3*x-5)*6"));
Arithmetic  [0, 17]["1", "+", "2", "-", "3", "*", "x", "-", "5", "*", "6"]                          
 +-Term  [0, 17]["1", "+", "2", "-", "3", "*", "x", "-", "5", "*", "6"]                             
    +-Factor  [0, 2]["1"]                                                                           
    |  +-Primary  [0, 2]["1"]                                                                       
    |     +-Number  [0, 2]["1"]                                                                     
    +-Add  [2, 6]["+", "2"]                                                                         
    |  +-Factor  [4, 6]["2"]
    |     +-Primary  [4, 6]["2"]
    |        +-Number  [4, 6]["2"]
    +-Sub  [6, 17]["-", "3", "*", "x", "-", "5", "*", "6"]
       +-Factor  [8, 17]["3", "*", "x", "-", "5", "*", "6"]
          +-Primary  [8, 15]["3", "*", "x", "-", "5"]
          |  +-Parens  [8, 15]["3", "*", "x", "-", "5"]
          |     +-Term  [9, 14]["3", "*", "x", "-", "5"]
          |        +-Factor  [9, 12]["3", "*", "x"]
          |        |  +-Primary  [9, 10]["3"]
          |        |  |  +-Number  [9, 10]["3"]
          |        |  +-Mul  [10, 12]["*", "x"]
          |        |     +-Primary  [11, 12]["x"]
          |        |        +-Variable  [11, 12]["x"]
          |        +-Sub  [12, 14]["-", "5"]
          |           +-Factor  [13, 14]["5"]
          |              +-Primary  [13, 14]["5"]
          |                 +-Number  [13, 14]["5"]
          +-Mul  [15, 17]["*", "6"]
             +-Primary  [16, 17]["6"]
                +-Number  [16, 17]["6"]
```

As you can see, even with a simple grammar, a parse tree can become a big beast quite rapidly. See [[Extended PEG Syntax]] to learn how to tell **Pegged** to drop branches from the tree.