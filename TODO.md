Pegged TODO:
============

TODO: when a rule is templated, both the string and the parse string version must be templated
      Add a ,T : ParseTree)(T p) or ,T : string)(T p) at the end of the name 
      For the string case: 
    static ParseTree rule(alias b, T : string)(T p)
    {
        memo = null;        
        ParseTree result = named!(code, "rule")(ParseTree("",false,[],p));
        memo[tuple("rule",0)] = result;
    }

TODO: The same for parameterized grammars. opCall must be modified.
TODO: updates doc to explain spacing can be user-defined (spaceAnd will call it)
TODO: add an enum inside ParseTree's, containing the rules's name, to enable final switch selection
TODO: qualified names for rules (grammarName.ruleName)
TODO: parameterize the grammars on a ParseTree type
TODO: inlining
TODO: modify the gen_grammar to regenerate correctly the new engine
TODO: modify the makefile
TODO: fuse with the master branch
TODO: grammar introspection: grammar name, rule names, call graph, 

