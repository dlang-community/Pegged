Grammar Testing
===================

Warning: The syntax and interface for this are still very new and are thus
likely to receive design changes in the future.

Grammars can be tested using the `GrammarTester` class in the `pegged.grammartester` module.  Here is an example:

```d
import pegged.grammar;
import pegged.grammartester;

mixin(grammar(`
Arithmetic:
    Term     < Factor (Add / Sub)*
    Add      < "+" Factor
    Sub      < "-" Factor
    Factor   < Primary (Mul / Div)*
    Mul      < "*" Primary
    Div      < "/" Primary
    Primary  < Parens / Neg / Number / Variable
    Parens   < :"(" Term :")"
    Neg      < "-" Primary
    Number   < ~([0-9]+)
    Variable <- identifier
`));

unittest
{
	// Instantiate a GrammarTester using the Arithmetic grammar and perform
	//   all tests by trying to match the "Term" nonterminal symbol.
	auto arithmeticTester = new GrammarTester!(Arithmetic, "Term");

	arithmeticTester.assertSimilar(`1 + 3`,
		`
		Term
		[
			Factor->Primary->Number
			Add~>
				Factor->Primary->Number
		]
		`);
}
```

In this case, the Arithmetic grammar from the beginning of the tutorial is tested.  The expression to be parsed by the Arithmetic parser is given as the first argument to assertSimilar, and the decimated tree we expect to see from it is given as the second argument.  

The Grammar for Testing Grammars
--------------------------------

The grammar for the string used to state an expected parse tree is made of the names of nonterminal symbols (node names) connected together with brackets and arrows used to describe the parent-child relationships.  These have the following meanings:

```
lhs->rhs                  Ordered Branch: the left-hand-side's first child matches the right-hand-side.
lhs~>rhs                  Unordered Branch: the left-hand-side has at least one child that matches the right-hand-side.
lhs[child ...children...] Ordered Branch: the left-hand-side has the given children, in the given order.
lhs{child ...children...} Unordered Branch: the left-hand-side has the given children, and in any order.
/Child                    Dereferences the given node in a shallow manner.  Any subsequent arrows will branch from the parent, and not from Child.
```

The `/` operator can be understood as follows:

```
Foo->Bar->Baz    Foo is a parent of Bar.  Bar is a parent of Baz.
Foo->Bar->/Baz   ditto
Foo->/Bar->Baz   Foo is a parent of Bar and Baz.
Foo->/Bar->/Baz  ditto
```

The `/` operator makes it possible to mix ordered and unordered branches.  This is illegal and will cause test cases to fail.  This restriction is in place due to the lack of an intuitive way to count children in places where this occurs: an unordered branch may or may not "steal" a child required for an ordered branch to match successfully.

Here is another example:

```d

	arithmeticTester.assertSimilar(`1*2 + 3/4`,
		`
		Term
		[
			Factor
			[
				Primary->Number
				Mul~>
				Primary->Number
			]
			Add~>
			Factor
			[
				Primary->Number
				Div~>
				Primary->Number
			]
		]
		`);
```

The above example demonstrates how the choice of arrows and braces allows deep trees to be described without introducing a large number of indentation or nesting levels.

When Tests Fail
---------------

If the earlier test were to be written like this:
```d
	arithmeticTester.assertSimilar(`1 + 3`,
		`
		Term
		[
			Factor->Primary->Number
			Add~>
				Factor->Primary->Number->Bug
		]
		`);
```

Then an assertion would be triggered that would print something similar to the following:

```
core.exception.AssertError@pegged/grammartester.d(52): 
  Nodes Found              |    Nodes Expected
                           |
Arithmetic.Term            =  Term
  Arithmetic.Factor        =    Factor
    Arithmetic.Primary     =      Primary
      Arithmetic.Number    =        Number
  Arithmetic.Add           =    Add
    Arithmetic.Factor      =      Factor
      Arithmetic.Primary   =        Primary
        Arithmetic.Number  =          Number
                           >            Bug
```

This output makes it clear which part of the test case failed.

----------

[[Pegged Tutorial]]

[[Home]]