module pegged.examples.parametrized;

import pegged.grammar;

/**
 * Example of parameterized rules. Pick and chose the ones you need.
 */
enum parameterizedExamples = `

# Standard list: skip spaces, drop the separator
# Minimum one Elem
List(Elem, Sep) < Elem (:Sep Elem)* 

# A list accepting 0 element as a valid input
List0(Elem, Sep) < List(Elem, Sep)?

# Standard comma-separated list
CommaList(Elem) < List(Elem, ',')

# Standard space-separated list
SpaceList(Elem) < List(Elem, ' ')

# Standard array rule: [1, 2, ... ]. Concatenates the entire match
Array(Elem) <~ '[' CommaList(Elem) ']'


# Apply Pattern until End
Until(Pattern, End) <- (!End Pattern)* End

# Everything but the End marker. Concatenates the entire match
But(End) <~ Until(., End)

# Input delimited by a begin and a close marker
Delimited(Begin, Close) <- Begin But(Close)

# Standard double-quoted string
String <- Delimited(DoubleQuote, DoubleQuote)

# Line (everything to the end of the line)
Line <- Delimited(Eps, EOL)

# Line comment
LineComment <- Delimited("//", EOL)
`));