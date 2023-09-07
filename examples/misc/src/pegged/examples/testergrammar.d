module pegged.examples.testergrammar;

enum string testerGrammar = `
TesterGrammar:

Root < Node eoi

Node <
    / :'^' identifier
    / identifier (%Branch)*

Branch <
    / OrderedBranch
    / UnorderedBranch
    / ConciseBranch

OrderedBranch <
    / :'->' :'{' Node+ :'}'
    / :'->' Node

UnorderedBranch <
    / :'~>' :'{' Node+ :'}'
    / :'~>' Node

ConciseBranch < :'->' '{..}'

Spacing <: (blank / Comment)*

Comment <-
    / '//' (!eol .)* (eol)
    / '/*' (!'*/' .)* '*/'
    / NestedComment

NestedComment <- '/+' (!NestedCommentEnd . / NestedComment) NestedCommentEnd

# This is needed to make the /+ +/ nest when the grammar is placed into a D nested comment ;)
NestedCommentEnd <- '+/'
`;
