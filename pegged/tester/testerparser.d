/++
This module was automatically generated from the following grammar:


TesterGrammar:

Root < Node eoi

Node <
    / :'^' identifier
    / identifier (%Branch)*

Branch <
    / OrderedBranch
    / UnorderedBranch

OrderedBranch <
    / :'->' :'{' Node+ :'}'
    / :'->' Node

UnorderedBranch <
    / :'~>' :'{' Node+ :'}'
    / :'~>' Node

Spacing <: (blank / Comment)*

Comment <- 
    / '//' (!eol .)* (eol)
    / '/*' (!'*/' .)* '*/'
    / NestedComment

NestedComment <- '/+' (!NestedCommentEnd . / NestedComment) NestedCommentEnd

# This is needed to make the /+ +/ nest when the grammar is placed into a D nested comment ;)
NestedCommentEnd <- '+/'


+/
module pegged.tester.testerparser;

public import pegged.peg;
struct GenericTesterGrammar(TParseTree)
{
    struct TesterGrammar
    {
    enum name = "TesterGrammar";
    static bool isRule(string s)
    {
        switch(s)
        {
            case "TesterGrammar.Root":
            case "TesterGrammar.Node":
            case "TesterGrammar.Branch":
            case "TesterGrammar.OrderedBranch":
            case "TesterGrammar.UnorderedBranch":
            case "TesterGrammar.Spacing":
            case "TesterGrammar.Comment":
            case "TesterGrammar.NestedComment":
            case "TesterGrammar.NestedCommentEnd":
                return true;
            default:
                return false;
        }
    }
    mixin decimateTree;
    static TParseTree Root(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "TesterGrammar.Root")(p);
    }
    static TParseTree Root(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "TesterGrammar.Root")(TParseTree("", false,[], s));
    }
    static string Root(GetName g)
    {
        return "TesterGrammar.Root";
    }

    static TParseTree Node(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), "TesterGrammar.Node")(p);
    }
    static TParseTree Node(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), "TesterGrammar.Node")(TParseTree("", false,[], s));
    }
    static string Node(GetName g)
    {
        return "TesterGrammar.Node";
    }

    static TParseTree Branch(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing)), "TesterGrammar.Branch")(p);
    }
    static TParseTree Branch(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing)), "TesterGrammar.Branch")(TParseTree("", false,[], s));
    }
    static string Branch(GetName g)
    {
        return "TesterGrammar.Branch";
    }

    static TParseTree OrderedBranch(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.OrderedBranch")(p);
    }
    static TParseTree OrderedBranch(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.OrderedBranch")(TParseTree("", false,[], s));
    }
    static string OrderedBranch(GetName g)
    {
        return "TesterGrammar.OrderedBranch";
    }

    static TParseTree UnorderedBranch(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.UnorderedBranch")(p);
    }
    static TParseTree UnorderedBranch(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.UnorderedBranch")(TParseTree("", false,[], s));
    }
    static string UnorderedBranch(GetName g)
    {
        return "TesterGrammar.UnorderedBranch";
    }

    static TParseTree Spacing(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "TesterGrammar.Spacing")(p);
    }
    static TParseTree Spacing(string s)
    {
        return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "TesterGrammar.Spacing")(TParseTree("", false,[], s));
    }
    static string Spacing(GetName g)
    {
        return "TesterGrammar.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), NestedComment), "TesterGrammar.Comment")(p);
    }
    static TParseTree Comment(string s)
    {
        return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), NestedComment), "TesterGrammar.Comment")(TParseTree("", false,[], s));
    }
    static string Comment(GetName g)
    {
        return "TesterGrammar.Comment";
    }

    static TParseTree NestedComment(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/+"), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), "TesterGrammar.NestedComment")(p);
    }
    static TParseTree NestedComment(string s)
    {
        return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/+"), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), "TesterGrammar.NestedComment")(TParseTree("", false,[], s));
    }
    static string NestedComment(GetName g)
    {
        return "TesterGrammar.NestedComment";
    }

    static TParseTree NestedCommentEnd(TParseTree p)
    {
         return pegged.peg.named!(pegged.peg.literal!("+/"), "TesterGrammar.NestedCommentEnd")(p);
    }
    static TParseTree NestedCommentEnd(string s)
    {
        return pegged.peg.named!(pegged.peg.literal!("+/"), "TesterGrammar.NestedCommentEnd")(TParseTree("", false,[], s));
    }
    static string NestedCommentEnd(GetName g)
    {
        return "TesterGrammar.NestedCommentEnd";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Root(p));
        result.children = [result];
        result.name = "TesterGrammar";
        return result;
    }

    static TParseTree opCall(string input)
    {
        return TesterGrammar(TParseTree(``, false, [], input, 0, 0));
}
    static string opCall(GetName g)
    {
        return "TesterGrammar";
    }

    }
}

alias GenericTesterGrammar!(ParseTree).TesterGrammar TesterGrammar;

