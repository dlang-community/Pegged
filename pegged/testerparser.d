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
module pegged.testerparser;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericTesterGrammar(TParseTree)
{
    struct TesterGrammar
    {
    enum name = "TesterGrammar";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }
    static bool isRule(string s)
    {
        return s.startsWith("TesterGrammar.");
    }
    mixin decimateTree;
    static TParseTree Root(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "TesterGrammar.Root")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "TesterGrammar.Root"), "Root")(p);
    }
    static TParseTree Root(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "TesterGrammar.Root")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "TesterGrammar.Root"), "Root")(TParseTree("", false,[], s));
    }
    static string Root(GetName g)
    {
        return "TesterGrammar.Root";
    }

    static TParseTree Node(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), "TesterGrammar.Node")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), "TesterGrammar.Node"), "Node")(p);
    }
    static TParseTree Node(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), "TesterGrammar.Node")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), "TesterGrammar.Node"), "Node")(TParseTree("", false,[], s));
    }
    static string Node(GetName g)
    {
        return "TesterGrammar.Node";
    }

    static TParseTree Branch(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing)), "TesterGrammar.Branch")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing)), "TesterGrammar.Branch"), "Branch")(p);
    }
    static TParseTree Branch(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing)), "TesterGrammar.Branch")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing)), "TesterGrammar.Branch"), "Branch")(TParseTree("", false,[], s));
    }
    static string Branch(GetName g)
    {
        return "TesterGrammar.Branch";
    }

    static TParseTree OrderedBranch(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.OrderedBranch")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.OrderedBranch"), "OrderedBranch")(p);
    }
    static TParseTree OrderedBranch(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.OrderedBranch")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.OrderedBranch"), "OrderedBranch")(TParseTree("", false,[], s));
    }
    static string OrderedBranch(GetName g)
    {
        return "TesterGrammar.OrderedBranch";
    }

    static TParseTree UnorderedBranch(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.UnorderedBranch")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.UnorderedBranch"), "UnorderedBranch")(p);
    }
    static TParseTree UnorderedBranch(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.UnorderedBranch")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.UnorderedBranch"), "UnorderedBranch")(TParseTree("", false,[], s));
    }
    static string UnorderedBranch(GetName g)
    {
        return "TesterGrammar.UnorderedBranch";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "TesterGrammar.Spacing")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "TesterGrammar.Spacing"), "Spacing")(p);
    }
    static TParseTree Spacing(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "TesterGrammar.Spacing")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "TesterGrammar.Spacing"), "Spacing")(TParseTree("", false,[], s));
    }
    static string Spacing(GetName g)
    {
        return "TesterGrammar.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), NestedComment), "TesterGrammar.Comment")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), NestedComment), "TesterGrammar.Comment"), "Comment")(p);
    }
    static TParseTree Comment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), NestedComment), "TesterGrammar.Comment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), NestedComment), "TesterGrammar.Comment"), "Comment")(TParseTree("", false,[], s));
    }
    static string Comment(GetName g)
    {
        return "TesterGrammar.Comment";
    }

    static TParseTree NestedComment(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/+"), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), "TesterGrammar.NestedComment")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/+"), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), "TesterGrammar.NestedComment"), "NestedComment")(p);
    }
    static TParseTree NestedComment(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/+"), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), "TesterGrammar.NestedComment")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!("/+"), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), "TesterGrammar.NestedComment"), "NestedComment")(TParseTree("", false,[], s));
    }
    static string NestedComment(GetName g)
    {
        return "TesterGrammar.NestedComment";
    }

    static TParseTree NestedCommentEnd(TParseTree p)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("+/"), "TesterGrammar.NestedCommentEnd")(p);
        else
             return hooked!(pegged.peg.named!(pegged.peg.literal!("+/"), "TesterGrammar.NestedCommentEnd"), "NestedCommentEnd")(p);
    }
    static TParseTree NestedCommentEnd(string s)
    {
        if(__ctfe)
            return         pegged.peg.named!(pegged.peg.literal!("+/"), "TesterGrammar.NestedCommentEnd")(TParseTree("", false,[], s));
        else
            return hooked!(pegged.peg.named!(pegged.peg.literal!("+/"), "TesterGrammar.NestedCommentEnd"), "NestedCommentEnd")(TParseTree("", false,[], s));
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

