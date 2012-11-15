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
	/ :'->' '{' Node+ :'}'
	/ :'->' Node

UnorderedBranch <
	/ :'~>' '{' Node+ :'}'
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
struct GenericTesterGrammar(TParseTree)
{
    struct TesterGrammar
    {
    enum name = "TesterGrammar";
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
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
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), name ~ `.`~ `Root`)(p);
        }
        else
        {
            if(auto m = tuple(`Root`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), name ~ `.`~ `Root`)(p);
                memo[tuple(`Root`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Root(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), name ~ `.`~ `Root`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), name ~ `.`~ `Root`)(TParseTree("", false,[], s));
        }
    }
    static string Root(GetName g)
    {
        return name ~ `.`~ `Root`;
    }

    static TParseTree Node(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`^`), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), name ~ `.`~ `Node`)(p);
        }
        else
        {
            if(auto m = tuple(`Node`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`^`), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), name ~ `.`~ `Node`)(p);
                memo[tuple(`Node`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Node(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`^`), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), name ~ `.`~ `Node`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`^`), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), name ~ `.`~ `Node`)(TParseTree("", false,[], s));
        }
    }
    static string Node(GetName g)
    {
        return name ~ `.`~ `Node`;
    }

    static TParseTree Branch(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing)), name ~ `.`~ `Branch`)(p);
        }
        else
        {
            if(auto m = tuple(`Branch`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing)), name ~ `.`~ `Branch`)(p);
                memo[tuple(`Branch`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Branch(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing)), name ~ `.`~ `Branch`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing)), name ~ `.`~ `Branch`)(TParseTree("", false,[], s));
        }
    }
    static string Branch(GetName g)
    {
        return name ~ `.`~ `Branch`;
    }

    static TParseTree OrderedBranch(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`->`), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`{`), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`}`), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`->`), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), name ~ `.`~ `OrderedBranch`)(p);
        }
        else
        {
            if(auto m = tuple(`OrderedBranch`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`->`), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`{`), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`}`), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`->`), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), name ~ `.`~ `OrderedBranch`)(p);
                memo[tuple(`OrderedBranch`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OrderedBranch(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`->`), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`{`), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`}`), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`->`), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), name ~ `.`~ `OrderedBranch`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`->`), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`{`), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`}`), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`->`), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), name ~ `.`~ `OrderedBranch`)(TParseTree("", false,[], s));
        }
    }
    static string OrderedBranch(GetName g)
    {
        return name ~ `.`~ `OrderedBranch`;
    }

    static TParseTree UnorderedBranch(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`~>`), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`{`), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`}`), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`~>`), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), name ~ `.`~ `UnorderedBranch`)(p);
        }
        else
        {
            if(auto m = tuple(`UnorderedBranch`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`~>`), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`{`), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`}`), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`~>`), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), name ~ `.`~ `UnorderedBranch`)(p);
                memo[tuple(`UnorderedBranch`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnorderedBranch(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`~>`), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`{`), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`}`), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`~>`), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), name ~ `.`~ `UnorderedBranch`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`~>`), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`{`), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`}`), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(`~>`), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), name ~ `.`~ `UnorderedBranch`)(TParseTree("", false,[], s));
        }
    }
    static string UnorderedBranch(GetName g)
    {
        return name ~ `.`~ `UnorderedBranch`;
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), name ~ `.`~ `Spacing`)(p);
        }
        else
        {
            if(auto m = tuple(`Spacing`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), name ~ `.`~ `Spacing`)(p);
                memo[tuple(`Spacing`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), name ~ `.`~ `Spacing`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), name ~ `.`~ `Spacing`)(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return name ~ `.`~ `Spacing`;
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!(`//`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!(`/*`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(`*/`)), pegged.peg.any)), pegged.peg.literal!(`*/`)), NestedComment), name ~ `.`~ `Comment`)(p);
        }
        else
        {
            if(auto m = tuple(`Comment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!(`//`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!(`/*`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(`*/`)), pegged.peg.any)), pegged.peg.literal!(`*/`)), NestedComment), name ~ `.`~ `Comment`)(p);
                memo[tuple(`Comment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!(`//`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!(`/*`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(`*/`)), pegged.peg.any)), pegged.peg.literal!(`*/`)), NestedComment), name ~ `.`~ `Comment`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!(`//`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!(`/*`), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!(`*/`)), pegged.peg.any)), pegged.peg.literal!(`*/`)), NestedComment), name ~ `.`~ `Comment`)(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return name ~ `.`~ `Comment`;
    }

    static TParseTree NestedComment(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`/+`), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), name ~ `.`~ `NestedComment`)(p);
        }
        else
        {
            if(auto m = tuple(`NestedComment`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`/+`), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), name ~ `.`~ `NestedComment`)(p);
                memo[tuple(`NestedComment`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestedComment(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`/+`), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), name ~ `.`~ `NestedComment`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.and!(pegged.peg.literal!(`/+`), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), name ~ `.`~ `NestedComment`)(TParseTree("", false,[], s));
        }
    }
    static string NestedComment(GetName g)
    {
        return name ~ `.`~ `NestedComment`;
    }

    static TParseTree NestedCommentEnd(TParseTree p)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!(`+/`), name ~ `.`~ `NestedCommentEnd`)(p);
        }
        else
        {
            if(auto m = tuple(`NestedCommentEnd`,p.end) in memo)
                return *m;
            else
            {
                TParseTree result = pegged.peg.named!(pegged.peg.literal!(`+/`), name ~ `.`~ `NestedCommentEnd`)(p);
                memo[tuple(`NestedCommentEnd`,p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestedCommentEnd(string s)
    {
        if(__ctfe)
        {
            return pegged.peg.named!(pegged.peg.literal!(`+/`), name ~ `.`~ `NestedCommentEnd`)(TParseTree("", false,[], s));
        }
        else
        {
            memo = null;
            return pegged.peg.named!(pegged.peg.literal!(`+/`), name ~ `.`~ `NestedCommentEnd`)(TParseTree("", false,[], s));
        }
    }
    static string NestedCommentEnd(GetName g)
    {
        return name ~ `.`~ `NestedCommentEnd`;
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
        if(__ctfe)
        {
            return TesterGrammar(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            memo = null;
            return TesterGrammar(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "TesterGrammar";
    }

    }
}

alias GenericTesterGrammar!(ParseTree).TesterGrammar TesterGrammar;

