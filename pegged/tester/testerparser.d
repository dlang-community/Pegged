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

# This is needed to make the /+ +/ nest when the grammar is placed into a D # nested comment ;$(RPAREN)
NestedCommentEnd <- '+/'


+/
module pegged.tester.testerparser;

public import pegged.peg;

@safe struct GenericTesterGrammar(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct TesterGrammar
    {
    enum name = "TesterGrammar";
    static ParseTree delegate(ParseTree) @safe [string] before;
    static ParseTree delegate(ParseTree) @safe [string] after;
    static ParseTree delegate(ParseTree) @safe [string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this() @trusted
    {
        rules["Root"] = toDelegate(&Root);
        rules["Node"] = toDelegate(&Node);
        rules["Branch"] = toDelegate(&Branch);
        rules["OrderedBranch"] = toDelegate(&OrderedBranch);
        rules["UnorderedBranch"] = toDelegate(&UnorderedBranch);
        rules["ConciseBranch"] = toDelegate(&ConciseBranch);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p) @safe
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

        static ParseTree hooked(string input) @safe
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax) @safe
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax) @safe
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
        {
            if (ruleName != "Spacing")
                rules[ruleName] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s) pure nothrow @nogc
    {
        import std.algorithm : startsWith;
        return s.startsWith("TesterGrammar.");
    }
    mixin decimateTree;

    static TParseTree Root(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "TesterGrammar.Root")(p);
        }
        else
        {
            if (auto m = tuple(`Root`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "TesterGrammar.Root"), "Root")(p);
                memo[tuple(`Root`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Root(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "TesterGrammar.Root")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Node, Spacing), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "TesterGrammar.Root"), "Root")(TParseTree("", false,[], s));
        }
    }
    static string Root(GetName g)
    {
        return "TesterGrammar.Root";
    }

    static TParseTree Node(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), "TesterGrammar.Node")(p);
        }
        else
        {
            if (auto m = tuple(`Node`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), "TesterGrammar.Node"), "Node")(p);
                memo[tuple(`Node`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Node(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), "TesterGrammar.Node")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("^"), Spacing)), pegged.peg.wrapAround!(Spacing, identifier, Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.propagate!(pegged.peg.wrapAround!(Spacing, Branch, Spacing)), Spacing)))), "TesterGrammar.Node"), "Node")(TParseTree("", false,[], s));
        }
    }
    static string Node(GetName g)
    {
        return "TesterGrammar.Node";
    }

    static TParseTree Branch(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, ConciseBranch, Spacing)), "TesterGrammar.Branch")(p);
        }
        else
        {
            if (auto m = tuple(`Branch`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, ConciseBranch, Spacing)), "TesterGrammar.Branch"), "Branch")(p);
                memo[tuple(`Branch`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Branch(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, ConciseBranch, Spacing)), "TesterGrammar.Branch")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, OrderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, UnorderedBranch, Spacing), pegged.peg.wrapAround!(Spacing, ConciseBranch, Spacing)), "TesterGrammar.Branch"), "Branch")(TParseTree("", false,[], s));
        }
    }
    static string Branch(GetName g)
    {
        return "TesterGrammar.Branch";
    }

    static TParseTree OrderedBranch(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.OrderedBranch")(p);
        }
        else
        {
            if (auto m = tuple(`OrderedBranch`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.OrderedBranch"), "OrderedBranch")(p);
                memo[tuple(`OrderedBranch`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OrderedBranch(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.OrderedBranch")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.OrderedBranch"), "OrderedBranch")(TParseTree("", false,[], s));
        }
    }
    static string OrderedBranch(GetName g)
    {
        return "TesterGrammar.OrderedBranch";
    }

    static TParseTree UnorderedBranch(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.UnorderedBranch")(p);
        }
        else
        {
            if (auto m = tuple(`UnorderedBranch`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.UnorderedBranch"), "UnorderedBranch")(p);
                memo[tuple(`UnorderedBranch`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnorderedBranch(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.UnorderedBranch")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Node, Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("~>"), Spacing)), pegged.peg.wrapAround!(Spacing, Node, Spacing))), "TesterGrammar.UnorderedBranch"), "UnorderedBranch")(TParseTree("", false,[], s));
        }
    }
    static string UnorderedBranch(GetName g)
    {
        return "TesterGrammar.UnorderedBranch";
    }

    static TParseTree ConciseBranch(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{..}"), Spacing)), "TesterGrammar.ConciseBranch")(p);
        }
        else
        {
            if (auto m = tuple(`ConciseBranch`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{..}"), Spacing)), "TesterGrammar.ConciseBranch"), "ConciseBranch")(p);
                memo[tuple(`ConciseBranch`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ConciseBranch(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{..}"), Spacing)), "TesterGrammar.ConciseBranch")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("->"), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{..}"), Spacing)), "TesterGrammar.ConciseBranch"), "ConciseBranch")(TParseTree("", false,[], s));
        }
    }
    static string ConciseBranch(GetName g)
    {
        return "TesterGrammar.ConciseBranch";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "TesterGrammar.Spacing")(p);
        }
        else
        {
            if (auto m = tuple(`Spacing`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "TesterGrammar.Spacing"), "Spacing")(p);
                memo[tuple(`Spacing`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "TesterGrammar.Spacing")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(blank, Comment))), "TesterGrammar.Spacing"), "Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "TesterGrammar.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), NestedComment), "TesterGrammar.Comment")(p);
        }
        else
        {
            if (auto m = tuple(`Comment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), NestedComment), "TesterGrammar.Comment"), "Comment")(p);
                memo[tuple(`Comment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), NestedComment), "TesterGrammar.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol), pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), NestedComment), "TesterGrammar.Comment"), "Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "TesterGrammar.Comment";
    }

    static TParseTree NestedComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/+"), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), "TesterGrammar.NestedComment")(p);
        }
        else
        {
            if (auto m = tuple(`NestedComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/+"), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), "TesterGrammar.NestedComment"), "NestedComment")(p);
                memo[tuple(`NestedComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestedComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/+"), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), "TesterGrammar.NestedComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/+"), pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(NestedCommentEnd), pegged.peg.any), NestedComment), NestedCommentEnd), "TesterGrammar.NestedComment"), "NestedComment")(TParseTree("", false,[], s));
        }
    }
    static string NestedComment(GetName g)
    {
        return "TesterGrammar.NestedComment";
    }

    static TParseTree NestedCommentEnd(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("+/"), "TesterGrammar.NestedCommentEnd")(p);
        }
        else
        {
            if (auto m = tuple(`NestedCommentEnd`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("+/"), "TesterGrammar.NestedCommentEnd"), "NestedCommentEnd")(p);
                memo[tuple(`NestedCommentEnd`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NestedCommentEnd(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("+/"), "TesterGrammar.NestedCommentEnd")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("+/"), "TesterGrammar.NestedCommentEnd"), "NestedCommentEnd")(TParseTree("", false,[], s));
        }
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
        if(__ctfe)
        {
            return TesterGrammar(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return TesterGrammar(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "TesterGrammar";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericTesterGrammar!(ParseTree).TesterGrammar TesterGrammar;

