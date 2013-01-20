/**
This module contains function to inspect a Pegged grammar.
*/
module pegged.dev.introspection;

import std.typecons;

import pegged.grammar;
import pegged.parser;

/**
The different kinds of recursion for a rule.
'direct' means the rule name appears in its own definition. 'indirect' means the rule calls itself through another rule (the call chain can be long).
*/
enum Recursive { no, direct, indirect }

/**
Left-recursion diagnostic for a rule. A rule is left-recursive when its own name appears at the beginning of its definition or behind possibly-null-matching rules (see below for null matches).
For example A <- A 'a' is left-recursive, whereas A <- 'a' A is not. *But* A <- 'a'? A is left-recursive, since if the input does not begin
with 'a', then the parsing will continue by invoking A again, at the same position.

'direct' means the rule invokes itself in the first position of its definition (A <- A ...). 'hidden' means the rule names appears after a possibly-null other rule (A <- 'a'? A ...). 'indirect' means the rule calls itself trough another rule.
*/
enum LeftRecursive { no, direct, hidden, indirect }

/**
NullMatch.yes means a rule can succeed while consuming no input. For example e? or e*, for all expressions e.
Nullmatch.no means a rule will always consume at least a token while succeeding.
Nullmatch.indeterminate means the algorithm could not converge.
*/
enum NullMatch { no, yes, indeterminate }

/**
InfiniteLoop.yes means a rule can loop indefinitely while consuming nothing.
InfiniteLoop.no means a rule cannot loop indefinitely.
InfiniteLoop.indeterminate means the algorithm could not converge.
*/
enum InfiniteLoop { no, yes, indeterminate }

/**
Struct holding the introspection info on a rule.
*/
struct RuleInfo
{
    Recursive recursion; /// Is the rule recursive?
    LeftRecursive leftRecursion; /// Is the rule left-recursive?
    NullMatch nullMatch; /// Can the rule succeed while consuming nothing?
    InfiniteLoop infiniteLoop; /// Can the rule loop while indefinitely, while consuming nothing?
}

/**
Returns for all grammar rule:

- the recursion type (no recursion, direct or indirect recursion).
- the left-recursion type (no left-recursion, direct left-recursion, hidden, or indirect)
- the null-match for a grammar's rules: whether the rule can succeed while consuming nothing.
- the possibility of an infinite loop (if 'e' can null-match, then 'e*' can enter an infinite loop).

This kind of potential problem can be detected statically and should be transmitted to the grammar designer.
*/
RuleInfo[string] ruleInfo(string grammar)
{
    RuleInfo[string] result;
    ParseTree[string] rules;

    /**
    Returns the call graph of a grammar: the list of rules directly called by each rule of the grammar.
    The graph is represented as a bool[string][string] associative array, the string holding
    the rules names. graph["ruleName"] contains all rules called by ruleName, as a set (a bool[string] AA).

    graph.keys thus gives the grammar's rules names.

    If a rule calls itself, its own name will appear in the called set. If a rule calls an external rule, it will
    also appear in the call graph when the rule has a name: hence, calls to predefined rules like 'identifier' or
    'digit' will appear, but not a call to '[0-9]+', considered here as an anonymous rule.
    */
    bool[string][string] callGraph(ParseTree p)
    {
        bool[string] findIdentifiers(ParseTree p)
        {
            bool[string] idList;
            if (p.name == "Pegged.Identifier")
                idList[p.matches[0]] = true;
            else
                foreach(child; p.children)
                    foreach(name; findIdentifiers(child).keys)
                        idList[name] = true;

            return idList;
        }

        bool[string][string] graph;

        foreach(definition; p.children)
            if (definition.name == "Pegged.Definition")
            {
                auto ids = findIdentifiers(definition.children[2]);
                graph[definition.matches[0]] = ids;
                foreach(id, _; ids) // getting possible external calls
                    if (id !in graph)
                        graph[id] = (bool[string]).init;
            }

        return graph;
    }

    /**
    The transitive closure of a call graph.
    It will propagate the calls to find all rules called by a given rule,
    directly (already in the call graph) or indirectly (through another rule).
    */
    bool[string][string] closure(bool[string][string] graph)
    {
        bool[string][string] path;
        foreach(rule, children; graph) // deep-dupping, to avoid children aliasing
            path[rule] = children.dup;

        bool changed = true;

        while(changed)
        {
            changed = false;
            foreach(rule1; graph.keys)
                foreach(rule2; graph.keys)
                    if (rule2 in path[rule1])
                        foreach(rule3; graph.keys)
                            if (rule3 in path[rule2] && rule3 !in path[rule1])
                            {
                                path[rule1][rule3] = true;
                                changed = true;
                            }
        }

        return path;
    }

    Recursive[string] recursions(bool[string][string] graph)
    {
        bool[string][string] path = closure(graph);

        Recursive[string] result;
        foreach(rule, children; path)
        {
            result[rule] = Recursive.no;
            if (rule in children)
            {
                if (rule in graph[rule])
                    result[rule] = Recursive.direct;
                else
                    result[rule] = Recursive.indirect;
            }
        }

        return result;
    }

    NullMatch nullMatching(ParseTree p)
    {
        switch (p.name)
        {
            case "Pegged.Expression": // choice expressions null-match whenever one of their components can null-match
                foreach(seq; p.children)
                {
                    auto nm = nullMatching(seq);
                    if (nm == NullMatch.yes)
                        return NullMatch.yes;
                    if (nm == NullMatch.indeterminate)
                        return NullMatch.indeterminate;
                }
                return NullMatch.no;
            case "Pegged.Sequence": // sequence expressions can null-match when all their components can null-match
                foreach(seq; p.children)
                {
                    auto nm = nullMatching(seq);
                    if (nm == NullMatch.indeterminate)
                        return NullMatch.indeterminate;
                    if (nm == NullMatch.no)
                        return NullMatch.no;
                }
                return NullMatch.yes;
            case "Pegged.Prefix":
                foreach(pref; p.children[0..$-1])
                    if (pref.name == "Pegged.POS" || pref.name == "Pegged.NEG")
                        return NullMatch.yes;
                return nullMatching(p.children[$-1]);
            case "Pegged.Suffix":
                foreach(pref; p.children[1..$])
                    if (pref.name == "Pegged.ZEROORMORE" || pref.name == "Pegged.OPTION")
                        return NullMatch.yes;
                return nullMatching(p.children[0]);
            case "Pegged.Primary":
                return nullMatching(p.children[0]);
            case "Pegged.RhsName":
                if (p.matches[0] in result)
                    return result[p.matches[0]].nullMatch;
                else
                    return nullMatching(p.children[0]);
            case "Pegged.Literal":
                if (p.matches[0].length == 0) // Empty literal, '' or ""
                    return NullMatch.yes;
                else
                    return NullMatch.no;
            case "Pegged.CharClass":
                return NullMatch.no;
            case "Pegged.ANY":
                return NullMatch.no;
            case "eps":
                return NullMatch.yes;
            case "eoi":
                return NullMatch.yes;
            default:
                return NullMatch.indeterminate;
        }
    }

    InfiniteLoop infiniteLooping(ParseTree p)
    {
        switch (p.name)
        {
            case "Pegged.Expression": // choice expressions loop whenever one of their components can loop
                foreach(seq; p.children)
                {
                    auto nm = infiniteLooping(seq);
                    if (nm == InfiniteLoop.yes)
                        return InfiniteLoop.yes;
                    if (nm == InfiniteLoop.indeterminate)
                        return InfiniteLoop.indeterminate;
                }
                return InfiniteLoop.no;
            case "Pegged.Sequence": // sequence expressions can loop when one of their components can loop
                foreach(seq; p.children)
                {
                    auto nm = infiniteLooping(seq);
                    if (nm == InfiniteLoop.yes)
                        return InfiniteLoop.yes;
                    if (nm == InfiniteLoop.indeterminate)
                        return InfiniteLoop.indeterminate;
                }
                return InfiniteLoop.no;
            case "Pegged.Prefix":
                return infiniteLooping(p.children[$-1]);
            case "Pegged.Suffix":
                foreach(pref; p.children[1..$])
                    if ((  pref.name == "Pegged.ZEROORMORE" || pref.name == "Pegged.ONEORMORE")
                        && p.matches[0] in result
                        && result[p.matches[0]].nullMatch == NullMatch.yes)
                        return InfiniteLoop.yes;
                return infiniteLooping(p.children[0]);
            case "Pegged.Primary":
                return infiniteLooping(p.children[0]);
            case "Pegged.RhsName":
                if (p.matches[0] in result)
                    return result[p.matches[0]].infiniteLoop;
                else
                    return infiniteLooping(p.children[0]);
            case "Pegged.Literal":
                return InfiniteLoop.no;
            case "Pegged.CharClass":
                return InfiniteLoop.no;
            case "Pegged.ANY":
                return InfiniteLoop.no;
            case "eps":
                return InfiniteLoop.no;
            case "eoi":
                return InfiniteLoop.no;
            default:
                return InfiniteLoop.indeterminate;
        }
    }

    LeftRecursive leftRecursion(ParseTree p, string target)
    {
        switch (p.name)
        {
            case "Pegged.Expression": // Choices are left-recursive is any choice is left-recursive
                foreach(seq; p.children)
                {
                    auto lr = leftRecursion(seq, target);
                    if (lr != LeftRecursive.no)
                        return lr;
                }
                return LeftRecursive.no;
            case "Pegged.Sequence": // Sequences are left-recursive when the leftmost member is left-recursive
                                    // or behind null-matching members
                foreach(i, seq; p.children)
                {
                    auto lr = leftRecursion(seq, target);
                    if (lr == LeftRecursive.direct)
                        return (i == 0 ? LeftRecursive.direct : LeftRecursive.hidden);
                    else if (lr == LeftRecursive.hidden || lr == LeftRecursive.indirect)
                        return lr;
                    else if (nullMatching(seq) == NullMatch.yes)
                        continue;
                    else
                        return LeftRecursive.no;
                }
                return LeftRecursive.no; // found only null-matching rules!
            case "Pegged.Prefix":
                return leftRecursion(p.children[$-1], target);
            case "Pegged.Suffix":
                return leftRecursion(p.children[0], target);
            case "Pegged.Primary":
                return leftRecursion(p.children[0], target);
            case "Pegged.RhsName":
                if (p.matches[0] == target) // ?? Or generateCode(p) ?
                    return LeftRecursive.direct;
                else if ((p.matches[0] in rules) && (leftRecursion(rules[p.matches[0]], target) != LeftRecursive.no))
                    return LeftRecursive.hidden;
                else
                    return LeftRecursive.no;
            case "Pegged.Literal":
                return LeftRecursive.no;
            case "Pegged.CharClass":
                return LeftRecursive.no;
            case "Pegged.ANY":
                return LeftRecursive.no;
            case "eps":
                return LeftRecursive.no;
            case "eoi":
                return LeftRecursive.no;
            default:
                return LeftRecursive.no;
        }
    }

    ParseTree p = Pegged(grammar).children[0];
    foreach(definition; p.children)
        if (definition.name == "Pegged.Definition")
        {
            rules[definition.matches[0]] = definition.children[2];
            result[definition.matches[0]] = RuleInfo(Recursive.no, LeftRecursive.no,
                                                     NullMatch.indeterminate,InfiniteLoop.indeterminate);
        }

    auto rec = recursions(callGraph(p));
    foreach(rule, recursionType; rec)
        if (rule in result) // external rules are in rec, but not in result
            result[rule].recursion = recursionType;

    foreach(name, tree; rules)
    {
        if (result[name].recursion != Recursive.no)
        {
            result[name].leftRecursion = leftRecursion(tree, name);
        }
    }

    bool changed = true;

    while(changed) // while something new happened, the process is not over
    {
        changed = false;
        foreach(name, tree; rules)
            if (result[name].nullMatch == NullMatch.indeterminate) // not done yet
            {
                result [name].nullMatch = nullMatching(tree); // try to find if it's null-matching
                if (result[name].nullMatch != NullMatch.indeterminate)
                    changed = true;
            }
    }

    changed = true;

    while(changed) // while something new happened, the process is not over
    {
        changed = false;
        foreach(name, tree; rules)
            if (result[name].infiniteLoop == InfiniteLoop.indeterminate) // not done yet
            {
                result [name].infiniteLoop = infiniteLooping(tree); // try to find if it's looping
                if (result[name].infiniteLoop != InfiniteLoop.indeterminate)
                    changed = true;
            }
    }

    return result;
}

/**
Act on rules parse tree as produced by pegged.parser.
Replace every occurence of child in parent by child's parse tree
*/
ParseTree replaceInto(ParseTree parent, ParseTree child)
{
    if (parent.name == "Pegged.RhsName" && parent.matches[0] == child.matches[0])
        return ParseTree("Pegged.Named", true, child.matches[0..1], "",0,0,
                       [child.children[2],
                        ParseTree("Pegged.Identifier", true, child.matches[0..1])]);
    else
        foreach(ref branch; parent.children)
            branch = replaceInto(branch, child);
    return parent;
}
