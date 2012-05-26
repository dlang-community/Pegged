module test;

import std.algorithm;
import std.array;
import std.conv;
import std.range;
import std.stdio;
import std.typecons;

import pegged.grammar;


enum input =
`
Test:
A <- B
B <- 'b'
`;


enum InfiniteLoop  { NoLoop, MightConsumeNothing, PossibleInfiniteLoop, Undecided }
enum LeftRecursion { NoLeftRecursion, DirectLeftRecursion, IndirectLeftRecursion, HiddenLeftRecursion, Undecided }
enum ReduceFurther { No, Yes }

struct Diagnostic
{
    InfiniteLoop[dstring]  infiniteLoops;
    LeftRecursion[dstring] leftRecursions;
}

Diagnostic checkGrammar(dstring g, ReduceFurther reduceFurther = ReduceFurther.Yes)
{
    import std.stdio;
    auto grammarAsInput = PEGGED.parse(g);
    Diagnostic diag;
    ParseTree[dstring] rules;
    
    InfiniteLoop checkLoops(ParseTree p)
    {
        switch(p.name)
        {
            case "PEGGED.Definition":
                return checkLoops(p.children[2]);
            case "PEGGED.Expression":
                InfiniteLoop result;
                foreach(i, child; p.children)
                {
                    if (i % 2 == 1) 
                        continue; // skipping 'PEGGED.OR'
                    auto c = checkLoops(child);
                    if (c > result)
                        result = c;
                }
                return result;
            case "PEGGED.Sequence":
                InfiniteLoop result = checkLoops(p.children[0]);
                foreach(i, child; p.children[1..$])
                {
                    auto c = checkLoops(child);
                    switch (c)
                    {
                        case InfiniteLoop.NoLoop:
                            if (result < InfiniteLoop.PossibleInfiniteLoop)
                                result = c;
                            break;
                        case InfiniteLoop.MightConsumeNothing:
                            break; // Never change the sequence diagnostic
                        case InfiniteLoop.PossibleInfiniteLoop:
                            return c;
                        case InfiniteLoop.Undecided:
                            return c;
                        default:
                            break;
                    }
                }
                return result;
            case "PEGGED.Prefix":
                return checkLoops(p.children[$-1]);
            case "PEGGED.Suffix":
                if (p.children.length > 1) // Suffix present
                {
                    if (p.children[1].name == "PEGGED.ONEORMORE" || p.children[1].name == "PEGGED.ZEROORMORE")
                    {
                        auto c = checkLoops(p.children[0]);
                        if (c == InfiniteLoop.MightConsumeNothing)
                            return InfiniteLoop.PossibleInfiniteLoop;
                    }
                    if (p.children[1].name == "PEGGED.OPTION")
                        return InfiniteLoop.MightConsumeNothing;
                }
                return checkLoops(p.children[0]);
            case "PEGGED.Primary":
                return checkLoops(p.children[0]);
            case "PEGGED.Name":
                if (auto res = p.capture[0] in diag.infiniteLoops)
                    return *res;
                else
                    return InfiniteLoop.NoLoop; // unknown name -> External grammar name
            case "PEGGED.Eps":
                return InfiniteLoop.MightConsumeNothing;
            default:
                return InfiniteLoop.NoLoop;
        }
    }
    
    /// Search for toFind as the first child in an expression
    bool findName(ParseTree p, dstring toFind)
    {
        if (p.name == "PEGGED.Name" && p.capture[0] == toFind)
            return true;
        else
        {
            if (p.children.length == 0)
                return false;
            return findName(p.children[0], toFind);
        }
    }
    
    LeftRecursion checkLeftRecursion(ParseTree p)
    {
        switch(p.name)
        {
            case "PEGGED.Definition":
                return checkLeftRecursion(p.children[2]);
            case "PEGGED.Expression":
                LeftRecursion result;
                foreach(i, child; p.children)
                {
                    if (i % 2 == 1) 
                    continue; // skipping 'PEGGED.OR'
                    auto c = checkLeftRecursion(child);
                    if (c > result)
                        result = c;
                }
                return result;
            case "PEGGED.Sequence":
                LeftRecursion result = checkLeftRecursion(p.children[0]);
                foreach(i, child; p.children[1..$])
                {
                    
                    LeftRecursion c = checkLeftRecursion(child);
                    switch (c)
                    {
//                         case InfiniteLoop.NoLoop:
//                             if (result < InfiniteLoop.PossibleInfiniteLoop)
//                                 result = c;
//                             break;
//                         case InfiniteLoop.MightConsumeNothing:
//                             break; // Never change the sequence diagnostic
//                         case InfiniteLoop.PossibleInfiniteLoop:
//                             return c;
//                         case InfiniteLoop.Undecided:
//                             return c;
                        default:
                            break;
                    }
                }
                return result;
            case "PEGGED.Prefix":
                return checkLeftRecursion(p.children[$-1]);
            case "PEGGED.Suffix":
                if (p.children.length > 1) // Suffix present
                {
                    if (p.children[1].name == "PEGGED.ONEORMORE" || p.children[1].name == "PEGGED.ZEROORMORE")
                    {
                        LeftRecursion c = checkLeftRecursion(p.children[0]);
//                         if (c == InfiniteLoop.MightConsumeNothing)
//                             return InfiniteLoop.PossibleInfiniteLoop;
                    }
                    if (p.children[1].name == "PEGGED.OPTION")
                        return LeftRecursion.DirectLeftRecursion;
                }
                return checkLeftRecursion(p.children[0]);
            case "PEGGED.Primary":
                return checkLeftRecursion(p.children[0]);
            case "PEGGED.Name":
                if (auto res = p.capture[0] in diag.leftRecursions)
                    return *res;
                else
                    return LeftRecursion.NoLeftRecursion;
            case "PEGGED.Eps":
                return LeftRecursion.NoLeftRecursion;
            default:
                return LeftRecursion.NoLeftRecursion;
        }
    }
    
    foreach(index, definition; grammarAsInput.children)
        if (definition.name == "PEGGED.Definition")
        {
            diag.infiniteLoops[definition.capture[0]]= InfiniteLoop.Undecided; // entering the rule's name in canLoop
            rules[definition.capture[0]] = definition;
            //writeln("Finding rule `"~definition.capture[0]~"`");
        }
    
    writeln("Found ", diag.infiniteLoops.length, " rules.");
    
    size_t stillUndecidedRules = diag.infiniteLoops.length;
    size_t before = stillUndecidedRules+1;   

    size_t reduceUndecided()
    {
        size_t howManyReduced;
        while(before > stillUndecidedRules)
        {
            before = stillUndecidedRules;
            foreach(name, diagnostic; diag.infiniteLoops)
            {
                if (diagnostic == InfiniteLoop.Undecided)
                {
                    diag.infiniteLoops[name] = checkLoops(rules[name]);
                    if (diag.infiniteLoops[name] != InfiniteLoop.Undecided)
                    {
                        --stillUndecidedRules;
                        ++howManyReduced;
                    }
                }
            }
        }
        dstring s;
        foreach(name, diagnostic; diag.infiniteLoops)
            if (diagnostic == InfiniteLoop.Undecided) s ~= (" " ~ name);
        if (stillUndecidedRules > 0)
            writeln("still ", stillUndecidedRules, " undecided rules: ", s);
        writeln(howManyReduced, " rules were deduced at this step.");
        return howManyReduced;
    }
    reduceUndecided();

    if (reduceFurther == ReduceFurther.Yes)
    {
        size_t breaker = 0;
        size_t wrapAround = 0;
        // standard reduction didn't work: mutually recursive rules block the algorithm
        // We will try to cut the Gordian node by forcefully changing a rule to NoLoop and see if that helps
        while (stillUndecidedRules > 0 && wrapAround < stillUndecidedRules)
        {
            writeln("Trying to reduce further...");
            while(diag.infiniteLoops[diag.infiniteLoops.keys[breaker]] != InfiniteLoop.Undecided)
            {
                breaker = (breaker + 1) % diag.infiniteLoops.length;
            }
            writeln("Changing ", diag.infiniteLoops.keys[breaker]);
            diag.infiniteLoops[diag.infiniteLoops.keys[breaker]] = InfiniteLoop.NoLoop;
            --stillUndecidedRules;
            before = stillUndecidedRules+1; // used in reduceUndecided()
            auto howManyReduced = reduceUndecided();
            if (howManyReduced == 0)
                ++wrapAround;
            else
                wrapAround = 0;
            auto c = checkLoops(rules[diag.infiniteLoops.keys[breaker]]);
            if (c == InfiniteLoop.Undecided)
                ++stillUndecidedRules;
            diag.infiniteLoops[diag.infiniteLoops.keys[breaker]] = c;
            breaker = (breaker + 1) % diag.infiniteLoops.length;
        }
    }
    
    return diag;
}


void main()
{
    writeln(checkGrammar(input));
}


