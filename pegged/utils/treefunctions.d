module pegged.utils.treefunctions;

import pegged.peg;

import std.conv;

dstring[] leaves(ParseTree p)
{
    dstring[] result;
    if (p.children.length == 0)
        return p.capture[];
    else
    {
        foreach(child; p.children)
            result ~= leaves(child);
    }
    return result;
}

dstring[2][] treeUnification(ParseTree p1, ParseTree p2)
{
    dstring[2][] result;
    
    if (p1.name != p2.name)
    {
        p1 = fuseCaptures(p1);
        p2 = fuseCaptures(p2);
        result ~= [p1.capture[0], p2.capture[0]];
    }
    else
    {
        if (p1.children.length != p2.children.length)
            throw new Exception(to!string("Impossible unification between\n" ~ p1.toString() ~ "and\n"~p2.toString()));
        
        foreach(i, child; p1.children)
        {
            result ~= treeUnification(child, p2.children[i]);
        }
    }
    return result;
}
