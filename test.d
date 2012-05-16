module test;

import std.algorithm;
import std.array;
import std.conv;
import std.range;
import std.stdio;
import std.typecons;

import pegged.grammar;

version(none)
{
mixin(grammar(`
RangeComprehension < Yield '|' Element (';' Element)*
Yield    < ~(!'|' .)+
Element  < Generate / Filter
Generate < Identifier :"in" ~(!';' .)+
Filter   <  ~(!';' .)+
`));

string comp(string input)
{
    auto tree = RangeComprehension.parse(input);
    if (!tree.success) 
        return "static assert(0, `Bad range comprehension input: `" ~ input ~ "`);";
    string result;
    return result;
}


class RC
{
    typeof(iota(long.max)) xRange;
    ElementType!(typeof(xRange)) x;
    typeof(iota(x)) yRange;
    ElementType!(typeof(yRange)) y;
    
    this()
    {
        xRange = iota(long.max);
        x = xRange.front;
        yRange = iota(x);
        y = yRange.front;
    }
    
    bool empty() @property
    {
        return xRange.empty || yRange.empty;
    }
    
    void popFront() @property
    {
        while(!yRange.empty && !(y*y<x))
        {
            yRange.popFront();
            y = yRange.front;
        }

        if (yRange.empty)
        {
            xRange.popFront();
            x = xRange.front;
            yRange = iota(x);
            if (!yRange.empty)
                y = yRange.front;
            while(!yRange.empty && !(y*y<x))
            {
                yRange.popFront();
                y = yRange.front;
            }
        }
        
        auto y2 = filter!(y => y*y<x)(yRange);
        writeln(y2);
        
    }
    
    typeof(tuple(x,y)) front() @property
    {
        return tuple(x,y);
    }
}

class ItemGetter
{
    int opCall(out ItemGetter next)
    {
        return 0;
    }
}
}


import pegged.grammar;
mixin(grammar(`Number < [0123456789]*`));
enum result = Number.parse("123");


void main()
{
    writeln(result);
}


