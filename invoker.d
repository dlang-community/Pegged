module invoker;

import pegged.grammar;
import std.stdio;
import dparser;

void main(string[] args)
{
    string res = D.decimateTree(D(q{// My First Macro!
macro foo(IfCondition cond, ThenStatement then) : Statement
{
    unless (cond)
        then;
}
return
{
    if (!cond)
        then;
}
    })).toString();

    writeln(res);
}