module invoker;


import std.stdio;
import dparser;

void main(string[] args)
{

    string res = D.decimateTree(D.MacroDeclaration(q{
macro foo(IfCondition cond, ThenStatement then)
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
    //result.write(res);
    writeln(res);

}