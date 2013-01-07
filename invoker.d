module invoker;

import std.stdio;

import dparser;

void main(string[] args)
{

    string res = D.decimateTree(D(q{

module foo;
double d;
int foo(string[] args) { return args;}

    })).toString();
    //result.write(res);
    writeln(res);

}