module remakeparser;


import pegged.grammar;
import pegged.examples.dgrammar;

void main(string[] args)
{
    asModule("dparser", "dparser", Dgrammar);
}
