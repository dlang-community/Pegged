/// Testing Pegged modifications.
module pegged.dev.test;

//import std.algorithm;
//import std.array;
//import std.conv;
import std.datetime;
//import std.functional;
//import std.range;
import std.stdio;
//import std.typecons;
//import std.typetuple;

import pegged.grammar;
import pegged.dynamic.peg;
import pegged.dynamic.grammar;

import pegged.examples.c;

void main()
{
    //writeln(makeSwitch(40));
    Dynamic[string] predefined =
    [ "quote":      (ParseTree p) => literal("'")(p)
    , "doublequote":(ParseTree p) => literal("\"")(p)
    , "backquote":  (ParseTree p) => literal("`")(p)
    , "slash":      (ParseTree p) => literal("/")(p)
    , "backslash":  (ParseTree p) => literal("\\")(p)
    , "endOfLine":  (ParseTree p) => or(literal("\n"), literal("\r\n"), literal("\r"))(p)
    , "space":      (ParseTree p) => or(literal(" "), literal("\t"), literal("\n"), literal("\r\n"), literal("\r"))(p)
    , "digit":      (ParseTree p) => charRange('0', '9')(p)
    , "identifier": (ParseTree p) => fuse(and( or(charRange('a','z'), charRange('A','Z'), literal("_"))
                                             , oneOrMore(or(charRange('a','z'), charRange('A','Z'), literal("_"), charRange('0', '9')))))(p)
    ];

    StopWatch sw;
    writeln("Generating C dynamic parser...");
    sw.start();
    DynamicGrammar dg = pegged.dynamic.grammar.grammar(Cgrammar, predefined);
    sw.stop(); 
    auto last = sw.peek().msecs;
    writeln("Done. Parser generated in ", last, " ms.");
    auto space = zeroOrMore(or(literal(" "), literal("\t"), literal("\n"), literal("\r\n"), literal("\r")));

    dg["UnlessStatement"] = and(literal("unless"), space, literal("("), space, dg["Expression"], space, literal(")"), dg["Statement"]);
    dg["Statement"] = or(dg["UnlessStatement"], dg["Statement"]);
    writeln("Parsing...");
    sw.start();
    auto result  = (dg(
`
main()
{
   int n, i = 3, count, c;

   printf("Enter the number of prime numbers required\n");
   scanf("%d",&n);

   if ( n >= 1 )
   {
      printf("First %d prime numbers are :\n",n);
      printf("2\n");
   }

   for ( count = 2 ; count <= n ;  )
   {
      for ( c = 2 ; c <= i - 1 ; c++ )
      {
         if ( i%c == 0 )
            break;
      }
      if ( c == i )
      {
         printf("%d\n",i);
         count++;
      }
      i++;
   }

   return 0;
}
`));
    sw.stop();
    writeln("Done. Parsing in ", sw.peek().msecs - last, " ms.");
    writeln(result);
    //writeln(makeSwitch(10));
    /+
    string input = "1";
	writeln(dg(input));
    writeln(Arithmetic(input));

    int N = 100;
    foreach(n; 0..50)
    {
        auto b = benchmark!(()=> Arithmetic(input), ()=>dg(input))(N);
        auto t1 = b[0].to!("usecs", float)/N;
        auto t2 = b[1].to!("usecs", float)/N;
        writefln("%d: %.1f %.1f => %.2f", input.length, t1, t2, t2/t1);
        input ~= "+1";
    }
    +/
}

