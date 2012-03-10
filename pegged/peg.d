module pegged.peg;

import std.algorithm;
import std.conv;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.utils.associative; // as long as associative arrays do not function correctly at CT.

struct ParseTree
{
    string name;
    bool success;
    string[] capture;
    ParseTree[] children;
    
    string toString(int level = 0) @property
    {
        string tabs;        
        foreach(i; 0..level) tabs ~= "  ";
        string ch;
        foreach(child; children)
            ch ~= tabs ~ child.toString(level+1);
        return tabs ~ name ~ ": " 
             ~ (success ? to!string(capture) : "") 
//                        ~ next[0..to!int(capture[0])] ~ "` / `" ~ next[to!int(capture[0])..$] ~ "`") 
             ~ (children.length > 0 ? "\n" ~ ch : "\n");
    }
}

alias Tuple!(string, ParseTree) NamedCapture;
alias AssociativeList!(string, ParseTree) NamedCaptures;

struct Input
{
    string text;
    NamedCaptures namedCaptures;
    alias text this; // automatic 'conversion' into text, to simplify some expressions
    
    string toString() @property
    {
        return "Input: " ~text ~ "\n" 
             ~ "Named captures: " ~ namedCaptures.toString ~ "\n";
    }
}

struct Output
{
    string next;
    NamedCaptures namedCaptures;
    
    ParseTree parseTree;
    alias parseTree this; // automatic 'conversion' into parseTree, to simplify some expressions

    string toString() @property
    {
        return "Parse output: " ~ (parseTree.success ? "success" : "failure") ~ "\n"
             ~ "not parsed: `"~next~"`\n"
             ~ "named captures: " ~ namedCaptures.toString ~ "\n"
             ~ parseTree.toString()
             ~ (parseTree.success ? "" : to!string(parseTree.capture));
    }
}

string inheritMixin(string name, string code)
{
    return
    "class " ~ name ~ " : " ~ code ~ " {\n"
  ~ "static Output parse(Input input)
    {
        mixin(okfailMixin(\""~name~"\"));
        auto p = typeof(super).parse(input);
        p.parseTree.name = \""~name~ "\";
        p.parseTree.children = [p.parseTree];
        return p;                     
    }
    
    mixin(stringToInputMixin());
}\n";
}

string wrapMixin(string name, string code)
{
    return
"class " ~ name ~ " : " ~ code 
~ "\n{\n"
~ "    static Output parse(Input input)
    {
        //mixin(okfailMixin(\""~name~"\"));
        auto p = typeof(super).parse(input);
        p.parseTree.name = \""~name~"\";
        return p;
    }
    
    mixin(stringToInputMixin());
}\n";
}

string okfailMixin(string name)
{
    return
    "Output ok(string[] capture, string next, ParseTree[] children = (ParseTree[]).init, NamedCaptures newCaptures = NamedCaptures.init)
    {
        if (newCaptures.length > 0)
            input.namedCaptures ~= newCaptures;
        return Output(next,
                      input.namedCaptures,
                      ParseTree(\""~name~"\", true, capture, children));
    }
   
    Output fail(string message = string.init)
    {
        return Output(input.text,
                      input.namedCaptures,
                      ParseTree(\""~name~"\", false, [message], (ParseTree[]).init));
    }
    ";
}

string stringToInputMixin() @property
{
    return 
"static Output parse(string input)
{
    return parse(Input(input, NamedCaptures.init));
}

static Output parse(Output input)
{
    return parse(Input(input.next, input.namedCaptures));
}";
}

class Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Parser"));
        
        return fail();
    }
    
    mixin(stringToInputMixin());
}

alias Parser Failure;

/// Eps = epsilon, "", the empty string
class Eps : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Eps"));  
        return ok([""], input); // alias this conversion input -> input.text
    }
    
    mixin(stringToInputMixin());
}

class Any : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Any"));  
        if (input.length > 0)
            return ok([input[0..1]], input[1..$]);
        else
            return fail();
    }
    
    mixin(stringToInputMixin());
}

class End : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("EndOfInput"));
        return (input.length == 0) ? ok(null, input)
                                   : fail();
    }
    
    mixin(stringToInputMixin());
}

alias End EOI;

class Char(char c) : Parser
{
    static Output parse(Input input) 
    { 
        mixin(okfailMixin("Char"));
        return (input.length > 0 && input[0] == c) ? ok(["" ~ c], input[1..$])
                                                   : fail();
    }

    mixin(stringToInputMixin());
}

class Lit(string s) : Parser
{
    static Output parse(Input input)
    {   
        mixin(okfailMixin("Lit"));
        return (input.length >= s.length
             && input[0..s.length] == s ) ? ok([s], input[s.length..$])
                                          : fail();
    }
    
    mixin(stringToInputMixin());
}

class Range(char begin, char end) : Parser
{
    static Output parse(Input input) 
    { 
        mixin(okfailMixin("Range"));//!("~begin~","~end~")"));
        return (input.length 
             && input[0] >= begin 
             && input[0] <= end  ) ? ok([input[0..1]], input[1..$])
                                   : fail();
    }
    
    mixin(stringToInputMixin());
}

class Seq(Exprs...) if (Exprs.length > 0) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Seq!"~Exprs.stringof));
        Output result = ok((string[]).init, input);
        
        //writeln("Entering Seq" ~ Exprs.stringof ~", namedCapture = ", result.namedCaptures.length);
        foreach(i,expr; Exprs)
        {
            //writeln("Testing expr #", i, " (", expr.stringof, ")");
            
            // munch space
            static if (i>0)
                result.next = Spaces.parse(result.next).next;
            
            //writeln("Seq: expr #", to!string(i));
            auto p = expr.parse(result);
            //writeln("Seq"~Exprs.stringof~" test #", i, "(", expr.stringof, ") before if namedCaptures = ", result.namedCaptures.length);
            if (p.success)
            {
                //writeln("Seq: expr #", to!string(i), " success. namedCaptures: ", input.namedCaptures);
                if (p.capture.length > 0) 
                {
                    result.capture ~= p.capture;
                    result.children ~= p;
                }
                result.next = p.next;
                result.namedCaptures = p.namedCaptures;
                //writeln("Seq"~Exprs.stringof~" test #", i, "(", expr.stringof, ") after if namedCaptures = ", result.namedCaptures.length);
                //writeln("Seq, adding namedCapture ", p.namedCaptures);
            }
            else
            {
                int pos = input.length-result.next.length;
                return fail("Seq fail for expression #"~to!string(i)~" (" ~ expr.stringof ~") at position " 
                          ~ to!string(pos) ~ " [" ~ input[0..pos] ~ "]/[" ~ input[pos..$] ~ "]");
            }
        }
        return result;
    }

    mixin(stringToInputMixin());
}

/**
 * Like a Seq, but space-sensitive: it doesn't munch spaces.
 */
class Join(Exprs...) if (Exprs.length > 0) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Join!" ~ Exprs.stringof));
        Output result = ok((string[]).init, input);

        foreach(i,expr; Exprs)
        {
            auto p = expr.parse(result);
            if (p.success)
            {
                if (p.capture.length > 0) 
                {
                    result.capture ~= p.capture;
                    result.children ~= p;
                }
                result.next = p.next;
                result.namedCaptures = p.namedCaptures;
            }
            else
            {
                int pos = input.length-result.next.length;
                return fail("Join fail for expression #"~to!string(i)~" (" ~ expr.stringof ~") at position " 
                          ~ to!string(pos) ~ " [" ~ input[0..pos] ~ "]/[" ~ input[pos..$] ~ "]");
            }
        }
        return result;
    }

    mixin(stringToInputMixin());
}

/**
 * Trying to replace the old action system
 */
class Action(Expr, alias action)
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Action!("~Expr.stringof~", "~__traits(identifier, action)~")"));
        auto p = Expr.parse(input);
        if (p.success)
            return action(p);
        return p;
    }
    
    mixin(stringToInputMixin());
}

/// stores a named capture (that is, an entire parse tree)
class Named(Expr, string name) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Named!("~Expr.stringof ~", " ~ name~")"));
        auto p = Expr.parse(input);
        if (p.success) 
            p.namedCaptures[name] = p.parseTree;
        //writeln("Named: adding capture ", name);
        return p;
    }
    
    mixin(stringToInputMixin());
}

/// Verifies that a match is equal to a named capture
class EqualMatch(Expr, string name) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("EqualMatch!("~Expr.stringof ~", " ~ name~")"));
        //writeln("Entering EqualMatch with input ", input);
        auto p = Expr.parse(input);
        //writeln("EqualMatch parse: ", p.success, p);
        if (p.success) 
        {
            foreach(named; input.namedCaptures)
            {
                if (named[0] == name)
                {
                    foreach(i, capt; named[1].capture)
                        if (capt != p.parseTree.capture[i]) return fail();
                    return p;
                }
            }
        }
        return fail();
    }
    
    mixin(stringToInputMixin());
}

/// Verifies that a parse tree is equal to a named capture
class EqualParseTree(Expr, string name) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("EqualParseTree!("~Expr.stringof ~", " ~ name~")"));
        //writeln("EqPT: ", name);
        auto p = Expr.parse(input);
        //writeln("Eqpt p: ",p);
        //writeln("nameCaptures in Eqpt: ", input.namedCaptures);
        if (p.success) 
        {
            foreach(named; input.namedCaptures)
            {
                if (named[0] == name)
                {
                    auto np = named[1]; // second field: parseTree
                    auto pp = p.parseTree;
                    //writeln("Found name == ", name);
                    //writeln(np, pp);
                    if ( np.name == pp.name)
                    {
                        if (np.capture.length != pp.capture.length || np.children.length != pp.children.length)
                            return fail();
                            
                        foreach(i, capt; np.capture)
                            if (capt != pp.capture[i]) return fail();
                        
                        foreach(i,child; np.children)
                            if ( child != pp.children[i]) return fail();
                    
                        return p;
                    }
                }
            }
        }
        return fail();
    }
    
    mixin(stringToInputMixin());
}


class PushName(Expr) : Parser
{
    static Output parse(Input input)
    {
        //writeln("Entering PushName("~Expr.stringof ~")", input.text," ", input.namedCaptures.length);
        auto p = Expr.parse(input);
        //writeln("PushName after Expr.parse calculation");
        //writeln("p = ", p.namedCaptures);
        if (p.success)
        {
            string cap;
            foreach(capt; p.parseTree.capture) cap ~= capt;
            NamedCapture nc = NamedCapture(cap, p.parseTree);
            //writeln("Pushing ", nc.name);
            p.namedCaptures ~= nc;
            
        }
        //writeln("Ending PushName ", p.namedCaptures.length);
        return p;
    }
    
    mixin(stringToInputMixin());
}


/// Compares the match with the named capture and pops the capture
class FindAndPop(Expr) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("FindAndPop!("~Expr.stringof~")"));
        auto p = Expr.parse(input);
        if (p.success)
        {
            string cap;
            foreach(capt; p.capture) cap ~= capt;
            if (cap in p.namedCaptures)
            {
                p.namedCaptures.discard(cap);
                return p;
            }
        }
        return fail();
    }
    
    mixin(stringToInputMixin());
}

class Or(Exprs...) if (Exprs.length) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Or!("~Exprs.stringof~")"));
        
        Output p;
        
        foreach(expr; Exprs)
        {
            p = expr.parse(input);
            if (p.success)
                return p;
        }
        return p;// fail, return the last one.
    }
    
    mixin(stringToInputMixin());
}

class Option(Expr) : Parser 
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Option!("~Expr.stringof~")"));
        
        auto p = Expr.parse(input);
        if (p.success)
            return p;
        else
            return ok(null, input);
    }
    
    mixin(stringToInputMixin());
}

class ZeroOrMore(Expr) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("ZeroOrMore!("~Expr.stringof~")"));
        
        string[] capture;
        ParseTree[] children;
        int len = input.length; 
        auto p = Expr.parse(input);
        if (!p.success)
            return ok(capture, input.text);
        //writeln("Before while loop: ", p);
        while (p.success
            && p.next.length < len) // to avoid an infinite loop if nothing is consumed by p
        {
            //writeln("In while loop: ", p);
            capture ~= p.capture;
            children ~= p;
            len = p.next.length;
            p = Expr.parse(p);
            //writeln("End of while loop: ", p);
        }
        //writeln("After while loop: ", p);
        
        //writeln("Ending 0+ with ", input.namedCaptures);
        return ok(capture, p.next, children, p.namedCaptures);
    }

    mixin(stringToInputMixin());
}

class OneOrMore(Expr) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("OneOrMore!("~Expr.stringof~")"));
        
        string[] capture;
        ParseTree[] children;
        int len = input.length;
        auto p = Expr.parse(input);
        if (!p.success) 
            return fail("OneOrMore!("~Expr.stringof~") failure on first parse.");
        
        
        while (p.success
            && p.next.length < len)
        {
            capture ~= p.capture;
            children ~= p;
            len = p.next.length;
            p = Expr.parse(p);
        }
                
        return ok(capture, p.next, children, p.namedCaptures);
    }
    
    mixin(stringToInputMixin());
}

class PosLookAhead(Expr) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("PosLookAhead!("~Expr.stringof~")"));   
        return Expr.parse(input).success ? ok((string[]).init,input) // no name nor capture transmission
                                         : fail("PosLookAhead failure on input: " ~ input);
    }
    
    mixin(stringToInputMixin());                                                                                
}

class NegLookAhead(Expr) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("NegLookAhead!("~Expr.stringof~")")); 
        return Expr.parse(input).success ? fail("NegLookAhead failure on input: " ~ input)
                                         : ok((string[]).init,input); // no name nor capture transmission
    }
    
    mixin(stringToInputMixin());    
}

class Drop(Expr) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Drop!(" ~ Expr.stringof ~ ")")); 
        
        auto p = Expr.parse(input);
        p.capture = null; 
        p.namedCaptures = input.namedCaptures; // Seems logical, but...
        return p;
    }
    
    mixin(stringToInputMixin());  
}

class Fuse(Expr) : Parser
{
    static Output parse(Input input)
    {
        mixin(okfailMixin("Fuse!(" ~ Expr.stringof ~ ")")); 
        
        auto p = Expr.parse(input);
        
        string capture;
        foreach(cap; p.capture) capture ~= cap;
        p.capture = [capture];
        p.children = null;
        return p;
    }
    
    mixin(stringToInputMixin());  
}


class List(Expr, Sep = Lit!",") : Parser
{
    static Output parse(Input input)
    {
        alias Seq!(Expr, Spacing, ZeroOrMore!(Seq!(Sep, Spacing, Expr, Spacing))) L;
        return L.parse(input);
    }
    
    mixin(stringToInputMixin());
}


mixin(wrapMixin("letter", `Range!('a','z')`));
mixin(wrapMixin("Letter", `Range!('A','Z')`));
mixin(wrapMixin("Alpha", `Or!(letter, Letter, Lit!"_")`));
mixin(wrapMixin("Digit", "Range!('0','9')"));
mixin(wrapMixin("Alphanum", "Or!(Alpha, Digit)"));

mixin(wrapMixin("Identifier", "Fuse!(Join!(Alpha, ZeroOrMore!(Alphanum)))"));
mixin(wrapMixin("QualifiedIdentifier", `Fuse!(Join!(Identifier, ZeroOrMore!(Seq!(Lit!".", Identifier))))`));

mixin(wrapMixin("Space", `Lit!" "`));
mixin(wrapMixin("Blank", `Or!(Space, Lit!"\t")`));
alias Lit!"\n" LF;
alias Lit!"\r" CR;
alias Lit!"\r\n" CRLF;
mixin(wrapMixin("EOL", `Or!(CRLF, LF, CR)`));
mixin(wrapMixin("Spacing", `Drop!(ZeroOrMore!(Or!(Blank, EOL)))`));
mixin(wrapMixin("Spaces", `Fuse!(ZeroOrMore!(Or!(Blank, EOL)))`));

alias Char!'"' DoubleQuote;
alias Lit!"'"  Quote;
alias Lit!"`" BackQuote;
alias Lit!"/" Slash;
alias Lit!(`\\`) BackSlash;

alias Fuse!(Join!(ZeroOrMore!(Join!(NegLookAhead!(EOL), Any)), Or!(EOL,EOI))) Line;
alias OneOrMore!Line Lines;

string[] leaves(ParseTree p)
{
    string[] result;
    if (p.children.length == 0)
        return p.capture[];
    else
    {
        foreach(child; p.children)
            result ~= leaves(child);
    }
    return result;
}

string[2][] treeUnification(ParseTree p1, ParseTree p2)
{
    string[2][] result;
    
    if (p1.name != p2.name)
    {
        p1 = fuseCaptures(p1);
        p2 = fuseCaptures(p2);
        result ~= [p1.capture[0], p2.capture[0]];
    }
    else
    {
        if (p1.children.length != p2.children.length)
            throw new Exception("Impossible unification between\n" ~ p1.toString() ~ "and\n"~p2.toString());
        
        foreach(i, child; p1.children)
        {
            result ~= treeUnification(child, p2.children[i]);
        }
    }
    return result;
}

ParseTree regenerateCaptures(ParseTree p)
{
    if (p.children.length == 0)
        return p;
    
    string[] capture = p.capture;
    ParseTree[] children;
    foreach(child; p.children)
    {
        children ~= regenerateCaptures(child);
        capture ~= children[$-1].capture;
    }
    return ParseTree(p.name, p.success, capture, children);
}

ParseTree fuseCaptures(ParseTree p)
{
    if (p.capture.length < 2) return p;
    foreach(capt; p.capture[1..$])
        p.capture[0] ~= capt;
    p.capture = p.capture[0..1];
    return p;
}
