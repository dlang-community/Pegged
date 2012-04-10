module pegged.peg;

import std.algorithm;
import std.array;
import std.conv;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.utils.associative; // as long as associative arrays do not function correctly at CT.

struct Pos
{
    size_t index; // linear index
    size_t line;  // input line
    size_t col;   // input column
    
    string toString() @property
    {
        return "[index: " ~ to!string(index) ~ ", line: " ~ to!string(line) ~ ", col: " ~ to!string(col) ~ "]";
    }
}

Pos addCapture(Pos pos, string capture)
{
    bool foundCR;
    foreach(dchar c; capture)
        if (c =='\r') 
        { 
            foundCR = true; 
            ++pos.index; 
            ++pos.line; 
            pos.col = 0; 
        }
        else if (c =='\n') 
        { 
            if (foundCR) // \r\n -> just one line termination
            {
                foundCR = false;
                ++pos.index; // but still two chars in the input?
                continue;
            }
            else
            {
                ++pos.index; 
                ++pos.line; 
                pos.col = 0;
            }
        }
        else
        {
            ++pos.index;
            ++pos.col;
        }
    return pos;        
}

Pos addCaptures(Pos pos, string[] captures)
{
    foreach(capt; captures) pos = addCapture(pos, capt);
    return pos;
}

struct ParseTree
{
    string name;
    bool success;
    string[] capture;
    Pos begin, end;
    ParseTree[] children;
    
    string toString(int level = 0) @property
    {
        string tabs;        
        foreach(i; 0..level) tabs ~= "  ";
        string ch;
        foreach(child; children)
            ch ~= tabs ~ child.toString(level+1);
        return tabs ~ name ~ ": " 
             ~ "[" ~ begin.toString() ~ " - " ~ end.toString() ~ "]"
             ~ to!string(capture)
//                        ~ next[0..to!int(capture[0])] ~ "` / `" ~ next[to!int(capture[0])..$] ~ "`") 
             ~ (children.length > 0 ? "\n" ~ ch : "\n");
    }
}

alias Tuple!(string, ParseTree) NamedCapture;
alias AssociativeList!(string, ParseTree) NamedCaptures;

struct Input
{
    string text;
    Pos pos;
    NamedCaptures namedCaptures;
    
    string target() @property
    {
        return text[pos.index..$];
    }
    
    alias target this;
    
    string toString() @property
    {
        return "Input: " ~ target ~ "\n" 
             ~ "Named captures: " ~ namedCaptures.toString ~ "\n";
    }
}

struct Output
{
    string text;
    Pos pos;
    NamedCaptures namedCaptures;
    
    ParseTree parseTree;
    alias parseTree this; // automatic 'conversion' into parseTree, to simplify some expressions

    string toString() @property
    {
        string errorStack;
        if (!parseTree.success)
        {
            string tab = "  ";
            foreach(errorMsg; parseTree.capture)
            {
                errorStack ~= errorMsg ~ "\n" ~ tab;
                tab ~= "  ";
            }
        }
        
        return "Parse output: " ~ (parseTree.success ? "success" : "failure") ~ "\n"
             ~ "named captures: " ~ namedCaptures.toString ~ "\n"
             ~ "position: " ~ pos.toString() ~ "\n"
             //~ "parsed : `"~text[0..pos.index]~"`\n"
             //~ "not parsed: `"~text[pos.index..$]~"`\n"
             ~ (parseTree.success ? "parse tree:\n" ~ parseTree.toString() : "")
             ~ (parseTree.success ? "" : errorStack); // error message stored in parseTree.capture
    }
}


string inheritMixin(string name, string code)
{
    return
    "class " ~ name ~ " : " ~ code ~ " {\n"
  ~ "enum name = `" ~ name ~ "`;\n"
  ~ "static Output parse(Input input)
    {
        mixin(okfailMixin());
        auto p = typeof(super).parse(input);
        p.parseTree.name = `"~name~ "`;
        p.parseTree.children = [p.parseTree];
        return p;                     
    }
    
    mixin(stringToInputMixin());
}\n";
}


string wrapMixin(string name, string code)
{
    return
"class " ~ name
~ " : " ~ code 
~ "\n{\n"
~ "enum name = `" ~ name ~ "`;\n"
~ "    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        p.parseTree.name = `"~name~"`;
        return p;
    }
    
    mixin(stringToInputMixin());
}\n";

}

string okfailMixin() @property
{
    return
    `Output ok(string[] capture, ParseTree[] children = (ParseTree[]).init, NamedCaptures newCaptures = NamedCaptures.init)
    {
        if (newCaptures.length > 0)
            input.namedCaptures ~= newCaptures;
        
        // Updating the index
        auto end = addCaptures(input.pos, capture);
        
        return Output(input.text,
                      end,
                      input.namedCaptures,
                      ParseTree(name, true, capture, input.pos, end, children));
    }
   
    Output fail(Pos farthest = Pos.init, string[] errors = null)
    {
        return Output(input.text,
                      input.pos,
                      input.namedCaptures,
                      ParseTree(name, false, (errors.empty ? [name ~ " failure at pos " ~ input.pos.toString()] : errors), 
                                input.pos, (farthest == Pos.init) ? input.pos : farthest,
                                (ParseTree[]).init));
    }
    `;
}

string stringToInputMixin() @property
{
    return 
"static Output parse(string input)
{
    return parse(Input(input, Pos(0,0,0), NamedCaptures.init));
}

static Output parse(Output input)
{
    return parse(Input(input.text, input.pos, input.namedCaptures));
}";
}

string getName(string s, Exprs...)() @property
{
    string result = s ~ "!(";
    foreach(i,e;Exprs) 
        //static if (__traits(compiles, e.name))
        result ~= e.name ~ ", ";
        //else
        //    result ~= e.stringof ~ ", ";
        
    static if (Exprs.length > 0)
        result = result[0..$-2];
    return result ~ ")";
}

class Parser
{
    enum name = "Parser";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
        
        return fail();
    }
    
    mixin(stringToInputMixin());
}

alias Parser Failure;

/// Eps = epsilon, "", the empty string
class Eps : Parser
{
    enum name = "Eps";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());  
        return ok([""]);
    }
    
    mixin(stringToInputMixin);
}

class Any : Parser
{
    enum name = "Any";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());  
        if (input.length > 0)
            return ok([input[0..1]]);
        else
            return fail();
    }
    
    mixin(stringToInputMixin());
}

class EOI : Parser
{
    enum name = "EOI";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
        return (input.pos.index == input.text.length) ? ok(null)
                                                      : fail();
    }
    
    mixin(stringToInputMixin());
}

class Char(char c) : Parser
{
    enum name = "Char!(" ~ c ~ ")";
    
    static Output parse(Input input) 
    { 
        mixin(okfailMixin());
        return (input.length > 0 && input[0] == c) ? ok(["" ~ c])
                                                   : fail();
    }

    mixin(stringToInputMixin());
}

class Lit(string s) : Parser
{
    enum name = "Lit!("~s~")";
    
    static Output parse(Input input)
    {   
        mixin(okfailMixin());
        return (input.length >= s.length
             && input[0..s.length] == s ) ? ok([s])
                                          : fail();
    }
    
    mixin(stringToInputMixin());
}

class Range(char begin, char end) : Parser
{
    enum name = "Range!("~begin~","~end~")";
    
    static Output parse(Input input) 
    { 
        mixin(okfailMixin());
        return (input.length 
             && input[0] >= begin 
             && input[0] <= end  ) ? ok([input[0..1]])
                                   : fail();
    }
    
    mixin(stringToInputMixin());
}

class Seq(Exprs...) : Parser
{
    enum name = "Seq!" ~ Exprs.stringof;// getName!("Seq",Exprs)();  // Segmentation fault???
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
        Output result = ok((string[]).init);
        
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
                
                result.pos = p.pos;
                result.parseTree.end = p.pos;
                result.namedCaptures = p.namedCaptures;
            }
            else
            {
                return fail(p.parseTree.end,
                            (name ~ " failure with sub-expr #" ~ to!string(i) ~ " `" ~ expr.name ~ "` at pos " ~ to!string(result.pos)) ~ p.capture);
            }
        }
        return result;
    }

    mixin(stringToInputMixin());
}

class SpaceSeq(Exprs...) : Parser
{
    enum name = getName!("SpaceSeq", Exprs)();
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());

        // consuming space before the first expression
        input.pos = Spacing.parse(input).pos;
        
        Output result = ok((string[]).init);
        
        foreach(i,expr; Exprs)
        {            
            auto p = Seq!(expr, Spacing).parse(result);
            if (p.success)
            {
                if (p.capture.length > 0) 
                {
                    result.capture ~= p.capture;
                    result.children ~= p.children[0];
                }
                
                result.pos = p.pos;
                result.parseTree.end = p.pos;
                result.namedCaptures = p.namedCaptures;
            }
            else
            {
                return fail(p.parseTree.end,
                            (name ~ " failure with sub-expr #" ~ to!string(i) ~ " `" ~ expr.name ~ "` at pos " ~ to!string(result.pos)) ~ p.capture);
            }
            
        }
        return result;
    }

    mixin(stringToInputMixin());
}

class Action(Expr, alias action)
{
    enum name = "Action!("~Expr.name~", "~__traits(identifier, action)~")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
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
    enum name = "Named!("~Expr.name ~", " ~ name~")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
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
    enum name = "EqualMatch!("~Expr.name ~", " ~ name~")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
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
    enum name = "EqualParseTree!("~Expr.name ~", " ~ name~")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
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
    enum name = "PushName!(" ~ Expr.name ~ ")";
    
    static Output parse(Input input)
    {
        //writeln("Entering PushName("~Expr.name ~")", input.text," ", input.namedCaptures.length);
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
    enum name = "FindAndPop!("~Expr.name~")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
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

class Or(Exprs...) : Parser
{
    enum name = getName!("Or", Exprs)();
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
        
        Output p;
        
        size_t errorIndex;
        string errorName;
        Pos errorPos;
        string[] errorMessages;
        
        foreach(i, expr; Exprs)
        {
            p = expr.parse(input);
            if (p.success) 
            {
                return p;
            }
            else // store the expr which went the farthest
            {
                if (i == 0 || p.parseTree.end.index > errorPos.index)
                {
                    errorIndex = i;
                    errorName = Exprs[i].name; 
                    errorPos = p.parseTree.end;
                    errorMessages = p.capture;
                }
            }
        }
        return fail(errorPos,
                    (name ~ " failure for sub-expr # " ~ to!string(errorIndex) ~ " `" ~ errorName ~ "` at pos " ~ to!string(errorPos)) ~ errorMessages);
    }
    
    mixin(stringToInputMixin());
}

class Option(Expr) : Parser 
{
    enum name = "Option!("~Expr.name~")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
        
        auto p = Expr.parse(input);
        if (p.success)
            return p;
        else
            return ok(null);
    }
    
    mixin(stringToInputMixin());
}

class ZeroOrMore(Expr) : Parser
{
    enum name = "ZeroOrMore!("~Expr.name~")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
        
        string[] capture;
        ParseTree[] children;
        
        auto p = Expr.parse(input);
        Pos end = input.pos; 
        if (!p.success) return ok(capture);
        while (p.success)
        {
            capture ~= p.capture;
            children ~= p;
            end = p.pos;
            p = Expr.parse(p);
        }
        
        return Output(input.text,
                      end,
                      p.namedCaptures,
                      ParseTree(name, true, capture, input.pos, end, children));
    }

    mixin(stringToInputMixin());
}

class OneOrMore(Expr) : Parser
{
    enum name = "OneOrMore!("~Expr.name~")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());
        
        string[] capture;
        ParseTree[] children;
        
        Pos start = input.pos; 
        auto p = Expr.parse(input);
        
        if (!p.success) 
            return fail(p.parseTree.end, p.capture);
        
        while (p.success)
        {
            capture ~= p.capture;
            children ~= p;
            start = p.pos;
            p = Expr.parse(p);
        }
                
        return Output(input.text,
                      start,
                      p.namedCaptures,
                      ParseTree(name, true, capture, input.pos, start, children));
    }
    
    mixin(stringToInputMixin());
}

class PosLookAhead(Expr) : Parser
{
    enum name = "PosLookAhead!("~Expr.name~")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin());   
        return Expr.parse(input).success ? ok((string[]).init) // no name nor capture transmission
                                         : fail();
    }
    
    mixin(stringToInputMixin());                                                                                
}

class NegLookAhead(Expr) : Parser
{
    enum name = "NegLookAhead!("~Expr.name~")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin()); 
        return Expr.parse(input).success ? fail()
                                         : ok((string[]).init); // no name nor capture transmission
    }
    
    mixin(stringToInputMixin());    
}

class Drop(Expr) : Parser
{
    enum name = "Drop!(" ~ Expr.name ~ ")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin()); 
        
        auto p = Expr.parse(input);
        if (!p.success)
            return p;
        
        //p.pos = addCaptures(p.pos, p.capture); // advancing the index
        p.capture = null;  // dropping the captures
        p.namedCaptures = input.namedCaptures; // also dropping the named captures
        return p;
    }
    
    mixin(stringToInputMixin());  
}

// To keep an expression in the parse tree, even though Pegged would cut it naturally
class Keep(Expr)
{
    enum name = "Keep!(" ~ Expr.name ~ ")";
    
    static Output parse(Input input)
    {
        auto p = Expr.parse(input);
        p.name = name;
        return p;
    }
    
    mixin(stringToInputMixin());  
}


class Fuse(Expr) : Parser
{
    enum name = "Fuse!(" ~ Expr.name ~ ")";
    
    static Output parse(Input input)
    {
        mixin(okfailMixin()); 
        
        auto p = Expr.parse(input);
        if (!p.success) 
            return p;
        
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


mixin(wrapMixin("letter",   `Range!('a','z')`));
mixin(wrapMixin("Letter",   `Range!('A','Z')`));
mixin(wrapMixin("Alpha",    `Or!(letter, Letter, Lit!"_")`));

mixin(wrapMixin("Digit",    `Range!('0','9')`));
mixin(wrapMixin("Alphanum", `Or!(Alpha, Digit)`));

mixin(wrapMixin("Identifier", `Fuse!(Seq!(Alpha, ZeroOrMore!(Alphanum)))`));
mixin(wrapMixin("QualifiedIdentifier", `Fuse!(Seq!(Identifier, ZeroOrMore!(Seq!(Lit!".", Identifier))))`));

mixin(wrapMixin("Space",   `Lit!" "`));
mixin(wrapMixin("Blank",   `Or!(Space, Lit!"\t", Lit!"\b", Lit!"\v", Lit!"\a")`));
mixin(wrapMixin("LF",      `Lit!"\n"`));
mixin(wrapMixin("CR",      `Lit!"\r"`));
mixin(wrapMixin("CRLF",    `Lit!"\r\n"`));
mixin(wrapMixin("EOL",     `Or!(CRLF, LF, CR)`));
mixin(wrapMixin("Spacing", `Drop!(ZeroOrMore!(Or!(Blank, EOL)))`));

mixin(wrapMixin("DoubleQuote",q{Lit!`"`}));
mixin(wrapMixin("Quote",       `Lit!"'"`));
mixin(wrapMixin("BackQuote",  q{Lit!"`"}));
mixin(wrapMixin("Slash",       `Lit!"/"`));
mixin(wrapMixin("BackSlash",  q{Lit!`\`}));

mixin(wrapMixin("Line", `Fuse!(Seq!(ZeroOrMore!(Seq!(NegLookAhead!(EOL), Any)), Or!(EOL,EOI)))`));
mixin(wrapMixin("Lines", `OneOrMore!Line`));

class List(E) : SpaceSeq!(E,ZeroOrMore!(SpaceSeq!(Lit!(","),E)))
{
    enum name = `List`;

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        return Output(p.text, p.pos, p.namedCaptures,
                      ParseTree(`List`, p.success, p.capture, input.pos, p.pos, [p.parseTree]));
    }
    
    mixin(stringToInputMixin());
    
}


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
    return ParseTree(p.name, p.success, capture, p.begin, p.end, children);
}

ParseTree fuseCaptures(ParseTree p)
{
    if (p.capture.length < 2) return p;
    foreach(capt; p.capture[1..$])
        p.capture[0] ~= capt;
    p.capture = p.capture[0..1];
    return p;
}
