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
    
    dstring toString() @property
    {
        return "[index: "d ~ to!dstring(index) ~ ", line: "d ~ to!dstring(line) ~ ", col: "d ~ to!dstring(col) ~ "]"d;
    }
}

Pos addCapture(Pos pos, dstring capture)
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

Pos addCaptures(Pos pos, dstring[] captures)
{
    foreach(capt; captures) pos = addCapture(pos, capt);
    return pos;
}

enum ParseLevel { validating, matching, parsing, noDecimation, fullest }

struct ParseTree
{
    dstring grammarName;
    dstring ruleName;
    bool success;
    dstring[] capture;
    Pos begin, end;
    ParseTree[] children;
    
    dstring name() @property { return grammarName ~ "."d ~ ruleName;}
    
    dstring toString(int level = 0) @property
    {
        dstring tabs;        
        foreach(i; 0..level) tabs ~= "  "d;
        dstring ch;
        foreach(child; children)
            ch ~= tabs ~ child.toString(level+1);
        return tabs ~ name ~ ": "d
             ~ "["d ~ begin.toString() ~ " - "d ~ end.toString() ~ "]"d
             ~ to!dstring(capture)
//                        ~ next[0..to!int(capture[0])] ~ "` / `" ~ next[to!int(capture[0])..$] ~ "`") 
             ~ (children.length > 0 ? "\n"d ~ ch : "\n"d);
    }
}

template CaptureTemplate(TParseTree)
{
    alias Tuple!(dstring, TParseTree) NamedCapture;
    alias AssociativeList!(dstring, TParseTree) NamedCaptures;
}

/// Returns true if T is a type that Pegged can use to construct parse trees.
template isParseTree(T)
{
    enum bool isParseTree = is(typeof(
    {
        T t = void;        // can define a parse tree
        
        if (t.success) {}  // can test for success
        auto s = t.success;
        t.success = s;     // success is writable
        
        // has a rule name (read/write)
        dstring r = t.ruleName;
        t.ruleName = r;
        
        // has a list of captured elements (read/write)
        dstring[] c = t.capture;
        t.capture = c;
        
        // has an array of children (read/write)
        // (TODO: maybe someday this could be relaxed into ranges of T?)
        T[] ch = t.children;
        t.children = ch;
    }));
}

static assert(isParseTree!(ParseTree));

struct Input(TParseTree) if ( isParseTree!(TParseTree) )
{
    mixin CaptureTemplate!TParseTree;
    
    dstring text;
    Pos pos;
    NamedCaptures namedCaptures;
    
    dstring target() @property
    {
        return text[pos.index..$];
    }
    
    alias target this;
    
    dstring toString() @property
    {
        return "Input: "d ~ target ~ "\n"d
             ~ "Named captures: "d ~ to!dstring(namedCaptures.toString) ~ "\n"d;
    }
}

struct Output(TParseTree) if ( isParseTree!(TParseTree) )
{
    mixin CaptureTemplate!TParseTree;
    
    dstring text;
    Pos pos;
    NamedCaptures namedCaptures;
    
    TParseTree parseTree;
    alias parseTree this; // automatic 'conversion' into parseTree, to simplify some expressions

    dstring toString() @property
    {
        dstring errorStack;
        if (!parseTree.success)
        {
            dstring tab = "  "d;
            foreach(errorMsg; parseTree.capture)
            {
                errorStack ~= errorMsg ~ "\n"d ~ tab;
                tab ~= "  "d;
            }
        }
        
        return "Parse output: "d ~ (parseTree.success ? "success"d : "failure"d) ~ "\n"d
             ~ "named captures: "d ~ to!dstring(namedCaptures.toString) ~ "\n"d
             ~ "position: "d ~ pos.toString() ~ "\n"d
             //~ "parsed : `"~text[0..pos.index]~"`\n"
             //~ "not parsed: `"~text[pos.index..$]~"`\n"
             ~ (parseTree.success ? "parse tree:\n"d ~ parseTree.toString() : ""d)
             ~ (parseTree.success ? ""d : errorStack); // error message stored in parseTree.capture
    }
}


dstring inheritMixin(dstring name, dstring code)
{
    return
    "class "d ~ name ~ " : "d ~ code ~ " {\n"d
  ~ "enum grammarName = ``; enum dstring ruleName = `"d ~ name ~ "`;\n"d
  ~ "static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
    {
        mixin(okfailMixin());
        Output p = typeof(super).parse!(pl)(input);
        static if (pl == ParseLevel.validating)
            p.capture = null;
        static if (pl <= ParseLevel.matching)
            p.children = null;
        static if (pl >= ParseLevel.parsing)
        {
            if (p.success)
            {
                if (p.grammarName == grammarName || pl >= ParseLevel.noDecimation)
                {
                    p.children = [p];
                }
                
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            else
                return fail(p.parseTree.end,
                            (grammarName~`.`~ruleName ~ ` failure at pos `d ~ to!dstring(p.parseTree.end) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture)));
        }
        return p;
    }
    
    mixin(stringToInputMixin());
}\n"d;
}

dstring parseCode()
{
    return 
"    static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
    {
        mixin(okfailMixin());
        
        Output p = typeof(super).parse!(pl)(input);
        static if (pl == ParseLevel.validating)
            p.capture = null;
        static if (pl <= ParseLevel.matching)
            p.children = null;
        static if (pl >= ParseLevel.parsing)
        {
            if (p.success)
            {                                
                //static if (pl == ParseLevel.parsing)
                //    p.parseTree = decimateTree(p.parseTree);
                
                if (p.grammarName == grammarName)
                {
                    p.children = [p];
                }
                
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            else
                return fail(p.parseTree.end,
                            (grammarName~`.`~ruleName ~ ` failure at pos `d ~ to!dstring(p.parseTree.end) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture)));
        }
                
        return p;
    }"d;
}


dstring wrapMixin(dstring name, dstring code)
{
    return
"class "d ~ name
~ " : "d ~ code 
~ "\n{\n"d
~ "    enum grammarName = `Pegged`; enum dstring ruleName = `"d ~ name ~ "`;\n"d
~ parseCode()
~ "    mixin(stringToInputMixin());
}\n"d;

}

dstring okfailMixin() @property
{
    return
    `Output ok(dstring[] capture, TParseTree[] children = (TParseTree[]).init, NamedCaptures newCaptures = NamedCaptures.init)
    {
        if (newCaptures.length > 0)
            input.namedCaptures ~= newCaptures;
        
        // Updating the index
        Pos end = addCaptures(input.pos, capture);
        static if (pl == ParseLevel.validating)
            capture = null;
        static if (pl <= ParseLevel.matching)
            children = null;

        return Output(input.text,
                      end,
                      input.namedCaptures,
                      TParseTree(grammarName, ruleName, true, capture, input.pos, end, children));
    }
   
    Output fail(Pos farthest = Pos.init, dstring[] errors = null)
    {
        return Output(input.text,
                      input.pos,
                      input.namedCaptures,
                      TParseTree(grammarName, ruleName, false, (errors.empty ? [grammarName~"."d~ruleName ~ " failure at pos "d ~ input.pos.toString()] : errors), 
                                input.pos, (farthest == Pos.init) ? input.pos : farthest,
                                (TParseTree[]).init));
    }
    `d;
}

dstring stringToInputMixin() @property
{
    return 
"static Output parse(ParseLevel pl = ParseLevel.parsing)(string s)
{
    return parse!(pl)(Input(to!dstring(s), Pos(0,0,0), NamedCaptures.init));
}

static Output parse(ParseLevel pl = ParseLevel.parsing)(wstring w)
{
    return parse!(pl)(Input(to!dstring(w), Pos(0,0,0), NamedCaptures.init));
}

static Output parse(ParseLevel pl = ParseLevel.parsing)(dstring d)
{
    return parse!(pl)(Input(d, Pos(0,0,0), NamedCaptures.init));
}

static Output parse(ParseLevel pl = ParseLevel.parsing)(Output input)
{
    return parse!(pl)(Input(input.text, input.pos, input.namedCaptures));
}"d;
}

dstring getName(string s, Exprs...)() @property
{
    dstring result = s ~ "!("d;
    foreach(i,e;Exprs) 
        //static if (__traits(compiles, e.name))
        result ~= e.ruleName ~ ", "d;
        //else
        //    result ~= e.stringof ~ ", ";
        
    static if (Exprs.length > 0)
        result = result[0..$-2];
    return result ~ ")"d;
}

template BuiltinRules(TParseTree) if ( isParseTree!TParseTree )
{
    mixin CaptureTemplate!TParseTree;

    class Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Parser"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
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
        enum grammarName = `Pegged`d; enum dstring ruleName = "Eps"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());
            return ok([""d]);
        }
        
        mixin(stringToInputMixin);
    }

    class Any : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Any"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());
            if (input.length > 0)
                return ok([to!dstring(input.front)]);
            else
                return fail();
        }
        
        mixin(stringToInputMixin());
    }

    class EOI : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "EOI"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());
            return (input.pos.index == input.text.length) ? ok(null)
                                                        : fail();
        }
        
        mixin(stringToInputMixin());
    }

    /+
    class Char(dchar c) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Char!(" ~ c ~ ")";
        
        static Output parse(Input input) 
        { 
            mixin(okfailMixin());
            return (input.length > 0 && input.front == c) ? ok(["" ~ c])
                                                    : fail();
        }

        mixin(stringToInputMixin());
    }
    +/

    class Lit(dstring s) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Lit!("d~s~")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {   
            mixin(okfailMixin());
            return (input.length >= s.length
                && input[0..s.length] == s ) ? ok([s])
                                            : fail();
        }
        
        mixin(stringToInputMixin());
    }

    class Range(dchar begin, dchar end) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Range!("d~begin~","d~end~")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        { 
            mixin(okfailMixin());
            return (input.length 
                && input.front >= begin 
                && input.front <= end  ) ? ok([to!dstring(input.front)])
                                        : fail();
        }
        
        mixin(stringToInputMixin());
    }

    class Seq(Exprs...) : Parser
    {
        enum grammarName = `Pegged`d; 
        enum dstring ruleName = "Seq!"d ~ to!dstring(Exprs.stringof); // ~ getName!("Seq",Exprs)();  // Segmentation fault???
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());
            dstring[] capture;
            Output result = Output(input.text,
                        input.pos,
                        input.namedCaptures,
                        TParseTree(grammarName, ruleName, true, capture, input.pos, input.pos, (TParseTree[]).init));
            //ok((dstring[]).init);
            
            foreach(i,expr; Exprs)
            {            
                Output p = expr.parse!(pl)(result);
                if (p.success)
                {
                    static if (pl >= ParseLevel.matching)
                        if (p.capture.length > 0) 
                        {
                            result.capture ~= p.capture;
                            static if (pl >= ParseLevel.parsing)
                                result.children ~= p;
                        }
                    
                    result.pos = p.pos;
                    result.parseTree.end = p.pos;
                    result.namedCaptures = p.namedCaptures;
                }
                else
                {
                    return fail(p.parseTree.end,
                                (grammarName~"."d~ruleName ~ " failure with sub-expr #"d ~ to!dstring(i) ~ " `"d ~ expr.ruleName ~ "` at pos "d ~ to!dstring(result.pos)) ~ p.capture);
                }
            }
            return result;
        }

        mixin(stringToInputMixin());
    }

    class SpaceSeq(Exprs...) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "SpaceSeq"d ~ to!dstring(Exprs.stringof); //getName!("SpaceSeq", Exprs)();
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());

            // consuming space before the first expression
            input.pos = Spacing.parse!(pl)(input).pos;
            dstring[] capture;
            Output result = Output(input.text,
                        input.pos,
                        input.namedCaptures,
                        TParseTree(grammarName, ruleName, true, capture, input.pos, input.pos, (TParseTree[]).init));
            //ok((dstring[]).init);
            
            foreach(i,expr; Exprs)
            {            
                Output p = Seq!(expr, Spacing).parse!(pl)(result);
                if (p.success)
                {
                    static if (pl >= ParseLevel.matching)
                        if (p.capture.length > 0) 
                        {
                            result.capture ~= p.capture;
                            static if (pl >= ParseLevel.parsing)
                                result.children ~= p;
                        }
                    
                    result.pos = p.pos;
                    result.parseTree.end = p.pos;
                    result.namedCaptures = p.namedCaptures;
                }
                else
                {
                    return fail(p.parseTree.end,
                                (grammarName ~ "."d ~ ruleName ~ " failure with sub-expr #"d ~ to!dstring(i) ~ " `"d ~ expr.ruleName ~ "` at pos "d ~ to!dstring(result.pos)) ~ p.capture);
                }
                
            }
            return result;
        }

        mixin(stringToInputMixin());
    }

    class Action(Expr, alias action)
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Action!("d~Expr.ruleName~", "d~__traits(identifier, action)~")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());
            Output p = Expr.parse!(pl)(input);
            if (p.success)
                return action(p);
            return p;
        }
        
        mixin(stringToInputMixin());
    }

    /// stores a named capture (that is, an entire parse tree)
    class Named(Expr, dstring Name) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Named!("d~Expr.ruleName ~", "d ~ Name~")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());
            Output p = Expr.parse!(pl)(input);
            if (p.success) 
                p.namedCaptures[Name] = p.parseTree;
            return p;
        }
        
        mixin(stringToInputMixin());
    }

    class Or(Exprs...) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Or"d ~ to!dstring(Exprs.stringof);//getName!("Or", Exprs)();
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());
            
            Output p;
            
            size_t errorIndex;
            dstring errorName;
            Pos errorPos;
            dstring[] errorMessages;
            
            foreach(i, expr; Exprs)
            {
                p = expr.parse!(pl)(input);
                if (p.success) 
                {
                    return p;
                }
                else // store the expr which went the farthest
                {
                    if (i == 0 || p.parseTree.end.index > errorPos.index)
                    {
                        errorIndex = i;
                        errorName = Exprs[i].ruleName; 
                        errorPos = p.parseTree.end;
                        errorMessages = p.capture;
                    }
                }
            }
            return fail(errorPos,
                        (grammarName~"."d~ruleName ~ " failure for sub-expr # "d ~ to!dstring(errorIndex) ~ " `"d ~ errorName ~ "` at pos "d ~ to!dstring(errorPos)) ~ errorMessages);
        }
        
        mixin(stringToInputMixin());
    }

    class Option(Expr) : Parser 
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Option!("d~Expr.ruleName~")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());
            
            Output p = Expr.parse!(pl)(input);
            if (p.success)
                return p;
            else
                return ok(null);
        }
        
        mixin(stringToInputMixin());
    }

    class ZeroOrMore(Expr) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "ZeroOrMore!("d~Expr.ruleName~")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());
            
            dstring[] capture;
            TParseTree[] children;
            
            Output p = Expr.parse!(pl)(input);
            Pos end = input.pos; 
            if (!p.success) return ok(capture);
            while (p.success)
            {
                capture ~= p.capture;
                children ~= p;
                end = p.pos;
                p = Expr.parse!(pl)(p);
            }
            
            static if (pl == ParseLevel.validating)
                p.capture = null;
            static if (pl <= ParseLevel.matching)
                p.children = null;
            
            return Output(input.text,
                        end,
                        p.namedCaptures,
                        TParseTree(grammarName, ruleName, true, capture, input.pos, end, children));
        }

        mixin(stringToInputMixin());
    }

    class OneOrMore(Expr) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "OneOrMore!("d~Expr.ruleName~")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());
            
            dstring[] capture;
            TParseTree[] children;
            
            Pos start = input.pos; 
            Output p = Expr.parse!(pl)(input);
            
            if (!p.success) 
                return fail(p.parseTree.end, p.capture);
            
            while (p.success)
            {
                capture ~= p.capture;
                children ~= p;
                start = p.pos;
                p = Expr.parse!(pl)(p);
            }
                    
            static if (pl == ParseLevel.validating)
                p.capture = null;
            static if (pl <= ParseLevel.matching)
                p.children = null;

            return Output(input.text,
                        start,
                        p.namedCaptures,
                        TParseTree(grammarName, ruleName, true, capture, input.pos, start, children));
        }
        
        mixin(stringToInputMixin());
    }

    class PosLookAhead(Expr) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "PosLookAhead!("d~Expr.ruleName~")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin());   
            return Expr.parse!(pl)(input).success ? ok((dstring[]).init) // no name nor capture transmission
                                                : fail();
        }
        
        mixin(stringToInputMixin());                                                                                
    }

    class NegLookAhead(Expr) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "NegLookAhead!("d~Expr.ruleName~")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin()); 
            return Expr.parse!(pl)(input).success ? fail()
                                                : ok((dstring[]).init); // no name nor capture transmission
        }
        
        mixin(stringToInputMixin());    
    }

    class Drop(Expr) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Drop!("d ~ Expr.ruleName ~ ")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin()); 
            
            Output p = Expr.parse!(pl)(input);
            if (!p.success)
                return p;
            
            //p.pos = addCaptures(p.pos, p.capture); // advancing the index
            static if (pl < ParseLevel.fullest)
            {
                p.capture = null;  // dropping the captures
                p.namedCaptures = input.namedCaptures; // also dropping the named captures
            }
            return p;
        }
        
        mixin(stringToInputMixin());  
    }

    // To keep an expression in the parse tree, even though Pegged would cut it naturally
    class Keep(Expr, dstring GrammarName)
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Keep!("d ~ Expr.ruleName ~ ", "d ~ GrammarName ~ ")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            Output p = Expr.parse!(pl)(input);
            p.grammarName = GrammarName;
            return p;
        }
        
        mixin(stringToInputMixin());  
    }


    class Fuse(Expr) : Parser
    {
        enum grammarName = `Pegged`d; enum dstring ruleName = "Fuse!("d ~ Expr.ruleName ~ ")"d;
        
        static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
        {
            mixin(okfailMixin()); 
            
            Output p = Expr.parse!(pl)(input);
            if (!p.success) 
                return p;
            
            dstring capture;
            foreach(cap; p.capture) capture ~= cap;
            p.capture = [capture];
            static if (pl < ParseLevel.fullest)
                p.children = null;
            
            return p;
        }
        
        mixin(stringToInputMixin());  
    }


    mixin(wrapMixin("letter"d,   `Range!('a','z')`d));
    mixin(wrapMixin("Letter"d,   `Range!('A','Z')`d));
    mixin(wrapMixin("Alpha"d,    `Or!(letter, Letter, Lit!"_")`d));

    mixin(wrapMixin("Digit"d,    `Range!('0','9')`d));
    mixin(wrapMixin("Alphanum"d, `Or!(Alpha, Digit)`d));

    mixin(wrapMixin("Identifier"d, `Fuse!(Seq!(Alpha, ZeroOrMore!(Alphanum)))`d));
    mixin(wrapMixin("QualifiedIdentifier"d, `Fuse!(Seq!(Identifier, ZeroOrMore!(Seq!(Lit!"."d, Identifier))))`d));

    mixin(wrapMixin("Space"d,   `Lit!" "`d));
    mixin(wrapMixin("Blank"d,   `Or!(Space, Lit!"\t", Lit!"\b", Lit!"\v", Lit!"\a")`d));
    mixin(wrapMixin("LF"d,      `Lit!"\n"`d));
    mixin(wrapMixin("CR"d,      `Lit!"\r"`d));
    mixin(wrapMixin("CRLF"d,    `Lit!"\r\n"`d));
    mixin(wrapMixin("EOL"d,     `Or!(CRLF, LF, CR)`d));
    mixin(wrapMixin("Spacing"d, `Drop!(ZeroOrMore!(Or!(Blank, EOL)))`d));

    mixin(wrapMixin("DoubleQuote"d,q{Lit!`"`}));
    mixin(wrapMixin("Quote"d,       `Lit!"'"`d));
    mixin(wrapMixin("BackQuote"d,  q{Lit!"`"}));
    mixin(wrapMixin("Slash"d,       `Lit!"/"`d));
    mixin(wrapMixin("BackSlash"d,  q{Lit!`\`}));

    mixin(wrapMixin("Line"d, `Fuse!(Seq!(ZeroOrMore!(Seq!(NegLookAhead!(EOL), Any)), Or!(EOL,EOI)))`d));
    mixin(wrapMixin("Lines"d, `OneOrMore!Line`d));


}


TParseTree regenerateCaptures(TParseTree)(TParseTree p) if ( isParseTree!TParseTree )
{
    if (p.children.length == 0)
        return p;
    
    dstring[] capture = p.capture;
    TParseTree[] children;
    foreach(child; p.children)
    {
        children ~= regenerateCaptures(child);
        capture ~= children[$-1].capture;
    }
    return TParseTree(p.grammarName, p.ruleName, p.success, capture, p.begin, p.end, children);
}

TParseTree fuseCaptures(TParseTree)(TParseTree p) if ( isParseTree!TParseTree )
{
    if (p.capture.length < 2) return p;
    foreach(capt; p.capture[1..$])
        p.capture[0] ~= capt;
    p.capture = p.capture[0..1];
    return p;
}