module pegged.internalpeg;

import std.algorithm;
import std.conv;
import std.stdio;
import std.typecons;
import std.typetuple;

import pegged.peg;
import pegged.utils.associative; // as long as associative arrays do not function correctly at CT.


string internalOkFailMixin() @property
{
    return
    `ParseTree ok(string[] capture, ParseTree[] children = (ParseTree[]).init, NamedCaptures newCaptures = NamedCaptures.init)
    {
        if (newCaptures.length > 0)
            input.namedCaptures ~= newCaptures;
        
        // Updating the index
        auto begin = input.pos;
        input.pos = addCaptures(input.pos, capture);
        
        return ParseTree(name, true, capture, begin, input.pos, children);
    }
   
    ParseTree fail()
    {
        return ParseTree(name, false, [name ~ " failure at pos " ~ input.pos.toString()], input.pos, input.pos, (ParseTree[]).init);
    }
    `;
}

mixin template internalPeg()
{
    class Any : Parser
    {
        enum name = "Any";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());  
            return (input.length > 0) ? ok([input[0..1]])
                                      : fail();
        }
    }

    class EOI : Parser
    {
        enum name = "EOI";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());
            return (input.pos.index == input.text.length) ? ok(null)
                                                          : fail();
        }
        
        //mixin(stringToInputMixin());
    }

    class Char(char c) : Parser
    {
        enum name = "Char!(" ~ c ~ ")";
        
        ParseTree parse() 
        { 
            mixin(internalOkFailMixin());
            return (input.length > 0 && input[0] == c) ? ok(["" ~ c])
                                                       : fail();
        }

        //mixin(stringToInputMixin());
    }

    class Lit(string s) : Parser
    {
        enum name = "Lit!("~s~")";
        
        ParseTree parse()
        {   
            mixin(internalOkFailMixin());
            return (input.length >= s.length
                && input[0..s.length] == s ) ? ok([s])
                                             : fail();
        }
        
        //mixin(stringToInputMixin());
    }

    class Range(char begin, char end) : Parser
    {
        enum name = "Range!("~begin~","~end~")";
        
        ParseTree parse() 
        { 
            mixin(internalOkFailMixin());
            return (input.length 
                && input[0] >= begin 
                && input[0] <= end  ) ? ok([input[0..1]])
                                      : fail();
        }
        
        //mixin(stringToInputMixin());
    }

    class Seq(Exprs...) if (Exprs.length > 0) : Parser
    {
        enum name = "Seq!(" ~ Exprs.stringof ~ ")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());
            ParseTree result = ok((string[]).init);
            
            foreach(i,expr; Exprs)
            {            
                auto p = (new expr()).parse();
                if (p.success)
                {
                    if (p.capture.length > 0) 
                    {
                        result.capture ~= p.capture;
                        result.children ~= p;
                    }
                    
                    result.end = p.end;
                }
                else
                {
                    return fail();
                }
            }
            return result;
        }

        //mixin(stringToInputMixin());
    }

    class SpaceSeq(Exprs...) if (Exprs.length > 0) : Parser
    {
        enum name = "SpaceSeq!(" ~ Exprs.stringof ~ ")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());

            ParseTree result = ok((string[]).init);
            
            foreach(i,expr; Exprs)
            {            
                auto p = (new Seq!(expr, Spacing)()).parse(result);
                if (p.success)
                {
                    if (p.capture.length > 0) 
                    {
                        result.capture ~= p.capture;
                        result.children ~= p;
                    }
                    
                    result.end = p.end;
                }
                else
                {
                    return fail();
                }
                
            }
            return result;
        }

        //mixin(stringToInputMixin());
    }

    class Action(Expr, alias action)
    {
        enum name = "Action!("~Expr.name~", "~__traits(identifier, action)~")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());
            auto p = (new Expr()).parse(input);
            if (p.success)
                return action(p);
            return p;
        }
        
        //mixin(stringToInputMixin());
    }

    /// stores a named capture (that is, an entire parse tree)
    class Named(Expr, string name) : Parser
    {
        enum name = "Named!("~Expr.name ~", " ~ name~")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());
            auto p = (new Expr()).parse(input);
            if (p.success) 
                input.namedCaptures[name] = p.parseTree;
            return p;
        }
        
        //mixin(stringToInputMixin());
    }
    
    class Or(Exprs...) : Parser
    {
        enum name = "Or!("~Exprs.stringof~")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());
            
            ParseTree p;
            
            foreach(expr; Exprs)
            {
                p = (new expr()).parse();
                if (p.success) return p;
            }
            return p;// fail, return the last one.
        }
        
        //mixin(stringToInputMixin());
    }

    class Option(Expr) : Parser 
    {
        enum name = "Option!("~Expr.name~")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());
            
            auto p = (new Expr()).parse();
            if (p.success)
                return p;
            else
                return ok(null);
        }
        
        //mixin(stringToInputMixin());
    }

    class ZeroOrMore(Expr) : Parser
    {
        enum name = "ZeroOrMore!("~Expr.name~")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());
            
            string[] capture;
            ParseTree[] children;
            auto begin = input.pos;
            
            auto p = (new Expr()).parse();
            if (!p.success) return ok(capture);
            
            while (p.success)
            {
                capture ~= p.capture;
                children ~= p;
                p = (new Expr()).parse();
            }
            
            return ParseTree(name, true, capture, begin, p.end, children);
        }

        //mixin(stringToInputMixin());
    }

    class OneOrMore(Expr) : Parser
    {
        enum name = "OneOrMore!("~Expr.name~")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());
            
            string[] capture;
            ParseTree[] children;
            auto begin = input.pos;
            
            auto p = (new Expr()).parse();            
            if (!p.success) return fail();            
            
            while (p.success)
            {
                capture ~= p.capture;
                children ~= p;
                p = (new Expr()).parse();
            }
                    
            return ParseTree(name, true, capture, begin, p.end, children);
        }
        
        //mixin(stringToInputMixin());
    }

    class PosLookAhead(Expr) : Parser
    {
        enum name = "PosLookAhead!("~Expr.name~")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin());   
            return (new Expr()).parse().success ? ok((string[]).init) // no name nor capture transmission
                                                : fail();
        }
        
        //mixin(stringToInputMixin());                                                                                
    }

    class NegLookAhead(Expr) : Parser
    {
        enum name = "NegLookAhead!("~Expr.name~")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin()); 
            return (new Expr()).parse().success ? fail()
                                        : ok((string[]).init); // no name nor capture transmission
        }
        
        //mixin(stringToInputMixin());    
    }

    class Drop(Expr) : Parser
    {
        enum name = "Drop!(" ~ Expr.name ~ ")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin()); 
            
            auto nc = input.namedCaptures;
            auto p = (new Expr()).parse();
            p.capture = null;  // dropping the captures
            input.namedCaptures = nc;
            return p;
        }
        
        //mixin(stringToInputMixin());  
    }

    class Fuse(Expr) : Parser
    {
        enum name = "Fuse!(" ~ Expr.name ~ ")";
        
        ParseTree parse()
        {
            mixin(internalOkFailMixin()); 
            
            auto p = (new Expr()).parse();
            
            string capture;
            foreach(cap; p.capture) capture ~= cap;
            p.capture = [capture];
            p.children = null;
            return p;
        }
        
        //mixin(stringToInputMixin());  
    }
    
    alias Or!(Lit!"\r\n", Lit!"\n", Lit!"\r") EOL;
    alias Drop!(ZeroOrMore!(Or!(Lit!" ", Lit!"\t", EOL))) Spacing;
}