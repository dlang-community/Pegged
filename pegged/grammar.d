/**
This module was automatically generated from the following grammar:


# This is the PEG extended grammar used by Pegged
PEGGED:

Grammar     <- S GrammarName? Definition+ EOI
GrammarName <- RuleName ":" S             # Ext: named grammars
Definition  <- RuleName Arrow Expression S
RuleName    <- Identifier ParamList? S    # Ext: different arrows
Expression  <- Sequence (OR Sequence)*
Sequence    <- Prefix+
Prefix      <- (LOOKAHEAD / NOT / DROP / KEEP / FUSE)? Suffix
Suffix      <- Primary ( OPTION 
                       / ONEORMORE 
                       / ZEROORMORE 
                       / NamedExpr 
                       / WithAction)? S
Primary     <- Name !Arrow
             / GroupExpr
             / Literal 
             / Class 
             / ANY

Name        <- QualifiedIdentifier ArgList? S #Ext: names can be qualified
GroupExpr   <- :OPEN Expression :CLOSE S
Literal     <~ :Quote (!Quote Char)* :Quote S
             / :DoubleQuote (!DoubleQuote Char)* :DoubleQuote S
Class       <- '[' (!']' CharRange)* ']' S
CharRange   <- Char :'-' Char / Char
Char        <~ BackSlash ( Quote
                         / DoubleQuote
                         / BackQuote
                         / BackSlash 
                         / '-'         # Ext: escaping -,[,] in char ranges
                         / '[' 
                         / ']' 
                         / [nrt]
                         / [0-2][0-7][0-7]
                         / [0-7][0-7]?
                         / 'x' Hex Hex
                         / 'u' Hex Hex Hex Hex
                         / 'U' Hex Hex Hex Hex Hex Hex Hex Hex)
             / .
Hex         <- [0-9a-fA-F]
             
# Ext: parameterized rules
ParamList   <~  OPEN Identifier (',' S Identifier)*  CLOSE S 
ArgList     <- :OPEN Expression (',' S Expression)* :CLOSE S

NamedExpr   <- NAME Identifier? S # Ext: named captures
WithAction  <~ :ACTIONOPEN Identifier :ACTIONCLOSE S # Ext: semantic actions

Arrow       <- LEFTARROW / FUSEARROW / DROPARROW / ACTIONARROW / SPACEARROW
LEFTARROW   <- "<-" S
FUSEARROW   <- "<~" S           # Ext: rule-level fuse
DROPARROW   <- "<:" S           # Ext: rule-level drop
ACTIONARROW <- "<" WithAction S # Ext: rule-level semantic action
SPACEARROW  <- "<" S            # Ext: rule-level space-munching
  
OR          <- '/' S
    
LOOKAHEAD   <- '&' S
NOT         <- '!' S

DROP        <- ':' S # Ext: dropping the current node from the parse tree
KEEP        <- '^' S # Ext: keeping an expression, even when Pegged would drop it
FUSE        <- '~' S # Ext: fusing the captures of the current node
      
NAME        <- '=' S 
ACTIONOPEN  <- '{' S 
ACTIONCLOSE <- '}' S
    
OPTION     <- '?' S
ZEROORMORE <- '*' S
ONEORMORE  <- '+' S
    
OPEN       <- '(' S
CLOSE      <- ')' S
    
ANY        <- '.' S
    
S          <: ~(Blank / EOL / Comment)*
Comment    <- "#" (!EOL .)* (EOL/EOI)


*/
module pegged.grammar;

public import pegged.peg;
import std.array, std.algorithm, std.conv;

class PEGGED : Parser
{
    enum name = `PEGGED`;
    enum ruleNames = ["Grammar":true,"GrammarName":true,"Definition":true,"RuleName":true,"Expression":true,"Sequence":true,"Prefix":true,"Suffix":true,"Primary":true,"Name":true,"GroupExpr":true,"Literal":true,"Class":true,"CharRange":true,"Char":true,"Hex":true,"ParamList":true,"ArgList":true,"NamedExpr":true,"WithAction":true,"Arrow":true,"LEFTARROW":true,"FUSEARROW":true,"DROPARROW":true,"ACTIONARROW":true,"SPACEARROW":true,"OR":true,"LOOKAHEAD":true,"NOT":true,"DROP":true,"KEEP":true,"FUSE":true,"NAME":true,"ACTIONOPEN":true,"ACTIONCLOSE":true,"OPTION":true,"ZEROORMORE":true,"ONEORMORE":true,"OPEN":true,"CLOSE":true,"ANY":true,"S":true,"Comment":true];

    static Output parse(Input input)
    {
        return Grammar.parse(input);
    }
    
    mixin(stringToInputMixin());

    static ParseTree decimateTree(ParseTree p)
    {
        if(p.children.length == 0) return p;
        ParseTree[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else if (child.name.startsWith(`Keep!(`))
            {
                child.name = child.name[6..$-1];
                filteredChildren ~= child;
            }
            else
            {
                child = decimateTree(child);
                filteredChildren ~= child.children;
            }
        }
        p.children = filteredChildren;
        return p;
    }

    
class Grammar : Seq!(S,Option!(GrammarName),OneOrMore!(Definition),EOI)
{
    enum name = `Grammar`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class GrammarName : Seq!(RuleName,Lit!(":"),S)
{
    enum name = `GrammarName`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Definition : Seq!(RuleName,Arrow,Expression,S)
{
    enum name = `Definition`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class RuleName : Seq!(Identifier,Option!(ParamList),S)
{
    enum name = `RuleName`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Expression : Seq!(Sequence,ZeroOrMore!(Seq!(OR,Sequence)))
{
    enum name = `Expression`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Sequence : OneOrMore!(Prefix)
{
    enum name = `Sequence`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Prefix : Seq!(Option!(Or!(LOOKAHEAD,NOT,DROP,KEEP,FUSE)),Suffix)
{
    enum name = `Prefix`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Suffix : Seq!(Primary,Option!(Or!(OPTION,ONEORMORE,ZEROORMORE,NamedExpr,WithAction)),S)
{
    enum name = `Suffix`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Primary : Or!(Seq!(Name,NegLookAhead!(Arrow)),GroupExpr,Literal,Class,ANY)
{
    enum name = `Primary`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Name : Seq!(QualifiedIdentifier,Option!(ArgList),S)
{
    enum name = `Name`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class GroupExpr : Seq!(Drop!(OPEN),Expression,Drop!(CLOSE),S)
{
    enum name = `GroupExpr`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Literal : Fuse!(Or!(Seq!(Drop!(Quote),ZeroOrMore!(Seq!(NegLookAhead!(Quote),Char)),Drop!(Quote),S),Seq!(Drop!(DoubleQuote),ZeroOrMore!(Seq!(NegLookAhead!(DoubleQuote),Char)),Drop!(DoubleQuote),S)))
{
    enum name = `Literal`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Class : Seq!(Lit!("["),ZeroOrMore!(Seq!(NegLookAhead!(Lit!("]")),CharRange)),Lit!("]"),S)
{
    enum name = `Class`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class CharRange : Or!(Seq!(Char,Drop!(Lit!("-")),Char),Char)
{
    enum name = `CharRange`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Char : Fuse!(Or!(Seq!(BackSlash,Or!(Quote,DoubleQuote,BackQuote,BackSlash,Lit!("-"),Lit!("["),Lit!("]"),Or!(Lit!("n"),Lit!("r"),Lit!("t")),Seq!(Range!('0','2'),Range!('0','7'),Range!('0','7')),Seq!(Range!('0','7'),Option!(Range!('0','7'))),Seq!(Lit!("x"),Hex,Hex),Seq!(Lit!("u"),Hex,Hex,Hex,Hex),Seq!(Lit!("U"),Hex,Hex,Hex,Hex,Hex,Hex,Hex,Hex))),Any))
{
    enum name = `Char`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Hex : Or!(Range!('0','9'),Range!('a','f'),Range!('A','F'))
{
    enum name = `Hex`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ParamList : Fuse!(Seq!(OPEN,Identifier,ZeroOrMore!(Seq!(Lit!(","),S,Identifier)),CLOSE,S))
{
    enum name = `ParamList`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ArgList : Seq!(Drop!(OPEN),Expression,ZeroOrMore!(Seq!(Lit!(","),S,Expression)),Drop!(CLOSE),S)
{
    enum name = `ArgList`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class NamedExpr : Seq!(NAME,Option!(Identifier),S)
{
    enum name = `NamedExpr`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class WithAction : Fuse!(Seq!(Drop!(ACTIONOPEN),Identifier,Drop!(ACTIONCLOSE),S))
{
    enum name = `WithAction`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Arrow : Or!(LEFTARROW,FUSEARROW,DROPARROW,ACTIONARROW,SPACEARROW)
{
    enum name = `Arrow`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class LEFTARROW : Seq!(Lit!("<-"),S)
{
    enum name = `LEFTARROW`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class FUSEARROW : Seq!(Lit!("<~"),S)
{
    enum name = `FUSEARROW`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class DROPARROW : Seq!(Lit!("<:"),S)
{
    enum name = `DROPARROW`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ACTIONARROW : Seq!(Lit!("<"),WithAction,S)
{
    enum name = `ACTIONARROW`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class SPACEARROW : Seq!(Lit!("<"),S)
{
    enum name = `SPACEARROW`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class OR : Seq!(Lit!("/"),S)
{
    enum name = `OR`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class LOOKAHEAD : Seq!(Lit!("&"),S)
{
    enum name = `LOOKAHEAD`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class NOT : Seq!(Lit!("!"),S)
{
    enum name = `NOT`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class DROP : Seq!(Lit!(":"),S)
{
    enum name = `DROP`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class KEEP : Seq!(Lit!("^"),S)
{
    enum name = `KEEP`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class FUSE : Seq!(Lit!("~"),S)
{
    enum name = `FUSE`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class NAME : Seq!(Lit!("="),S)
{
    enum name = `NAME`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ACTIONOPEN : Seq!(Lit!("{"),S)
{
    enum name = `ACTIONOPEN`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ACTIONCLOSE : Seq!(Lit!("}"),S)
{
    enum name = `ACTIONCLOSE`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class OPTION : Seq!(Lit!("?"),S)
{
    enum name = `OPTION`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ZEROORMORE : Seq!(Lit!("*"),S)
{
    enum name = `ZEROORMORE`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ONEORMORE : Seq!(Lit!("+"),S)
{
    enum name = `ONEORMORE`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class OPEN : Seq!(Lit!("("),S)
{
    enum name = `OPEN`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class CLOSE : Seq!(Lit!(")"),S)
{
    enum name = `CLOSE`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ANY : Seq!(Lit!("."),S)
{
    enum name = `ANY`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class S : Drop!(Fuse!(ZeroOrMore!(Or!(Blank,EOL,Comment))))
{
    enum name = `S`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Comment : Seq!(Lit!("#"),ZeroOrMore!(Seq!(NegLookAhead!(EOL),Any)),Or!(EOL,EOI))
{
    enum name = `Comment`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

}

/+ from here, the code comes from pegged.development.grammarfunctions +/

void asModule(string moduleName, string grammarString)
{
    asModule(moduleName, moduleName~".d", grammarString);
}

void asModule(string moduleName, string fileName, string grammarString)
{
    import std.stdio;
    auto f = File(fileName,"w");
    
    f.write("/**\nThis module was automatically generated from the following grammar:\n\n");
    f.write(grammarString);
    f.write("\n\n*/\n");
    
    f.write("module " ~ moduleName ~ ";\n\n");
    f.write("public import pegged.peg;\n");
    f.write(grammar(grammarString));
}


string grammar(string g)
{    
    auto grammarAsOutput = PEGGED.parse(g);
    if (grammarAsOutput.children.length == 0) return "static assert(false, `Bad grammar: " ~ to!string(grammarAsOutput.capture) ~ "`);";
    string[] names;
    bool rootIsParametrized;
    string rootParameters;
    foreach(i,definition; grammarAsOutput.children)
        if (definition.name == "Definition") 
        {
            names ~= definition.capture[0];
            if (i == 0 && definition.children[0].capture.length == 2) // first rule is a parametrized rule.
            {   
                rootIsParametrized = true;
                rootParameters = definition.children[0].capture[1];
            }
        }
    string ruleNames = "    enum ruleNames = [";
    foreach(name; names)
        ruleNames ~= "\"" ~ name ~ "\":true,";
    ruleNames = ruleNames[0..$-1] ~ "];\n";
    
    string PEGtoCode(ParseTree p)
    {
        string result;
        auto ch = p.children;
        
        switch (p.name)
        {
            case "PEGGED":
                return PEGtoCode(ch[0]);
            case "Grammar":
                bool named = ch[0].name == "GrammarName";
                bool parametrized = ch[0].children[0].capture.length == 2;
                string grammarName = named ? (ch[0].capture[0] ~ (parametrized? ch[0].capture[1] : ""))
                                           : (names.front ~ (rootIsParametrized? rootParameters : ""));
                
                result =  "import std.array, std.algorithm, std.conv;\n\n"
                        ~ "class " ~ grammarName ~ " : Parser\n{\n" 
                        ~ "    enum name = `"~ grammarName ~ "`;\n"
                        ~ ruleNames ~ "\n"
                        ~
"    static Output parse(Input input)
    {
        mixin(okfailMixin());
        "
/*
~ (named ? "auto p = "~names.front~".parse(input);
        
        return p.success ? Output(p.text, p.pos, p.namedCaptures,
                                  ParseTree(name, p.success, p.capture, input.pos, p.pos, [p.parseTree]))
                         : fail(p.parseTree.end, p.capture);"
                   
        : */
~       "return "~names.front~".parse(input);"
~ "
    }
    
    mixin(stringToInputMixin());

    static ParseTree decimateTree(ParseTree p)
    {
        if(p.children.length == 0) return p;
        ParseTree[] filteredChildren;
        foreach(child; p.children)
        {
            child  = decimateTree(child);
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else if (child.name.startsWith(`Keep!(`))
            {
                child.name = child.name[6..$-1];
                filteredChildren ~= child;
            }
            else
            {
                if (child.children.length != 0)
                    filteredChildren ~= child;
            }
        }
        p.children = filteredChildren;
        return p;
    }

    
";
                string rulesCode;
                // if the grammar is anonymous and the first rule is parametrized,
                // we must drop the parameter list for the root.
                if (!named && rootIsParametrized)
                    ch[0].children[0].capture.popBack();
                                                         
                foreach(child; named ? ch[1..$] : ch)
                {
                    // child is a Definition
                    // Its first child is the rule's name
                    // If it has 2 captures, it's a parameterized rule, else a normal rule
                    // Parameterized rules are templates and their code must placed first.
                    if ( child.children[0].capture.length == 1) // normal rule
                        rulesCode ~= PEGtoCode(child);
                    else // Parameterized rule: to be put first
                        rulesCode = PEGtoCode(child) ~ rulesCode;
                }
                result ~= rulesCode;
                
                return result ~ "}\n";
            case "Definition":
                string code = "    enum name = `" ~ch[0].capture[0]~ "`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.name in ruleNames)
                p.children = [p];
            if (p.name.startsWith(`Keep!`))
            {
                p.name = p.name[6..$-1];
                p.children = [p];
            }
    
            p.name = name;
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (name ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    ";

                string inheritance;
                switch(ch[1].children[0].name)
                {
                    case "LEFTARROW":
                        inheritance = PEGtoCode(ch[2]);
                        break;
                    case "FUSEARROW":
                        inheritance = "Fuse!(" ~ PEGtoCode(ch[2]) ~ ")";
                        break;
                    case "DROPARROW":
                        inheritance = "Drop!(" ~ PEGtoCode(ch[2]) ~ ")";
                        break;
                    case "ACTIONARROW":
                        inheritance = "Action!(" ~ PEGtoCode(ch[2]) ~ ", " ~ ch[1].capture[1] ~ ")";
                        break;
                    case "SPACEARROW":
                        string temp = PEGtoCode(ch[2]);
                        // changing all Seq in the inheritance list into SpaceSeq. Hacky, but it works.
                        foreach(i, c; temp)
                        {
                            if (temp[i..$].startsWith("Seq!(")) inheritance ~= "Space";
                            inheritance ~= c;
                        }   
                        break;
                    default:
                        inheritance ="ERROR: Bad arrow: " ~ ch[1].name;
                        break;
                }

                return "class " 
                    ~ ch[0].capture[0] // name 
                    ~ (ch[0].capture.length == 2 ? ch[0].capture[1] : "") // parameter list
                    ~ " : " ~ inheritance // inheritance code
                    ~ "\n{\n" 
                    ~ code // inner code
                    ~ "\n}\n\n";
            case "Expression":
                if (ch.length > 1) // OR present
                {
                    result = "Or!(";
                    foreach(i,child; ch)
                        if (i%2 == 0) result ~= PEGtoCode(child) ~ ",";
                    result = result[0..$-1] ~ ")";
                }
                else // one-element Or -> dropping the Or!( )
                    result = PEGtoCode(ch[0]);
                return result;
            case "Sequence":
                if (ch.length > 1)
                {
                    result = "Seq!(";
                    foreach(child; ch) 
                    {
                        auto temp = PEGtoCode(child);
                        if (temp.startsWith("Seq!("))
                            temp = temp[5..$-1];
                        result ~= temp ~ ",";
                    }
                    result = result[0..$-1] ~ ")";
                }
                else
                    result = PEGtoCode(ch[0]);
                return result;
            case "Prefix":
                if (ch.length > 1)
                    switch (ch[0].name)
                    {
                        case "NOT":
                            result = "NegLookAhead!(" ~ PEGtoCode(ch[1]) ~ ")";
                            break;
                        case "LOOKAHEAD":
                            result = "PosLookAhead!(" ~ PEGtoCode(ch[1]) ~ ")";
                            break;
                        case "DROP":
                            result = "Drop!(" ~ PEGtoCode(ch[1]) ~ ")";
                            break;
                        case "KEEP":
                            result = "Keep!(" ~ PEGtoCode(ch[1]) ~ ")";
                            break;                       
                        case "FUSE":
                            result = "Fuse!(" ~ PEGtoCode(ch[1]) ~ ")";
                            break;
                        default:
                            break;
                    }
                else
                    result = PEGtoCode(ch[0]);
                return result;
            case "Suffix":
                if (ch.length > 1)
                    switch (ch[1].name)
                    {
                        case "OPTION":
                            result = "Option!(" ~ PEGtoCode(ch[0]) ~ ")";
                            break;
                        case "ZEROORMORE":
                            result = "ZeroOrMore!(" ~ PEGtoCode(ch[0]) ~ ")";
                            break;
                        case "ONEORMORE":
                            result = "OneOrMore!(" ~ PEGtoCode(ch[0]) ~ ")";
                            break;
                        case "NamedExpr":
                            if (ch[1].capture.length == 2)
                                result = "Named!(" ~ PEGtoCode(ch[0]) ~ ", \"" ~ ch[1].capture[1] ~ "\")";
                            else
                                result = "PushName!(" ~ PEGtoCode(ch[0]) ~ ")";
                            break;
                        case "WithAction":
                            result = "Action!(" ~ PEGtoCode(ch[0]) ~ ", " ~ ch[1].capture[0] ~ ")";
                            break;
                        default:
                            break;
                    }
                else
                    result = PEGtoCode(ch[0]);
                return result;
            case "Primary":
                foreach(child; ch) result ~= PEGtoCode(child);
                return result;
            case "Name":
                result = p.capture[0];
                if (ch.length == 1) result ~= PEGtoCode(ch[0]);
                return result;
            case "ArgList":
                result = "!(";
                foreach(child; ch)
                    result ~= PEGtoCode(child) ~ ","; // Wow! Allow  A <- List('A'*,',') 
                result = result[0..$-1] ~ ")";
                return result;
            case "GroupExpr":
                if (ch.length == 0) return "ERROR: Empty group ()";
                auto temp = PEGtoCode(ch[0]);
                if (ch.length == 1 || temp.startsWith("Seq!(")) return temp;
                result = "Seq!(" ~ temp ~ ")";
                return result;
            case "Ident":
                return p.capture[0];
            case "Literal":
                if (p.capture[0].length == 0)
                    return "ERROR: empty literal";
                return "Lit!(\"" ~ p.capture[0] ~ "\")";
            case "Class":
                if (ch.length == 0)
                    return "ERROR: Empty Class of chars []";
                else 
                {
                    if (ch.length > 1)
                    {
                        result = "Or!(";
                        foreach(child; ch)
                        {
                            auto temp = PEGtoCode(child);
                            if (temp.startsWith("Or!("))
                                temp = temp[4..$-1];
                            result ~= temp ~ ",";
                        }
                        result = result[0..$-1] ~ ")";
                    }
                    else
                        result = PEGtoCode(ch[0]);
                }
                return result;
            case "CharRange":
                if (p.capture.length == 2) // [a-z...
                    return "Range!('" ~ p.capture[0] ~ "','" ~ p.capture[1] ~ "')";
                else                // [a...
                    return "Lit!(\"" ~ p.capture[0] ~ "\")"; 
            case "Char":
                //if (p.capture.length == 2) // escape sequence \-, \[, \] 
                //    return "'" ~ p.capture[1] ~ "'";
                //else
                    return "Lit!(\"" ~ p.capture[0] ~ "\")"; 
            case "OR":
                foreach(child; ch) result ~= PEGtoCode(child);
                return result;
            case "ANY":
                return "Any";
            default:
                return "";
        }
    }

    return PEGtoCode(grammarAsOutput.parseTree);
}

