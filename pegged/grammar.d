/**
This module was automatically generated from the following grammar:


# This is the PEG extended grammar used by Pegged
PEGGED:

Grammar     <- S GrammarName? Definition+ EOI
Definition  <- RuleName Arrow Expression S
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

GrammarName <- RuleName ":" S                 # Ext: named grammars
RuleName    <- Identifier ParamList? S        # Ext: parametrized rules
Name        <- QualifiedIdentifier ArgList? S # Ext: names can be qualified
GroupExpr   <- :OPEN Expression :CLOSE S
Literal     <~ :Quote (!Quote Char)* :Quote S
             / :DoubleQuote (!DoubleQuote Char)* :DoubleQuote S
Class       <- '[' (!']' CharRange)* ']' S
CharRange   <- Char :'-' Char / Char
Char        <~ BackSlash ( Quote
                         / DoubleQuote
                         / BackQuote
                         / BackSlash 
                         / '-'                # Ext: escaping -,[,] in char ranges
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
             
# Ext: parametrized rules
ParamList   <- :OPEN Param (',' S Param)*  :CLOSE S
Param       <- DefaultParam / SingleParam
DefaultParam <- Identifier S "=" S Expression S
SingleParam <- Identifier S
ArgList     <- :OPEN Expression (',' S Expression)* :CLOSE S

NamedExpr   <- NAME Identifier? S                    # Ext: named captures
WithAction  <- :ACTIONOPEN Identifier S (:',' S Identifier)* :ACTIONCLOSE S # Ext: semantic actions

# Ext: different kinds of arrows
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
    enum grammarName = `PEGGED`; enum ruleName =  `PEGGED`;

    static Output parse(Input input)
    {
        mixin(okfailMixin());
        return Grammar.parse(input);
    }
    
    mixin(stringToInputMixin());

    static ParseTree decimateTree(ParseTree p)
    {
        if(p.children.length == 0) return p;
        ParseTree[] filteredChildren;
        foreach(child; p.children)
        {
            child  = decimateTree(child);
            if (child.grammarName == grammarName)
                filteredChildren ~= child;
            else
                filteredChildren ~= child.children;
        }
        p.children = filteredChildren;
        return p;
    }

    
class Grammar : Seq!(S,Option!(GrammarName),OneOrMore!(Definition),EOI)
{
    enum grammarName = `PEGGED`; enum ruleName =  `Grammar`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Definition : Seq!(RuleName,Arrow,Expression,S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `Definition`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Expression : Seq!(Sequence,ZeroOrMore!(Seq!(OR,Sequence)))
{
    enum grammarName = `PEGGED`; enum ruleName =  `Expression`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Sequence : OneOrMore!(Prefix)
{
    enum grammarName = `PEGGED`; enum ruleName =  `Sequence`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Prefix : Seq!(Option!(Or!(LOOKAHEAD,NOT,DROP,KEEP,FUSE)),Suffix)
{
    enum grammarName = `PEGGED`; enum ruleName =  `Prefix`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Suffix : Seq!(Primary,Option!(Or!(OPTION,ONEORMORE,ZEROORMORE,NamedExpr,WithAction)),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `Suffix`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Primary : Or!(Seq!(Name,NegLookAhead!(Arrow)),GroupExpr,Literal,Class,ANY)
{
    enum grammarName = `PEGGED`; enum ruleName =  `Primary`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class GrammarName : Seq!(RuleName,Lit!(":"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `GrammarName`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class RuleName : Seq!(Identifier,Option!(ParamList),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `RuleName`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Name : Seq!(QualifiedIdentifier,Option!(ArgList),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `Name`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class GroupExpr : Seq!(Drop!(OPEN),Expression,Drop!(CLOSE),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `GroupExpr`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Literal : Fuse!(Or!(Seq!(Drop!(Quote),ZeroOrMore!(Seq!(NegLookAhead!(Quote),Char)),Drop!(Quote),S),Seq!(Drop!(DoubleQuote),ZeroOrMore!(Seq!(NegLookAhead!(DoubleQuote),Char)),Drop!(DoubleQuote),S)))
{
    enum grammarName = `PEGGED`; enum ruleName =  `Literal`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Class : Seq!(Lit!("["),ZeroOrMore!(Seq!(NegLookAhead!(Lit!("]")),CharRange)),Lit!("]"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `Class`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class CharRange : Or!(Seq!(Char,Drop!(Lit!("-")),Char),Char)
{
    enum grammarName = `PEGGED`; enum ruleName =  `CharRange`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Char : Fuse!(Or!(Seq!(BackSlash,Or!(Quote,DoubleQuote,BackQuote,BackSlash,Lit!("-"),Lit!("["),Lit!("]"),Or!(Lit!("n"),Lit!("r"),Lit!("t")),Seq!(Range!('0','2'),Range!('0','7'),Range!('0','7')),Seq!(Range!('0','7'),Option!(Range!('0','7'))),Seq!(Lit!("x"),Hex,Hex),Seq!(Lit!("u"),Hex,Hex,Hex,Hex),Seq!(Lit!("U"),Hex,Hex,Hex,Hex,Hex,Hex,Hex,Hex))),Any))
{
    enum grammarName = `PEGGED`; enum ruleName =  `Char`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Hex : Or!(Range!('0','9'),Range!('a','f'),Range!('A','F'))
{
    enum grammarName = `PEGGED`; enum ruleName =  `Hex`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ParamList : Seq!(Drop!(OPEN),Param,ZeroOrMore!(Seq!(Lit!(","),S,Param)),Drop!(CLOSE),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `ParamList`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Param : Or!(DefaultParam,SingleParam)
{
    enum grammarName = `PEGGED`; enum ruleName =  `Param`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class DefaultParam : Seq!(Identifier,S,Lit!("="),S,Expression,S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `DefaultParam`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class SingleParam : Seq!(Identifier,S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `SingleParam`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ArgList : Seq!(Drop!(OPEN),Expression,ZeroOrMore!(Seq!(Lit!(","),S,Expression)),Drop!(CLOSE),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `ArgList`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class NamedExpr : Seq!(NAME,Option!(Identifier),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `NamedExpr`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class WithAction : Seq!(Drop!(ACTIONOPEN),Identifier,S,ZeroOrMore!(Seq!(Drop!(Lit!(",")),S,Identifier)),Drop!(ACTIONCLOSE),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `WithAction`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Arrow : Or!(LEFTARROW,FUSEARROW,DROPARROW,ACTIONARROW,SPACEARROW)
{
    enum grammarName = `PEGGED`; enum ruleName =  `Arrow`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class LEFTARROW : Seq!(Lit!("<-"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `LEFTARROW`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class FUSEARROW : Seq!(Lit!("<~"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `FUSEARROW`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class DROPARROW : Seq!(Lit!("<:"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `DROPARROW`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ACTIONARROW : Seq!(Lit!("<"),WithAction,S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `ACTIONARROW`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class SPACEARROW : Seq!(Lit!("<"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `SPACEARROW`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class OR : Seq!(Lit!("/"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `OR`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class LOOKAHEAD : Seq!(Lit!("&"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `LOOKAHEAD`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class NOT : Seq!(Lit!("!"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `NOT`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class DROP : Seq!(Lit!(":"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `DROP`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class KEEP : Seq!(Lit!("^"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `KEEP`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class FUSE : Seq!(Lit!("~"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `FUSE`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class NAME : Seq!(Lit!("="),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `NAME`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ACTIONOPEN : Seq!(Lit!("{"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `ACTIONOPEN`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ACTIONCLOSE : Seq!(Lit!("}"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `ACTIONCLOSE`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class OPTION : Seq!(Lit!("?"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `OPTION`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ZEROORMORE : Seq!(Lit!("*"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `ZEROORMORE`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ONEORMORE : Seq!(Lit!("+"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `ONEORMORE`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class OPEN : Seq!(Lit!("("),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `OPEN`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class CLOSE : Seq!(Lit!(")"),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `CLOSE`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class ANY : Seq!(Lit!("."),S)
{
    enum grammarName = `PEGGED`; enum ruleName =  `ANY`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class S : Drop!(Fuse!(ZeroOrMore!(Or!(Blank,EOL,Comment))))
{
    enum grammarName = `PEGGED`; enum ruleName =  `S`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    
}

class Comment : Seq!(Lit!("#"),ZeroOrMore!(Seq!(NegLookAhead!(EOL),Any)),Or!(EOL,EOI))
{
    enum grammarName = `PEGGED`; enum ruleName =  `Comment`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~"."~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
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
    
    bool rootIsParametrized;
    ParseTree rootParameters;
    bool named = (grammarAsOutput.children[0].ruleName == "GrammarName");
    
    string rootName = named ? grammarAsOutput.children[1].capture[0]
                            : grammarAsOutput.children[0].capture[0];

    if (!named && grammarAsOutput.children[0].children[0].children.length > 0) // first rule is a parametrized rule.
    {
        rootIsParametrized = true;
        rootParameters = grammarAsOutput.children[0].children[0].children[0];
    }
    
    string gn; // future grammar name
    
    string PEGtoCode(ParseTree p)
    {
        string result;
        auto ch = p.children;
        
        switch (p.ruleName)
        {
            case "PEGGED":
                return PEGtoCode(ch[0]);
            case "Grammar":    
                gn = named ? ch[0].capture[0] // user-defined grammar name
                           : rootName; // first definition's name
                
                string externalName; // the grammar name used in D code, different from the (simpler) one used in the parse tree nodes
                externalName = named ? PEGtoCode(ch[0])
                                     : rootName ~ (rootIsParametrized? PEGtoCode(rootParameters) : "");
                result =  "import std.array, std.algorithm, std.conv;\n\n"
                        ~ "class " ~ externalName 
                        ~ " : Parser\n{\n" 
                        ~ "    enum grammarName = `" ~ gn ~ "`;\n"
                        ~ "    enum ruleName = `"~ gn ~ "`;\n"
                        ~
"    static Output parse(Input input)
    {
        return "~rootName~".parse(input);
    }
    
    mixin(stringToInputMixin());

    static ParseTree decimateTree(ParseTree p)
    {
        if(p.children.length == 0) return p;
        ParseTree[] filteredChildren;
        foreach(child; p.children)
        {
            child  = decimateTree(child);
            if (child.grammarName == grammarName)
                filteredChildren ~= child;
            else
                filteredChildren ~= child.children;
        }
        p.children = filteredChildren;
        return p;
    }
    
";
                string rulesCode;
                // if the grammar is anonymous and the first rule is parametrized,
                // we must drop the parameter list for the root.
                if (!named && rootIsParametrized)
                {
                    ch[0].children[0].capture = ch[0].children[0].capture[0..1];
                    ch[0].children[0].children = null;
                }
                                                         
                foreach(child; named ? ch[1..$] : ch)
                {
                    // child is a Definition
                    // Its first child is the rule's name
                    // Parametrized rules are templates and their code must placed first.
                    if ( child.children[0].children.length == 0) // normal rule
                        rulesCode ~= PEGtoCode(child);
                    else // Parametrized rule: to be put first
                        rulesCode = PEGtoCode(child) ~ rulesCode;
                }
                result ~= rulesCode;
                
                return result ~ "}\n";
            case "GrammarName":
                return PEGtoCode(ch[0]);
            case "Definition":
                string code = "    enum grammarName = `" ~ gn ~ "`;
    enum ruleName = `" ~ch[0].capture[0]~ "`;

    static Output parse(Input input)
    {
        mixin(okfailMixin);
        
        auto p = typeof(super).parse(input);
        if (p.success)
        {
            p.parseTree = decimateTree(p.parseTree);
            
            if (p.grammarName != grammarName)
            {
                p.grammarName = grammarName;
                p.ruleName = ruleName;
            }
            
            return p;
        }
        else
            return fail(p.parseTree.end,
                        (grammarName~`.`~ruleName ~ ` failure at pos ` ~ to!string(p.parseTree.end)) ~ (p.capture.length > 0 ? p.capture[1..$] : p.capture));
    }
    
    mixin(stringToInputMixin());
    ";

                string inheritance;
                switch(ch[1].ruleName)
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
                    ~ PEGtoCode(ch[0])
                    ~ " : " ~ inheritance // inheritance code
                    ~ "\n{\n" 
                    ~ code // inner code
                    ~ "\n}\n\n";
            case "RuleName":
                if (ch.length > 0)
                    return p.capture[0] ~ PEGtoCode(ch[0]);
                else
                    return p.capture[0];
            case "ParamList":
                result = "(";
                foreach(i,child; ch)
                    result ~= PEGtoCode(child) ~ (i < ch.length -1 ? ", " : "");
                return result ~ ")";
            case "Param":
                return PEGtoCode(ch[0]);
            case "SingleParam":
                return p.capture[0];
            case "DefaultParam":
                return p.capture[0] ~ "= " ~ PEGtoCode(ch[0]);
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
                    switch (ch[0].ruleName)
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
                            result = "Keep!(" ~ PEGtoCode(ch[1]) ~ ", `" ~ gn ~ "`)";
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
                    switch (ch[1].ruleName)
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
                            result = PEGtoCode(ch[0]);
                            foreach(action; ch[1].capture)
                                result = "Action!(" ~ result ~ ", " ~ action ~ ")";
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
                    result ~= PEGtoCode(child) ~ ","; // Allow  A <- List('A'*,',') 
                result = result[0..$-1] ~ ")";
                return result;
            case "GroupExpr":
                if (ch.length == 0) return "ERROR: Empty group ()";
                auto temp = PEGtoCode(ch[0]);
                if (ch.length == 1 || temp.startsWith("Seq!(")) return temp;
                result = "Seq!(" ~ temp ~ ")";
                return result;
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

