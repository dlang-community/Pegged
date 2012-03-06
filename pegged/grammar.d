module pegged.grammar;

import std.algorithm : startsWith;
import std.conv;

public import pegged.peg;

class Grammar : Seq!(S,OneOrMore!(Definition), EOI)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Grammar", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Grammar", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Definition : Seq!(RuleName,Arrow, Expression)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Definition", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Definition", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class RuleName : Join!(Identifier,Option!(ParamList))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("RuleName", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("RuleName", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Expression : Seq!(Sequence,ZeroOrMore!(Seq!(OR,Sequence)))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Expression", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Expression", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Sequence : ZeroOrMore!(Element)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Sequence", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Sequence", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Element : Seq!(Prefix,ZeroOrMore!(Seq!(JOIN,Prefix)))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Element", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Element", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Prefix : Seq!(Option!(Or!(LOOKAHEAD, NOT, DROP, FUSE)),Suffix)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Prefix", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Prefix", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Suffix : Seq!(Primary,Option!(Or!(OPTION, ONEORMORE, ZEROORMORE, NamedExpr, WithAction)))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Suffix", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Suffix", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Primary : Or!(Seq!(Name,NegLookAhead!(Arrow)), GroupExpr, Literal, Class, ANY)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Primary", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Primary", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Name : Join!(QualifiedIdentifier,Option!(ArgList))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Name", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Name", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class GroupExpr : Seq!(Drop!(OPEN),Expression, Drop!(CLOSE))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("GroupExpr", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("GroupExpr", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Literal : Fuse!(Or!(Seq!(Drop!(Quote),ZeroOrMore!(Seq!(NegLookAhead!(Quote),Char)), Drop!(Quote), S), Seq!(Drop!(DoubleQuote),ZeroOrMore!(Seq!(NegLookAhead!(DoubleQuote),Char)), Drop!(DoubleQuote), S)))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Literal", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Literal", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Class : Seq!(Drop!(Lit!"["),ZeroOrMore!(Seq!(NegLookAhead!(Lit!"]"),Range)), Drop!(Lit!"]"), S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Class", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Class", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Range : Or!(Seq!(Char,Drop!(Lit!"-"), Char), Char)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Range", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Range", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Char : Fuse!(Or!(Seq!(BackSlash,Or!(Lit!"n", Lit!"r", Lit!"t")), Seq!(NegLookAhead!(BackSlash),Any)))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Char", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Char", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class ParamList : Fuse!(Seq!(OPEN,Identifier, ZeroOrMore!(Seq!(Lit!",",Identifier)), CLOSE))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ParamList", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ParamList", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class ArgList : Seq!(Drop!(OPEN),Expression, ZeroOrMore!(Seq!(Drop!(Lit!","),Expression)), Drop!(CLOSE))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ArgList", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ArgList", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class NamedExpr : Join!(NAME,Option!(Identifier))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("NamedExpr", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("NamedExpr", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class WithAction : Fuse!(Seq!(Drop!(ACTIONOPEN),Identifier, Drop!(ACTIONCLOSE)))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("WithAction", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("WithAction", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Arrow : Or!(LEFTARROW, FUSEARROW, DROPARROW, ACTIONARROW)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Arrow", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Arrow", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class LEFTARROW : Seq!(Lit!"<-",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("LEFTARROW", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("LEFTARROW", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class FUSEARROW : Seq!(Lit!"<:",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("FUSEARROW", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("FUSEARROW", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class DROPARROW : Seq!(Lit!"<:",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("DROPARROW", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("DROPARROW", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class ACTIONARROW : Join!(Lit!"<",WithAction)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ACTIONARROW", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ACTIONARROW", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class OR : Seq!(Lit!"/",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("OR", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("OR", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class LOOKAHEAD : Seq!(Lit!"&",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("LOOKAHEAD", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("LOOKAHEAD", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class NOT : Seq!(Lit!"!",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("NOT", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("NOT", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class DROP : Seq!(Lit!":",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("DROP", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("DROP", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class FUSE : Seq!(Lit!"~",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("FUSE", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("FUSE", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class JOIN : Lit!">"
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("JOIN", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("JOIN", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class NAME : Seq!(Lit!"=",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("NAME", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("NAME", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class ACTIONOPEN : Seq!(Lit!"{",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ACTIONOPEN", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ACTIONOPEN", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class ACTIONCLOSE : Seq!(Lit!"}",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ACTIONCLOSE", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ACTIONCLOSE", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class OPTION : Seq!(Lit!"?",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("OPTION", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("OPTION", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class ZEROORMORE : Seq!(Lit!"*",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ZEROORMORE", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ZEROORMORE", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class ONEORMORE : Seq!(Lit!"+",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ONEORMORE", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ONEORMORE", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class OPEN : Seq!(Lit!"(",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("OPEN", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("OPEN", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class CLOSE : Seq!(Lit!")",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("CLOSE", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("CLOSE", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class ANY : Seq!(Lit!".",S)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ANY", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("ANY", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class S : Drop!(ZeroOrMore!(Or!(Blank, Comment)))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("S", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("S", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Blank : Or!(Lit!" ", Lit!"\t", EOL)
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Blank", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Blank", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

class Comment : Fuse!(Join!(Lit!"#",ZeroOrMore!(Join!(NegLookAhead!(EOL),Any)), Or!(EOL, EOI)))
{
   
enum bool[string] ruleNames = ["Grammar":true, "Definition":true, "RuleName":true, "Expression":true, "Sequence":true, "Element":true, "Prefix":true, "Suffix":true, "Primary":true, "Name":true, "GroupExpr":true, "Literal":true, "Class":true, "Range":true, "Char":true, "ParamList":true, "ArgList":true, "NamedExpr":true, "WithAction":true, "Arrow":true, "LEFTARROW":true, "FUSEARROW":true, "DROPARROW":true, "ACTIONARROW":true, "OR":true, "LOOKAHEAD":true, "NOT":true, "DROP":true, "FUSE":true, "JOIN":true, "NAME":true, "ACTIONOPEN":true, "ACTIONCLOSE":true, "OPTION":true, "ZEROORMORE":true, "ONEORMORE":true, "OPEN":true, "CLOSE":true, "ANY":true, "S":true, "Blank":true, "Comment":true];

static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }
    
    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Comment", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                                p.namedCaptures,
                                ParseResult("Comment", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());
}

string PEGtoCode(ParseResult p, string[] names = [""])
{
    string result;
    auto ch = p.children;
    
    void recurse() 
    {
        foreach(child; ch) result ~= PEGtoCode(child);
    }
    
    switch (p.name)
    {
        case "Grammar":
            foreach(child; ch) result ~= PEGtoCode(child, names); // the only point where names is passed (for "Definition" to work upon)
            break;
        case "Definition":
            if (ch.length < 3)
                return "ERROR, Bad Definition";
            string code;
            if (names.length > 1)
            {
                string ruleNames = "    enum ruleNames = [";
                foreach(name; names)
                    ruleNames ~= "\"" ~ name ~ "\":true,";
                ruleNames = ruleNames[0..$-1] ~ "];\n";
                code = ruleNames ~
"    static ParseResult[] filterChildren(ParseResult p)
    {
        ParseResult[] filteredChildren;
        foreach(child; p.children)
        {
            if (child.name in ruleNames)
                filteredChildren ~= child;
            else
            {
                if (child.children.length > 0)
                    filteredChildren ~= filterChildren(child);
            }
        }
        return filteredChildren;
    }

    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return Output(p.next,
                        p.namedCaptures,
                        ParseResult(\""~ch[0].capture[0]~"\", p.success, p.capture, [p.parseTree]));
        else
            return Output(p.next,
                        p.namedCaptures,
                        ParseResult(\""~ch[0].capture[0]~"\", p.success, p.capture, filterChildren(p.parseTree)));
    }
    
    mixin(stringToInputMixin());";
            }
            else
            {
                code =
"    static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        p.name = \""~ch[0].capture[0]~"\";
        return p;
    }
    
    mixin(stringToInputMixin());";                
            }
            string inheritance;
            switch(ch[1].children[0].name) // ch[1] is the arrow symbol
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
                default:
                    inheritance ="ERROR: Bad arrow: " ~ ch[1].name;
                    break;
            }

            result = "class " 
                   ~ ch[0].capture[0] // name 
                   ~ (ch[0].capture.length == 2 ? ch[0].capture[1] : "") // parameter list
                   ~ " : " ~ inheritance // inheritance code
                   ~ "\n{\n" 
                   ~ code // inner code
                   ~ "\n}\n";
            break;
        case "Expression":
            if (ch.length > 1) // OR present
            {
                result ~= "Or!(";
                foreach(i,child; ch)
                    if (i%2 == 0) result ~= PEGtoCode(child) ~ ",";
                result = result[0..$-1] ~ ")";
            }
            else
                result ~= PEGtoCode(ch[0]);
            break;
        case "Sequence":
            if (ch.length > 1)
            {
                result ~= "Seq!(";
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
                result ~= PEGtoCode(ch[0]);
            break;
        case "Element":
            if (ch.length > 1)
            {
                result ~= "Join!(";
                foreach(i,child; ch) 
                {
                    if (i%2 == 0) // "Suffix JOIN Suffix JOIN ..."
                    {
                        auto temp = PEGtoCode(child);
                        if (temp.startsWith("Join!("))
                            temp = temp[6..$-1];
                        result ~= temp ~ ",";
                    }
                }
                result = result[0..$-1] ~ ")";
            }
            else
                result ~= PEGtoCode(ch[0]);
            break;
        case "Prefix":
            if (ch.length > 1)
                switch (ch[0].name)
                {
                    case "NOT":
                        result ~= "NegLookAhead!(" ~ PEGtoCode(ch[1]) ~ ")";
                        break;
                    case "LOOKAHEAD":
                        result ~= "PosLookAhead!(" ~ PEGtoCode(ch[1]) ~ ")";
                        break;
                    case "DROP":
                        result ~= "Drop!(" ~ PEGtoCode(ch[1]) ~ ")";
                        break;
                    case "JOIN":
                        result ~= "Join!(" ~ PEGtoCode(ch[1]) ~ ")";
                        break;
                    default:
                        break;
                }
            else
                result ~= PEGtoCode(ch[0]);
            break;
        case "Suffix":
            if (ch.length > 1)
                switch (ch[1].name)
                {
                    case "OPTION":
                        result ~= "Option!(" ~ PEGtoCode(ch[0]) ~ ")";
                        break;
                    case "ZEROORMORE":
                        result ~= "ZeroOrMore!(" ~ PEGtoCode(ch[0]) ~ ")";
                        break;
                    case "ONEORMORE":
                        result ~= "OneOrMore!(" ~ PEGtoCode(ch[0]) ~ ")";
                        break;
                    case "NamedExpr":
                        if (ch[1].capture.length == 2)
                            result ~= "Named!(" ~ PEGtoCode(ch[0]) ~ ", \"" ~ ch[1].capture[1] ~ "\")";
                        else
                            result ~= "PushName!(" ~ PEGtoCode(ch[0]) ~ ")";
                        break;
                    case "WithAction":
                        result ~= "Action!(" ~ PEGtoCode(ch[0]) ~ ", " ~ ch[1].capture[0] ~ ")";
                        break;
                    default:
                        break;
                }
            else
                result ~= PEGtoCode(ch[0]);
            break;
        case "Primary":
            recurse();
            break;
        case "Name":
            result ~= p.capture[0];
            if (ch.length == 1)
                result ~= PEGtoCode(ch[0]);
            break;
        case "ArgList":
            result ~= "!(";
            foreach(child; ch)
                result ~= PEGtoCode(child) ~ ","; // Wow! Allow  A <- List('A'*,',') 
            result = result[0..$-1] ~ ")";
            break;
        case "GroupExpr":
            if (ch.length == 0) return "ERROR: Empty group ()";
            auto temp = PEGtoCode(ch[0]);
            if (ch.length == 1 || temp.startsWith("Seq!(")) return temp;
            result ~= "Seq!(" ~ temp ~ ")";
            break;
        case "Ident":
            result ~= p.capture[0];
            break;
        case "Literal":
            if (p.capture[0].length == 0)
                return "ERROR: empty literal";
            result ~= "Lit!(\"" ~ p.capture[0] ~ "\")";
            break;
        case "Class":
            result ~= "Or!(";
            if (ch.length == 0)
                return "ERROR: Empty Class of chars []";
            else 
            {
                foreach(child; ch)
                {
                    auto temp = PEGtoCode(child);
                    if (temp.startsWith("Or!("))
                        temp = temp[4..$-1];
                    result ~= temp ~ ",";
                }
                result = result[0..$-1] ~ ")";
            }
            break;
        case "Range":
            if (ch.length == 2)
            {
                result ~= "Range!('" ~ ch[0].capture[0] 
                            ~ "','" 
                            ~ ch[1].capture[0] ~ "')";
            }
            else
                result ~= "Char!('" ~ ch[0].capture[0] ~ "')"; 
            break;
        case "Char":
            result ~= ch[0].capture[0];
            break;
        case "LEFTARROW":
            break;
        case "OR":
            recurse();
            break;
        case "LOOKAHEAD":
            break;
        case "NOT":
            break;
        case "OPTION":
            break;
        case "ZEROORMORE":
            break;
        case "ONEORMORE":
            break;
        case "OPEN":
            break;
        case "CLOSE":
            break;
        case "ANY":
            result ~= "Any";
            break;
        case "S":
            break;
        case "Comment":
            break;
        default:
            result ~= "ERROR: Unknown name: " ~ p.name;
    }
    return result;
}

string grammar(string g)
{    
    auto grammarAsOutput = Grammar.parse(g);
    string[] names;
    foreach(definition; grammarAsOutput.parseTree.children)
        names ~= definition.capture[0];
    return PEGtoCode(grammarAsOutput.parseTree, names);
}
