/**
 * This module contains the code complementing the Pegged grammar
 * to create an complete pegged.grammar module
 * 
 */
module pegged.development.grammarfunctions;

import std.array;
import std.algorithm;
import std.stdio;
import std.conv;

import pegged.peg;
import pegged.grammar;


// -- snip -- // Code above this line will be removed when copying to pegged/grammar.d

void asModule(string moduleName, dstring grammarString)
{
    asModule(moduleName, moduleName~".d", grammarString);
}

void asModule(string moduleName, string fileName, dstring grammarString)
{
    import std.stdio;
    auto f = File(fileName,"w");
    
    f.write("/**\nThis module was automatically generated from the following grammar:\n\n");
    f.write(grammarString);
    f.write("\n\n*/\n");
    
    f.write("module " ~ moduleName ~ ";\n\n");
    f.write("public import pegged.peg;\n");
    f.write("public import std.traits:isSomeString;\n");
    f.write(grammar(grammarString));
}

dstring decimateTree()
{
    return
"    static TParseTree decimateTree(TParseTree p)
    {
        if(p.children.length == 0) return p;
        TParseTree[] filteredChildren;
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
    }"d;
}

dstring innerParseCode()
{
    return
"    static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
    {
        mixin(okfailMixin());
        
        auto p = typeof(super).parse!(pl)(input);
        static if (pl == ParseLevel.validating)
            p.capture = null;
        static if (pl <= ParseLevel.matching)
            p.children = null;
        static if (pl >= ParseLevel.parsing)
        {
            if (p.success)
            {                                
                static if (pl == ParseLevel.parsing)
                    p.parseTree = decimateTree(p.parseTree);
                
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
    }"d;
}

dstring grammar(TParseTree = ParseTree)(dstring g) if ( isParseTree!TParseTree )
{    
    auto grammarAsOutput = PEGGED!(TParseTree).parse(g);
    if (grammarAsOutput.children.length == 0) 
        return "static assert(false, `Bad grammar: "d ~ to!dstring(grammarAsOutput.capture) ~ "`);"d;
    
    bool rootIsParametrized;
    TParseTree rootParameters;
    bool named = (grammarAsOutput.children[0].ruleName == "GrammarName"d);
    
    dstring rootName = named ? grammarAsOutput.children[1].capture[0]
                            : grammarAsOutput.children[0].capture[0];

    if (!named && grammarAsOutput.children[0].children[0].children.length > 0) // first rule is a parametrized rule.
    {
        rootIsParametrized = true;
        rootParameters = grammarAsOutput.children[0].children[0].children[0];
    }
    
    dstring gn; // future grammar name
    
    dstring PEGtoCode(TParseTree p)
    {
        dstring result;
        auto ch = p.children;
        
        switch (p.ruleName)
        {
            case "PEGGED"d:
                return PEGtoCode(ch[0]);
            case "Grammar"d:    
                gn = named ? ch[0].capture[0] // user-defined grammar name
                           : rootName; // first definition's name
                
                dstring externalName; // the grammar name used in D code, different from the (simpler) one used in the parse tree nodes
                externalName = named ? PEGtoCode(ch[0])
                                     : rootName ~ (rootIsParametrized? PEGtoCode(rootParameters) : ""d);
                result =
"import std.array, std.algorithm, std.conv, std.typecons;\n"d ~
"import pegged.utils.associative;\n\n"d ~
"class "d ~ externalName ~ "(TParseTree = ParseTree) if ( isParseTree!TParseTree ) :
    BuiltinRules!(TParseTree).Parser
{
    mixin BuiltinRules!TParseTree;
    mixin Input!TParseTree;
    mixin Output!TParseTree;\n"d ~
"    enum grammarName = `"d ~ gn ~ "`;\n"d ~
"    enum ruleName = `"d~ gn ~ "`;\n"d ~
"    static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
    {
        return "~rootName~".parse!(pl)(input);
    }
    
    mixin(stringToInputMixin());
    static Output validate(T)(T input) if (is(T == Input) || isSomeString!(T) || is(T == Output))
    {
        return "~rootName~".parse!(ParseLevel.validating)(input);
    }
    
    static Output match(T)(T input) if (is(T == Input) || isSomeString!(T) || is(T == Output))
    {
        return "~rootName~".parse!(ParseLevel.matching)(input);
    }
    
    static Output fullParse(T)(T input) if (is(T == Input) || isSomeString!(T) || is(T == Output))
    {
        return "~rootName~".parse!(ParseLevel.noDecimation)(input);
    }
    
    static Output fullestParse(T)(T input) if (is(T == Input) || isSomeString!(T) || is(T == Output))
    {
        return "~rootName~".parse!(ParseLevel.fullest)(input);
    }
" ~ decimateTree() ~ "\n"d;

                dstring rulesCode;
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
                
                return result ~ "}\n"d;
            case "GrammarName"d:
                return PEGtoCode(ch[0]);
            case "Definition"d:
                dstring code = 
"    enum grammarName = `"d ~ gn ~ "`;
    enum ruleName = `"d ~ch[0].capture[0]~ "`;

" ~ innerParseCode()
~ "    
    mixin(stringToInputMixin());
    "d;

                dstring inheritance;
                switch(ch[1].children[0].ruleName)
                {
                    case "LEFTARROW":
                        inheritance = PEGtoCode(ch[2]);
                        break;
                    case "FUSEARROW":
                        inheritance = "Fuse!("d ~ PEGtoCode(ch[2]) ~ ")"d;
                        break;
                    case "DROPARROW":
                        inheritance = "Drop!("d ~ PEGtoCode(ch[2]) ~ ")"d;
                        break;
                    case "ACTIONARROW":
                        inheritance = "Action!("d ~ PEGtoCode(ch[2]) ~ ", "d ~ ch[1].capture[1] ~ ")"d;
                        break;
                    case "SPACEARROW":
                        dstring temp = PEGtoCode(ch[2]);
                        // changing all Seq in the inheritance list into SpaceSeq. Hacky, but it works.
                        foreach(i, c; temp)
                        {
                            if (temp[i..$].startsWith("Seq!("d)) inheritance ~= "Space"d;
                            inheritance ~= c;
                        }   
                        break;
                    default:
                        inheritance ="ERROR: Bad arrow: "d ~ ch[1].name;
                        break;
                }

                return "class "d
                    ~ PEGtoCode(ch[0])
                    ~ " : "d ~ inheritance // inheritance code
                    ~ "\n{\n"d 
                    ~ code // inner code
                    ~ "\n}\n\n"d;
            case "RuleName":
                if (ch.length > 0)
                    return p.capture[0] ~ PEGtoCode(ch[0]);
                else
                    return p.capture[0];
            case "ParamList":
                result = "("d;
                foreach(i,child; ch)
                    result ~= PEGtoCode(child) ~ (i < ch.length -1 ? ", "d : ""d);
                return result ~ ")"d;
            case "Param":
                return PEGtoCode(ch[0]);
            case "SingleParam":
                return p.capture[0];
            case "DefaultParam":
                return p.capture[0] ~ "= "d ~ PEGtoCode(ch[0]);
            case "Expression":
                if (ch.length > 1) // OR present
                {
                    result = "Or!("d;
                    foreach(i,child; ch)
                        if (i%2 == 0) result ~= PEGtoCode(child) ~ ","d;
                    result = result[0..$-1] ~ ")"d;
                }
                else // one-element Or -> dropping the Or!( )
                    result = PEGtoCode(ch[0]);
                return result;
            case "Sequence":
                if (ch.length > 1)
                {
                    result = "Seq!("d;
                    foreach(child; ch) 
                    {
                        auto temp = PEGtoCode(child);
                        if (temp.startsWith("Seq!("d))
                            temp = temp[5..$-1];
                        result ~= temp ~ ","d;
                    }
                    result = result[0..$-1] ~ ")"d;
                }
                else
                    result = PEGtoCode(ch[0]);
                return result;
            case "Prefix":
                if (ch.length > 1)
                    switch (ch[0].ruleName)
                    {
                        case "NOT":
                            result = "NegLookAhead!("d ~ PEGtoCode(ch[1]) ~ ")"d;
                            break;
                        case "LOOKAHEAD":
                            result = "PosLookAhead!("d ~ PEGtoCode(ch[1]) ~ ")"d;
                            break;
                        case "DROP":
                            result = "Drop!("d ~ PEGtoCode(ch[1]) ~ ")"d;
                            break;
                        case "KEEP":
                            result = "Keep!("d ~ PEGtoCode(ch[1]) ~ ", `"d ~ gn ~ "`)"d;
                            break;                       
                        case "FUSE":
                            result = "Fuse!("d ~ PEGtoCode(ch[1]) ~ ")"d;
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
                            result = "Option!("d ~ PEGtoCode(ch[0]) ~ ")"d;
                            break;
                        case "ZEROORMORE":
                            result = "ZeroOrMore!("d ~ PEGtoCode(ch[0]) ~ ")"d;
                            break;
                        case "ONEORMORE":
                            result = "OneOrMore!("d ~ PEGtoCode(ch[0]) ~ ")"d;
                            break;
                        case "NamedExpr":
                            if (ch[1].capture.length == 2)
                                result = "Named!("d ~ PEGtoCode(ch[0]) ~ ", \""d ~ ch[1].capture[1] ~ "\")"d;
                            else
                                result = "PushName!("d ~ PEGtoCode(ch[0]) ~ ")"d;
                            break;
                        case "WithAction":
                            result = PEGtoCode(ch[0]);
                            foreach(action; ch[1].capture)
                                result = "Action!("d ~ result ~ ", "d ~ action ~ ")"d;
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
                result = "!("d;
                foreach(child; ch)
                    result ~= PEGtoCode(child) ~ ","d; // Allow  A <- List('A'*,',') 
                result = result[0..$-1] ~ ")"d;
                return result;
            case "GroupExpr":
                if (ch.length == 0) return "ERROR: Empty group ()"d;
                auto temp = PEGtoCode(ch[0]);
                if (ch.length == 1 || temp.startsWith("Seq!("d)) return temp;
                result = "Seq!("d ~ temp ~ ")"d;
                return result;
            case "Literal":
                if (p.capture[0].length == 0)
                    return "ERROR: empty literal"d;
                return "Lit!(\""d ~ p.capture[0] ~ "\")"d;
            case "Class":
                if (ch.length == 0)
                    return "ERROR: Empty Class of chars []"d;
                else 
                {
                    if (ch.length > 1)
                    {
                        result = "Or!("d;
                        foreach(child; ch)
                        {
                            auto temp = PEGtoCode(child);
                            if (temp.startsWith("Or!("d))
                                temp = temp[4..$-1];
                            result ~= temp ~ ","d;
                        }
                        result = result[0..$-1] ~ ")"d;
                    }
                    else
                        result = PEGtoCode(ch[0]);
                }
                return result;
            case "CharRange":
                if (p.capture.length == 2) // [a-z...
                    return "Range!('"d ~ p.capture[0] ~ "','"d ~ p.capture[1] ~ "')"d;
                else                // [a...
                    return "Lit!(\""d ~ p.capture[0] ~ "\")"d; 
            case "Char":
                    return "Lit!(\""d ~ p.capture[0] ~ "\")"d; 
            case "OR":
                foreach(child; ch) result ~= PEGtoCode(child);
                return result;
            case "ANY":
                return "Any"d;
            default:
                return ""d;
        }
    }

    return PEGtoCode(grammarAsOutput.parseTree);
}

enum InfiniteLoop  { NoLoop, MightConsumeNothing, PossibleInfiniteLoop, Undecided }
enum LeftRecursion { NoLeftRecursion, DirectLeftRecursion, IndirectLeftRecursion, HiddenLeftRecursion, Undecided }
enum ReduceFurther { No, Yes }

struct Diagnostic
{
    InfiniteLoop[dstring]  infiniteLoops;
    LeftRecursion[dstring] leftRecursions;
}

Diagnostic checkGrammar(dstring g, ReduceFurther reduceFurther = ReduceFurther.Yes)
{
    import std.stdio;
    auto grammarAsInput = PEGGED!(ParseTree).parse(g);
    Diagnostic diag;
    ParseTree[dstring] rules;
    
    InfiniteLoop checkLoops(ParseTree p)
    {
        switch(p.name)
        {
            case "PEGGED.Definition":
                return checkLoops(p.children[2]);
            case "PEGGED.Expression":
                InfiniteLoop result;
                foreach(i, child; p.children)
                {
                    if (i % 2 == 1) 
                        continue; // skipping 'PEGGED.OR'
                    auto c = checkLoops(child);
                    if (c > result)
                        result = c;
                }
                return result;
            case "PEGGED.Sequence":
                InfiniteLoop result = checkLoops(p.children[0]);
                foreach(i, child; p.children[1..$])
                {
                    auto c = checkLoops(child);
                    switch (c)
                    {
                        case InfiniteLoop.NoLoop:
                            if (result < InfiniteLoop.PossibleInfiniteLoop)
                                result = c;
                            break;
                        case InfiniteLoop.MightConsumeNothing:
                            break; // Never change the sequence diagnostic
                        case InfiniteLoop.PossibleInfiniteLoop:
                            return c;
                        case InfiniteLoop.Undecided:
                            return c;
                        default:
                            break;
                    }
                }
                return result;
            case "PEGGED.Prefix":
                return checkLoops(p.children[$-1]);
            case "PEGGED.Suffix":
                if (p.children.length > 1) // Suffix present
                {
                    if (p.children[1].name == "PEGGED.ONEORMORE" || p.children[1].name == "PEGGED.ZEROORMORE")
                    {
                        auto c = checkLoops(p.children[0]);
                        if (c == InfiniteLoop.MightConsumeNothing)
                            return InfiniteLoop.PossibleInfiniteLoop;
                    }
                    if (p.children[1].name == "PEGGED.OPTION")
                        return InfiniteLoop.MightConsumeNothing;
                }
                return checkLoops(p.children[0]);
            case "PEGGED.Primary":
                return checkLoops(p.children[0]);
            case "PEGGED.Name":
                if (auto res = p.capture[0] in diag.infiniteLoops)
                    return *res;
                else
                    return InfiniteLoop.NoLoop; // unknown name -> External grammar name
            case "PEGGED.Eps":
                return InfiniteLoop.MightConsumeNothing;
            default:
                return InfiniteLoop.NoLoop;
        }
    }
    
    /// Search for toFind as the first child in an expression
    bool findName(ParseTree p, dstring toFind)
    {
        if (p.name == "PEGGED.Name" && p.capture[0] == toFind)
            return true;
        else
        {
            if (p.children.length == 0)
                return false;
            return findName(p.children[0], toFind);
        }
    }
    
    LeftRecursion checkLeftRecursion(ParseTree p)
    {
        switch(p.name)
        {
            case "PEGGED.Definition":
                return checkLeftRecursion(p.children[2]);
            case "PEGGED.Expression":
                LeftRecursion result;
                foreach(i, child; p.children)
                {
                    if (i % 2 == 1) 
                    continue; // skipping 'PEGGED.OR'
                    auto c = checkLeftRecursion(child);
                    if (c > result)
                        result = c;
                }
                return result;
            case "PEGGED.Sequence":
                LeftRecursion result = checkLeftRecursion(p.children[0]);
                foreach(i, child; p.children[1..$])
                {
                    
                    LeftRecursion c = checkLeftRecursion(child);
                    switch (c)
                    {
//                         case InfiniteLoop.NoLoop:
//                             if (result < InfiniteLoop.PossibleInfiniteLoop)
//                                 result = c;
//                             break;
//                         case InfiniteLoop.MightConsumeNothing:
//                             break; // Never change the sequence diagnostic
//                         case InfiniteLoop.PossibleInfiniteLoop:
//                             return c;
//                         case InfiniteLoop.Undecided:
//                             return c;
                        default:
                            break;
                    }
                }
                return result;
            case "PEGGED.Prefix":
                return checkLeftRecursion(p.children[$-1]);
            case "PEGGED.Suffix":
                if (p.children.length > 1) // Suffix present
                {
                    if (p.children[1].name == "PEGGED.ONEORMORE" || p.children[1].name == "PEGGED.ZEROORMORE")
                    {
                        LeftRecursion c = checkLeftRecursion(p.children[0]);
//                         if (c == InfiniteLoop.MightConsumeNothing)
//                             return InfiniteLoop.PossibleInfiniteLoop;
                    }
                    if (p.children[1].name == "PEGGED.OPTION")
                        return LeftRecursion.DirectLeftRecursion;
                }
                return checkLeftRecursion(p.children[0]);
            case "PEGGED.Primary":
                return checkLeftRecursion(p.children[0]);
            case "PEGGED.Name":
                if (auto res = p.capture[0] in diag.leftRecursions)
                    return *res;
                else
                    return LeftRecursion.NoLeftRecursion;
            case "PEGGED.Eps":
                return LeftRecursion.NoLeftRecursion;
            default:
                return LeftRecursion.NoLeftRecursion;
        }
    }
    
    foreach(index, definition; grammarAsInput.children)
        if (definition.name == "PEGGED.Definition")
        {
            diag.infiniteLoops[definition.capture[0]]= InfiniteLoop.Undecided; // entering the rule's name in canLoop
            rules[definition.capture[0]] = definition;
            //writeln("Finding rule `"~definition.capture[0]~"`");
        }
    
    writeln("Found ", diag.infiniteLoops.length, " rules.");
    
    size_t stillUndecidedRules = diag.infiniteLoops.length;
    size_t before = stillUndecidedRules+1;   

    size_t reduceUndecided()
    {
        size_t howManyReduced;
        while(before > stillUndecidedRules)
        {
            before = stillUndecidedRules;
            foreach(name, diagnostic; diag.infiniteLoops)
            {
                if (diagnostic == InfiniteLoop.Undecided)
                {
                    diag.infiniteLoops[name] = checkLoops(rules[name]);
                    if (diag.infiniteLoops[name] != InfiniteLoop.Undecided)
                    {
                        --stillUndecidedRules;
                        ++howManyReduced;
                    }
                }
            }
        }
        dstring s;
        foreach(name, diagnostic; diag.infiniteLoops)
            if (diagnostic == InfiniteLoop.Undecided) s ~= (" " ~ name);
        if (stillUndecidedRules > 0)
            writeln("still ", stillUndecidedRules, " undecided rules: ", s);
        writeln(howManyReduced, " rules were deduced at this step.");
        return howManyReduced;
    }
    reduceUndecided();

    if (reduceFurther == ReduceFurther.Yes)
    {
        size_t breaker = 0;
        size_t wrapAround = 0;
        // standard reduction didn't work: mutually recursive rules block the algorithm
        // We will try to cut the Gordian node by forcefully changing a rule to NoLoop and see if that helps
        while (stillUndecidedRules > 0 && wrapAround < stillUndecidedRules)
        {
            writeln("Trying to reduce further...");
            while(diag.infiniteLoops[diag.infiniteLoops.keys[breaker]] != InfiniteLoop.Undecided)
            {
                breaker = (breaker + 1) % diag.infiniteLoops.length;
            }
            writeln("Changing ", diag.infiniteLoops.keys[breaker]);
            diag.infiniteLoops[diag.infiniteLoops.keys[breaker]] = InfiniteLoop.NoLoop;
            --stillUndecidedRules;
            before = stillUndecidedRules+1; // used in reduceUndecided()
            auto howManyReduced = reduceUndecided();
            if (howManyReduced == 0)
                ++wrapAround;
            else
                wrapAround = 0;
            auto c = checkLoops(rules[diag.infiniteLoops.keys[breaker]]);
            if (c == InfiniteLoop.Undecided)
                ++stillUndecidedRules;
            diag.infiniteLoops[diag.infiniteLoops.keys[breaker]] = c;
            breaker = (breaker + 1) % diag.infiniteLoops.length;
        }
    }
    
    return diag;
}
