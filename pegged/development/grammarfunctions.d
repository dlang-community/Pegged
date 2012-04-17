/**
 * This module contains the code complementing the Pegged grammar
 * to create an complete pegged.grammar module
 * 
 */
module pegged.development.grammarfunctions;

import std.array;
import std.algorithm;
import std.stdio;

import pegged.peg;
import pegged.grammar;



/+ from here, the code comes from pegged.development.grammarfunctions +/

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
"    static ParseTree decimateTree(ParseTree p)
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

dstring grammar(dstring g)
{    
    auto grammarAsOutput = PEGGED.parse(g);
    if (grammarAsOutput.children.length == 0) 
        return "static assert(false, `Bad grammar: "d ~ to!dstring(grammarAsOutput.capture) ~ "`);"d;
    
    bool rootIsParametrized;
    ParseTree rootParameters;
    bool named = (grammarAsOutput.children[0].ruleName == "GrammarName"d);
    
    dstring rootName = named ? grammarAsOutput.children[1].capture[0]
                            : grammarAsOutput.children[0].capture[0];

    if (!named && grammarAsOutput.children[0].children[0].children.length > 0) // first rule is a parametrized rule.
    {
        rootIsParametrized = true;
        rootParameters = grammarAsOutput.children[0].children[0].children[0];
    }
    
    dstring gn; // future grammar name
    
    dstring PEGtoCode(ParseTree p)
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
                result =  "import std.array, std.algorithm, std.conv;\n\n"d
~ "class "d ~ externalName ~ " : Parser\n{\n"d 
~ "    enum grammarName = `"d ~ gn ~ "`;\n"d
~ "    enum ruleName = `"d~ gn ~ "`;\n"d
~ "    static Output parse(ParseLevel pl = ParseLevel.parsing)(Input input)
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
