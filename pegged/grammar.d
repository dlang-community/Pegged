/**
Parser generation module for Pegged.
The Pegged parser itself is in pegged.parser, generated from pegged.examples.peggedgrammar.

The documentation is in the /docs directory.
*/
module pegged.grammar;

import std.conv: to;
import std.stdio;

public import pegged.peg;
import pegged.parser;

version(unittest)
{
    import std.stdio;
}

/**
Option enum to get internal memoization (parse results storing).
*/
enum Memoization { no, yes }

/**
This function takes a (future) module name, a (future) file name and a grammar as a string or a file.
It writes the corresponding parser inside a module with the given name.
*/
void asModule(Memoization withMemo = Memoization.no)(string moduleName, string grammarString)
{
    asModule!(withMemo)(moduleName, moduleName, grammarString);
}

/// ditto
void asModule(Memoization withMemo = Memoization.no)(string moduleName, string fileName, string grammarString)
{
    import std.stdio;
    auto f = File(fileName ~ ".d","w");

    f.write("/**\nThis module was automatically generated from the following grammar:\n\n");
    f.write(grammarString);
    f.write("\n\n*/\n");

    f.write("module " ~ moduleName ~ ";\n\n");
    f.write("public import pegged.peg;\n");
    f.write(grammar!(withMemo)(grammarString));
}

/// ditto
void asModule(Memoization withMemo = Memoization.no)(string moduleName, File file)
{
    string grammarDefinition;
    foreach(line; file.byLine)
    {
        grammarDefinition ~= line ~ '\n';
    }
    asModule!(withMemo)(moduleName, grammarDefinition);
}

// Helper to insert ':Spacing*' before and after Primaries
ParseTree spaceArrow(ParseTree input)
{
    ParseTree wrapInSpaces(ParseTree p)
    {
        ParseTree spacer =
        ParseTree("Pegged.Prefix", true, null, null, 0,0, [
            ParseTree("Pegged.DISCARD", true),
            ParseTree("Pegged.Suffix", true, null, null, 0, 0, [
                ParseTree("Pegged.Primary", true, null, null, 0, 0, [
                    ParseTree("Pegged.RhsName", true, null, null, 0,0, [
                        ParseTree("Pegged.Identifier", true, ["Spacing"])
                    ])
                ]),
                ParseTree("Pegged.ZEROORMORE", true)
            ])
        ]);
        ParseTree result = ParseTree("Pegged.WrapAround", true, p.matches, p.input, p.begin, p.end, p.children);
        result.children = spacer ~ result.children ~ spacer;
        return result;
    }
    return modify!( p => p.name == "Pegged.Primary",
                    wrapInSpaces)(input);
}

// Expression-level code: [abc] or 'a'+, *not* A <- 'a'+
string generateCode(string s)
{
    return generateCode(Pegged.decimateTree(Pegged.Expression(s)));
}

string generateCode(ParseTree p)
{
    string result;

    switch (p.name)
    {
        case "Pegged":
            result = generateCode(p.children[0]);
            break;
        case "Pegged.Grammar":
            string grammarName = generateCode(p.children[0]);
            string shortGrammarName = p.children[0].matches[0];
            //string invokedGrammarName = generateCode(transformName(p.children[0]));
            string firstRuleName = generateCode(p.children[1].children[0]);

            result =  "struct Generic" ~ shortGrammarName ~ "(TParseTree)\n"
                    ~ "{\n"
                    ~ "    struct " ~ grammarName ~ "\n    {\n"
                    ~ "    enum name = \"" ~ shortGrammarName ~ "\";\n"
                    ~ "    import std.typecons:Tuple, tuple;\n"
                    ~ "    static TParseTree[Tuple!(string, size_t)] memo;\n"
                    ~ "    static bool isRule(string s)\n"
                    ~ "    {\n"
                    ~ "        switch(s)\n"
                    ~ "        {\n";

            ParseTree[] definitions = p.children[1 .. $];
            bool[string] ruleNames; // to avoid duplicates, when using parameterized rules
            string parameterizedRulesSpecialCode; // because param rules need to be put in the 'default' part of the switch
            bool userDefinedSpacing = false;

            string paramRuleHandler(string target)
            {
                return "if (s.length >= "~to!string(shortGrammarName.length + target.length + 3)
                        ~" && s[0.."~to!string(shortGrammarName.length + target.length + 3)~"] == \""
                        ~shortGrammarName ~ "." ~ target~"!(\") return true;";
            }

            foreach(i,def; definitions)
            {
                if (def.matches[0] !in ruleNames)
                {
                    ruleNames[def.matches[0]] = true;

                    if (def.children[0].children.length > 1)
                        parameterizedRulesSpecialCode ~= "                " ~ paramRuleHandler(def.matches[0])~ "\n";
                    else
                        result ~= "            case \"" ~ shortGrammarName ~ "." ~ def.matches[0] ~ "\":\n";
                }
                if (def.matches[0] == "Spacing") // user-defined spacing
                    userDefinedSpacing = true;
            }
            result ~= "                return true;\n"
                    ~ "            default:\n"
                    ~ parameterizedRulesSpecialCode
                    ~ "                return false;\n        }\n    }\n";

            result ~= "    mixin decimateTree;\n";

            // If the grammar provides a Spacing rule, then this will be used.
            // else, the predefined 'spacing' rule is used.
            result ~= userDefinedSpacing ? "" : "    alias spacing Spacing;\n\n";

            // Creating the inner functions, each corresponding to a grammar rule
            foreach(def; definitions)
                result ~= generateCode(def);

            // if the first rule is parameterized (a template), it's impossible to get an opCall
            // because we don't know with which template arguments it should be called.
            // So no opCall is generated in this case.
            if (p.children[1].children[0].children.length == 1)
            {
                // General calling interface
                result ~= "    static TParseTree opCall(TParseTree p)\n"
                        ~  "    {\n"
                        ~  "        TParseTree result = decimateTree(" ~ firstRuleName ~ "(p));\n"
                        ~  "        result.children = [result];\n"
                        ~  "        result.name = \"" ~ shortGrammarName ~ "\";\n"
                        ~  "        return result;\n"
                        ~  "    }\n\n"
                        ~  "    static TParseTree opCall(string input)\n"
                        ~  "    {\n"
                        ~  "        if(__ctfe)\n"
                        ~  "        {\n"
                        ~  "            return " ~ shortGrammarName ~ "(TParseTree(``, false, [], input, 0, 0));\n"
                        ~  "        }\n"
                        ~  "        else\n"
                        ~  "        {\n"
                        ~  "            memo = null;\n"
                        ~  "            return " ~ shortGrammarName ~ "(TParseTree(``, false, [], input, 0, 0));\n"
                        ~  "        }\n"
                        ~  "    }\n";

                result ~= "    static string opCall(GetName g)\n"
                        ~ "    {\n"
                        ~ "        return \"" ~ shortGrammarName ~ "\";\n"
                        ~ "    }\n\n";
            }
            result ~= "    }\n" // end of grammar struct definition
                    ~ "}\n\n" // end of template definition
                    ~ "alias Generic" ~ shortGrammarName ~ "!(ParseTree)."
                    ~ shortGrammarName ~ " " ~ shortGrammarName ~ ";\n\n";
            break;
        case "Pegged.Definition":
            // children[0]: name
            // children[1]: arrow (arrow type as first child)
            // children[2]: description

            string code = "pegged.peg.named!(";

            switch(p.children[1].children[0].name)
            {
                case "Pegged.LEFTARROW":
                    code ~= generateCode(p.children[2]);
                    break;
                case "Pegged.FUSEARROW":
                    code ~= "pegged.peg.fuse!(" ~ generateCode(p.children[2]) ~ ")";
                    break;
                case "Pegged.DISCARDARROW":
                    code ~= "pegged.peg.discard!(" ~ generateCode(p.children[2]) ~ ")";
                    break;
                case "Pegged.KEEPARROW":
                    code ~= "pegged.peg.keep!("~ generateCode(p.children[2]) ~ ")";
                    break;
                case "Pegged.DROPARROW":
                    code ~= "pegged.peg.drop!("~ generateCode(p.children[2]) ~ ")";
                    break;
                case "Pegged.PROPAGATEARROW":
                    code ~= "pegged.peg.propagate!("~ generateCode(p.children[2]) ~ ")";
                    break;
                case "Pegged.SPACEARROW":
                    ParseTree modified = spaceArrow(p.children[2]);
                    code ~= generateCode(modified);
                    break;
                default:
                    break;
            }

            bool parameterizedRule = p.children[0].children.length > 1;
            string completeName = generateCode(p.children[0]);
            string shortName = p.matches[0];
            string innerName;

            if (parameterizedRule)
            {
                result =  "    template " ~ completeName ~ "\n"
                        ~ "    {\n";
                innerName ~= "`" ~ shortName ~ "!(` ~ ";
                foreach(i,param; p.children[0].children[1].children)
                    innerName ~= "pegged.peg.getName!("~ param.children[0].matches[0]
                                ~ (i<p.children[0].children[1].children.length-1 ? ")() ~ `, ` ~ " : ")");
                innerName ~= " ~ `)`";
            }
            else
            {
                innerName ~= "`" ~ completeName ~ "`";
            }

            code ~= ", name ~ `.`~ " ~ innerName ~ ")";

            result ~= "    static TParseTree " ~ shortName ~ "(TParseTree p)\n"
                    ~  "    {\n"
                    ~  "        if(__ctfe)\n"
                    ~  "        {\n"
                    ~  "            return " ~ code ~ "(p);\n"
                    ~  "        }\n"
                    ~  "        else\n"
                    ~  "        {\n"
                    ~  "            if(auto m = tuple("~innerName~",p.end) in memo)\n"
                    ~  "                return *m;\n"
                    ~  "            else\n"
                    ~  "            {\n"
                    ~  "                TParseTree result = " ~ code ~ "(p);\n"
                    ~  "                memo[tuple("~innerName~",p.end)] = result;\n"
                    ~  "                return result;\n"
                    ~  "            }\n"
                    ~  "        }\n"
                    ~  "    }\n\n"
                    ~  "    static TParseTree " ~ shortName ~ "(string s)\n"
                    ~  "    {\n"
                    ~  "        if(__ctfe)\n"
                    ~  "        {\n"
                    ~  "            return " ~ code ~ "(TParseTree(\"\", false,[], s));\n"
                    ~  "        }\n"
                    ~  "        else\n"
                    ~  "        {\n"
                    ~  "            memo = null;\n"
                    ~  "            return " ~ code ~ "(TParseTree(\"\", false,[], s));\n"
                    ~  "        }\n"
                    ~  "    }\n"
                    ~  "    static string " ~ shortName ~ "(GetName g)\n"
                    ~  "    {\n"
                    ~  "        return name ~ `.`~ " ~ innerName ~ ";\n"
                    ~  "    }\n\n";

            if (parameterizedRule)
                result ~= "    }\n";

            break;
        case "Pegged.GrammarName":
            result = generateCode(p.children[0]);
            if (p.children.length == 2)
                result ~= generateCode(p.children[1]);
            break;
        case "Pegged.LhsName":
            result = generateCode(p.children[0]);
            if (p.children.length == 2)
                result ~= generateCode(p.children[1]);
            break;
        case "Pegged.ParamList":
            result = "(";
            foreach(i,child; p.children)
                result ~= generateCode(child) ~ ", ";
            result = result[0..$-2] ~ ")";
            break;
        case "Pegged.Param":
            result = "alias " ~ generateCode(p.children[0]);
            break;
        case "Pegged.SingleParam":
            result = p.matches[0];
            break;
        case "Pegged.DefaultParam":
            result = p.matches[0] ~ " = " ~ generateCode(p.children[1]);
            break;
        case "Pegged.Expression":
            if (p.children.length > 1) // OR expression
            {
                // Keyword list detection: "abstract"/"alias"/...
                bool isLiteral(ParseTree p)
                {
                    return ( p.name == "Pegged.Sequence"
                            && p.children.length == 1
                            && p.children[0].children.length == 1
                            && p.children[0].children[0].children.length == 1
                            && p.children[0].children[0].children[0].children.length == 1
                            && p.children[0].children[0].children[0].children[0].name == "Pegged.Literal");
                }
                bool keywordList = true;
                foreach(child;p.children)
                    if (!isLiteral(child))
                    {
                        keywordList = false;
                        break;
                    }

                if (keywordList)
                {
                    result = "pegged.peg.keywords!(";
                    foreach(elem; p.children)
                        result ~= "\"" ~ elem.matches[0] ~ "\", ";
                    result = result[0..$-2] ~ ")";
                }
                else
                {
                    result = "pegged.peg.or!(";
                    foreach(elem; p.children)
                        result ~= generateCode(elem) ~ ", ";
                    result = result[0..$-2] ~ ")";
                }
            }
            else // One child -> just a sequence, no need for a or!( , )
            {
                result = generateCode(p.children[0]);
            }
            break;
        case "Pegged.Sequence":
            if (p.children.length > 1) // real sequence
            {
                result = "pegged.peg.and!(";
                foreach(elem; p.children)
                {
                    string elementCode = generateCode(elem);
                    // flattening inner sequences
                    if (elementCode.length > 6 && elementCode[0..5] == "pegged.peg.and!(")
                        elementCode = elementCode[5..$-1]; // cutting 'and!(' and ')'
                    result ~= elementCode ~ ", ";
                }
                result = result[0..$-2] ~ ")";
            }
            else // One child -> just a Suffix, no need for a and!( , )
            {
                result = generateCode(p.children[0]);
            }
            break;
        case "Pegged.Prefix":
            result = generateCode(p.children[$-1]);
            foreach(child; p.children[0..$-1])
                result = generateCode(child) ~ result ~ ")";
            break;
        case "Pegged.Suffix":
            result = generateCode(p.children[0]);
            foreach(child; p.children[1..$])
            {
                switch (child.name)
                {
                    case "Pegged.OPTION":
                        result = "pegged.peg.option!(" ~ result ~ ")";
                        break;
                    case "Pegged.ZEROORMORE":
                        result = "pegged.peg.zeroOrMore!(" ~ result ~ ")";
                        break;
                    case "Pegged.ONEORMORE":
                        result = "pegged.peg.oneOrMore!(" ~ result ~ ")";
                        break;
                    case "Pegged.Action":
                        foreach(action; child.matches)
                            result = "pegged.peg.action!(" ~ result ~ ", " ~ action ~ ")";
                        break;
                    default:
                        break;
                }
            }
            break;
        case "Pegged.Primary":
            result = generateCode(p.children[0]);
            break;
        case "Pegged.RhsName":
            result = "";
            foreach(i,child; p.children)
                result ~= generateCode(child);
            break;
        case "Pegged.ArgList":
            result = "!(";
            foreach(child; p.children)
                result ~= generateCode(child) ~ ", "; // Allow  A <- List('A'*,',')
            result = result[0..$-2] ~ ")";
            break;
        case "Pegged.Identifier":
            result = p.matches[0];
            break;
        case "Pegged.NAMESEP":
            result = ".";
            break;
        case "Pegged.Literal":
            result = "pegged.peg.literal!(`" ~ p.matches[1] ~ "`)";
            break;
        case "Pegged.CharClass":
            if (p.children.length > 1)
            {
                result = "pegged.peg.or!(";
                foreach(elem; p.children)
                    result ~= generateCode(elem) ~ ", ";
                result = result[0..$-2] ~ ")";
            }
            else // One child -> just a sequence, no need for a or!( , )
            {
                result = generateCode(p.children[0]);
            }
            break;
        case "Pegged.CharRange":
            if (p.children.length > 1) // a-b range
            {
                result = "pegged.peg.charRange!('" ~ generateCode(p.children[0])
                                                    ~ "', '"
                                                    ~ generateCode(p.children[1])
                                                    ~ "')";
            }
            else // lone char
            {
                result = "pegged.peg.literal!(\"" ~ generateCode(p.children[0]) ~ "\")";
            }
            break;
        case "Pegged.Char":
            string ch = p.matches[0];
            if(ch == "\\[" || ch == "\\]" || ch == "\\-")
                result = ch[1..$];
            else
                result = ch;
            break;
        case "Pegged.POS":
            result = "pegged.peg.posLookahead!(";
            break;
        case "Pegged.NEG":
            result = "pegged.peg.negLookahead!(";
            break;
        case "Pegged.FUSE":
            result = "pegged.peg.fuse!(";
            break;
        case "Pegged.DISCARD":
            result = "pegged.peg.discard!(";
            break;
        //case "Pegged.CUT":
        //    result = "discardChildren!(";
        //    break;
        case "Pegged.KEEP":
            result = "pegged.peg.keep!(";
            break;
        case "Pegged.DROP":
            result = "pegged.peg.drop!(";
            break;
        case "Pegged.PROPAGATE":
            result = "pegged.peg.propagate!(";
            break;
        case "Pegged.OPTION":
            result = "pegged.peg.option!(";
            break;
        case "Pegged.ZEROORMORE":
            result = "pegged.peg.zeroOrMore!(";
            break;
        case "Pegged.ONEORMORE":
            result = "pegged.peg.oneOrMore!(";
            break;
        case "Pegged.Action":
            result = generateCode(p.children[0]);
            foreach(action; p.matches[1..$])
                result = "pegged.peg.action!(" ~ result ~ ", " ~ action ~ ")";
            break;
        case "Pegged.ANY":
            result = "pegged.peg.any";
            break;
        case "Pegged.WrapAround":
            result = "pegged.peg.wrapAround!(" ~ generateCode(p.children[0]) ~ ", "
                                                ~ generateCode(p.children[1]) ~ ", "
                                                ~ generateCode(p.children[2]) ~ ")";
            break;
        default:
            result = "Bad tree: " ~ p.toString();
            break;
    }
    return result;
}

/**
Generate a parser from a PEG definition.
The parser is a string containing D code, to be mixed in or written in a file.

----
enum string def = "
Gram:
    A <- 'a' B*
    B <- 'b' / 'c'
";

mixin(grammar(def));

ParseTree p = Gram("abcbccbcd");
----
*/
string grammar(Memoization withMemo = Memoization.no)(string definition)
{
    ParseTree defAsParseTree = Pegged(definition);

    if (!defAsParseTree.successful)
    {
        // To work around a strange bug with ParseTree printing at compile time
        string result = "static assert(false, `" ~ defAsParseTree.toString("") ~ "`);";
        return result;
    }

    return generateCode(defAsParseTree);
}

/**
Returns for all grammar rules:

- the recursion type (no recursion, direct or indirect recursion).
- the left-recursion type (no left-recursion, direct left-recursion, hidden, or indirect)
- the null-match for a grammar's rules: whether the rule can succeed while consuming nothing.
- the possibility of an infinite loop (if 'e' can null-match, then 'e*' can enter an infinite loop).

This kind of potential problem can be detected statically and should be transmitted to the grammar designer.
*/
RuleIntrospection[string] grammarIntrospection(ParseTree gram, RuleIntrospection[string] external = null)
{
    RuleIntrospection[string] result;
    ParseTree[string] rules;

    /**
    Returns the call graph of a grammar: the list of rules directly called by each rule of the grammar.
    The graph is represented as a bool[string][string] associative array, the string holding
    the rules names. graph["ruleName"] contains all rules called by ruleName, as a set (a bool[string] AA).

    graph.keys thus gives the grammar's rules names.

    If a rule calls itself, its own name will appear in the called set. If a rule calls an external rule, it will
    also appear in the call graph when the rule has a name: hence, calls to predefined rules like 'identifier' or
    'digit' will appear, but not a call to '[0-9]+', considered here as an anonymous rule.
    */
    bool[string][string] callGraph(ParseTree p)
    {
        bool[string] findIdentifiers(ParseTree p)
        {
            bool[string] idList;
            if (p.name == "Pegged.RhsName")
            {
                string qualifiedName;
                foreach(match; p.matches)
                    qualifiedName ~= match;
                idList[qualifiedName] = true;
            }
            else
                foreach(child; p.children)
                    foreach(name; findIdentifiers(child).keys)
                        idList[name] = true;

            return idList;
        }

        bool[string][string] graph;

        foreach(definition; p.children)
            if (definition.name == "Pegged.Definition")
            {
                auto ids = findIdentifiers(definition.children[2]);
                graph[definition.matches[0]] = ids;
                foreach(id, _; ids) // getting possible external calls
                    if (id !in graph)
                        graph[id] = (bool[string]).init;
            }

        return graph;
    }

    /**
    The transitive closure of a call graph.
    It will propagate the calls to find all rules called by a given rule,
    directly (already in the call graph) or indirectly (through another rule).
    */
    bool[string][string] closure(bool[string][string] graph)
    {
        bool[string][string] path;
        foreach(rule, children; graph) // deep-dupping, to avoid children aliasing
            path[rule] = children.dup;

        bool changed = true;

        while(changed)
        {
            changed = false;
            foreach(rule1; graph.keys)
                foreach(rule2; graph.keys)
                    if (rule2 in path[rule1])
                        foreach(rule3; graph.keys)
                            if (rule3 in path[rule2] && rule3 !in path[rule1])
                            {
                                path[rule1][rule3] = true;
                                changed = true;
                            }
        }

        return path;
    }

    Recursive[string] recursions(bool[string][string] graph)
    {
        bool[string][string] path = graph;

        Recursive[string] result;
        foreach(rule, children; path)
        {
            result[rule] = Recursive.no;
            if (rule in children)
            {
                if (rule in graph[rule])
                    result[rule] = Recursive.direct;
                else
                    result[rule] = Recursive.indirect;
            }
        }

        return result;
    }

    NullMatch nullMatching(ParseTree p, RuleIntrospection[string] external = null)
    {
        switch (p.name)
        {
            case "Pegged.Expression": // choice expressions null-match whenever one of their components can null-match
                bool someIndetermination;
                foreach(elem; p.children)
                {
                    NullMatch nm = nullMatching(elem, external);
                    if (nm == NullMatch.yes)
                        return NullMatch.yes;
                    else if (nm == NullMatch.indeterminate)
                        someIndetermination = true;
                }
                if (someIndetermination) // All were indeterminate
                    return NullMatch.indeterminate;
                else
                    return NullMatch.no;
            case "Pegged.Sequence": // sequence expressions can null-match when all their components can null-match
                foreach(elem; p.children)
                {
                    NullMatch nm = nullMatching(elem, external);
                    if (nm == NullMatch.indeterminate)
                        return NullMatch.indeterminate;
                    if (nm == NullMatch.no)
                        return NullMatch.no;
                }
                return NullMatch.yes;
            case "Pegged.Prefix":
                foreach(pref; p.children[0..$-1])
                    if (pref.name == "Pegged.POS" || pref.name == "Pegged.NEG")
                        return NullMatch.yes;
                return nullMatching(p.children[$-1], external);
            case "Pegged.Suffix":
                foreach(pref; p.children[1..$])
                    if (pref.name == "Pegged.ZEROORMORE" || pref.name == "Pegged.OPTION")
                        return NullMatch.yes;
                return nullMatching(p.children[0], external);
            case "Pegged.Primary":
                return nullMatching(p.children[0], external);
            case "Pegged.RhsName":
                if (auto m1 = p.matches[0] in result)
                    return m1.nullMatch;
                else if (auto m2 = p.matches[0] in external)
                    return m2.nullMatch;
                else
                    return NullMatch.indeterminate;
            case "Pegged.Literal":
                if (p.matches[0].length == 0) // Empty literal, '' or ""
                    return NullMatch.yes;
                else
                    return NullMatch.no;
            case "Pegged.CharClass":
                return NullMatch.no;
            case "Pegged.ANY":
                return NullMatch.no;
            default:
                return NullMatch.indeterminate;
        }
    }

    InfiniteLoop infiniteLooping(ParseTree p, RuleIntrospection[string] external = null)
    {
        /+
        if (  p.matches[0] in result
           && result[p.matches[0]].nullMatch == NullMatch.yes
           && result[p.matches[0]].recursion != Recursive.no) // Calls itself while possibly null-matching
            return InfiniteLoop.yes;
        +/

        switch (p.name)
        {
            case "Pegged.Expression": // choice expressions loop whenever one of their components can loop
                foreach(i,elem; p.children)
                {
                    auto nm = infiniteLooping(elem, external);
                    if (nm == InfiniteLoop.yes)
                        return InfiniteLoop.yes;
                    if (nm == InfiniteLoop.indeterminate)
                        return InfiniteLoop.indeterminate;
                }
                return InfiniteLoop.no;
            case "Pegged.Sequence": // sequence expressions can loop when one of their components can loop
                foreach(i,elem; p.children)
                {
                    auto nm = infiniteLooping(elem, external);
                    if (nm == InfiniteLoop.yes)
                        return InfiniteLoop.yes;
                    if (nm == InfiniteLoop.indeterminate && i == 0) // because if i>0, then the previous elems are all
                                                                    // InfiniteLoop.no (.yes would cause en exit)
                        return InfiniteLoop.indeterminate;
                }
                return InfiniteLoop.no;
            case "Pegged.Prefix":
                return infiniteLooping(p.children[$-1], external);
            case "Pegged.Suffix":
                foreach(pref; p.children[1..$])
                    if ((  pref.name == "Pegged.ZEROORMORE" || pref.name == "Pegged.ONEORMORE")
                        && p.matches[0] in result
                        && result[p.matches[0]].nullMatch == NullMatch.yes)
                        return InfiniteLoop.yes;
                return infiniteLooping(p.children[0], external);
            case "Pegged.Primary":
                return infiniteLooping(p.children[0], external);
            case "Pegged.RhsName":
                if (auto m1 = p.matches[0] in result)
                    return m1.infiniteLoop;
                else if (auto m2 = p.matches[0] in external)
                    return m2.infiniteLoop;
                else
                    return InfiniteLoop.indeterminate;
            case "Pegged.Literal":
                return InfiniteLoop.no;
            case "Pegged.CharClass":
                return InfiniteLoop.no;
            case "Pegged.ANY":
                return InfiniteLoop.no;
            default:
                return InfiniteLoop.indeterminate;
        }
    }

    LeftRecursive leftRecursion(ParseTree p, string target)
    {
        switch (p.name)
        {
            case "Pegged.Expression": // Choices are left-recursive is any choice is left-recursive
                foreach(elem; p.children)
                {
                    auto lr = leftRecursion(elem, target);
                    if (lr != LeftRecursive.no)
                        return lr;
                }
                return LeftRecursive.no;
            case "Pegged.Sequence": // Sequences are left-recursive when the leftmost member is left-recursive
                                    // or behind null-matching members
                foreach(i, elem; p.children)
                {
                    auto lr = leftRecursion(elem, target);
                    if (lr == LeftRecursive.direct)
                        return (i == 0 ? LeftRecursive.direct : LeftRecursive.hidden);
                    else if (lr == LeftRecursive.hidden || lr == LeftRecursive.indirect)
                        return lr;
                    else if (nullMatching(elem) == NullMatch.yes)
                        continue;
                    else
                        return LeftRecursive.no;
                }
                return LeftRecursive.no; // found only null-matching rules!
            case "Pegged.Prefix":
                return leftRecursion(p.children[$-1], target);
            case "Pegged.Suffix":
                return leftRecursion(p.children[0], target);
            case "Pegged.Primary":
                return leftRecursion(p.children[0], target);
            case "Pegged.RhsName":
                if (p.matches[0] == target) // ?? Or generateCode(p) ?
                    return LeftRecursive.direct;
                else if ((p.matches[0] in rules) && (leftRecursion(rules[p.matches[0]], target) != LeftRecursive.no))
                    return LeftRecursive.hidden;
                else
                    return LeftRecursive.no;
            case "Pegged.Literal":
                return LeftRecursive.no;
            case "Pegged.CharClass":
                return LeftRecursive.no;
            case "Pegged.ANY":
                return LeftRecursive.no;
            case "eps":
                return LeftRecursive.no;
            case "eoi":
                return LeftRecursive.no;
            default:
                return LeftRecursive.no;
        }
    }


    string expectedInput(ParseTree p, RuleIntrospection[string] external = null)
    {
        switch(p.name)
        {
            case "Pegged.Expression":
                string expectation;
                foreach(i, child; p.children)
                    expectation ~= expectedInput(child, external) ~ (i < p.children.length -1 ? " or " : "");
                return expectation;
            case "Pegged.Sequence":
                string expectation;
                foreach(i, expr; p.children)
                    expectation ~= expectedInput(expr, external) ~ (i < p.children.length -1 ? " followed by " : "");
                return expectation;
            case "Pegged.Prefix":
                return expectedInput(p.children[$-1], external);
            case "Pegged.Suffix":
                string expectation;
                string end;
                foreach(prefix; p.children[1..$])
                    switch(prefix.name)
                    {
                        case "Pegged.ZEROORMORE":
                            expectation ~= "zero or more times (";
                            end ~= ")";
                            break;
                        case "Pegged.ONEORMORE":
                            expectation ~= "one or more times (";
                            end ~= ")";
                            break;
                        case "Pegged.OPTION":
                            expectation ~= "optionally (";
                            end ~= ")";
                            break;
                        case "Pegged.Action":
                            break;
                        default:
                            break;
                    }
                return expectation ~ expectedInput(p.children[0], external) ~ end;
            case "Pegged.Primary":
                return expectedInput(p.children[0], external);
            case "Pegged.RhsName":
                string expectation;
                foreach(elem; p.matches)
                    expectation ~= elem;
                if (auto m = expectation in external)
                    return m.expected;
                else
                    return "rule " ~ expectation;
            case "Pegged.Literal":
                return "the literal `" ~ p.matches[0] ~ "`";
            case "Pegged.CharClass":
                string expectation;
                foreach(i, child; p.children)
                    expectation ~= expectedInput(child, external) ~ (i < p.children.length -1 ? " or " : "");
                return expectation;
            case "Pegged.CharRange":
                if (p.children.length == 1)
                    return expectedInput(p.children[0], external);
                else
                    return "any character between '" ~ p.matches[0] ~ "' and '" ~ p.matches[2] ~ "'";
            case "Pegged.Char":
                return "the character '" ~ p.matches[0] ~ "'";
            case "Pegged.ANY":
                return "any character";
            default:
                return "unknow rule (" ~ p.matches[0] ~ ")";
        }
    }

    if (gram.name == "Pegged")
        gram = gram.children[0];
    string grammarName;

    bool first = true; // to catch the first real definition
    foreach(index,definition; gram.children)
    {
        if (definition.name == "Pegged.GrammarName")
            grammarName = definition.matches[0];
        else if (definition.name == "Pegged.Definition")
        {
            // !!!!!!!!! To change: use generateCode to get the correct name for parameterized rules !!!!!
            rules[grammarName ~ "." ~ definition.matches[0]] = definition.children[2];
            RuleIntrospection ri;
            // !!!!!!!!! To change: use generateCode to get the correct name for parameterized rules !!!!!
            ri.name = grammarName ~ "." ~ definition.matches[0];
            ri.startRule = first;
            first = false;
            ri.recursion = Recursive.no;
            ri.leftRecursion = LeftRecursive.no;
            ri.nullMatch = NullMatch.indeterminate;
            ri.infiniteLoop = InfiniteLoop.indeterminate;
            ri.expected = expectedInput(definition.children[2], external);
            // !!!!!!!!! To change: use generateCode to get the correct name for parameterized rules !!!!!
            result[grammarName ~ "." ~ definition.matches[0]] = ri;
        }
    }

    // Filling the calling informations
    auto cg = callGraph(gram);
    foreach(name, node; cg)
        foreach(called, _; node)
            result[grammarName ~ "." ~ name].directCalls[(called in result ? grammarName ~ "." ~ called : called)] = true;

    auto cl = closure(cg);
    foreach(name, node; cl)
        foreach(called, _; node)
        {
            result[grammarName ~ "." ~ name].calls[(called in result ? grammarName ~ "." ~ called : called)] = true;
            if (called in result)
                result[grammarName ~ "." ~ called].calledBy[grammarName ~ "." ~ name] = true;
        }

    // Filling the recursion informations
    auto rec = recursions(cl);
    foreach(rule, recursionType; rec)
        if (rule in result) // external rules are in rec, but not in result
            result[rule].recursion = recursionType;

    foreach(name, tree; rules)
        if (result[name].recursion != Recursive.no)
            result[name].leftRecursion = leftRecursion(tree, name);

    // Filling the null-matching information
    bool changed = true;
    while(changed) // while something new happened, the process is not over
    {
        changed = false;
        foreach(name, tree; rules)
            if (result[name].nullMatch == NullMatch.indeterminate) // not done yet
            {
                result [name].nullMatch = nullMatching(tree, external); // try to find if it's null-matching
                if (result[name].nullMatch != NullMatch.indeterminate)
                    changed = true;
            }
    }

    // Filling the infinite looping information
    changed = true;
    while(changed) // while something new happened, the process is not over
    {
        changed = false;
        foreach(name, tree; rules)
            if (result[name].infiniteLoop == InfiniteLoop.indeterminate) // not done yet
            {
                result [name].infiniteLoop = infiniteLooping(tree, external); // try to find if it's looping
                if (result[name].infiniteLoop != InfiniteLoop.indeterminate)
                    changed = true; // something changed, we will continue the process
            }
    }

    return result;
}

bool usefulRule(RuleIntrospection ri)
{
    return ri.startRule || ri.calledBy.length == 0;
}

bool terminal(RuleIntrospection ri)
{
    return ri.calls.length == 0;
}

/**
Act on rules parse tree as produced by pegged.parser.
Replace every occurence of child in parent by child's parse tree
*/
ParseTree replaceInto(ParseTree parent, ParseTree child)
{
    if (parent.name == "Pegged.RhsName" && parent.matches[0] == child.matches[0])
        return ParseTree("Pegged.Named", true, child.matches[0..1], "",0,0,
                       [child.children[2],
                        ParseTree("Pegged.Identifier", true, child.matches[0..1])]);
    else
        foreach(ref branch; parent.children)
            branch = replaceInto(branch, child);
    return parent;
}

unittest // 'grammar' unit test: low-level functionalities
{
    mixin(grammar(`
    Test1:
        Rule1 <- 'a'
        Rule2 <- 'b'
    `));

    assert(is(Test1 == struct), "A struct name Test1 is created.");
    assert(is(typeof(Test1("a"))), "Test1 is callable with a string arg");
    assert(__traits(hasMember, Test1, "Rule1"), "Test1 has a member named Rule1.");
    assert(__traits(hasMember, Test1, "Rule2"), "Test1 has a member named Rule2.");
    assert(is(typeof(Test1.Rule1("a"))), "Test1.Rule1 is callable with a string arg");
    assert(is(typeof(Test1.Rule2("a"))), "Test1.Rule2 is callable with a string arg");

    assert(__traits(hasMember, Test1, "decimateTree"), "Test1 has a member named decimateTree.");
    assert(__traits(hasMember, Test1, "name"), "Test1 has a member named name.");
    assert(__traits(hasMember, Test1, "isRule"), "Test1 has a member named isRule.");
}

unittest // 'grammar' unit test: PEG syntax
{
    // Here we do not test pegged.peg.*, just the grammar transformations
    // From a PEG to a Pegged expression template.

    mixin(grammar(`
    Terminals:
        Literal1 <- "abc"
        Literal2 <- 'abc'
        EmptyLiteral1 <- ""
        EmptyLiteral2 <- ''
        Any <- .
        Eps <- eps
        Letter <- [a-z]
        Digit  <- [0-9]
        ABC    <- [abc]
        Alpha1  <- [a-zA-Z_]
        Alpha2  <- [_a-zA-Z]
    `));

    ParseTree result = Terminals("abc");

    assert(result.name == "Terminals", "Grammar name test.");
    assert(result.children[0].name == "Terminals.Literal1", "First rule name test.");
    assert(result.begin == 0);
    assert(result.end == 3);
    assert(result.matches == ["abc"]);

    ParseTree reference = Terminals.decimateTree(Terminals.Literal1("abc"));

    assert(result.children[0] == reference, "Invoking a grammar is like invoking its first rule.");

    assert(Terminals.Literal1("abc").successful, "Standard terminal test. Double quote syntax.");
    assert(Terminals.Literal2("abc").successful, "Standard terminal test. Simple quote syntax.");
    assert(Terminals.EmptyLiteral1("").successful , "Standard terminal test. Double quote syntax.");
    assert(Terminals.EmptyLiteral2("").successful, "Standard terminal test. Simple quote syntax.");

    foreach(char c; char.min .. char.max)
        assert(Terminals.Any(""~c).successful, "Any terminal ('.') test.");

    assert(Terminals.Eps("").successful, "Eps test.");
    assert(Terminals.Eps("abc").successful, "Eps test.");

    string lower  = "abcdefghijklmnopqrstuvwxyz";
    string upper  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    string under  = "_";
    string digits = "0123456789";
    string others = "?./,;:!*&()[]<>";

    foreach(dchar dc; lower)
        assert(Terminals.Letter(to!string(dc)).successful);
    foreach(dchar dc; upper)
        assert(!Terminals.Letter(to!string(dc)).successful);
    foreach(dchar dc; digits)
        assert(!Terminals.Letter(to!string(dc)).successful);
    foreach(dchar dc; others)
        assert(!Terminals.Letter(to!string(dc)).successful);

    foreach(dchar dc; lower)
        assert(!Terminals.Digit(to!string(dc)).successful);
    foreach(dchar dc; upper)
        assert(!Terminals.Digit(to!string(dc)).successful);
    foreach(dchar dc; digits)
        assert(Terminals.Digit(to!string(dc)).successful);
    foreach(dchar dc; others)
        assert(!Terminals.Letter(to!string(dc)).successful);

    foreach(dchar dc; lower ~ upper ~ under)
        assert(Terminals.Alpha1(to!string(dc)).successful);
    foreach(dchar dc; digits ~ others)
        assert(!Terminals.Alpha1(to!string(dc)).successful);

    foreach(dchar dc; lower ~ upper ~ under)
        assert(Terminals.Alpha2(to!string(dc)).successful);
    foreach(dchar dc; digits ~ others)
        assert(!Terminals.Alpha2(to!string(dc)).successful);

    foreach(size_t index, dchar dc; lower ~ upper ~ under)
        assert( (index < 3  && Terminals.ABC(to!string(dc)).successful)
             || (index >= 3 && !Terminals.ABC(to!string(dc)).successful));

    mixin(grammar(`
    Structure:
        Rule1 <- Rule2 / Rule3 / Rule4   # Or test
        Rule2 <- Rule3 Rule4             # And test
        Rule3 <- "abc"
        Rule4 <- "def"

        Rule5 <- (Rule2 /  Rule4) Rule3  # parenthesis test
        Rule6 <-  Rule2 /  Rule4  Rule3
        Rule7 <-  Rule2 / (Rule4  Rule3)
    `));

    // Invoking Rule2 (and hence, Rule3 Rule4)
    result = Structure("abcdef");

    assert(result.successful, "Calling Rule2.");
    assert(result.name == "Structure", "Grammar name test.");
    assert(result.children[0].name == "Structure.Rule1", "First rule name test");
    assert(result.children[0].children.length == 1);
    assert(result.children[0].children[0].name == "Structure.Rule2");
    assert(result.children[0].children[0].children[0].name == "Structure.Rule3");
    assert(result.children[0].children[0].children[1].name == "Structure.Rule4");
    assert(result.matches == ["abc", "def"]);
    assert(result.begin ==0);
    assert(result.end == 6);

    // Invoking Rule3
    result = Structure("abc");

    assert(result.successful, "Calling Rule3.");
    assert(result.name == "Structure", "Grammar name test.");
    assert(result.children[0].name == "Structure.Rule1", "First rule name test");
    assert(result.children[0].children.length == 1);
    assert(result.children[0].children[0].name == "Structure.Rule3");
    assert(result.children[0].children[0].children.length == 0);
    assert(result.matches == ["abc"]);
    assert(result.begin ==0);
    assert(result.end == 3);

    // Invoking Rule4
    result = Structure("def");

    assert(result.successful, "Calling Rule2.");
    assert(result.name == "Structure", "Grammar name test.");
    assert(result.children[0].name == "Structure.Rule1", "First rule name test");
    assert(result.children[0].children.length == 1);
    assert(result.children[0].children[0].name == "Structure.Rule4");
    assert(result.children[0].children[0].children.length == 0);
    assert(result.matches == ["def"]);
    assert(result.begin ==0);
    assert(result.end == 3);

    // Failure
    result =Structure("ab_def");
    assert(!result.successful);
    assert(result.name == "Structure", "Grammar name test.");
    assert(result.begin == 0);
    assert(result.end == 0);

    // Parenthesis test
    // Rule5 <- (Rule2 /  Rule4) Rule3
    result = Structure.decimateTree(Structure.Rule5("abcdefabc"));
    assert(result.successful);
    assert(result.children.length == 2, "Two children: (Rule2 / Rule4), followed by Rule3.");
    assert(result.children[0].name == "Structure.Rule2");
    assert(result.children[1].name == "Structure.Rule3");

    result = Structure.decimateTree(Structure.Rule5("defabc"));
    assert(result.successful);
    assert(result.children.length == 2, "Two children: (Rule2 / Rule4), followed by Rule3.");
    assert(result.children[0].name == "Structure.Rule4");
    assert(result.children[1].name == "Structure.Rule3");

    // Rule6 <-  Rule2 /  Rule4  Rule3
    result = Structure.decimateTree(Structure.Rule6("abcdef"));
    assert(result.successful);
    assert(result.children.length == 1, "One child: Rule2.");
    assert(result.children[0].name == "Structure.Rule2");

    result = Structure.decimateTree(Structure.Rule6("defabc"));
    assert(result.successful);
    assert(result.children.length == 2, "Two children: Rule4, followed by Rule3.");
    assert(result.children[0].name == "Structure.Rule4");
    assert(result.children[1].name == "Structure.Rule3");

    // Rule7 <-  Rule2 / (Rule4  Rule3)
    // That is, like Rule6
    result = Structure.decimateTree(Structure.Rule7("abcdef"));
    assert(result.successful);
    assert(result.children.length == 1, "One child: Rule2.");
    assert(result.children[0].name == "Structure.Rule2");

    result = Structure.decimateTree(Structure.Rule7("defabc"));
    assert(result.successful);
    assert(result.children.length == 2, "Two children: Rule4, followed by Rule3.");
    assert(result.children[0].name == "Structure.Rule4");
    assert(result.children[1].name == "Structure.Rule3");

    // Prefixes and Suffixes
    mixin(grammar(`
    PrefixSuffix:
        Rule1 <- &"abc"
        Rule2 <- !"abc"
        Rule3 <- "abc"?
        Rule4 <- "abc"*
        Rule5 <- "abc"+
    `));

    // Verifying &"abc" creates a positive look-ahead construct
    result = PrefixSuffix.Rule1("abc");
    reference = posLookahead!(literal!"abc")("abc");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    result = PrefixSuffix.Rule1("def");
    reference = posLookahead!(literal!"abc")("def");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);


    // Verifying !"abc" creates a negative look-ahead construct
    result = PrefixSuffix.Rule2("abc");
    reference = negLookahead!(literal!"abc")("abc");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    result = PrefixSuffix.Rule2("def");
    reference = negLookahead!(literal!"abc")("def");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    // Verifying "abc"? creates an optional construct
    result = PrefixSuffix.Rule3("abc");
    reference = option!(literal!"abc")("abc");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    result = PrefixSuffix.Rule3("def");
    reference = option!(literal!"abc")("def");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    // Verifying "abc"* creates a zero or more construct
    result = PrefixSuffix.Rule4("");
    reference = zeroOrMore!(literal!"abc")("");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    result = PrefixSuffix.Rule4("abc");
    reference = zeroOrMore!(literal!"abc")("abc");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    result = PrefixSuffix.Rule4("abcabc");
    reference = zeroOrMore!(literal!"abc")("abcabc");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    // Verifying "abc"+ creates a one or more construct
    result = PrefixSuffix.Rule5("");
    reference = oneOrMore!(literal!"abc")("");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    result = PrefixSuffix.Rule5("abc");
    reference = oneOrMore!(literal!"abc")("abc");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    result = PrefixSuffix.Rule5("abcabc");
    reference = oneOrMore!(literal!"abc")("abcabc");

    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);
}

unittest // Multilines rules
{
    mixin(grammar(`
Indentation:
Rule1 <
'a'
    'b'
'c'
    Rule2
<-
 'd'
Rule3
<
'e'
Rule4 <- 'f' Rule5   # Rule4 ends with 'f', then it's Rule5
<- 'g'




    'h'
`));


    assert(Indentation("abc").successful);
    assert(Indentation.Rule2("d").successful);
    assert(Indentation.Rule3("e").successful);
    assert(Indentation.Rule4("f").successful);
    assert(Indentation.Rule5("gh").successful);
}

unittest // Parsing at compile-time
{
    mixin(grammar(`
    Test:
        Rule1 <- 'a' Rule2('b')
        Rule2(B) <- B
    `));

    // Equality on success
    ParseTree result = Test("ab");

    enum CTsuccess = Test("ab");

    assert(CTsuccess == result, "Compile-time parsing is equal to runtime parsing on success.");

    // Equality on failure
    result = Test("ac");
    enum CTfailure = Test("ac");

    assert(CTfailure == result, "Compile-time parsing is equal to runtime parsing on failure.");
}

unittest // PEG extensions (arrows, prefixes, suffixes)
{
    mixin(grammar(`
    Arrows:
        Rule1 <- ABC DEF  # Standard arrow
        Rule2 <  ABC DEF  # Space arrow
        Rule3 <  ABC DEF* # Space arrow
        Rule4 <  ABC+ DEF # Space arrow

        Rule5 <- ABC*
        Rule6 <~ ABC*     # Fuse arrow

        Rule7 <: ABC DEF  # Discard arrow
        Rule8 <^ ABC DEF  # Keep arrow
        Rule9 <; ABC DEF  # Drop arrow
        Rule10 <% ABC Rule1 DEF  # Propagate arrow

        ABC <- "abc"
        DEF <- "def"
    `));

    // Comparing <- ABC DEF and < ABC DEF
    ParseTree result = Arrows.decimateTree(Arrows.Rule1("abcdef"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end ==6);
    assert(result.matches == ["abc", "def"]);
    assert(result.children.length == 2);
    assert(result.children[0].name == "Arrows.ABC");
    assert(result.children[1].name == "Arrows.DEF");

    result = Arrows.decimateTree(Arrows.Rule1("abc  def"));
    assert(!result.successful);

    result = Arrows.decimateTree(Arrows.Rule2("abcdef"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["abc", "def"]);
    assert(result.children.length == 2);
    assert(result.children[0].name == "Arrows.ABC");
    assert(result.children[1].name == "Arrows.DEF");

    result = Arrows.decimateTree(Arrows.Rule2("abc  def  "));
    assert(result.successful, "space arrows consume spaces.");
    assert(result.begin == 0);
    assert(result.end == "abc  def  ".length, "The entire input is parsed.");
    assert(result.matches == ["abc", "def"]);
    assert(result.children.length == 2);
    assert(result.children[0].name == "Arrows.ABC");
    assert(result.children[1].name == "Arrows.DEF");

    result = Arrows.decimateTree(Arrows.Rule3("abcdefdef"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcdefdef".length);
    assert(result.matches == ["abc", "def", "def"]);
    assert(result.children.length == 3);
    assert(result.children[0].name == "Arrows.ABC");
    assert(result.children[1].name == "Arrows.DEF");
    assert(result.children[2].name == "Arrows.DEF");

    result = Arrows.decimateTree(Arrows.Rule3("abc  def  defdef"));
    assert(result.successful, "space arrows consume spaces.");
    assert(result.begin == 0);
    assert(result.end == "abc  def  defdef".length, "The entire input is parsed.");
    assert(result.matches == ["abc", "def", "def", "def"]);
    assert(result.children.length == 4);
    assert(result.children[0].name == "Arrows.ABC");
    assert(result.children[1].name == "Arrows.DEF");
    assert(result.children[2].name == "Arrows.DEF");
    assert(result.children[3].name == "Arrows.DEF");

    result = Arrows.decimateTree(Arrows.Rule4("abcabcdef"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcdef".length);
    assert(result.matches == ["abc", "abc", "def"]);
    assert(result.children.length == 3);
    assert(result.children[0].name == "Arrows.ABC");
    assert(result.children[1].name == "Arrows.ABC");
    assert(result.children[2].name == "Arrows.DEF");

    result = Arrows.decimateTree(Arrows.Rule4("   abc abcabc  def  "));
    assert(result.successful, "space arrows consume spaces.");
    assert(result.begin == 0);
    assert(result.end == "   abc abcabc  def  ".length, "The entire input is parsed.");
    assert(result.matches == ["abc", "abc", "abc", "def"]);
    assert(result.children.length == 4);
    assert(result.children[0].name == "Arrows.ABC");
    assert(result.children[1].name == "Arrows.ABC");
    assert(result.children[2].name == "Arrows.ABC");
    assert(result.children[3].name == "Arrows.DEF");

    //Comparing <- ABC* and <~ ABC*
    result = Arrows.decimateTree(Arrows.Rule5("abcabcabc"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcabc".length, "The entire input is parsed.");
    assert(result.matches == ["abc", "abc", "abc"]);
    assert(result.children.length == 3, "With the * operator, all children are kept.");
    assert(result.children[0].name == "Arrows.ABC");
    assert(result.children[1].name == "Arrows.ABC");
    assert(result.children[2].name == "Arrows.ABC");

    result = Arrows.decimateTree(Arrows.Rule6("abcabcabc"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcabc".length, "The entire input is parsed.");
    assert(result.matches == ["abcabcabc"], "Matches are fused.");
    assert(result.children.length == 0, "The <~ arrow cuts children.");

    // Comparing <- ABC DEF and <: ABC DEF
    result = Arrows.decimateTree(Arrows.Rule7("abcdef"));
    assert(result.successful);
    assert(result.begin == "abcdef".length);
    assert(result.end == "abcdef".length, "The entire input is parsed.");
    assert(result.matches is null, "No match with the discard arrow.");
    assert(result.children.length == 0, "No children with the discard arrow.");

    // Comparing <- ABC DEF and <^ ABC DEF
    //But <^ is not very useful anyways. It does not distribute ^ among the subrules.
    result = Arrows.decimateTree(Arrows.Rule8("abcdef"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcdef".length, "The entire input is parsed.");
    assert(result.matches == ["abc", "def"]);
    assert(result.children.length == 2);

    // Comparing <- ABC DEF and <; ABC DEF
    result = Arrows.decimateTree(Arrows.Rule9("abcdef"));
    assert(result.successful);
    assert(result.begin == "abcdef".length);
    assert(result.end == "abcdef".length, "The entire input is parsed.");
    assert(result.matches == ["abc", "def"], "The drop arrow keeps the matches.");
    assert(result.children.length == 0, "The drop arrow drops the children.");

    // Comparing <- ABC DEF and <% ABC Rule1 DEF
    //But <% is not very useful anyways. It does not distribute % among the subrules.
    result = Arrows.decimateTree(Arrows.Rule10("abcabcdefdef"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcdefdef".length, "The entire input is parsed.");
    assert(result.matches == ["abc", "abc", "def", "def"]);
    assert(result.children.length == 3);
    assert(result.children[0].name == "Arrows.ABC");
    assert(result.children[1].name == "Arrows.Rule1", "No rule replacement by its own children. See % for that.");
    assert(result.children[2].name == "Arrows.DEF");
}

unittest //More space arrow tests
{
    mixin(grammar(`
    Spaces:
        Rule1 < A (B C)+
        A <- 'a'
        B <- 'b'
        C <- 'c'
    `));

    ParseTree result = Spaces.decimateTree(Spaces.Rule1("abcbc"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcbc".length);
    assert(result.children.length == 5);
    assert(result.children[0].name == "Spaces.A");
    assert(result.children[1].name == "Spaces.B");
    assert(result.children[2].name == "Spaces.C");
    assert(result.children[3].name == "Spaces.B");
    assert(result.children[4].name == "Spaces.C");

    result = Spaces.decimateTree(Spaces.Rule1(" a bc  b  c "));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == " a bc  b  c ".length);
    assert(result.children.length == 5);
    assert(result.children[0].name == "Spaces.A");
    assert(result.children[1].name == "Spaces.B");
    assert(result.children[2].name == "Spaces.C");
    assert(result.children[3].name == "Spaces.B");
    assert(result.children[4].name == "Spaces.C");
}

unittest // Prefix and suffix tests
{
    mixin(grammar(`
    PrefixSuffix:
        # Reference
        Rule1 <-  ABC   DEF
        Rule2 <- "abc" "def"
        Rule3 <-    ABC*
        Rule4 <-   "abc"*

        # Discard operator
        Rule5 <-  :ABC    DEF
        Rule6 <-   ABC   :DEF
        Rule7 <- :"abc"  "def"
        Rule8 <-  "abc" :"def"

        # Drop operator
        Rule9  <-  ;ABC    DEF
        Rule10 <-   ABC   ;DEF
        Rule11 <- ;"abc"  "def"
        Rule12 <-  "abc" ;"def"

        # Fuse operator

        Rule13 <- ~( ABC* )
        Rule14 <- ~("abc"*)

        # Keep operator
        Rule15 <- ^"abc" ^"def"

        # Propagate operator
        Rule16 <- ABC  Rule1 DEF
        Rule17 <- ABC %Rule1 DEF


        ABC <- "abc"
        DEF <- "def"
    `));


    // Comparing standard and discarded rules
    auto result = PrefixSuffix.decimateTree(PrefixSuffix.Rule1("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["abc", "def"]);
    assert(result.children.length == 2);
    assert(result.children[0].name == "PrefixSuffix.ABC");
    assert(result.children[1].name == "PrefixSuffix.DEF");

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule5("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["def"]);
    assert(result.children.length == 1, "The first child is discarded.");
    assert(result.children[0].name == "PrefixSuffix.DEF");

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule6("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["abc"]);
    assert(result.children.length == 1, "The second child is discarded.");
    assert(result.children[0].name == "PrefixSuffix.ABC");


    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule2("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["abc", "def"]);
    assert(result.children.length == 0, "Literals do not create children.");

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule7("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["def"]);
    assert(result.children.length == 0);

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule8("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["abc"]);
    assert(result.children.length == 0);

    // Comparing standard and dropped rules

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule9("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["abc", "def"], "All matches are there.");
    assert(result.children.length == 1, "The first child is discarded.");
    assert(result.children[0].name == "PrefixSuffix.DEF");

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule10("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["abc", "def"], "All matches are there.");
    assert(result.children.length == 1, "The second child is discarded.");
    assert(result.children[0].name == "PrefixSuffix.ABC");

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule11("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["abc", "def"], "All matches are there.");
    assert(result.children.length == 0);

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule12("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 6);
    assert(result.matches == ["abc", "def"], "All matches are there.");
    assert(result.children.length == 0);


    // Comparing standard and fused rules

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule3("abcabcabc"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcabc".length);
    assert(result.matches == ["abc", "abc", "abc"]);
    assert(result.children.length == 3, "Standard '*': 3 children.");
    assert(result.children[0].name == "PrefixSuffix.ABC");
    assert(result.children[1].name == "PrefixSuffix.ABC");
    assert(result.children[2].name == "PrefixSuffix.ABC");

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule4("abcabcabc"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcabc".length);
    assert(result.matches == ["abc", "abc", "abc"]);
    assert(result.children.length == 0, "All literals are discarded by the tree decimation.");

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule13("abcabcabc"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcabc".length);
    assert(result.matches == ["abcabcabc"], "All matches are fused.");
    assert(result.children.length == 0, "Children are discarded by '~'.");

    result = PrefixSuffix.decimateTree(PrefixSuffix.Rule14("abcabcabc"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcabc".length);
    assert(result.matches == ["abcabcabc"], "All matches are there.");
    assert(result.children.length == 0, "Children are discarded by '~'.");

    // Testing the keep (^) operator

    result  = PrefixSuffix.decimateTree(PrefixSuffix.Rule15("abcdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcdef".length);
    assert(result.matches == ["abc", "def"], "All matches are there.");
    assert(result.children.length == 2, "Both children are kept by '^'.");
    assert(result.children[0].name == `literal!("abc")`,
           `literal!("abc") is kept even though it's not part of the grammar rules.`);
    assert(result.children[1].name == `literal!("def")`,
           `literal!("def") is kept even though it's not part of the grammar rules.`);

    // Comparing standard and propagated (%) rules.
    result  = PrefixSuffix.decimateTree(PrefixSuffix.Rule16("abcabcdefdef"));

    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcdefdef".length);
    assert(result.matches == ["abc", "abc", "def", "def"], "All matches are there.");
    assert(result.children.length == 3, "Standard rule: three children.");
    assert(result.children[0].name == "PrefixSuffix.ABC");
    assert(result.children[1].name == "PrefixSuffix.Rule1");
    assert(result.children[1].children.length == 2, "Rule1 creates two children.");
    assert(result.children[1].children[0].name, "PrefixSuffix.ABC");
    assert(result.children[1].children[1].name, "PrefixSuffix.DEF");
    assert(result.children[2].name == "PrefixSuffix.DEF");

    result  = PrefixSuffix.decimateTree(PrefixSuffix.Rule17("abcabcdefdef"));

    // From (ABC, Rule1(ABC,DEF), DEF) to (ABC,ABC,DEF,DEF)
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcdefdef".length);
    assert(result.matches == ["abc", "abc", "def", "def"], "All matches are there.");
    assert(result.children.length == 4, "%-affected rule: four children.");
    assert(result.children[0].name == "PrefixSuffix.ABC");
    assert(result.children[1].name == "PrefixSuffix.ABC");
    assert(result.children[2].name == "PrefixSuffix.DEF");
    assert(result.children[2].name == "PrefixSuffix.DEF");

    // Testing % and < together
    mixin(grammar(`
    PropTest:
        Rule1 < B C+
        Rule2 <- B (%C)+
        Rule3 <  B (%C)+
        Rule4 <  B %(D E)+

        B <- 'b'
        C <- D E
        D <- 'd'
        E <- 'e'
    `));

    result = PropTest.decimateTree(PropTest.Rule1("bdedede"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "bdedede".length);
    assert(result.matches == ["b", "d", "e", "d", "e", "d", "e"]);
    assert(result.children.length == 4, "b and de, de, de");
    assert(result.children[0].name == "PropTest.B");
    assert(result.children[1].name == "PropTest.C");
    assert(result.children[2].name == "PropTest.C");
    assert(result.children[3].name == "PropTest.C");

    result = PropTest.decimateTree(PropTest.Rule2("bdedede"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "bdedede".length);
    assert(result.matches == ["b", "d", "e", "d", "e", "d", "e"]);
    assert(result.children.length == 7, "b and (d and e), thrice.");
    assert(result.children[0].name == "PropTest.B");
    assert(result.children[1].name == "PropTest.D");
    assert(result.children[2].name == "PropTest.E");
    assert(result.children[3].name == "PropTest.D");
    assert(result.children[4].name == "PropTest.E");
    assert(result.children[5].name == "PropTest.D");
    assert(result.children[6].name == "PropTest.E");

    result = PropTest.decimateTree(PropTest.Rule3("bdedede"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "bdedede".length);
    assert(result.matches == ["b", "d", "e", "d", "e", "d", "e"]);
    assert(result.children.length == 7, "b and (d and e), thrice.");
    assert(result.children[0].name == "PropTest.B");
    assert(result.children[1].name == "PropTest.D");
    assert(result.children[2].name == "PropTest.E");
    assert(result.children[3].name == "PropTest.D");
    assert(result.children[4].name == "PropTest.E");
    assert(result.children[5].name == "PropTest.D");
    assert(result.children[6].name == "PropTest.E");

    result = PropTest.decimateTree(PropTest.Rule3("  b  de de  de "));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "  b  de de  de ".length);
    assert(result.matches == ["b", "d", "e", "d", "e", "d", "e"]);
    assert(result.children.length == 7, "b and (d and e), thrice.");
    assert(result.children[0].name == "PropTest.B");
    assert(result.children[1].name == "PropTest.D");
    assert(result.children[2].name == "PropTest.E");
    assert(result.children[3].name == "PropTest.D");
    assert(result.children[4].name == "PropTest.E");
    assert(result.children[5].name == "PropTest.D");
    assert(result.children[6].name == "PropTest.E");

    result = PropTest.decimateTree(PropTest.Rule4("bdedede"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "bdedede".length);
    assert(result.matches == ["b", "d", "e", "d", "e", "d", "e"]);
    assert(result.children.length == 7, "b and (d and e), thrice.");
    assert(result.children[0].name == "PropTest.B");
    assert(result.children[1].name == "PropTest.D");
    assert(result.children[2].name == "PropTest.E");
    assert(result.children[3].name == "PropTest.D");
    assert(result.children[4].name == "PropTest.E");
    assert(result.children[5].name == "PropTest.D");
    assert(result.children[6].name == "PropTest.E");

    result = PropTest.decimateTree(PropTest.Rule4("  b  de de  de "));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "  b  de de  de ".length);
    assert(result.matches == ["b", "d", "e", "d", "e", "d", "e"]);
    assert(result.children.length == 7, "b and (d and e), thrice.");
    assert(result.children[0].name == "PropTest.B");
    assert(result.children[1].name == "PropTest.D");
    assert(result.children[2].name == "PropTest.E");
    assert(result.children[3].name == "PropTest.D");
    assert(result.children[4].name == "PropTest.E");
    assert(result.children[5].name == "PropTest.D");
    assert(result.children[6].name == "PropTest.E");

    // More than one prefix, more than one suffixes
    mixin(grammar(`
    MoreThanOne:
        Rule1 <- ~:("abc"*)   # Two prefixes (nothing left for ~, after :)
        Rule2 <- :~("abc"*)   # Two prefixes (: will discard everything ~ did)
        Rule3 <- ;:~"abc"     # Many prefixes
        Rule4 <- ~~~("abc"*)  # Many fuses (no global effect)

        Rule5 <- "abc"+*      # Many suffixes
        Rule6 <- "abc"+?      # Many suffixes

        Rule7 <- !!"abc"      # Double negation, equivalent to '&'
        Rule8 <-  &"abc"

        Rule9 <- ^^"abc"+*   # Many suffixes and prefixes
    `));

    assert(is(MoreThanOne), "This compiles all right.");

    result = MoreThanOne.decimateTree(MoreThanOne.Rule1("abcabcabc"));
    assert(result.successful);
    assert(result.begin == "abcabcabc".length);
    assert(result.end == "abcabcabc".length);
    assert(result.matches is null);
    assert(result.children is null);

    result = MoreThanOne.decimateTree(MoreThanOne.Rule2("abcabcabc"));
    assert(result.successful);
    assert(result.begin == "abcabcabc".length);
    assert(result.end == "abcabcabc".length);
    assert(result.matches is null);
    assert(result.children is null);

    result = MoreThanOne.decimateTree(MoreThanOne.Rule3("abcabcabc"));
    assert(result.successful);
    assert(result.begin == "abc".length);
    assert(result.end == "abc".length);
    assert(result.matches is null);
    assert(result.children is null);

    result = MoreThanOne.decimateTree(MoreThanOne.Rule4("abcabcabc"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcabc".length);
    assert(result.matches == ["abcabcabc"]);
    assert(result.children is null);

    // +* and +?
    result = MoreThanOne.decimateTree(MoreThanOne.Rule5("abcabcabc"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcabc".length);
    assert(result.matches == ["abc", "abc", "abc"]);
    assert(result.children.length == 0);

    result = MoreThanOne.decimateTree(MoreThanOne.Rule6("abcabcabc"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "abcabcabc".length);
    assert(result.matches == ["abc", "abc", "abc"]);
    assert(result.children.length == 0);

    // !! is equivalent to &
    result = MoreThanOne.decimateTree(MoreThanOne.Rule7("abc"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.matches is null);
    assert(result.children is null);

    result = MoreThanOne.decimateTree(MoreThanOne.Rule8("abc"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.matches is null);
    assert(result.children is null);

    // ^^"abc"+*
    result = MoreThanOne.decimateTree(MoreThanOne.Rule9("abcabcabc"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 9);
    assert(result.matches == ["abc", "abc", "abc"]);
    assert(result.children.length == 1);
    assert(result.children[0].name == `zeroOrMore!(oneOrMore!(literal!("abc")))`);
    assert(result.children[0].children.length == 1);
    assert(result.children[0].children[0].name == `oneOrMore!(literal!("abc"))`);
    assert(result.children[0].children[0].children.length == 3);
    assert(result.children[0].children[0].children[0].name == `literal!("abc")`);
    assert(result.children[0].children[0].children[1].name == `literal!("abc")`);
    assert(result.children[0].children[0].children[2].name == `literal!("abc")`);
}

unittest // Leading alternation
{
    mixin(grammar(`
    LeadingAlternation:
        Rule1 <- / 'a'
        Rule2 <- / 'a' / 'b'
        Rule3 <- (/ 'a' / 'b')
    `));

    auto result = LeadingAlternation.decimateTree(LeadingAlternation.Rule1("a"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 1);
    assert(result.matches == ["a"]);

    result = LeadingAlternation.decimateTree(LeadingAlternation.Rule2("b"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 1);
    assert(result.matches == ["b"]);

    result = LeadingAlternation.decimateTree(LeadingAlternation.Rule3("b"));
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == 1);
    assert(result.matches == ["b"]);
}

unittest // Extended chars tests
{
mixin(grammar("
    Chars:
        # Lone chars
        Rule1 <- '\t' '0' 'A' '~'
        Rule2 <- '\11' '\60' '\101' '\176'           # \t 0 A ~ in octal
        Rule3 <- '\011' '\060' '\101' '\176'         # \t 0 A ~ in octal (prefix 0)
        Rule4 <- '\x09' '\x30' '\x41' '\x7E'         # \t 0 A ~ in hexadecimal
        Rule5 <- '\u0009' '\u0030' '\u0041' '\u007E' # \t 0 A ~ in unicode
        Rule6 <- '\U00000009' '\U00000030' '\U00000041' '\U0000007E' # \t 0 A ~ in unicode

        # Strings literals
        Rule7 <- '\t0A~'
        Rule8 <- '\11\60\101\176'            # \t 0 A ~ in octal
        Rule9 <- '\011\060\101\176'          # \t 0 A ~ in octal (prefix 0)
        Rule10 <- '\x09\x30\x41\x7E'         # \t 0 A ~ in hexadecimal
        Rule11 <- '\u0009\u0030\u0041\u007E' # \t 0 A ~ in unicode
        Rule12 <- '\U00000009\U00000030\U00000041' '\U0000007E' # \t 0 A ~ in unicode

        # Outside Latin
        Rule13 <- '\u03B1\u03B9\u03C6\u03B1' # alpha in greek
        Rule14 <- 'αιφα'

        # Hello's
        English <- 'Hello'
        Russian <- 'Здравствуйте'
        Arabic <- 'السلام عليك'
        Chinese <- '你好'
        Japanese <- '今日は'
        Spanish <- '¡Hola!'
    "));


    assert(Chars.decimateTree(Chars.Rule1("\t0A~")).successful);
    assert(Chars.decimateTree(Chars.Rule2("\t0A~")).successful);
    assert(Chars.decimateTree(Chars.Rule3("\t0A~")).successful);
    assert(Chars.decimateTree(Chars.Rule4("\t0A~")).successful);
    assert(Chars.decimateTree(Chars.Rule5("\t0A~")).successful);
    assert(Chars.decimateTree(Chars.Rule6("\t0A~")).successful);

    assert(Chars.decimateTree(Chars.Rule7("\t0A~")).successful);
    assert(Chars.decimateTree(Chars.Rule8("\t0A~")).successful);
    assert(Chars.decimateTree(Chars.Rule9("\t0A~")).successful);
    assert(Chars.decimateTree(Chars.Rule10("\t0A~")).successful);
    assert(Chars.decimateTree(Chars.Rule11("\t0A~")).successful);
    assert(Chars.decimateTree(Chars.Rule12("\t0A~")).successful);

    assert(Chars.decimateTree(Chars.Rule13("\u03B1\u03B9\u03C6\u03B1")).successful);
    assert(Chars.decimateTree(Chars.Rule13("αιφα")).successful);

    assert(Chars.decimateTree(Chars.Rule14("\u03B1\u03B9\u03C6\u03B1")).successful);
    assert(Chars.decimateTree(Chars.Rule14("αιφα")).successful);

    assert(Chars.decimateTree(Chars.English("Hello")).successful);
    assert(Chars.decimateTree(Chars.Russian("Здравствуйте")).successful);
    assert(Chars.decimateTree(Chars.Arabic("السلام عليك")).successful);
    assert(Chars.decimateTree(Chars.Chinese("你好")).successful);
    assert(Chars.decimateTree(Chars.Japanese("今日は'")).successful);
    assert(Chars.decimateTree(Chars.Spanish("¡Hola!")).successful);
}

unittest // Extended char range tests
{
    import std.conv;

    mixin(grammar(`
    CharRanges:
        Rule1 <- [a-z]
        Rule2 <- [\141-\172]             # a-z in octal
        Rule3 <- [\x61-\x7A]             # a-z in hexadecimal
        Rule4 <- [\u0061-\u007A]         # a-z in UTF16
        Rule5 <- [\U00000061-\U0000007A] # a-z in UTF32

        Rule6 <- [\-\[\]\\\'\"\n\r\t]
    `));

    string lower = "abcdefghijklmnopqrstuvwxyz";
    string upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    string digits = "0123456789";
    string others = "?./,;:!*&()[]<>";
    string escapes = "-[]\\\'\"\n\r\t";

    foreach(dchar c; lower)
    {
        assert(CharRanges.Rule1(to!string(c)).successful);
        assert(CharRanges.Rule2(to!string(c)).successful);
        assert(CharRanges.Rule3(to!string(c)).successful);
        assert(CharRanges.Rule4(to!string(c)).successful);
        assert(CharRanges.Rule5(to!string(c)).successful);
    }

    foreach(dchar c; upper ~ digits ~ others)
    {
        assert(!CharRanges.Rule1(to!string(c)).successful);
        assert(!CharRanges.Rule2(to!string(c)).successful);
        assert(!CharRanges.Rule3(to!string(c)).successful);
        assert(!CharRanges.Rule4(to!string(c)).successful);
        assert(!CharRanges.Rule5(to!string(c)).successful);
    }

    foreach(dchar c; escapes)
    {
        assert(CharRanges.Rule6(to!string(c)).successful);
    }
}

unittest // qualified names for rules
{
    mixin(grammar(`
    First:
        Rule1 <- "abc"
        Rule2 <- "def"
    `));

    mixin(grammar(`
    Second:
        Rule1 <- First.Rule1
        Rule2 <- First.Rule2
        Rule3 <- pegged.peg.list(pegged.peg.identifier, ',')
    `));

    // Equal on success
    ParseTree reference = First("abc");
    ParseTree result = Second("abc");
    assert(reference.successful);
    assert(result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);

    // Equal on failure
    reference = First("def");
    result = Second("def");
    assert(!reference.successful);
    assert(!result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);

    // Second rule test
    reference = First.Rule2("def");
    result = Second.Rule2("def");
    assert(reference.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);

    // External (predefined) rule call:
    result = Second.Rule3("foo,bar,baz");
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "foo,bar,baz".length);
    assert(result.matches == ["foo", "bar", "baz"]);
}

unittest // Parameterized rules
{
    mixin(grammar(`
    Parameterized:
        # Different arities
        Rule1(A)     <- A+
        Rule1(A,B)   <- (A B)+
        Rule1(A,B,C) <- (A B C)+

        # Inner call
        Call1    <- Rule1('a')
        Call2    <- Rule1('a','b')
        Call3    <- Rule1('a','b','c')
        Call4(A) <- Rule1(A, A, A)
        Call5(A) <- Rule1('a', A, 'c')

        # Default values
        Rule2(A = 'a', B = 'b') <- (A B)+

        # Re-using the parameters
        Rule3(A,B) <- A B B A  # Money money money!

        # The standard parameterized rule
        List(Elem, Sep) < Elem (:Sep Elem)*

        # Another common PEG pattern
        AllUntil(End) <~ (!End .)* :End
    `));

    alias Parameterized.Rule1!(literal!"a") R1;
    alias oneOrMore!(literal!"a") Ref1;

    ParseTree reference = Ref1("aaaa");
    ParseTree result = R1("aaaa");

    assert(result.name == `Parameterized.Rule1!(literal!("a"))`);
    assert(reference.successful);
    assert(result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);

    result = Parameterized.Call1("aaaa");

    assert(result.name == `Parameterized.Call1`);
    assert(result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);

    alias Parameterized.Rule1!(literal!"abc") R1long;
    alias oneOrMore!(literal!"abc") Ref1long;

    reference = Ref1long("abcabcabcabc");
    result = R1long("abcabcabcabc");

    assert(result.name == `Parameterized.Rule1!(literal!("abc"))`);
    assert(reference.successful);
    assert(result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);

    alias Parameterized.Rule1!(literal!"a", literal!"b") R2;
    alias oneOrMore!(and!(literal!"a", literal!"b")) Ref2;

    reference = Ref2("abababab");
    result = R2("abababab");

    assert(result.name == `Parameterized.Rule1!(literal!("a"), literal!("b"))`);
    assert(reference.successful);
    assert(result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);

    result = Parameterized.Call2("abababab");

    assert(result.name == `Parameterized.Call2`);
    assert(result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);

    alias Parameterized.Rule1!(literal!"a", literal!"b", literal!"c") R3;
    alias oneOrMore!(and!(literal!"a", literal!"b", literal!"c")) Ref3;

    reference = Ref3("abcabcabcabc");
    result = R3("abcabcabcabc");

    assert(result.name == `Parameterized.Rule1!(literal!("a"), literal!("b"), literal!("c"))`);
    assert(reference.successful);
    assert(result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);

    result = Parameterized.Call3("abcabcabcabc");

    assert(result.name == `Parameterized.Call3`);
    assert(result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);

    result = Parameterized.Call4!(literal!"A")("AAAAAA");

    assert(result.name == `Parameterized.Call4!(literal!("A"))`);
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "AAAAAA".length);
    assert(result.matches == ["A","A","A","A","A","A"]);

    result = Parameterized.Call5!(literal!"A")("aAcaAc");

    assert(result.name == `Parameterized.Call5!(literal!("A"))`);
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "aAcaAc".length);
    assert(result.matches == ["a","A","c","a","A","c"]);

    // Default parameters
    alias Parameterized.Rule2!() R2_1;
    alias Parameterized.Rule2!(literal!"a") R2_2;
    alias Parameterized.Rule2!(literal!"a", literal!"b") R2_3;

    assert(R2_1("ababab").successful);
    assert(R2_2("ababab").successful);
    assert(R2_3("ababab").successful);

    // Re-using a parameter (A B B A)
    result = Parameterized.Rule3!(literal!"A", literal!"B")("ABBA");

    assert(result.name == `Parameterized.Rule3!(literal!("A"), literal!("B"))`);
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "ABBA".length);
    assert(result.matches == ["A", "B", "B", "A"]);

    alias Parameterized.List!(identifier, literal!",") IdList; // Identifiers separated by ','
    alias Parameterized.List!(IdList, literal!";") IdList2; // IdList's separated by ';'

    result = IdList("foo, bar, baz");

    assert(result.name == `Parameterized.List!(identifier, literal!(","))`);
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "foo, bar, baz".length);
    assert(result.matches == ["foo", "bar", "baz"]);

    result = Parameterized.decimateTree(IdList2("foo,bar,baz; abc, def,   ghi"));

    assert(result.name == `Parameterized.List!(Parameterized.List!(identifier, literal!(",")), literal!(";"))`);
    assert(result.successful);
    assert(result.begin == 0);
    assert(result.end == "foo,bar,baz; abc, def,   ghi".length);
    assert(result.matches == ["foo", "bar", "baz", "abc", "def", "ghi"]);

    assert(result.children.length == 2);

    assert(result.children[0].name == `Parameterized.List!(identifier, literal!(","))`);
    assert(result.children[0].matches == ["foo", "bar", "baz"]);

    assert(result.children[1].name == `Parameterized.List!(identifier, literal!(","))`);
    assert(result.children[1].matches == ["abc", "def", "ghi"]);

    alias Parameterized.AllUntil!(or!(endOfLine)) Line;
    alias zeroOrMore!(Line) Lines;

    string input =
"This is an input text.
Here is another line.

    And the last one.
";

    result = Lines(input);
    assert(result.successful);
    assert(result.children.length == 4);
    assert(result.children[0].matches == ["This is an input text."]);
    assert(result.children[1].matches == ["Here is another line."]);
    assert(result.children[2].matches is null);
    assert(result.children[3].matches == ["    And the last one."]);


    // Parameterized grammar test
    mixin(grammar(`
    Arithmetic(Atom) :
        Expr     <  Factor  (('+'/'-') Factor)*
        Factor   <  Primary (('*'/'/') Primary)*
        Primary  <  '(' Expr ')' / '-' Expr / Atom
    `));

    alias Arithmetic!(identifier) Arith1;
    alias Arithmetic!(or!(identifier, digits)) Arith2;

    assert(Arith1("x + y*z/foo").successful);
    assert(Arith2("x + y*z/foo").successful);

    assert(!Arith1("1 + 2*3/456").successful);
    assert(Arith2("1 + 2*3/456").successful);
    assert(Arith2("1 + 2*3/z").successful);
}

// TODO: failure cases: unnamed grammar, no-rule grammar, syntax errors, etc.


RuleIntrospection introspectCode(string s)() @property
{
    return introspect!(mixin(generateCode(Pegged.decimateTree(Pegged.Expression(s)))));
}