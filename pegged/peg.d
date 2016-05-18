/**
This module contains the engine behind Pegged, the expression templates building blocks to create a top-down
recursive-descent parser.

The terminals and non-terminals described here are meant to be used inside a Pegged grammar.
As such, they are a bit less user-friendly than what's output by pegged.grammar.
For example they take a ParseTree as input, not a string.

See the /docs directory for the full documentation as markdown files.
*/
module pegged.peg;

/*
NOTE:
Do not use the GrammarTester for unittesting in this module.  This module needs
to be able to pass its unittests before the GrammarTester is even trustable.
Writing tests the long way is preferred here, as it will avoid the circular
dependency.
*/

import std.algorithm: equal, map, startsWith;
import std.uni : isAlpha, icmp;
import std.array;
import std.conv;
import std.string: strip;
import std.typetuple;

package string stringified(string inp)
{
    import std.format : format;
    return format(`%(%s%)`, (&inp)[0..1]);
}

version (tracer)
{
    import std.experimental.logger;
    import std.algorithm.comparison : min;

    // Function pointers.
    private static bool function(string ruleName) traceConditionFunction;
    private static bool delegate(string ruleName) traceConditionDelegate;
    private static int traceLevel;
    private static bool traceBlocked;
    private static bool logTraceLevel = false;

    private void incTraceLevel()
    {
        if (!__ctfe)
            traceLevel++;
    }

    private void decTraceLevel()
    {
        if (!__ctfe)
            traceLevel--;
    }

    private bool shouldTrace(string ruleName)
    {
        if (__ctfe || traceBlocked)
            return false;
        if (traceConditionDelegate != null)
            return traceConditionDelegate(ruleName);
        if (traceConditionFunction != null)
            return traceConditionFunction(ruleName);
        return false;
    }

    static this()
    {
        traceLevel = 0;
        traceNothing();
        traceBlocked = false;
    }

    /++ Supply a function to dynamically switch tracing on and off based on the rule name.
     +
     + Example:
     + ---
     + /* Exclude build-in parsers, only trace parsers generated from MyGrammar. */
     + setTraceConditionFunction(ruleName => ruleName.startsWith("MyGrammar"));
     + ---
     +/
    void setTraceConditionFunction(bool delegate(string ruleName) condition)
    {
        traceConditionDelegate = condition;
        traceConditionFunction = null;
    }

    /// ditto
    void setTraceConditionFunction(bool function(string ruleName) condition)
    {
        traceConditionFunction = condition;
        traceConditionDelegate = null;
    }

    /** Trace all rules.
     *
     * This can produce a lot of output.
     */
    void traceAll()
    {
        setTraceConditionFunction(string => true);
    }

    /** Do not trace any rules. */
    void traceNothing()
    {
        traceConditionFunction = null;
        traceConditionDelegate = null;
    }

    private string traceMsg(ParseTree p, string expression, string name)
    {
        import std.format;
        Position pos = position(p);
        enum inputLength = 15;
        string result;
        for (auto i = 1; i <= traceLevel; i++)
            result ~= format("%d|", i);
        result ~= format(" (l:%d, c:%d)\t", pos.line, pos.col) ~
            expression.stringified ~ " considering rule " ~ name.stringified ~ " on " ~
            p.input[p.end .. min(p.input.length, p.end + inputLength)].stringified ~
            (p.end + inputLength > p.input.length ? "" : "...");
        return result;
    }

    private string traceResultMsg(ParseTree p, string name)
    {
        import std.format;
        import std.range: chain;
        import std.algorithm.iteration: joiner;
        Position pos = position(p);
        enum inputLength = 15;
        string result;
        for (auto i = 1; i <= traceLevel; i++)
            result ~= format("%d|", i);
        if (p.successful)
        {
            string consumed;
            foreach (match; p.matches)
                consumed ~= match;
            result ~= format(" (l:%d, c:%d)\t", pos.line, pos.col) ~ name.stringified ~ " SUCCEEDED on " ~
                consumed.stringified;
        }
        else
            result ~= format(" (l:%d, c:%d)\t", pos.line, pos.col) ~ name.stringified ~ " FAILED on " ~
                p.input[p.end .. min(p.input.length, p.end + inputLength)].stringified ~
                (p.end + inputLength > p.input.length ? "" : "...");
        return result;
    }

    /**
    Overrides FileLogger to remove the time stamp.

    Example:
    ---
    sharedLog = new TraceLogger("TraceLog.txt");
    ---
    */
    class TraceLogger : FileLogger
    {
        this(in string fn) @safe
        {
            super(fn);
        }
        override protected void beginLogMsg(string file, int line, string funcName,
            string prettyFuncName, string moduleName, LogLevel logLevel,
            Tid threadId, SysTime timestamp, Logger logger)
            @safe
        {
        }
    }
}

/**
CT Switch for testing 'keywords' implementations
*/
enum
{
    IFCHAIN,
    TRIE
}
enum KEYWORDS = IFCHAIN;

/**
The basic parse tree, as used throughout the project.
You can define your own parse tree node, but respect the basic layout.
*/
struct ParseTree
{
    string name; /// The node name
    bool successful; /// Indicates whether a parsing was successful or not
    string[] matches; /// The matched input's parts. Some expressions match at more than one place, hence matches is an array.

    string input; /// The input string that generated the parse tree. Stored here for the parse tree to be passed to other expressions, as input.
    size_t begin, end; /// Indices for the matched part (from the very beginning of the first match to the last char of the last match.

    ParseTree[] children; /// The sub-trees created by sub-rules parsing.

    /**
    Basic toString for easy pretty-printing.
    */
    string toString(string tabs = "") const
    {
        string result = name;

        string childrenString;
        bool allChildrenSuccessful = true;

        foreach(i,child; children)
        {
            childrenString ~= tabs ~ " +-" ~ child.toString(tabs ~ ((i < children.length -1 ) ? " | " : "   "));
            if (!child.successful)
                allChildrenSuccessful = false;
        }

        if (successful)
        {
            result ~= " " ~ to!string([begin, end]) ~ to!string(matches) ~ "\n";
        }
        else // some failure info is needed
        {
            if (allChildrenSuccessful) // no one calculated the position yet
            {
                Position pos = position(this);
                string left, right;

                if (pos.index < 10)
                    left = input[0 .. pos.index];
                else
                    left = input[pos.index-10 .. pos.index];
                //left = strip(left);

                if (pos.index + 10 < input.length)
                    right = input[pos.index .. pos.index + 10];
                else
                    right = input[pos.index .. $];
                //right = strip(right);

                result ~= " failure at line " ~ to!string(pos.line) ~ ", col " ~ to!string(pos.col) ~ ", "
                       ~ (left.length > 0 ? "after " ~ left.stringified ~ " " : "")
                       ~ "expected "~ (matches.length > 0 ? matches[$-1].stringified : "NO MATCH")
                       ~ ", but got " ~ right.stringified ~ "\n";
            }
            else
            {
                result ~= " (failure)\n";
            }
        }

        return result ~ childrenString;
    }

    @property string failMsg()
    {
        foreach(i, child; children)
        {
            if (!child.successful)
                return child.failMsg;
        }

        if (!successful)
        {
            Position pos = position(this);
            string left, right;

            if (pos.index < 10)
                left = input[0 .. pos.index];
            else
                left = input[pos.index - 10 .. pos.index];

            if (pos.index + 10 < input.length)
                right = input[pos.index .. pos.index + 10];
            else
                right = input[pos.index .. $];

            return "Failure at line " ~ to!string(pos.line) ~ ", col " ~ to!string(pos.col) ~ ", "
                ~ (left.length > 0 ? "after " ~ left.stringified ~ " " : "")
                ~ "expected " ~ (matches.length > 0 ? matches[$ - 1].stringified : "NO MATCH")
                ~ `, but got ` ~ right.stringified;
        }

        return "Success";
    }

    ParseTree dup() @property
    {
        ParseTree result = this;
        result.matches = result.matches.dup;
        result.children = map!(p => p.dup)(result.children).array();
        return result;
    }
}


unittest // ParseTree testing
{
    ParseTree p;
    assert(p == p, "Self-identity on null tree.");

    p = ParseTree("Name", true, ["abc", "", "def"], "input", 0, 1, null);
    assert(p == p, "Self identity on non-null tree.");

    ParseTree child = ParseTree("Child", true, ["abc", "", "def"], "input", 0, 1, null);
    p.children = [child, child];

    ParseTree q = p;
    assert(p == q, "Copying creates equal trees.");

    assert(p.children == q.children);
    p.children = [child, child, child];
    assert(q.children != p.children, "Disconnecting children.");

    p = ParseTree("Name", true, ["abc", "", "def"], "input", 0, 1, null);
    p.children = [child, child];

    q = p.dup;
    assert(p == q, "Dupping creates equal trees.");

    assert(p.children == q.children, "Equal children for dupped trees.");
    p.children = null;
    assert(q.children != p.children);

    q.children = [p,p];
    assert(p != q, "Tree with different children are not equal.");

    p.children = [p,p];
    assert(p == q, "Adding equivalent children is OK.");

    p.matches = null;
    assert(p != q, "Nulling matches makes trees unequal.");

    p.matches = q.matches;
    assert(p == q, "Copying matches makes equal trees.");
}

/// To compare two trees for content (not bothering with node names)
/// That's useful to compare the results from two different grammars.
bool softCompare(ParseTree p1, ParseTree p2)
{
    return p1.successful == p2.successful
        && p1.matches == p2.matches
        && p1.begin == p2.begin
        && p1.end == p2.end
        && equal!(softCompare)(p1.children, p2.children); // the same for children
}

unittest // softCompare
{
    ParseTree p = ParseTree("Name", true, ["abc", "", "def"], "input", 0, 1, null);
    ParseTree child = ParseTree("Child", true, ["abc", "", "def"], "input", 0, 1, null);
    p.children = [child, child];

    ParseTree q = p;
    assert(p == q, "Copy => Equal trees.");
    assert(softCompare(p,q), "Copy => Trees equal for softCompare.");

    q.name = "Another One";
    assert(p != q, "Name change => non-equal trees.");
    assert(softCompare(p,q), "Name change => Trees equal for softCompare.");
}

/// To record a position in a text
struct Position
{
    size_t line;/// line number (starts at 0)
    size_t col;/// column number (starts at 0)
    size_t index;/// index (starts at 0)
}

/**
Given an input string, returns the position corresponding to the end of the string.

For example:
---
assert(position("abc") == Position(0,3,3));
assert(position("abc
") == Position(1,0,4));
assert(position("abc

    ") == Position(2,4,8));
---
*/
Position position(string s)
{
    size_t col, line, index;
    version (tracer)
    {
        // Do not trace the eol scan below, prevent recursion.
        traceBlocked = true;
    }
    foreach(i,c; s)
    {
        if (eol(ParseTree("", false, [], s, 0,i)).successful)
        {
            col = 0;
            ++line;
            ++index;
        }
        else
        {
            ++col;
            ++index;
        }
    }
    version (tracer)
    {
        traceBlocked = false;
    }

    return Position(line,col,index);
}

/**
Same as previous overload, but from the begin of P.input to p.end
*/
Position position(const ParseTree p)
{
    return position(p.input[0..p.end]);
}

unittest
{
    assert(position("") == Position(0,0,0), "Null string, position 0.");
    assert(position("abc") == Position(0,3,3), "length 3 string, no line feed.");
    assert(position("abc
") == Position(1,0,4), "One end of line.");
    assert(position("abc

----") == Position(2,4,9), "Three lines (second one empty).");
    assert(position("abc
----
----") == Position(2,4,13), "Three lines.");
    assert(position("


") == Position(3,0,3), "Four lines, all empty.");
}

string getName(alias expr)() @property
{
    static if (is(typeof( { expr(GetName()); })))
        return expr(GetName());
    else
        return __traits(identifier, expr);
}

struct GetName {}

/**
Basic rule, that always fail without consuming.
*/
ParseTree fail(ParseTree p)
{
    return ParseTree("fail", false, [], p.input, p.end, p.end, null);
}

/// ditto
ParseTree fail(string input)
{
    return fail(ParseTree("", false, [], input));
}

string fail(GetName g)
{
    return "fail";
}

unittest // 'fail' unit test
{
    ParseTree input = ParseTree("input", true, [], "This is the input string.", 0,0, null);
    ParseTree result = fail(input);
    assert(result.name == "fail");
    assert(!result.successful, "'fail' fails.");
    assert(result.matches is null, "'fail' makes no match.");
    assert(result.input == input.input, "'fail' does not change the input.");
    assert(result.end == input.end, "'fail' puts the index after the previous parse.");
    assert(result.children is null, "'fail' has no children.");

    result = fail("This is the input string.");
    assert(!result.successful, "'fail' fails.");
}

/**
Matches the end of input. Fails if there is any character left.
*/
ParseTree eoi(ParseTree p)
{
    if (p.end == p.input.length)
        return ParseTree("eoi", true, [], p.input, p.end, p.end);
    else
        return ParseTree("eoi", false, ["end of input"], p.input, p.end, p.end);
}

/// ditto
ParseTree eoi(string input)
{
    return eoi(ParseTree("", false, [], input));
}

string eoi(GetName g)
{
    return "eoi";
}

alias eoi endOfInput; /// helper alias.

unittest // 'eoi' unit test
{
    ParseTree input = ParseTree("input", true, [], "This is the input string.", 0,0, null);
    ParseTree result = eoi(input);
    assert(result.name == "eoi");
    assert(!result.successful, "'eoi' fails on non-null string.");
    assert(result.matches == ["end of input"], "'eoi' error message.");
    assert(result.input == input.input, "'eoi' does not change the input.");
    assert(result.end == input.end, "'eoi' puts the index after the previous parse.");
    assert(result.children is null, "'eoi' has no children.");

    input = ParseTree("input", true, [], "", 0,0, null);
    result = eoi(input);
    assert(result.successful, "'eoi' succeeds on strings of length 0.");
    result = eoi("");
    assert(result.successful, "'eoi' succeeds on strings of length 0.");
    result = eoi(null);
    assert(result.successful, "'eoi' succeeds on null strings");
}

/**
Match any character. As long as there is at least a character left in the input, it succeeds.
Conversely, it fails only if called at the end of the input.
*/
ParseTree any(ParseTree p)
{
    if (p.end < p.input.length)
        return ParseTree("any", true, [p.input[p.end..p.end+1]], p.input, p.end, p.end+1);
    else
        return ParseTree("any", false, ["any char"], p.input, p.end, p.end);
}

/// ditto
ParseTree any(string input)
{
    return any(ParseTree("", false, [], input));
}

string any(GetName g)
{
    return "any";
}

unittest // 'any' unit test
{
    ParseTree input = ParseTree("input", true, [], "This is the input string.", 0,0, null);
    ParseTree result = any(input);

    assert(result.name == "any");
    assert(result.successful, "'any' succeeds on non-null strings.");
    assert(result.matches  == ["T"], "'any' matches the first char in an input.");
    assert(result.input == input.input, "'any' does not change the input.");
    assert(result.end == input.end+1, "'any' advances the index by one position.");
    assert(result.children is null, "'any' has no children.");

    result = any("a");
    assert(result.successful, "'any' matches on strings of length one.");
    assert(result.matches == ["a"], "'any' matches the first char in an input.");
    assert(result.input == "a", "'any' does not change the input.");
    assert(result.end == 1, "'any' advances the index by one position.");
    assert(result.children is null, "'any' has no children.");

    input = ParseTree("input", true, [], "", 0,0, null);

    result = any(input);
    assert(!result.successful, "'any' fails on strings of length 0.");
    assert(result.matches == ["any char"], "'any' error message on strings of length 0.");
    assert(result.end == 0, "'any' does not advance the index.");

    result = any("");
    assert(!result.successful, "'any' fails on strings of length 0.");
    assert(result.matches == ["any char"], "'any' error message on strings of length 0.");
    assert(result.end == 0, "'any' does not advance the index.");

    result = any(null);
    assert(!result.successful, "'any' fails on null strings.");
    assert(result.matches == ["any char"], "'any' error message on strings of length 0.");
    assert(result.end == 0, "'any' does not advance the index.");
}

/**
Predefined parser: matches word boundaries, as \b for regexes.
*/
ParseTree wordBoundary(ParseTree p)
{
	// TODO: I added more indexing guards and now this could probably use
	//         some simplification.  Too tired to write it better. --Chad
    bool matched =  (p.end == 0 && isAlpha(p.input.front()))
                 || (p.end == p.input.length && isAlpha(p.input.back()))
                 || (p.end > 0 && isAlpha(p.input[p.end-1])  && p.end < p.input.length && !isAlpha(p.input[p.end]))
                 || (p.end > 0 && !isAlpha(p.input[p.end-1]) && p.end < p.input.length &&  isAlpha(p.input[p.end]));
    if (matched)
        return ParseTree("wordBoundary", matched, [], p.input, p.end, p.end, null);
    else
        return ParseTree("wordBoundary", matched, ["word boundary"], p.input, p.end, p.end, null);
}

/// ditto
ParseTree wordBoundary(string input)
{
    return ParseTree("wordBoundary", isAlpha(input.front()), [], input, 0,0, null);
}

string wordBoundary(GetName g)
{
    return "wordBoundary";
}

unittest // word boundary
{
    ParseTree input = ParseTree("", false, [], "This is a word.");
    auto wb = [// "This"
               cast(size_t)(0):true, 1:false, 2:false, 3:false, 4: true,
               // "is"
               5: true, 6:false, 7: true,
               // "a"
               8: true, 9:true,
               // "word"
               10:true, 11:false, 12:false, 13:false, 14:true,
               // "."
               15:false
              ];

    foreach(size_t index; 0 .. input.input.length)
    {
        input.end = index;
        ParseTree result = wordBoundary(input);

        assert(result.name == "wordBoundary");
        assert(result.successful == wb[index]); // true, false, ...
        // for errors, there is an error message
        assert(result.successful && result.matches is null || !result.successful);
        assert(result.begin == input.end);
        assert(result.end == input.end);
        assert(result.children is null);
    }
}

/**
Represents a literal in a PEG, like "abc" or 'abc' (or even '').
It succeeds if a prefix of the input is equal to its template parameter and fails otherwise.
*/
template literal(string s)
{
    enum name = "literal!(\""~s~"\")";

    ParseTree literal(ParseTree p)
    {
        enum lit = "\"" ~ s ~ "\"";
        if (p.end+s.length <= p.input.length && p.input[p.end..p.end+s.length] == s)
            return ParseTree(name, true, [s], p.input, p.end, p.end+s.length);
        else
            return ParseTree(name, false, [lit], p.input, p.end, p.end);
    }

    ParseTree literal(string input)
    {
        return .literal!(s)(ParseTree("", false, [], input));
    }


    string literal(GetName g)
    {
        return name;
    }

}

unittest // 'literal' unit test
{
    ParseTree input = ParseTree("input", true, [], "abcdef", 0,0, null);

    alias literal!"a" a;
    alias literal!"abc" abc;
    alias literal!"" empty;

    ParseTree result = a(input);

    assert(result.name == `literal!("a")`, "Literal name test.");
    assert(result.successful, "'a' succeeds on inputs beginning with 'a'.");
    assert(result.matches  == ["a"], "'a' matches the 'a' at the beginning.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end+1, "'a' advances the index by one position.");
    assert(result.children is null, "'a' has no children.");

    result = a("abcdef");

    assert(result.successful, "'a' succeeds on inputs beginning with 'a'.");
    assert(result.matches  == ["a"], "'a' matches the 'a' at the beginning.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end+1, "'a' advances the index by one position.");
    assert(result.children is null, "'a' has no children.");

    result = abc(input);

    assert(result.name == `literal!("abc")`, "Literal name test.");
    assert(result.successful, "'abc' succeeds on inputs beginning with 'abc'.");
    assert(result.matches  == ["abc"], "'abc' matches 'abc' at the beginning.");
    assert(result.input == input.input, "'abc' does not change the input.");
    assert(result.end == input.end+3, "'abc' advances the index by 3 positions.");
    assert(result.children is null, "'abc' has no children.");

    result = abc("abcdef");

    assert(result.successful, "'abc' succeeds on inputs beginning with 'abc'.");
    assert(result.matches  == ["abc"], "'abc' matches 'abc' at the beginning.");
    assert(result.input == input.input, "'abc' does not change the input.");
    assert(result.end == input.end+3, "'abc' advances the index by 3 positions.");
    assert(result.children is null, "'abc' has no children.");

    result = empty(input);

    assert(result.name == `literal!("")`, "Literal name test.");
    assert(result.successful, "'' succeeds on non-null inputs.");
    assert(result.matches  == [""], "'' matches '' at the beginning.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");

    result = empty("abcdef");

    assert(result.successful, "'' succeeds on non-null inputs.");
    assert(result.matches  == [""], "'' matches '' at the beginning.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");

    input.input = "bcdef";

    result = a(input);

    assert(!result.successful, "'a' fails on inputs not beginning with 'a'.");
    assert(result.matches == ["\"a\""], "'a' makes no match on 'bcdef'.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end, "'a' does not advances the index on 'bcdef'.");
    assert(result.children is null, "'a' has no children.");

    result = abc(input);

    assert(!result.successful, "'abc' fails on inputs not beginning with 'abc'.");
    assert(result.matches == ["\"abc\""], "'abc' does no match on 'bcdef'.");
    assert(result.input == input.input, "'abc' does not change the input.");
    assert(result.end == input.end, "'abc' does not advance the index on 'bcdef'.");
    assert(result.children is null, "'abc' has no children.");

    result = empty(input);

    assert(result.successful, "'' succeeds on non-null inputs.");
    assert(result.matches == [""], "'' matches '' at the beginning.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");

    input.input = "";

    result = a(input);

    assert(!result.successful, "'a' fails on empty strings.");
    assert(result.matches == ["\"a\""], "'a' does not match ''.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end, "'a' does not advance the index on 'bcdef'.");
    assert(result.children is null, "'a' has no children.");

    result = abc(input);

    assert(!result.successful, "'abc' fails on empty strings.");
    assert(result.matches == ["\"abc\""], "'abc' does not match ''.");
    assert(result.input == input.input, "'abc' does not change the input.");
    assert(result.end == input.end, "'abc' does not advance the index on 'bcdef'.");
    assert(result.children is null, "'abc' has no children.");

    result = empty(input);

    assert(result.successful, "'' succeeds on empty strings.");
    assert(result.matches  == [""], "'' matches '' at the beginning, even on empty strings.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");
}

/**
Represents a case insensitive literal in a PEG, like "abc"i or 'abc'i (or even ''i).
It succeeds if a case insensitive comparison of a prefix of the input and its template
parameter yields no difference and fails otherwise.
*/
template caseInsensitiveLiteral(string s)
{
    enum name = "caseInsensitiveLiteral!(\""~s~"\")";

    ParseTree caseInsensitiveLiteral(ParseTree p)
    {
        enum lit = "\"" ~ s ~ "\"";
        if (p.end+s.length <= p.input.length && icmp(p.input[p.end..p.end+s.length], s) == 0)
            return ParseTree(name, true, [s], p.input, p.end, p.end+s.length);
        else
            return ParseTree(name, false, [lit], p.input, p.end, p.end);
    }

    ParseTree caseInsensitiveLiteral(string input)
    {
        return .caseInsensitiveLiteral!(s)(ParseTree("", false, [], input));
    }


    string caseInsensitiveLiteral(GetName g)
    {
        return name;
    }

}

unittest // 'caseInsensitiveLiteral' unit test
{
    ParseTree input = ParseTree("input", true, [], "AbCdEf", 0,0, null);

    alias caseInsensitiveLiteral!"a" a;
    alias caseInsensitiveLiteral!"aBC" abc;
    alias caseInsensitiveLiteral!"" empty;

    ParseTree result = a(input);

    assert(result.name == `caseInsensitiveLiteral!("a")`, "caseInsensitiveLiteral name test.");
    assert(result.successful, "'a' succeeds on inputs beginning with 'A'.");
    assert(result.matches == ["a"], "'a' matches the 'A' at the beginning.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end+1, "'a' advances the index by one position.");
    assert(result.children is null, "'a' has no children.");

    result = a("AbCdEf");

    assert(result.successful, "'a' succeeds on inputs beginning with 'A'.");
    assert(result.matches == ["a"], "'a' matches the 'A' at the beginning.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end+1, "'a' advances the index by one position.");
    assert(result.children is null, "'a' has no children.");

    result = abc(input);

    assert(result.name == `caseInsensitiveLiteral!("aBC")`, "caseInsensitiveLiteral name test.");
    assert(result.successful, "'aBC' succeeds on inputs beginning with 'AbC'.");
    assert(result.matches == ["aBC"], "'AbC' matches 'aBC' at the beginning.");
    assert(result.input == input.input, "'aBC' does not change the input.");
    assert(result.end == input.end+3, "'aBC' advances the index by 3 positions.");
    assert(result.children is null, "'aBC' has no children.");

    result = abc("AbCdEf");

    assert(result.successful, "'aBC' succeeds on inputs beginning with 'AbC'.");
    assert(result.matches == ["aBC"], "'AbC' matches 'aBC' at the beginning.");
    assert(result.input == input.input, "'aBC' does not change the input.");
    assert(result.end == input.end+3, "'aBC' advances the index by 3 positions.");
    assert(result.children is null, "'aBC' has no children.");

    result = empty(input);

    assert(result.name == `caseInsensitiveLiteral!("")`, "caseInsensitiveLiteral name test.");
    assert(result.successful, "'' succeeds on non-null inputs.");
    assert(result.matches  == [""], "'' matches '' at the beginning.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");

    result = empty("AbCdEf");

    assert(result.successful, "'' succeeds on non-null inputs.");
    assert(result.matches  == [""], "'' matches '' at the beginning.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");

    input.input = "bCdEf";

    result = a(input);

    assert(!result.successful, "'a' fails on inputs not beginning with 'a' or 'A'.");
    assert(result.matches == ["\"a\""], "'a' makes no match on 'bCdEf'.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end, "'a' does not advances the index on 'bCdEf'.");
    assert(result.children is null, "'a' has no children.");

    result = abc(input);

    assert(!result.successful, "'aBC' fails on inputs beginning with 'bCdEf'.");
    assert(result.matches == ["\"aBC\""], "'aBC' does no match on 'bCdEf'.");
    assert(result.input == input.input, "'aBC' does not change the input.");
    assert(result.end == input.end, "'aBC' does not advance the index on 'bCdEf'.");
    assert(result.children is null, "'aBC' has no children.");

    result = empty(input);

    assert(result.successful, "'' succeeds on non-null inputs.");
    assert(result.matches == [""], "'' matches '' at the beginning.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");

    input.input = "";

    result = a(input);

    assert(!result.successful, "'a' fails on empty strings.");
    assert(result.matches == ["\"a\""], "'a' does not match ''.");
    assert(result.input == input.input, "'a' does not change the input.");
    assert(result.end == input.end, "'a' does not advance the index on 'bCdEf'.");
    assert(result.children is null, "'a' has no children.");

    result = abc(input);

    assert(!result.successful, "'aBC' fails on empty strings.");
    assert(result.matches == ["\"aBC\""], "'aBC' does not match ''.");
    assert(result.input == input.input, "'aBC' does not change the input.");
    assert(result.end == input.end, "'aBC' does not advance the index on 'bCdEf'.");
    assert(result.children is null, "'aBC' has no children.");

    result = empty(input);

    assert(result.successful, "'' succeeds on empty strings.");
    assert(result.matches  == [""], "'' matches '' at the beginning, even on empty strings.");
    assert(result.input == input.input, "'' does not change the input.");
    assert(result.end == input.end+0, "'' does not advance the index.");
    assert(result.children is null, "'' has no children.");

    input.input = "ÄöÜæØå";
    result = caseInsensitiveLiteral!"äÖüÆøÅ"(input);

    assert(result.successful, "Unicode characters are matched case insensitively.");
}

/**
Represents a range of chars, from begin to end, included. So charRange!('a','z') matches
all English lowercase letters. If fails if the input is empty or does not begin with a character
between begin and end.

If begin == end, it will match one char (begin... or end).

begin > end is non-legal.
*/
template charRange(dchar begin, dchar end) if (begin <= end)
{
    enum name = "charRange!('"~to!string(begin)~"','" ~ to!string(end) ~ "')";

    ParseTree charRange(ParseTree p)
    {
        enum longname = "a char between '"~to!string(begin)~"' and '"~to!string(end)~"'";
        if (p.end < p.input.length && p.input[p.end] >= begin && p.input[p.end] <= end)
           return ParseTree(name, true, [p.input[p.end..p.end+1]], p.input, p.end, p.end+1);
        else
            return ParseTree(name, false, [longname], p.input, p.end, p.end);
    }

    ParseTree charRange(string input)
    {
        return .charRange!(begin,end)(ParseTree("",false,[],input));
    }

    string charRange(GetName g)
    {
        return name;
    }
}

unittest // 'charRange' unit test
{
    ParseTree input = ParseTree("input", true, [], "abcdef", 0,0, null);

    alias charRange!('a','a') aa;
    alias charRange!('a','b') ab;
    alias charRange!('a','z') az;
    alias charRange!(dchar.min,dchar.max) allChars;
    alias charRange!('\U00000000','\U000000FF') ASCII;

    static assert(!__traits(compiles, {alias charRange!('z','a') za;}));

    ParseTree result = aa(input);

    assert(result.name == "charRange!('a','a')", "charRange name test.");
    assert(result.successful, "'a-a' succeeds on inputs beginning with 'a'.");
    assert(result.matches  == ["a"], "'a-a' matches the 'a' at the beginning.");
    assert(result.input == input.input, "'a-a' does not change the input.");
    assert(result.end == input.end+1, "'a-a' advances the index by one position.");
    assert(result.children is null, "'a-a' has no children.");

    result = ab("abcdef");

    assert(result.name == "charRange!('a','b')", "charRange name test.");
    assert(result.successful, "'a-b' succeeds on inputs beginning with 'a'.");
    assert(result.matches  == ["a"], "'a-b' matches the 'a' at the beginning.");
    assert(result.input == input.input, "'a-b' does not change the input.");
    assert(result.end == input.end+1, "'a-b' advances the index by one position.");
    assert(result.children is null, "'a-b' has no children.");

    result = az(input);

    assert(result.name == "charRange!('a','z')", "charRange name test.");
    assert(result.successful, "'a-z' succeeds on inputs beginning with 'abc'.");
    assert(result.matches  == ["a"], "'a-z' matches 'a' at the beginning.");
    assert(result.input == input.input, "'a-z' does not change the input.");
    assert(result.end == input.end+1, "'a-z' advances the index by one position.");
    assert(result.children is null, "'a-z' has no children.");

    input.input = "bcdef";

    result = aa(input);

    assert(!result.successful, "'a-a' fails on inputs not beginning with 'a'.");
    assert(result.matches == ["a char between 'a' and 'a'"], "'a-a' makes no match on 'bcdef'.");
    assert(result.input == input.input, "'a-a' does not change the input.");
    assert(result.end == input.end, "'a-a' does not advances the index on 'bcdef'.");
    assert(result.children is null, "'a-a' has no children.");

    result = ab(input);

    assert(result.successful, "'a-b' succeeds on inputs beginning with 'b'.");
    assert(result.matches == ["b"], "'a-b' matches on 'bcdef'.");
    assert(result.input == input.input, "'a-b' does not change the input.");
    assert(result.end == input.end+1, "'a-b' advances the index by one position'.");
    assert(result.children is null, "'a-b' has no children.");

    result = az(input);

    assert(result.successful, "'a-z' succeeds on 'bcdef'.");
    assert(result.matches == ["b"], "'a-z' matches 'b' at the beginning of 'bcdef'.");
    assert(result.input == input.input, "'a-z' does not change the input.");
    assert(result.end == input.end+1, "'a-z' advances the index by one position.");
    assert(result.children is null, "'a-z' has no children.");

    input.input = "";

    result = aa(input);

    assert(!result.successful, "'a-a' fails on empty strings.");
    assert(result.matches == ["a char between 'a' and 'a'"], "'a-a' does not match ''.");
    assert(result.input == input.input, "'a-a' does not change the input.");
    assert(result.end == input.end, "'a-a' does not advance the index on ''.");
    assert(result.children is null, "'a-a' has no children.");

    result = ab(input);

    assert(!result.successful, "'a-b' fails on empty strings.");
    assert(result.matches == ["a char between 'a' and 'b'"], "'a-b' does not match ''.");
    assert(result.input == input.input, "'a-b' does not change the input.");
    assert(result.end == input.end, "'a-b' does not advance the index on ''.");
    assert(result.children is null, "'a-b' has no children.");

    result = az(input);

    assert(!result.successful, "'a-z' fails on empty strings.");
    assert(result.matches == ["a char between 'a' and 'z'"], "'a-z' does not match ''.");
    assert(result.input == input.input, "'a-z' does not change the input.");
    assert(result.end == input.end, "'a-z' does not advance the index on ''.");
    assert(result.children is null, "'a-z' has no children.");

    input.input = "123";

    result = aa(input);

    assert(!result.successful, "'a-a' fails on '123'.");
    assert(result.matches == ["a char between 'a' and 'a'"], "'a-a' does not match '123'.");
    assert(result.input == input.input, "'a-a' does not change the input.");
    assert(result.end == input.end, "'a-a' does not advance the index on '123'.");
    assert(result.children is null, "'a-a' has no children.");

    result = ab(input);

    assert(!result.successful, "'a-b' fails on '123'.");
    assert(result.matches == ["a char between 'a' and 'b'"], "'a-b' does not match '123'.");
    assert(result.input == input.input, "'a-b' does not change the input.");
    assert(result.end == input.end, "'a-b' does not advance the index on '123'.");
    assert(result.children is null, "'a-b' has no children.");

    result = az(input);

    assert(!result.successful, "'a-z' fails on '123'.");
    assert(result.matches == ["a char between 'a' and 'z'"], "'a-z' does not match '123'.");
    assert(result.input == input.input, "'a-z' does not change the input.");
    assert(result.end == input.end, "'a-z' does not advance the index on '123'.");
    assert(result.children is null, "'a-z' has no children.");


    foreach(dchar ch; 0..256*(128+64+16+8))
        assert(allChars(to!string(ch)).successful);

    assert(!allChars("").successful);
}

/**
eps matches the empty string (usually denoted by the Greek letter 'epsilon') and always succeeds.
It's equivalent to literal!"" (for example, it creates a match of [""]: one match, the empty string).
*/
ParseTree eps(ParseTree p)
{
    return ParseTree("eps", true, [""], p.input, p.end, p.end);
}

ParseTree eps(string input)
{
    return eps(ParseTree("",false,[], input));
}

string eps(GetName g)
{
    return "eps";
}

unittest // 'eps' unit test
{
    ParseTree input = ParseTree("input", true, [], "abcdef", 0,0, null);

    ParseTree result = eps(input);

    assert(result.name == "eps");
    assert(result.successful, "'eps' succeeds on non-null inputs.");
    assert(result.matches  == [""], "'eps' matches '' at the beginning.");
    assert(result.input == input.input, "'eps' does not change the input.");
    assert(result.end == input.end+0, "'eps' does not advance the index.");
    assert(result.children is null, "'eps' has no children.");

    input.input = "";

    result = eps(input);
    assert(result.name == "eps");
    assert(result.successful, "'eps' succeeds on empty strings.");
    assert(result.matches  == [""], "'eps' matches '' at the beginning, even on empty strings.");
    assert(result.input == input.input, "'eps' does not change the input.");
    assert(result.end == input.end+0, "'eps' does not advance the index.");
    assert(result.children is null, "'eps' has no children.");
}


/**
Basic operator: it matches if all its subrules (stored in the rules template parameter tuple) match
the input successively. Its subrules parse trees are stored as its children and its matches field
will contain all its subrules matches, in order.

----
alias and!(literal!"abc", charRange!('a','z')) rule; // abc followed by any letter between a and z.
ParseTree input = ParseTree("",false,[],"abcd"); // low-level plumbing,
                                                 // the rules described here act on ParseTree's not strings.
                                                 // It's equivalent to "abcd" as input
auto result = rule(input);

assert(result.successful); // OK, 'abc' followed by 'd'
assert(result.matches == ["abc", "d"]); // stores the matches
assert(result.children.length == 2); // two children, the result of "abc" on "abcd" and the result of [a-z] on "d"

input.input = "abc"; // changing the input string;
assert(!rule(input)).successful); // NOK, abc alone
input.input = "ab";
assert(!rule(input)).successful); // NOK, does not begin by abc
----

If it fails, the last children will contain the failed node. That way, when printing, as sort of diagnostic is given:

----
alias and!(literal!"abc", charRange!('a','z')) rule; // 'abc[a-z]', aka 'abc' followed by any letter between 'a' and 'z'.
ParseTree input = ParseTree("",false,[],"abc1"); // equivalent to "abc1"

auto failure = rule(input);
writeln(failure);
/+
writes:
and (failure)
 +-literal(abc) [0, 3]["abc"]
 +-charRange(a,z) failure at line 0, col 3, after "abc" a char between 'a' and 'z', but got "1"
+/
----

So we know the global 'and' failed, that the first sub-rule ('abc') succeeded on input[0..3] with "abc"
and that the second subrule ('[a-z]') failed at position 3 (so, on '1').
*/
template and(rules...) if (rules.length > 0)
{

    string ctfeGetNameAnd()
    {
        string name = "and!(";
        foreach(i,rule; rules)
            name ~= __traits(identifier, rule) // because using getName!(rule) causes an infinite loop during compilation
                                                // for recursive rules
                    ~ (i < rules.length -1 ? ", " : "");
        name ~= ")";
        return name;
    }

    enum name = ctfeGetNameAnd();

    ParseTree and(ParseTree p)
    {
        bool keepNode(ParseTree node)
        {
            return    node.name.startsWith("keep!(")
                || (  !node.name.startsWith("discard!(")
                   //&& !node.name.startsWith("drop!(")
                   && node.matches !is null
                   //&& node.begin != node.end
                   );
        }

        version (tracer)
        {
            incTraceLevel();
        }

        ParseTree result = ParseTree(name, false, [], p.input, p.end, p.end, []);

        foreach(i,r; rules)
        {
            version (tracer)
            {
                if (shouldTrace(getName!(r)()))
                    trace(traceMsg(result, name, getName!(r)()));
            }
            ParseTree temp = r(result);
            result.end = temp.end;
            if (temp.successful)
            {
                if (keepNode(temp))
                {
                    result.matches ~= temp.matches;
                    if (temp.name.startsWith("drop!("))
                    {}
                    else if (temp.name.startsWith("propagate!("))
                        result.children ~= temp.children;
                    else
                        result.children ~= temp;
                }
            }
            else
            {
                result.children ~= temp;// add the failed node, to indicate which failed
                if (temp.matches.length > 0)
                    result.matches ~= temp.matches[$-1];
                version (tracer)
                {
                    if (shouldTrace(getName!(r)()))
                        trace(traceResultMsg(result, getName!(r)()));
                    decTraceLevel();
                }
                return result; // and end the parsing attempt right there
            }
        }
        result.successful = true;
        version (tracer)
        {
            foreach(i, r; rules)
                if (shouldTrace(getName!(r)()))
                {
                    trace(traceResultMsg(result, name));
                    break;
                }
            decTraceLevel();
        }
        return result;
    }

    ParseTree and(string input)
    {
        return .and!(rules)(ParseTree("",false,[],input));
    }

    string and(GetName g)
    {
        return name;
    }
}

unittest // 'and' unit test
{
    alias literal!"abc" abc;
    alias literal!"de" de;
    alias literal!"f" f;

    alias and!(abc) abcAnd;
    alias and!(abc,de) abcde;
    alias and!(abc,de,f) abcdef;
    alias and!(eps, abc, eps, de, eps, f, eps) withEps;

    //assert(getName!(abcAnd)() == `and!(literal!("abc"))`);
    //assert(getName!(abcde)()  == `and!(literal!("abc"), literal!("de"))`);
    //assert(getName!(abcdef)() == `and!(literal!("abc"), literal!("de"), literal!("f"))`);

    ParseTree input = ParseTree("",false,[], "abcdefghi");

    ParseTree result = abcAnd(input);

    assert(result.successful, "and!('abc') parses 'abcdefghi'");
    assert(result.matches == ["abc"], "and!('abc') matches 'abc' at the beginning of 'abcdefghi'");
    assert(result.end == input.end+3, "and!('abc') advances the index by 'abc' size (3).");
    assert(result.children == [abc(input)], "and!('abc') has one child: the one created by 'abc'.");

    result = abcde(input);

    assert(result.successful, "and!('abc','de') parses 'abcdefghi'");
    assert(result.matches == ["abc","de"],
           "and!('abc','de') matches 'abc' and 'de' at the beginning of 'abcdefghi'");
    assert(result.end == input.end+5, "and!('abc','de') advances the index by 3+2 positions.");
    assert(result.children == [abc(input), de(abc(input))],
           "and!('abc','de') has two children, created by 'abc' and 'de'.");

    result = abcdef(input);

    assert(result.successful, "and!('abc','de','f') parses 'abcdefghi'");
    assert(result.matches == ["abc","de","f"],
           "and!('abc','de','f') matches 'abcdef' at the beginning of 'abcdefghi'");
    assert(result.end == input.end+6, "and!('abc','de','f') advances the index by 3+2+1 positions.");
    assert(result.children == [abc(input), de(abc(input)), f(de(abc(input)))]
            , "and!('abc','de') has two children, created by 'abc' and 'de'.");

    result = withEps(input);

    assert(result.successful, "and!('','abc','','de','','f','') parses 'abcdefghi'");
    assert(result.matches == ["","abc","","de","","f",""],
           "and!('','abc','','de','','f','') matches 'abcdef' at the beginning of 'abcdefghi'");
    assert(result.end == input.end+6,
           "and!('','abc','','de','','f','') advances the index by 0+3+0+2+0+1+0 positions.");

    input.input = "bcdefghi";

    result = abcdef(input);

    assert(!result.successful, "'abc' 'de' 'f' fails on 'bcdefghi'");
    //assert(result.matches is null, "'abc' 'de' 'f' has no match on 'bcdefghi'");
    assert(result.end == input.end);
    assert(result.children == [abc(input)], "'abc' 'de' 'f' has one child (a failure) on 'bcdefghi'");

    input.input = "abc_efghi";

    result = abcdef(input);

    assert(!result.successful, "'abc' 'de' 'f' fails on 'abc_efghi'");
    //assert(result.matches == ["abc"], "Only the first match, from 'abc'.");
    assert(result.end == input.end+3, "Advances by 3 positions, due to 'abc'");
    assert(result.children == [abc(input), de(abc(input))]
    , "'abc' 'de' 'f' has two child on 'abc_efghi', the one from 'abc' (success) and the one from 'de' (failure).");
}

template wrapAround(alias before, alias target, alias after)
{
    ParseTree wrapAround(ParseTree p)
    {
        ParseTree temp = before(p);
        if (!temp.successful)
            return temp;

        ParseTree result = target(temp);
        if (!result.successful)
            return result;
        result.begin = temp.begin;

        temp = after(result);
        if (!temp.successful)
            return temp;

        result.end = temp.end;
        return result;
    }

    ParseTree wrapAround(string input)
    {
        return .wrapAround!(before, target, after)(ParseTree("",false,[],input));
    }

    string wrapAround(GetName g)
    {
        return "wrapAround!(" ~ getName!(before)() ~
                ", " ~ getName!(target)() ~
                ", " ~ getName!(after)() ~ ")";
    }
}

/**
Basic operator: it matches if one of its subrules (stored in the rules template parameter tuple) match
the input. The subrules are tested in order, from rules[0] to rules[$-1].

The matching subrule parse trees is stored as its only child and its matches field
will contain all the subrule matches, in order.

----
alias or!(literal!"abc", charRange!('a','z')) rule; // abc or, failing that, any letter between a and z.
ParseTree input = ParseTree("",false,[],"defg"); // low-level plumbing, the rules described here act on ParseTree's not strings.
                                                 // It's equivalent to "defg" as input
auto result = rule(input);

assert(result.successful); // OK
assert(result.matches == ["d"]); // stores the (in this case) only match
assert(result.children.length == 1); // one child, the result of "abc" or [a-z], depending on which rule succeeded.

input.input = "abc"; // changing the input string;
assert(rule(input)).successful); // Still OK
input.input = "1abc";
assert(!rule(input)).successful); // NOK, does not begin by abc nor by [a-z]
----

If it fails, the last children will contain the failed node that matched furthest (longes match).
That way, when printing, as sort of diagnostic is given:

----
alias or!(literal!"abc", and!(literal!"ab", charRange!('0','9'))) rule; // 'abc' or 'ab[0-9]'
ParseTree input = ParseTree("",false,[],"abd"); // equivalent to "abd"

auto failure = rule(input);
writeln(failure);
/+
or (failure)
 +-and (failure)
    +-literal(ab) [0, 2]["ab"]
    +-charRange(0,9) failure at line 0, col 2, after "ab" expected a char between '0' and '9', but got "d"
+/
----

So we know 'or' failed, that the 'and' sub-rule had the longest match, matching 'ab' and failing for [0-9] on index 2.
*/
template or(rules...) if (rules.length > 0)
{
    string ctfeGetNameOr()
    {
        string name = "or!(";
        foreach(i,rule; rules)
            name ~= getName!(rule)
                    ~ (i < rules.length -1 ? ", " : "");
        name ~= ")";
        return name;
    }

    enum name = ctfeGetNameOr();

    ParseTree or(ParseTree p)
    {
        // error-management
        ParseTree longestFail = ParseTree(name, false, [], p.input, p.end, 0);
        string[] errorStrings;
        size_t errorStringChars;
        string orErrorString;

        ParseTree[rules.length] results;
        string[rules.length] names;
        size_t[rules.length] failedLength;
        size_t maxFailedLength;

        version (tracer)
        {
            incTraceLevel();
        }

		// Real 'or' loop
		foreach(i,r; rules)
        {
            version (tracer)
            {
                if (shouldTrace(getName!(r)()))
                    trace(traceMsg(p, name, getName!(r)()));
            }
            ParseTree temp = r(p);
            if (temp.successful)
            {
                temp.children = [temp];
                temp.name = name;
                version (tracer)
                {
                    if (shouldTrace(getName!(r)()))
                        trace(traceResultMsg(temp, getName!(r)()));
                    decTraceLevel();
                }
                return temp;
            }
            else
            {
                version (tracer)
                {
                    if (shouldTrace(getName!(r)()))
                        trace(traceResultMsg(temp, getName!(r)()));
                }
                enum errName = " (" ~ getName!(r)() ~")";
                failedLength[i] = temp.end;
                if (temp.end >= longestFail.end)
                {
                    maxFailedLength = temp.end;
                    longestFail = temp;
                    names[i] = errName;
                    results[i] = temp;

                    if (temp.end == longestFail.end)
                        errorStringChars += (temp.matches.length > 0 ? temp.matches[$-1].length : 0) + errName.length + 4;
                    else
                        errorStringChars = (temp.matches.length > 0 ? temp.matches[$-1].length : 0) + errName.length + 4;
                }
                // Else, this error parsed less input than another one: we discard it.
            }
        }
        version (tracer)
        {
            decTraceLevel();
        }


        // All subrules failed, we will take the longest match as the result
        // If more than one node failed at the same (farthest) position, we concatenate their error messages


        char[] errString;// = new char[](errorStringChars);
        errString.length = errorStringChars;
        uint start = 0;
        foreach(i; 0..rules.length)
        {
            if (failedLength[i] == maxFailedLength && results[i].matches.length > 0)
            {
                auto temp = results[i];
                auto len = temp.matches[$-1].length;
                auto nlen = names[i].length;
                errString[start .. start+len] = temp.matches[$-1][];
                errString[start+len .. start+len+names[i].length] = names[i][];
                errString[start+len+nlen .. start+len+nlen+4] = " or ";
                start += len + names[i].length + 4;
            }
        }
        orErrorString = cast(string)(errString[0..$-4]);

        longestFail.matches = longestFail.matches.length == 0 ? [orErrorString] :
                              longestFail.matches[0..$-1]  // discarding longestFail error message
                            ~ [orErrorString];             // and replacing it by the new, concatenated one.
        longestFail.name = name;
		longestFail.begin = p.end;
        return longestFail;
    }

    ParseTree or(string input)
    {
        return .or!(rules)(ParseTree("",false,[],input));
    }

    string or(GetName g)
    {
        return name;
    }
}

unittest // 'or' unit test
{
    alias charRange!('a','b') ab;
    alias charRange!('c','d') cd;

    alias or!(ab) abOr;
    alias or!(ab,cd) abOrcd;

    assert(getName!(ab)() == "charRange!('a','b')");
    assert(getName!(cd)() == "charRange!('c','d')");

    ParseTree input = ParseTree("",false,[], "abcdefghi");

    ParseTree result = abOr(input);

    assert(result.name == "or!(charRange!('a','b'))", "or name test.");
    assert(result.successful, "or!([a-b]) parses 'abcdefghi'");
    assert(result.matches == ["a"], "or!([a-b]) matches 'a' at the beginning of 'abcdefghi'");
    assert(result.end == input.end+1, "or!([a-b]) advances the index by 'a' size (1).");
    assert(result.children == [ab(input)], "or!([a-b]) has one child: the one created by '[a-b]'.");

    result = abOrcd(input);

    assert(result.name == "or!(charRange!('a','b'), charRange!('c','d'))", "or name test.");
    assert(result.successful, "or!([a-b],[c-d]) parses 'abcdefghi'");
    assert(result.matches == ["a"], "or!([a-b],[c-d]) matches 'a' at the beginning of 'abcdefghi'");
    assert(result.end == input.end+1, "or!([a-b],[c-d]) advances the index by 1 position.");

    assert(result.children == [ab(input)], "or!([a-b],[c-d]) has one child, created by [a-b].");

    input.input = "cdefghi";

    result = abOrcd(input);

    assert(result.name == "or!(charRange!('a','b'), charRange!('c','d'))", "or name test.");
    assert(result.successful, "or!([a-b],[c-d]) parses 'cdefghi'");
    assert(result.matches == ["c"], "or!([a-b],[c-d]) matches 'c' at the beginning of 'cdefghi'");
    assert(result.end == input.end+1, "or!([a-b],[c-d]) advances the index by 1 position.");

    assert(result.children == [cd(input)], "or!([a-b],[c-d]) has one child, created by [c-d].");

    input.input = "_abcdefghi";

    result = abOrcd(input);

    assert(!result.successful, "or!([a-b],[c-d]) fails on '_abcdefghi'");
    assert(result.end == input.end+0, "or!([a-b],[c-d]) does not advance the index.");
    assert(result.matches ==
           [ "a char between 'a' and 'b' (charRange!('a','b')) or a char between 'c' and 'd' (charRange!('c','d'))"]
                             , "or!([a-b],[c-d]) error message. |" ~ result.matches[0]~ "|");

    input.input = "";

    result = abOrcd(input);

    assert(!result.successful, "or!([a-b],[c-d]) fails on and empty input");
    assert(result.end == input.end+0, "or!([a-b],[c-d]) does not advance the index.");
    assert(result.matches ==
    [ "a char between 'a' and 'b' (charRange!('a','b')) or a char between 'c' and 'd' (charRange!('c','d'))"]
                             , "or!([a-b],[c-d]) error message.");
}


/**
Compile-time switch trie from Brian Schott
*/
class Trie(V) : TrieNode!(V)
{
	/**
	 * Adds the given value to the trie with the given key
	 */
	void add(string key, V value) pure
	{
		TrieNode!(V) current = this;
		foreach(dchar keyPart; key)
		{
			if ((keyPart in current.children) is null)
			{
				auto node = new TrieNode!(V);
				current.children[keyPart] = node;
				current = node;
			}
			else
				current = current.children[keyPart];
		}
		current.value = value;
	}
}

class TrieNode(V)
{
	V value;
	TrieNode!(V)[dchar] children;
}

string printCaseStatements(V)(TrieNode!(V) node, string indentString)
{
	string s = "";
	string idnt = indentString;

	void incIndent() { idnt ~= "  "; }
	void decIndent() { idnt = idnt[2..$]; }

	void put(string k) { s ~= idnt ~ k; }
	void append(string k) { s ~= k;}

	foreach(dchar k, TrieNode!(V) v; node.children)
	{
		put("case '");
		switch(k)
		{
            case '\n': append("\\n"); break;
            case '\t': append("\\t"); break;
            case '\r': append("\\r"); break;
            case 92:   append("\\");  break;
            default:   append(to!string(k));
		}
		append("':\n");
		incIndent();

		put("temp.end++;\n");
		if (v.children.length > 0)
		{
			put("if (temp.end >= temp.input.length)\n");
			put("{\n");
			incIndent();

			if (node.children[k].value.length != 0)
                put("return ParseTree(name, true, [`" ~ node.children[k].value ~ "`], temp.input, p.end, temp.end)");
            else
                put("return ParseTree(name, false, [failString], p.input, p.end, p.end)");

			append(";\n");
			decIndent();

			put("}\n");
			put("switch (temp.input[temp.end])\n");
			put("{\n");

			incIndent();
			append(printCaseStatements(v, idnt));

			put("default:\n");
			incIndent();
			if (v.value.length != 0)
                put("return ParseTree(name, true, [`" ~ v.value ~ "`], temp.input, p.end, temp.end)");
            else
                put("return ParseTree(name, false, [failString], p.input, p.end, p.end)");
			append(";\n");
			decIndent();
			decIndent();
			put("}\n");
		}
		else
		{
			if (v.value.length != 0)
                put("return ParseTree(name, true, [`" ~ v.value ~ "`], temp.input, p.end, temp.end)");
            else
                put("return ParseTree(name, false, [failString], p.input, p.end, p.end)");
			append(";\n");
		}
	}
	return s;
}

string generateCaseTrie(string[] args ...)
{
	auto t = new Trie!(string);
	foreach(arg; args)
	{
		t.add(arg, arg);
	}
	return printCaseStatements(t, "");
}

/**
or special case for literal list ("abstract"/"alias"/...)
*/
template keywords(kws...) if (kws.length > 0)
{
    string ctfeGetNameKeywords()
    {
        string name= "keywords!(";
        foreach(i,kw;kws)
            name ~= "\"" ~ kw ~ "\""~ (i < kws.length -1 ? ", " : "");
        name ~= ")";
        return name;
    }

    string ctfeConcatKeywords()
    {
        string s = "[";
        foreach(i, k; kws)
        {
            s ~= "\"" ~ k ~ "\"";
            if (i < kws.length - 1)
                s ~= ", ";
        }
        return s ~= "]";
    }

    enum name = ctfeGetNameKeywords();
    enum failString = "one among " ~ ctfeConcatKeywords();

    ParseTree keywords(ParseTree p)
    {
        string keywordCode(string[] keywords)
        {
            string result;
            foreach(kw; keywords)
                result ~= "if (p.end+"~to!string(kw.length) ~ " <= p.input.length "
                    ~" && p.input[p.end..p.end+"~to!string(kw.length)~"]==`"
                    ~kw~"`) return ParseTree(`"
                    ~name~"`,true,[`"~kw~"`],p.input,p.end,p.end+"
                    ~to!string(kw.length)~");\n";

            result ~= "return ParseTree(`"~name~"`,false,[`" ~ failString ~ "`],p.input,p.end,p.end);";

            return result;
        }

        static if (KEYWORDS == IFCHAIN)
        {
            mixin(keywordCode([kws]));
        }
        else static if (KEYWORDS == TRIE)
        {
            auto temp = p;
            if (p.end < p.input.length) // is this conditional required?
            {
                switch(p.input[p.end])
                {
                    mixin(generateCaseTrie([kws]));
                    mixin("default: return ParseTree(`"~name~"`,false,[`" ~ failString ~ "`],p.input,p.end,p.end);");
                }
            }
            else
            {
                mixin("return ParseTree(`"~name~"`,false,[`" ~ failString ~ "`],p.input,p.end,p.end);");
            }
        }
    }

    ParseTree keywords(string input)
    {
        return .keywords!(kws)(ParseTree("",false,[],input));
    }

    string keywords(GetName g)
    {
        return name;
    }
}

unittest
{
    alias keywords!("abc","de","f") kw;

    assert(getName!(kw)() == `keywords!("abc", "de", "f")`);

    ParseTree input = ParseTree("",false,[],"abcd");

    ParseTree result = kw(input);

    assert(result.name == `keywords!("abc", "de", "f")`, "keywords name test.");
    assert(result.successful, "keywords success on `abcd`");
    assert(result.matches == ["abc"], "keywords matches `abc` on `abcd`");
    assert(result.end == input.end+3, "keywords advances the index by 3 positions.");
    assert(result.children is null, "No children for `keywords`.");

    input.input = "def";

    result = kw(input);

    assert(result.successful, "keywords success on `def`");
    assert(result.matches == ["de"], "keywords matches `de` on `def`");
    assert(result.end == input.end+2, "keywords advances the index by 2 positions.");
    assert(result.children is null, "No children for `keywords`.");


    input.input = "ab_def";

    result = kw(input);

    assert(!result.successful, "keywords fails on `ab_def`.");
    assert(result.matches == [`one among ["abc", "de", "f"]`], "keywords error message." ~ result.matches[0]);
    assert(result.end == input.end, "keywords does not advance the index.");
    assert(result.children is null, "No children for `keywords`.");

    input.input = "";

    result = kw(input);

    assert(!result.successful, "keywords fails on an empty input.");
    assert(result.matches == [`one among ["abc", "de", "f"]`], "keywords error message.");
    assert(result.end == input.end, "keywords does not advance the index.");
    assert(result.children is null, "No children for `keywords`.");
}


/**
Tries to match subrule 'r' zero or more times. It always succeeds, since if 'r' fails
from the very beginning, it matched 'r' zero times...

Its matches are those of its subrules (they might be different for each match) and its
children are all the parse trees returned by the successive application of 'r'.

----
alias zeroOrMore!(or!(literal!"abc", literal!"d")) rule; // in PEG-speak:  '("abc" / "d")*'
ParseTree input = ParseTree("",false,[], "abcdabce");

ParseTree result = rule(input);

assert(result.successful);
assert(result.matches == ["abc", "d", "abc"]);
assert(result.end == 7); // matched "abcdabce"[0..7] => "abcdabc". "e" at the end is not part of the parse tree.
assert(result.children.length == 3);
writeln(result);
/+
writes:
zeroOrMore  [0, 7]["abc", "d", "abc"]
 +-or  [0, 3]["abc"]
 |  +-literal(abc)  [0, 3]["abc"]
 +-or  [3, 4]["d"]
 |  +-literal(d)  [3, 4]["d"]
 +-or  [4, 7]["abc"]
    +-literal(abc)  [4, 7]["abc"]
+/
----

So we know the first child used the 'literal!"abc"' sub-rule and matched input[0..3].
The second matched input[3..4] and the third input[4..7].

----
input = ParseTree("",false,[], "efgh");
result = rule(input);
assert(result.successful); // succeed, even though all patterns failed.
assert(result.children.length == 0);
----
*/
template zeroOrMore(alias r)
{
    enum name = "zeroOrMore!(" ~ getName!(r) ~ ")";

    ParseTree zeroOrMore(ParseTree p)
    {
        auto result = ParseTree(name, true, [], p.input, p.end, p.end);
        version (tracer)
        {
            incTraceLevel();
            if (shouldTrace(getName!(r)()))
                trace(traceMsg(result, name, getName!(r)()));
        }
        auto temp = r(result);
        while(temp.successful
            && (temp.begin < temp.end // To avoid infinite loops on epsilon-matching rules
            || temp.name.startsWith("discard!(")))
        {
            result.matches ~= temp.matches;
            result.children ~= temp;
            result.end = temp.end;
            version (tracer)
            {
                if (shouldTrace(getName!(r)()))
                    trace(traceMsg(result, name, getName!(r)()));
            }
            temp = r(result);
        }
        result.successful = true;
        version (tracer)
        {
            if (shouldTrace(getName!(r)()))
                trace(traceResultMsg(result, getName!(r)()));
            decTraceLevel();
        }
        return result;
    }

    ParseTree zeroOrMore(string input)
    {
        return .zeroOrMore!(r)(ParseTree("",false,[],input));
    }

    string zeroOrMore(GetName g)
    {
        return name;
    }
}

unittest // 'zeroOrMore' unit test
{
    alias literal!"a" a;
    alias literal!"abc" abc;
    alias charRange!('a','z') az;

    alias zeroOrMore!(a) as;
    alias zeroOrMore!(abc) abcs;
    alias zeroOrMore!(az) azs;

    assert(getName!(as)() == `zeroOrMore!(literal!("a"))`);
    assert(getName!(abcs)() == `zeroOrMore!(literal!("abc"))`);
    assert(getName!(azs)() == `zeroOrMore!(charRange!('a','z'))`);

    assert(as("").successful);
    assert(as("a").successful);
    assert(as("aa").successful);
    assert(as("aaa").successful);
    assert(as("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").successful);
    assert(as("b").successful);

    ParseTree result = as("aaa");

    assert(result.name == `zeroOrMore!(literal!("a"))`);
    assert(result.successful);
    assert(result.matches == ["a","a","a"]);
    assert(result.begin == 0);
    assert(result.end == 3);
    assert(result.children.length == 3);
    assert(result.children == [ a("aaa"), a(a("aaa")), a(a(a("aaa")))]);

    assert(abcs("").successful);
    assert(abcs("abc").successful);
    assert(abcs("abcabc").successful);
    assert(abcs("abcabcabc").successful);
    assert(abcs("abcabcabcabcabcabcabcabcabcabcabcabcabcabcabc").successful);
    assert(abcs("ab").successful);

    result = abcs("abcabcabc");

    assert(result.name == `zeroOrMore!(literal!("abc"))`);
    assert(result.successful);
    assert(result.matches == ["abc","abc","abc"]);
    assert(result.begin == 0);
    assert(result.end == 3*3);
    assert(result.children.length == 3);
    assert(result.children == [ abc("abcabcabc"), abc(abc("abcabcabc")), abc(abc(abc("abcabcabc")))]);

    assert(azs("").successful);
    assert(azs("a").successful);
    assert(azs("abc").successful);
    assert(azs("abcdefghijklmnoqrstuvwxyz").successful);
    assert(azs("abcdefghijklmnoqrstuvwxyz   1234567890").successful);

    result = azs("abc");

    assert(result.name == `zeroOrMore!(charRange!('a','z'))`);
    assert(result.successful);
    assert(result.matches == ["a","b","c"]);
    assert(result.begin == 0);
    assert(result.end == 3);
    assert(result.children.length == 3);
    assert(result.children == [ az("abc"), az(az("abc")), az(az(az("abc")))]);
}

/**
Tries to match subrule 'r' one or more times. If 'r' fails
from the very beginning, it fails and else succeeds.

Its matches are those of its subrules (they might be different for each match) and its
children are all the parse trees returned by the successive application of 'r'.

----
alias oneOrMore!(or!(literal!"abc", literal!"d")) rule; // in PEG-speak:  '("abc" / "d")*'
ParseTree input = ParseTree("",false,[], "abcdabce");

ParseTree result = rule(input);

assert(result.successful);
assert(result.matches == ["abc", "d", "abc"]);
assert(result.end == 7); // matched "abcdabce"[0..7] => "abcdabc". "e" at the end is not part of the parse tree.
assert(result.children.length == 3);
writeln(result);
/+
writes:
oneOrMore  [0, 7]["abc", "d", "abc"]
 +-or  [0, 3]["abc"]
 |  +-literal(abc)  [0, 3]["abc"]
 +-or  [3, 4]["d"]
 |  +-literal(d)  [3, 4]["d"]
 +-or  [4, 7]["abc"]
    +-literal(abc)  [4, 7]["abc"]
+/
----

So we know the first child used the 'literal!"abc"' sub-rule and matched input[0..3].
The second matched input[3..4] and the third input[4..7].

----
input = ParseTree("",false,[], "efgh");
result = rule(input);
assert(!result.successful); // fails, since it failed on the first try.
----
*/
template oneOrMore(alias r)
{
    enum name = "oneOrMore!(" ~ getName!(r) ~ ")";

    ParseTree oneOrMore(ParseTree p)
    {
        auto result = ParseTree(name, false, [], p.input, p.end, p.end);
        version (tracer)
        {
            incTraceLevel();
            if (shouldTrace(getName!(r)()))
                trace(traceMsg(result, name, getName!(r)()));
        }
        auto temp = r(result);

        if (!temp.successful)
        {
            result.matches = temp.matches;
            result.children = [temp];
            result.end = temp.end;
        }
        else
        {
            while(  temp.successful
                && (temp.begin < temp.end // To avoid infinite loops on epsilon-matching rules
            || temp.name.startsWith("discard!(")))
            {
                result.matches ~= temp.matches;
                result.children ~= temp;
                result.end = temp.end;
                version (tracer)
                {
                    if (shouldTrace(getName!(r)()))
                        trace(traceMsg(result, name, getName!(r)()));
                }
                temp = r(result);
            }
            result.successful = true;
        }
        version (tracer)
        {
            if (shouldTrace(getName!(r)()))
                trace(traceResultMsg(result, getName!(r)()));
            decTraceLevel();
        }
        return result;
    }

    ParseTree oneOrMore(string input)
    {
        return .oneOrMore!(r)(ParseTree("",false,[],input));
    }

    string oneOrMore(GetName g)
    {
        return name;
    }
}

unittest // 'oneOrMore' unit test
{
    alias literal!"a" a;
    alias literal!"abc" abc;
    alias charRange!('a','z') az;

    alias oneOrMore!(a) as;
    alias oneOrMore!(abc) abcs;
    alias oneOrMore!(az) azs;

    assert(getName!(as)() == `oneOrMore!(literal!("a"))`);
    assert(getName!(abcs)() == `oneOrMore!(literal!("abc"))`);
    assert(getName!(azs)() == `oneOrMore!(charRange!('a','z'))`);

    assert(!as("").successful);
    assert(as("a").successful);
    assert(as("aa").successful);
    assert(as("aaa").successful);
    assert(as("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").successful);
    assert(!as("b").successful);

    ParseTree result = as("aaa");

    assert(result.name == `oneOrMore!(literal!("a"))`);
    assert(result.successful);
    assert(result.matches == ["a","a","a"]);
    assert(result.begin == 0);
    assert(result.end == 3);
    assert(result.children.length == 3);
    assert(result.children == [ a("aaa"), a(a("aaa")), a(a(a("aaa")))]);

    assert(!abcs("").successful);
    assert(abcs("abc").successful);
    assert(abcs("abcabc").successful);
    assert(abcs("abcabcabc").successful);
    assert(abcs("abcabcabcabcabcabcabcabcabcabcabcabcabcabcabc").successful);
    assert(!abcs("ab").successful);

    result = abcs("abcabcabc");

    assert(result.name == `oneOrMore!(literal!("abc"))`);
    assert(result.successful);
    assert(result.matches == ["abc","abc","abc"]);
    assert(result.begin == 0);
    assert(result.end == 3*3);
    assert(result.children.length == 3);
    assert(result.children == [ abc("abcabcabc"), abc(abc("abcabcabc")), abc(abc(abc("abcabcabc")))]);

    assert(!azs("").successful);
    assert(azs("a").successful);
    assert(azs("abc").successful);
    assert(azs("abcdefghijklmnoqrstuvwxyz").successful);
    assert(azs("abcdefghijklmnoqrstuvwxyz   1234567890").successful);
    assert(!azs(".").successful);

    result = azs("abc");

    assert(result.name == `oneOrMore!(charRange!('a','z'))`);
    assert(result.successful);
    assert(result.matches == ["a","b","c"]);
    assert(result.begin == 0);
    assert(result.end == 3);
    assert(result.children.length == 3);
    assert(result.children == [ az("abc"), az(az("abc")), az(az(az("abc")))]);
}

/**
Given a subrule 'r', represents the expression 'r?'. It tries to match 'r' and if this matches
successfully, it returns this match. If 'r' failed, 'r?' is still a success, but without any child nor match.

----
alias option!(literal!"abc") rule; // Aka '"abc"?'
ParseTree input = ParseTree("",false,[],"abcd");

ParseTree result = rule(input);
assert(result.successful);
assert(result.matches == ["abc"]);
assert(result.children.length == 1);
assert(result.children[0] == literal!"abc"(input));
----
*/
template option(alias r)
{
    enum name = "option!(" ~ getName!(r) ~ ")";

    ParseTree option(ParseTree p)
    {
        version (tracer)
        {
            if (shouldTrace(getName!(r)()))
                trace(traceMsg(p, name, getName!(r)()));
        }
        ParseTree result = r(p);
        if (result.successful)
            return ParseTree(name, true, result.matches, result.input, result.begin, result.end, [result]);
        else
            return ParseTree(name, true, [], p.input, p.end, p.end, null);
    }

    ParseTree option(string input)
    {
        return .option!(r)(ParseTree("",false,[],input));
    }

    string option(GetName g)
    {
        return name;
    }
}

unittest // 'option' unit test
{
    alias literal!"a" a;
    alias literal!"abc" abc;

    alias option!(a) a_;
    alias option!(abc) abc_;

    assert(getName!(a_)() == `option!(literal!("a"))`);
    assert(getName!(abc_)() == `option!(literal!("abc"))`);

    assert(a_("").successful);
    assert(a_("a").successful);
    assert(a_("aa").successful);
    assert(a_("b").successful);

    ParseTree result = a_("a");

    assert(result.name == `option!(literal!("a"))`);
    assert(result.successful);
    assert(result.matches == ["a"]);
    assert(result.begin == 0);
    assert(result.end == 1);
    assert(result.children.length == 1);
    assert(result.children == [ a("a")]);

    result = a_("");

    assert(result.name == `option!(literal!("a"))`);
    assert(result.successful);
    assert(result.matches is null);
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.children.length == 0);


    assert(abc_("").successful);
    assert(abc_("abc").successful);
    assert(abc_("abcabc").successful);
    assert(abc_("ab").successful);

    result = abc_("abcdef");

    assert(result.name == `option!(literal!("abc"))`);
    assert(result.successful);
    assert(result.matches == ["abc"]);
    assert(result.begin == 0);
    assert(result.end == 3);
    assert(result.children.length == 1);
    assert(result.children == [ abc("abcdef")]);

    result = abc_("def");

    assert(result.name == `option!(literal!("abc"))`);
    assert(result.successful);
    assert(result.matches is null);
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.children.length == 0);
}

/**
Tries 'r' on the input. If it succeeds, the rule also succeeds, without consuming any input.
If 'r' fails, then posLookahead!r also fails. Low-level implementation of '&r'.
*/
template posLookahead(alias r)
{
    enum name = "posLookahead!(" ~ getName!(r) ~ ")";

    ParseTree posLookahead(ParseTree p)
    {
        version (tracer)
        {
            if (shouldTrace(getName!(r)()))
                trace(traceMsg(p, name, getName!(r)()));
        }
        ParseTree temp = r(p);
        if (temp.successful)
            return ParseTree(name, temp.successful, [], p.input, p.end, p.end);
        else
            return ParseTree(name, temp.successful, [temp.matches[$-1]], p.input, p.end, p.end);
    }

    ParseTree posLookahead(string input)
    {
        return .posLookahead!(r)(ParseTree("",false,[],input));
    }

    string posLookahead(GetName g)
    {
        return name;
    }
}

unittest // 'posLookahead' unit test
{
    alias literal!"a" a;
    alias literal!"abc" abc;

    alias posLookahead!(a) a_;
    alias posLookahead!(abc) abc_;

    assert(getName!(a_)() == `posLookahead!(literal!("a"))`);
    assert(getName!(abc_)() == `posLookahead!(literal!("abc"))`);

    assert(!a_("").successful);
    assert(a_("a").successful);
    assert(a_("aa").successful);
    assert(!a_("b").successful);

    ParseTree result = a_("a");

    assert(result.name == `posLookahead!(literal!("a"))`);
    assert(result.successful);
    assert(result.matches is null);
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.children.length == 0);

    result = a_("");

    assert(result.name == `posLookahead!(literal!("a"))`);
    assert(!result.successful);
    assert(result.matches == [`"a"`], "posLookahead error message.");
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.children.length == 0);

    assert(!abc_("").successful);
    assert(abc_("abc").successful);
    assert(abc_("abcabc").successful);
    assert(!abc_("ab").successful);

    result = abc_("abcdef");

    assert(result.name == `posLookahead!(literal!("abc"))`);
    assert(result.successful);
    assert(result.matches is null);
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.children.length == 0);

    result = abc_("def");

    assert(result.name == `posLookahead!(literal!("abc"))`);
    assert(!result.successful);
    assert(result.matches == [`"abc"`], "posLookahead error message.");
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.children.length == 0);
}

/**
Tries 'r' on the input. If it fails, the rule succeeds, without consuming any input.
If 'r' succeeds, then negLookahead!r fails. Low-level implementation of '!r'.
*/
template negLookahead(alias r)
{
    enum name = "negLookahead!(" ~ getName!(r) ~ ")";

    ParseTree negLookahead(ParseTree p)
    {
        version (tracer)
        {
            if (shouldTrace(getName!(r)()))
                trace(traceMsg(p, name, getName!(r)()));
        }
        ParseTree temp = r(p);
        if (temp.successful)
            return ParseTree(name, false, ["anything but \"" ~ p.input[temp.begin..temp.end] ~ "\""], p.input, p.end, p.end);
        else
            return ParseTree(name, true, [], p.input, p.end, p.end);
    }

    ParseTree negLookahead(string input)
    {
        return .negLookahead!(r)(ParseTree("",false,[],input));
    }

    string negLookahead(GetName g)
    {
        return name;
    }
}

unittest // 'negLookahead' unit test
{
    alias literal!"a" a;
    alias literal!"abc" abc;

    alias negLookahead!(a) a_;
    alias negLookahead!(abc) abc_;

    assert(getName!(a_)() == `negLookahead!(literal!("a"))`);
    assert(getName!(abc_)() == `negLookahead!(literal!("abc"))`);

    assert(a_("").successful);
    assert(!a_("a").successful);
    assert(!a_("aa").successful);
    assert(a_("b").successful);

    ParseTree result = a_("a");

    assert(result.name == `negLookahead!(literal!("a"))`);
    assert(!result.successful);
    assert(result.matches == [`anything but "a"`]);
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.children.length == 0);

    result = a_("");

    assert(result.name == `negLookahead!(literal!("a"))`);
    assert(result.successful);
    assert(result.matches is null);
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.children.length == 0);

    assert(abc_("").successful);
    assert(!abc_("abc").successful);
    assert(!abc_("abcabc").successful);
    assert(abc_("ab").successful);

    result = abc_("abcdef");

    assert(result.name == `negLookahead!(literal!("abc"))`);
    assert(!result.successful);
    assert(result.matches == [`anything but "abc"`]);
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.children.length == 0);

    result = abc_("def");

    assert(result.name == `negLookahead!(literal!("abc"))`);
    assert(result.successful);
    assert(result.matches is null);
    assert(result.begin == 0);
    assert(result.end == 0);
    assert(result.children.length == 0);
}

/**
Internal helper template, to get a parse tree node with a name. For example, given:
----
alias or!(literal!("abc"), charRange!('0','9')) myRule;
----

myRule gives nodes named "or", since its the parent rule. If you want nodes to be named "myRule",
use named. named just overwrites the original root node name, the children are left untouched.

See_Also: defined.

----
alias or!(literal!("abc"), charRange!('0','9')) rule;
alias named!(rule, "myRule") myRule;

auto input = "abc3";
auto p1 = rule(input);
auto p2 = myRule(input);

// They are both successful
assert(p1.successful && p2.successful);
assert(p1.matches == p2.matches);
// But the names are different
assert(p1.name == `or!(literal!("abc"), charRange!('0','9'))`);
assert(p2.name == `myRule`);
// Same children:
assert(p2.children == p1.children);

----
*/
template named(alias r, string name)
{
    ParseTree named(ParseTree p)
    {
        ParseTree result = r(p);
        result.name = name;
        return result;
    }

    ParseTree named(string input)
    {
        return .named!(r,name)(ParseTree("",false,[],input));
    }

    string named(GetName g)
    {
        return name;
    }
}

unittest // 'named' unit test
{
    alias or!(literal!("abc"), charRange!('0','9')) rule;
    alias named!(rule, "myRule") myRule;

    assert(getName!(rule)() == `or!(literal!("abc"), charRange!('0','9'))`);
    assert(getName!(myRule)() == "myRule");

    // Equality on success (except for the name)
    ParseTree result = rule("abc0");
    ParseTree myResult = myRule("abc0");

    assert(myResult.successful == result.successful);
    assert(myResult.name == "myRule");
    assert(myResult.matches == result.matches);
    assert(myResult.begin == result.begin);
    assert(myResult.end == result.end);
    assert(myResult.children == result.children);

    // Equality on failure (except for the name)
    result = rule("_abc");
    myResult = myRule("_abc");

    assert(myResult.successful == result.successful);
    assert(myResult.name == "myRule");
    assert(myResult.matches == result.matches);
    assert(myResult.begin == result.begin);
    assert(myResult.end == result.end);
    assert(myResult.children == result.children);
}

/**
Internal helper template, to get a parse tree node with a name, while keeping the original node (see also named).
For example, given:
----
alias or!(literal!("abc"), charRange!('0','9')) myRule;
----

myRule gives nodes named "or", since its the parent rule. If you want nodes to be named "myRule",
use defined. Contrary to named (see before), the original node is pushed as the child.

See_Also: named.

----
alias or!(literal!("abc"), charRange!('0','9')) rule;
alias defined!(rule, "myRule") myRule;

auto input = "abc3";
auto p1 = rule(input);
auto p2 = myRule(input);

// They are both successful
assert(p1.successful && p2.successful);
assert(p1.matches == p2.matches);
// But the names are different
assert(p1.name == `or!(literal!("abc"), charRange!('0','9'))`);
assert(p2.name == `myRule`);
// Here the original tree is not discarded:
assert(p2.children[0] == p1);
----
*/
template defined(alias r, string name)
{
    ParseTree defined(ParseTree p)
    {
        ParseTree result = r(p);
        result.children = [result];
        result.name = name;
        return result;
    }

    ParseTree defined(string input)
    {
        return .defined!(r,name)(ParseTree("",false,[],input));
    }

    string defined(GetName g)
    {
        return name;
    }
}

unittest // 'defined' unit test
{
    alias or!(literal!("abc"), charRange!('0','9')) rule;
    alias defined!(rule, "myRule") myRule;

    assert(getName!(rule)() == `or!(literal!("abc"), charRange!('0','9'))`);
    assert(getName!(myRule)() == "myRule");

    // Equality on success (except for the name)
    ParseTree result = rule("abc0");
    ParseTree myResult = myRule("abc0");

    assert(myResult.successful && result.successful);
    assert(myResult.name == "myRule");
    assert(myResult.matches == result.matches);
    assert(myResult.begin == result.begin);
    assert(myResult.end == result.end);
    assert(myResult.children[0] == result);

    // Equality on failure (except for the name)
    result = rule("_abc");
    myResult = myRule("_abc");

    assert(!myResult.successful && !result.successful);
    assert(myResult.name == "myRule");
    assert(myResult.matches == result.matches);
    assert(myResult.begin == result.begin);
    assert(myResult.end == result.end);
    assert(myResult.children[0] == result);
}

/**
Low-level representation for the expression 'r {act}'. That is, it applies rule 'r'
on the input and then calls 'act' on the resulting ParseTree.
*/
template action(alias r, alias act)
{
    ParseTree action(ParseTree p)
    {
        return act(r(p));
    }

    ParseTree action(string input)
    {
        return .action!(r,act)(ParseTree("",false,[],input));
    }

    string action(GetName g)
    {
        enum name = "action!("~ getName!(r)() ~ ", " ~ __traits(identifier, act) ~ ")";
        return name;
    }
}

unittest // 'action' unit test
{
    ParseTree foo(ParseTree p)
    {
        p.matches ~= p.matches; // doubling matches
        return p;
    }

    alias literal!("abc") abc;

    alias action!(abc, foo) abcfoo;

    assert(getName!(abcfoo) == `action!(literal!("abc"), foo)`);

    ParseTree result = abcfoo("abc");
    ParseTree reference = abc("abc");

    assert(result.successful == reference.successful);
    assert(result.successful);
    assert(result.matches == reference.matches ~ reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    // On failure
    result = abcfoo("_abc");
    reference = abc("_abc");

    assert(result.successful == reference.successful);
    assert(!result.successful);
    assert(result.matches == reference.matches ~ reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);
}

/**
Concatenates a ParseTree's matches as one match and discards its children. Equivalent to the expression '~r'.
*/
template fuse(alias r)
{
    ParseTree fuse(ParseTree p)
    {
        p = r(p);
        if(p.successful)
        {
            if (p.matches.length != 0)
                p.matches = [std.array.join(p.matches)];

            p.children = null; // also discard children
        }
        return p;
    }

    ParseTree fuse(string input)
    {
        return .fuse!(r)(ParseTree("",false,[],input));
    }

    string fuse(GetName g)
    {
        enum name = "fuse!(" ~ getName!(r)() ~ ")";
        return name;
    }
}

unittest // 'fuse' unit test
{
    alias oneOrMore!(literal!("abc")) abcs;

    alias fuse!(abcs) f;

    assert(getName!(f) == `fuse!(oneOrMore!(literal!("abc")))`);

    ParseTree result = f("abcabcabc");
    ParseTree reference = abcs("abcabcabc");

    assert(result.successful == reference.successful);
    assert(result.successful);
    assert(reference.matches == ["abc", "abc", "abc"]);
    assert(result.matches == ["abcabcabc"]);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children is null);

    // On failure
    result = f("_abc");
    reference = abcs("_abc");

    assert(result.successful == reference.successful);
    assert(!result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    alias discard!(literal!("abc")) dabc;
    alias fuse!(dabc) f2;

    result = f2("abcabc");
    reference = dabc("abcabc");

    assert(result.successful);
    assert(result.matches is null);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);
}

/**
Calls 'r' on the input and then discards its children nodes.
*/
template discardChildren(alias r)
{
    ParseTree discardChildren(ParseTree p)
    {
        p = r(p);
        p.children = null;
        return p;
    }

    ParseTree discardChildren(string input)
    {
        return .discardChildren!(r)(ParseTree("",false,[],input));
    }

    string discardChildren(GetName g)
    {
        enum name = "discardChildren!(" ~ getName!(r)() ~ ")";
        return name;
    }
}

/**
Calls 'r' on the input and then discards its matches.
*/
template discardMatches(alias r)
{
    ParseTree discardMatches(ParseTree p)
    {
        p = r(p);
        if (p.successful)
            p.matches = null;
        return p;
    }

    ParseTree discardMatches(string input)
    {
        return .discardMatches!(r)(ParseTree("",false,[],input));
    }

    string discardMatches(GetName g)
    {
        enum name = "discardMatches!(" ~ getName!(r)() ~ ")";
        return name;
    }
}

/**
Calls 'r' on the input and then discard everything 'r' returned: no children, no match and index
put at the end of the match. It's the low-level engine behind ':r'.
*/
template discard(alias r)
{
    ParseTree discard(ParseTree p)
    {
        ParseTree result = r(p);
        result.name = "discard!(" ~ getName!(r)() ~ ")";
        //result.begin = result.end;
        result.children = null;
        if (result.successful)
            result.matches = null;//to keep error messages, if any

        return result;
    }

    ParseTree discard(string input)
    {
        return .discard!(r)(ParseTree("",false,[],input));
    }

    string discard(GetName g)
    {
        return "discard!(" ~ getName!(r)() ~ ")";
    }
}

unittest // 'discard' unit test
{
    alias literal!"abc" abc;
    alias oneOrMore!abc abcs;
    alias discard!(literal!("abc")) dabc;
    alias discard!(oneOrMore!(literal!("abc")))dabcs;

    ParseTree reference = abc("abc");
    ParseTree result =dabc("abc");

    assert(result.successful == reference.successful);
    assert(result.successful);
    assert(result.name == `discard!(literal!("abc"))`);
    assert(result.matches is null);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children is null);

    reference = abcs("abcabcabc");
    result = dabcs("abcabcabc");

    assert(result.successful == reference.successful);
    assert(result.successful);
    assert(result.name == `discard!(oneOrMore!(literal!("abc")))`);
    assert(result.matches is null);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children is null);

    // On failure
    reference = abcs("");
    result = dabcs("");

    assert(result.successful == reference.successful);
    assert(!result.successful);
    assert(result.name == `discard!(oneOrMore!(literal!("abc")))`);
    assert(result.matches == [`"abc"`], "discard error message.");
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children is null);

    // Action on 'and'
    alias and!(abc,dabc,abc) discardMiddle;

    result = discardMiddle("abcabcabc");
    assert(result.successful);
    assert(result.matches == ["abc", "abc"]);
    assert(result.begin == 0);
    assert(result.end == 3*3);
    assert(result.children.length == 2);
    assert(result.children == [abc("abcabcabc"), abc(abc(abc("abcabcabc")))]);
}

/**
Calls 'r' on the input and then discards everything 'r' did, except its matches (so that
they propagate upwards). Equivalent to ';r'.
*/
template drop(alias r)
{
    ParseTree drop(ParseTree p)
    {
        ParseTree result = r(p);
        //result.begin = result.end;
        result.children = null;
        if (result.successful)
            result.name = "drop!(" ~ getName!(r)() ~ ")";
        return result;
    }

    ParseTree drop(string input)
    {
        return .drop!(r)(ParseTree("",false,[],input));
    }

    string drop(GetName g)
    {
        return "drop!(" ~ getName!(r)() ~ ")";
    }
}

unittest // 'drop' unit test
{
    alias literal!"abc" abc;
    alias oneOrMore!abc abcs;
    alias drop!(literal!("abc")) dabc;
    alias drop!(oneOrMore!(literal!("abc"))) dabcs;

    ParseTree reference = abc("abc");
    ParseTree result =dabc("abc");

    assert(result.successful == reference.successful);
    assert(result.successful);
    assert(result.name == `drop!(literal!("abc"))`);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children is null);

    reference = abcs("abcabcabc");
    result = dabcs("abcabcabc");

    assert(result.successful == reference.successful);
    assert(result.successful);
    assert(result.name == `drop!(oneOrMore!(literal!("abc")))`);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children is null);

    // On failure
    reference = abcs("");
    result = dabcs("");

    assert(result.successful == reference.successful);
    assert(!result.successful);
    assert(result.name == reference.name);
    assert(result.matches == [`"abc"`], "'drop' error message.");
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children is null);

    // Action on 'and'
    alias and!(abc,dabc,abc) discardMiddle;

    result = discardMiddle("abcabcabc");
    assert(result.successful);
    assert(result.matches == ["abc", "abc", "abc"], "3 matches.");
    assert(result.begin == 0);
    assert(result.end == 3*3);
    assert(result.children.length == 2, "but only 2 children.");
    assert(result.children == [abc("abcabcabc"), abc(abc(abc("abcabcabc")))]);
}

/**
Makes r disappear in a sequence, letting its children take its place. It's equivalent
to the '%' operator. Given A <- B %C D and C <- E F, a successful parse for A will
generate a three with four children: B, E, F and D parse trees.
*/
template propagate(alias r)
{
    ParseTree propagate(ParseTree p)
    {
        ParseTree result = r(p);
        if (result.successful)
            result.name = "propagate!(" ~ getName!(r)() ~ ")";
        return result;
    }

    ParseTree propagate(string input)
    {
        return .propagate!(r)(ParseTree("",false,[],input));
    }

    string propagate(GetName g)
    {
        return "propagate!(" ~ getName!(r)() ~ ")";
    }
}

/**
Makes 'r's result be kept when it would be discarded by the tree-decimation done by a grammar.
Equivalent to '^r'.
*/
template keep(alias r)
{
    ParseTree keep(ParseTree p)
    {
        ParseTree result = r(p);
        if (result.successful)
        {
            result.children = [result];
            result.name = "keep!(" ~ getName!(r)() ~ ")";
        }
        return result;
    }

    ParseTree keep(string input)
    {
        return .keep!(r)(ParseTree("",false,[],input));
    }

    string keep(GetName g)
    {
        return "keep!(" ~ getName!(r)() ~ ")";
    }
}

unittest // 'keep' unit test
{
    // Grammar mimicry
    struct KeepTest
    {
        static bool isRule(string s)
        {
            if (s == "A" || s == "KA")
                return true;
            else
                return false;
        }

        mixin decimateTree;

        // Equivalent to A <- 'a' 'b'
        static ParseTree A(string s)
        {
            return decimateTree(named!(and!(literal!"a", literal!"b"), "A")(s));
        }

        // Here we use keep to protect 'b'
        // Equivalent to KA <- 'a' ^'b'
        static ParseTree KA(string s)
        {
            return decimateTree(named!(and!(literal!"a", keep!(literal!"b")), "KA")(s));
        }
    }

    ParseTree reference = KeepTest.A("abc");
    ParseTree result = KeepTest.KA("abc");

    assert(result.successful == reference.successful);
    assert(result.matches == reference.matches);
    assert(result.matches == ["a","b"]);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(reference.children.length == 0);
    assert(result.children.length == 1);
    assert(result.children == [literal!("b")(literal!("a")("abc"))], "'b' node was kept.");
}

/* ****************** predefined rules ******************** */

alias named!(or!(literal!("\r\n"), literal!("\n"), literal!("\r")), "endOfLine") endOfLine; /// predefined end-of-line parser
alias endOfLine eol; /// helper alias.

alias or!(literal!(" "), literal!("\t"), literal!("\v")) space; /// predefined space-recognizing parser (space or tabulation).
alias named!(literal!"\t", "tab") tab; /// A parser recognizing \t (tabulation)
alias named!(fuse!(discardChildren!(oneOrMore!space)),
             "spaces") spaces; /// aka '~space+'
alias or!(space, endOfLine) blank; /// Any blank char (spaces or end of line).
alias named!(discard!(zeroOrMore!blank),
             "spacing") spacing; /// The basic space-management parser: discard zero or more blank spaces.

alias charRange!('0', '9') digit; /// Decimal digit: [0-9]
alias named!(fuse!(discardChildren!(oneOrMore!digit)), "digits") digits; /// [0-9]+

alias or!(charRange!('0','9'), charRange!('a','f'), charRange!('A', 'F')) hexDigit; /// Hexadecimal digit: [0-9a-fA-F]

alias charRange!('a', 'z') alpha; /// [a-z]
alias charRange!('A', 'Z') Alpha; /// [A-Z]

alias and!(oneOrMore!(or!(alpha, Alpha, literal!("_"))), zeroOrMore!(or!(digit, alpha, Alpha, literal!("_")))) ident;
alias named!(fuse!(discardChildren!ident),
             "identifier")  identifier; /// [a-zA-Z_][a-zA-Z_0-9]*, the basic C-family identifier
alias named!(fuse!(discardChildren!(and!(identifier, zeroOrMore!(and!(literal!".", identifier))))),
             "qualifiedIdentifier") qualifiedIdentifier; /// qualified identifiers (ids separated by dots: abd.def.g).

alias named!(literal!"/", "slash") slash; /// A parser recognizing '/'
alias named!(literal!"\\", "backslash") backslash; /// A parser recognizing '\'
alias named!(literal!"'", "quote") quote; /// A parser recognizing ' (single quote)
alias named!(literal!"\"", "doublequote") doublequote; /// A parser recognizing " (double quote)
alias named!(literal!"`", "backquote") backquote; /// A parser recognizing $(BACKTICK) (backquote)

/// A list of elem's separated by sep's. One element minimum.
template list(alias elem, alias sep)
{
    alias named!(spaceAnd!(oneOrMore!blank, and!(elem, zeroOrMore!(spaceAnd!(discardMatches!(sep), elem)))), "list") list;
}

/// A list of elem's separated by sep's. The empty list (no elem, no sep) is OK.
template list0(alias elem, alias sep)
{
    alias named!(spaceAnd!(oneOrMore!blank, option!(and!(elem, zeroOrMore!(spaceAnd!(discardMatches!(sep), elem))))), "list0") list0;
}

template AddSpace(alias sp)
{
    template AddSpace(alias r)
    {
        alias TypeTuple!(r, discard!sp) AddSpace;
    }
}

/**
The engine formerly behind the '< ' Pegged rule: all sequence subelements of a rule are interspersed
with a space-consuming rule, given as the first template parameter. It's not used by Pegged anymore
but can be useful for low-level code. It might become deprecated, but it's not there yet.

----
alias and!(literal!"abc", literal!"def") rule1; // "abc" "def", equivalent to "abcdef"
alias spaceAnd!(oneOrMore!blank, literal!"abc", literal!"def") rule2; // "abc" "def", but with spaces in-between.

string input1 = "abcdef";
string input2 = "  abc

def  "; // with spaces and end of line markers.

assert(rule1(input1).successful); // OK
assert(!rule1(input2).successful); // NOK, couldn't find "def" after "abc"

assert(rule2(input1).successful); // OK
assert(rule2(input2).successful); // Still OK
assert(rule2(input2).matches == ["abc","def"]);// rule2 finds the literals among the spaces
----

As you can see on the previous line, spaceAnd discards the matched spaces
and returns matches only for the 'real' subrules.

Note: by using a non-space rule as the first template argument,
you can use spaceAnd as a generic 'find these patterns, possibly separated by this pattern' rule.

For example, using digits as separators:
----
alias spaceAnd!(digit, literal!"abc", litera!"def") rule3;

ParseTree result = rule3("123abc45def67890");
assert(rule3.successful);
assert(rule3.matches == ["abc", "def"]);
assert(rule3.children.length == 2);

assert(rule3.begin == 0;)
assert(rule3.end == "123abc45def67890".length);
----
*/
template spaceAnd(alias sp, rules...)
{
    alias and!(discard!(zeroOrMore!sp), staticMap!(AddSpace!(zeroOrMore!sp), rules)) spaceAnd;
}

unittest // 'spaceAnd' unit test
{
    alias literal!"a" a;
    alias literal!"b" b;

    //Basic use
    alias and!(a,b) ab;
    alias spaceAnd!(oneOrMore!blank, a, b) a_b;

    ParseTree reference = ab("ab");
    ParseTree result = a_b("ab");

    assert(reference.successful);
    assert(result.successful);
    assert(result.matches == reference.matches);
    assert(result.begin == reference.begin);
    assert(result.end == reference.end);
    assert(result.children == reference.children);

    reference = ab("   a  b  ");
    result = a_b(  "   a  b  ");

    assert(!reference.successful);
    assert(result.successful);
    assert(result.matches == ["a","b"]);
    assert(result.begin == 0);
    assert(result.end == "   a  b  ".length);
    assert(result.children.length == 2);

    reference = ab("ac");
    result = a_b("ac");

    assert(!reference.successful);
    assert(!result.successful);

    reference = ab("   a  c   ");
    result = a_b("   a  c   ");

    assert(!reference.successful);
    assert(!result.successful);

    // With another separator than spaces
    alias spaceAnd!(digit, a, b) a0b;

    assert(a0b("ab").successful);
    assert(!a0b("  a b  ").successful);

    result = a0b("012a34b567");

    assert(result.successful);
    assert(result.matches == ["a", "b"]);
    assert(result.begin == 0);
    assert(result.end == "012a34b567".length);
    assert(result.children.length == 2);
}

/// Mixin to simplify a parse tree inside a grammar
mixin template decimateTree()
{
    static ParseTree decimateTree(ParseTree p)
    {
        if(p.children.length == 0) return p;

        ParseTree[] filterChildren(ParseTree pt)
        {
            ParseTree[] result;
            foreach(child; pt.children)
            {
				import std.algorithm : startsWith;
				
                if (  (isRule(child.name) && child.matches.length != 0)
                   || !child.successful && child.children.length == 0)
                {
                    child.children = filterChildren(child);
                    result ~= child;
                }
                else if (child.name.startsWith("keep!(")) // 'keep' node are never discarded.
                                               // They have only one child, the node to keep
                {
                    result ~= child.children[0];
                }
                else // discard this node, but see if its children contain nodes to keep
                {
                    result ~= filterChildren(child);
                }
            }
            return result;
        }
        p.children = filterChildren(p);
        return p;
    }
}

/**
Discard one-child nodes and replace them with their children.
Most grammar tend to produce long one-child/one-child branches,
simplifyTree compacts these.
*/
ParseTree simplifyTree(ParseTree p)
{
    foreach(ref child; p.children)
        child = simplifyTree(child);

    if (p.children.length != 1)
        return p;
    else // linear tree
        return p.children[0];
}

/**
Returns: the number of nodes in a parse tree.
*/
size_t size(ParseTree p)
{
        size_t result = 1;
        foreach(child; p.children)
                result += size(child);
        return result;
}

/**
Generic ParseTree modifier:
predicate must be callable with a ParseTree and return a boolean.
modifier must be callable with a ParseTree and return a ParseTree.

If predicate is true on input, modify calls modifier on input and return the result.
If not, it continues with the children.
*/
ParseTree modify(alias predicate, alias modifier)(ParseTree input)
{
    foreach(ref child; input.children)
        child = modify!(predicate, modifier)(child);

    if (predicate(input))
        input = modifier(input);

    return input;
}
