module pegged.grammar;

import std.algorithm;
import std.conv;
import std.functional;
import std.stdio;
import std.traits;
import std.typecons;
import std.typetuple;

public import pegged.peg;


alias Output delegate(Output) Action;

string[2] parseGlobalRule(string rule)
{
    //writeln("rule: ", rule);
    string[2] ruleParts;
    auto m = QualifiedIdentifier.parse(rule);
    //writeln("Ident: ", m.capture, " / ", m.next);
    ruleParts[0] = m.capture[0]; // rule name
    rule = m.next;

	mixin(inheritMixin("Parameters", `Seq!(Spacing, Lit!"(", Spacing, List!(Identifier), Lit!")") `));
	auto params = Fuse!(Parameters).parse(rule);
	if (params.success) // param list present
	{
		ruleParts[0] ~= params.capture[0];
		rule = params.next;
	}
    
    mixin(inheritMixin("LeftArrow", `Seq!(Spacing, Lit!"<-", Spacing)`));
    mixin(inheritMixin("SquigglyArrow", `Seq!(Spacing, Lit!"<~", Spacing)`));
    mixin(inheritMixin("ColonArrow", `Seq!(Spacing, Lit!"<:", Spacing)`));
    
    bool squiggly, colon;
    auto arrow  = LeftArrow.parse(rule);
    auto sarrow = SquigglyArrow.parse(rule);
    auto carrow = ColonArrow.parse(rule);
    if (sarrow.success) 
    {
        squiggly = true;
        rule = sarrow.next;
    }
    else if (carrow.success)
    {
        colon = true;
        rule = carrow.next;
    }
    else if (arrow.success)
    {
        rule = arrow.next;
    }
    else
    {
        return [ruleParts[0], "Error, bad arrow: "~rule];
    }
    
    //writeln("LeftArrow: ", m.capture, " / ", m.next);
    ruleParts[1] = parseRightHandSide(rule);
    
    if (squiggly) ruleParts[1] = "Fuse!("~ruleParts[1]~")";
    if (colon) ruleParts[1] = "Forget!("~ruleParts[1]~")";
    return ruleParts;
}


string parseRightHandSide(string rule)
{
    //writeln("Entering parseRHS with rule: ", rule);
    string[] ruleParts;
    
	bool inSequence = false;
	bool inGroup = false;
	
    bool posLookAhead = false;
    bool negLookAhead = false;
    bool addFuse = false;
    bool addForget = false;
    bool inOr = false;
    
    void munchSpace()
    {
        auto space = Spacing.parse(rule);
        if (space.success) rule = space.next;
    }
    
    string findClosing(string expr, char opening = '(', char closing = ')')
    {
        int level = 0;
        bool foundClosing;
        bool inString;
        foreach(int index, char dc; expr)
        {
            if (!inString)
            {
                if (dc == '"') 
                {
                    inString = true;
                    continue;
                }
                if (dc == opening) ++level;
                if (dc == closing) { --level; foundClosing = true;}
                if (foundClosing && level == -1) // Found end closing
                    return expr[0..index+1];
            }
            else
            {
                if (dc == '"') inString = false;
            }
        }
        return expr;
    }

    auto parseRange(string range)
    {
        //writeln("Entering parseRange with: [",range,"]");
        string[] parts;
        string result;
        bool inRange;
        while (range.length > 0)
        {
            //writeln("Testing char: ", range[0]);
            if (inRange)
            {
                parts[$-1] = "Range!('"~parts[$-1][$-2..$-1]~"','"~range[0]~"')";
                inRange = false;
            }
            else if (range[0] == '-' && parts.length > 0) // a '-' (but not in the first position)
            {
                inRange = true;
            }
            else
            {
                parts ~= ("Lit!\"" ~ range[0] ~ "\"");
            }
            
            range = range[1..$];
        }
        
        if (inRange) parts = ["ERROR: " ~ range];

        //writeln("parseRange, parts: [", parts, "]");

        if (parts.length > 1)
        {
            result = "Or!(";
            foreach(i, part; parts)
            {
                if (part.length > 5 && part[0..5] == "ERROR")
                {
                    return part;
                }
                result ~= part;
                if (i < parts.length -1)
                    result ~= ", ";
            }
            result ~= ")";
        }
        else if (parts.length == 1)
        {
            result = parts[0];
        }
        return result;
    }

    void addRule(string newRule, ref string rule)
    {
        if (newRule.length > 5 && newRule[0..5] == "ERROR") return;
            
        //writeln("************ Entering addRule with: ", newRule);
        //writeln("Rest of rule is: [",rule,"]");
        //writeln("inSequence is ", inSequence);
        
        if (posLookAhead)
        {
           ruleParts ~= ("PosLookAhead!(" ~ newRule ~ ")");
           posLookAhead = false;
           //return;
        }
        else if (negLookAhead)
        {
            ruleParts ~= ("NegLookAhead!(" ~ newRule ~ ")");
            negLookAhead = false;
            //return;
        }
        else if (addFuse)
        {
            ruleParts ~= ("Fuse!(" ~ newRule ~ ")");
            addFuse = false;
        }
        else if (addForget)
        {
            ruleParts ~= ("Forget!(" ~ newRule ~ ")");
            addForget = false;
        }
        else
        {
            //if (Identifier.parse(newRule).capture[0].length < QualifiedIdentifier.parse(newRule).capture[0].length) // qualified ident
            //    ruleParts ~= (`Wrapper!("` ~ newRule ~ `")`);
            //else
                ruleParts ~= newRule;
        }

        //writeln("RuleParts: ", ruleParts);
        //writeln("inSequence = ", inSequence);

        // Postfix operators (? + * ^ @ =)
        if (rule.length > 0)
        {
            switch (rule[0])
            {
                case '?':
                    ruleParts[$-1] = "Option!("~ruleParts[$-1]~")";
                    rule = rule[1..$];
                    break;
                case '+':
                    ruleParts[$-1] = "OneOrMore!("~ruleParts[$-1]~")";
                    rule = rule[1..$];
                    break;
                case '*':
                    ruleParts[$-1] = "ZeroOrMore!("~ruleParts[$-1]~")";
                    rule = rule[1..$];
                    break;
                case '@':
                    rule = rule[1..$]; // skipping '@'
                    auto name = QualifiedIdentifier.parse(rule);
                    if (name.success) // push the name
                    {
                        ruleParts[$-1] = "Named!("~ruleParts[$-1]~",\"" ~ name.capture[0] ~ "\")";
                        rule = rule[name.capture[0].length..$];
                    }
                    else // anonymous push
                    { 
                        ruleParts[$-1] = "PushName!("~ruleParts[$-1]~")";
                    }
                        
                    break;
                case '^':
                    rule = rule[1..$]; // skipping '^'
                    auto name = QualifiedIdentifier.parse(rule);
                    if (name.success)
                    {
                        ruleParts[$-1] = "EqualParseTree!("~ruleParts[$-1]~",\"" ~ name.capture[0] ~ "\")";
                        rule = rule[name.capture[0].length..$];
                    }               
                    break;
                case '=':
                    rule = rule[1..$]; // skipping '='
                    auto name = QualifiedIdentifier.parse(rule);
                    if (name.success)
                    {
                        ruleParts[$-1] = "EqualMatch!("~ruleParts[$-1]~",\"" ~ name.capture[0] ~ "\")";
                        rule = rule[name.capture[0].length..$];
                    }
                    else
                    {
                        ruleParts[$-1] = "FindAndPop!("~ruleParts[$-1] ~ ")";
                    }    
                    break;
                default:
                    break;
            }
        }
 
		if (inGroup) // Space-sensitive sequence
        {
            if (ruleParts[$-2].startsWith("Group!("))
                ruleParts = ruleParts[0..$-2] ~ (ruleParts[$-2][0..$-1] ~ ", " ~ ruleParts[$-1] ~ ")");
            else
                ruleParts = ruleParts[0..$-2] ~ ("Group!(" ~ ruleParts[$-2] ~ "," ~ ruleParts[$-1] ~ ")");

			inGroup = false;
        }
		
 
        if (inSequence)
        {
            
            //writeln("Entering if(inSequence)");
            //writeln("ruleParts[0..$-2]: ", ruleParts[0..$-2]);
            //writeln("ruleParts[$-2]: ", ruleParts[$-2]);
            //writeln("ruleParts[$-1]: ", ruleParts[$-1]);
            if (ruleParts[$-2].startsWith("Seq!("))
                ruleParts = ruleParts[0..$-2] ~ (ruleParts[$-2][0..$-1] ~ ", " ~ ruleParts[$-1] ~ ")");
            else
                ruleParts = ruleParts[0..$-2] ~ ("Seq!(" ~ ruleParts[$-2] ~ "," ~ ruleParts[$-1] ~ ")");

			inSequence = false;
        }
        
        // Ugly hack with \n, \t and such
        auto space = OneOrMore!(Or!(Blank, EOL)).parse(rule);
        if (space.success)
        {
            rule = space.next;
            // we munched a seq with a \n inside
            if (rule.length > 0 && rule[0] != '/') 
            // A (\n)
            // B
            // Which must be parsed as A B
                rule = " " ~ rule;
        }
        
        //writeln("************* End of addRule, ruleParts: ", ruleParts);
        //writeln("Rest of rule is : [", rule,"]");
    }

    munchSpace();
    
    while (rule.length)
    {
        char dc = rule[0];
        switch (dc)
        {
            case '(': // group
                rule = rule[1..$];
                string group = findClosing(rule);
                group = group[0..$-1];
                //writeln("Group: ", group);
                //writeln("rule before: ", rule);
                rule = rule[group.length+1..$];
                //writeln("Rule after: ", rule);
                auto groupRule = parseRightHandSide(group);
                //writeln("GroupRule: ", groupRule);
                
                addRule(groupRule, rule);
                break;
            case ' ': // sequence
                //writeln("Entering space case with : [",rule,"]");
                //skip other spaces
                munchSpace();
                //writeln("After skipping spaces: [", rule, "]");
                if (rule.length == 0) 
                {
                    //writeln("End of rule");
                    break;
                }
				if (rule[0] == '>') // space-sensitive sequence: Group
				{
					rule = rule[1..$]; // skip '>'
					munchSpace();
					inGroup = true;
					break;
				}
                if (rule[0] == '/')
                {
                    //writeln("Found /");
                    rule = rule[1..$]; // skip '|'
                    munchSpace();
                    //while(rule.length >0 && rule[0] == ' ') rule = rule[1..$];
                    //writeln("After /: [", rule, "]");
                    addRule("OR", rule);
                   break;
                }
                else
                {
                    inSequence = true; 
                    break;
                }
			case '>': // space-sensitive sequence: Group
					rule = rule[1..$]; // skip '>'
					munchSpace();
					inGroup = true;
					break;
            case '/': // or sequence
                    //writeln("Found /");
                    rule = rule[1..$];
                    munchSpace();
                    //while(rule.length >0 && rule[0] == ' ') rule = rule[1..$];
                    //writeln("After skipping spaces: [", rule, "]");
                    addRule("OR", rule);
                    break;
            case '\'': // char literal
                if (rule.length > 2 && rule[2] == '\'')
                {
                    string c = rule[1..2];
                    if (c == "\\") c ~= c;
                    if (c == "\"") c = "\\\"";
                    rule = rule[3..$];
                    addRule("Lit!\"" ~ c ~ "\"", rule);
                    break;
                }
                else
                {
                    return "ERROR: \"" ~ rule ~ "\"";
                }
            case '"': // string literal
                string literal = "Lit!\"";
                int index = 1;
                while(index < rule.length && rule[index] != '"')
                {
                    ++index;
                }
                literal ~= rule[1..index];
                if (index < rule.length)
                    rule = rule[index+1..$];
                else
                    rule = "";
                
                addRule(literal ~ "\"", rule);
                break;
            case '[': // range
                rule = rule[1..$];
                string group = findClosing(rule, '[', ']');
                group = group[0..$-1];
                //writeln("Group: ", group);
                //writeln("rule before: ", rule);
                rule = rule[group.length+1..$];
                //writeln("Rule after: ", rule);
                auto groupRule = parseRange(group);
                //writeln("GroupRule: ", groupRule);
    
                addRule(groupRule, rule);
                break;
            case '_':
                rule = rule[1..$];
                addRule("Any", rule);
                break;
            case '&':
                rule = rule[1..$];
                //addRule("POS", rule);
                posLookAhead = true;
                break;
            case '!':
                rule = rule[1..$];
                //addRule("NEG", rule);
                negLookAhead = true;
                break;
            case '~':
                rule = rule[1..$];
                //addRule("Fuse", rule);
                addFuse = true;
                break;
            case ':':
                rule = rule[1..$];
                //addRule("Forget", rule);
                addForget = true;
                break;
            default:
				//writeln("Entering default: [", rule,"]");
                auto ident = QualifiedIdentifier.parse(rule);
				//writeln(ident);
				auto toAdd = ident.capture[0];
                if (ident.success)
                {
					//writeln("Found ident: ", toAdd);
					rule = rule[toAdd.length..$];
					alias Fuse!(Seq!(Lit!"(", QualifiedIdentifier, ZeroOrMore!(Seq!(Lit!",", QualifiedIdentifier)), Lit!")")) IdentList; 
					auto params = IdentList.parse(rule);
					if (params.success)
					{
						rule = rule[params.capture[0].length .. $];
						toAdd ~= "!" ~ params.capture[0];
					}
                    addRule(toAdd, rule);
                    break;
                }
                else
                    return "ERROR: \"" ~ rule ~ "\"";
        }
    }

    //writeln("rule parts so far: ", ruleParts);

    //Dealing with the negative and positive lookahead
    int index;
    while (index < ruleParts.length)
    {
        if (ruleParts[index] == "POS")
        {
            ruleParts[index..$-1] = ("PosLookAhead!("~ruleParts[index+1]~")") ~ ruleParts[index+2..$];
            ruleParts.length = ruleParts.length -1;
        }
        
        if (ruleParts[index] == "NEG")
        {
            ruleParts[index..$-1] = ("NegLookAhead!("~ruleParts[index+1]~")") ~ ruleParts[index+2..$];
            ruleParts.length = ruleParts.length -1;
        }
        ++index;
    }

    string result;
        
    //Everything is done, we will deal with the OR's
    int i;
    
    while (i < ruleParts.length)
    {
        auto part = ruleParts[i];
        if (part == "OR")
        {
            //writeln("Result before: ", result);
            //writeln("ruleParts before: ", ruleParts);
            //writeln("We are at index ",i," (",ruleParts[i],")");
            result = result[0..$-ruleParts[i-1].length] ~ ("Or!(" ~ ruleParts[i-1] ~ ", ");
            
            while (part == "OR" && i < ruleParts.length)
            {
                part = ruleParts[i];
                result ~= ruleParts[i+1] ~ ", ";

                i = i + 2;
            }
            result = result[0..$-2] ~ ")";
            //writeln("Result after ", result);
        }
        else
            result ~= part;
        
        ++i;
    }

    //writeln("Returning from parseRHS with result: ", result);
    return result;
}

/// standard no-operation action, inserted where no action was inserted by the user.
Output NoOp(Output o) { return o;}

template InsertNoOps(rules...)
{
    static if (rules.length == 0)
        alias TypeTuple!() InsertNoOps;
    else static if (rules.length == 1 && isSomeString!(typeof(rules[0])))
        alias TypeTuple!(rules[0], NoOp) InsertNoOps;
    else static if (isSomeString!(typeof(rules[0])) && isSomeString!(typeof(rules[1])))
        alias TypeTuple!(rules[0], NoOp, InsertNoOps!(rules[1..$])) InsertNoOps;
    else static if (isSomeString!(typeof(rules[0])) && is(typeof(rules[1]) == Output delegate(Output)))
        alias TypeTuple!(rules[0], rules[1], InsertNoOps!(rules[2..$])) InsertNoOps;
    else
        static assert(false, "Bad rule/action combo: " ~ rules.stringof);
}

string ruleCode(string name, string code, string namesAA, string action)
{
    return
    "class " ~ name ~ " : " ~ code ~ " {\n"
  ~ namesAA
  ~ "static ParseResult[] filterChildren(ParseResult p)\n"
  ~ "{
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
}\n"
  ~ "static Output parse(Input input)
    {
        auto p = typeof(super).parse(input);
        if (p.name in ruleNames) // it's a grammar rule
            return " ~ action 
                     ~ "(Output(p.next,
                                p.namedCaptures,
                                ParseResult(\""~name~"\", p.success, p.capture, [p.parseTree])));
        else
            return " ~ action
                     ~ "(Output(p.next,
                                p.namedCaptures,
                                ParseResult(\""~name~"\", p.success, p.capture, filterChildren(p.parseTree))));
    }
    
    mixin(stringToInputMixin());
    }\n";
}

template FirstHalf(T...) if (T.length % 2 == 0)
{
    static if (T.length == 0)
        alias TypeTuple!() FirstHalf;
    else
        alias TypeTuple!(T[0], FirstHalf!(T[2..$])) FirstHalf;
}

template SecondHalf(T...) if (T.length % 2 == 0)
{
    static if (T.length == 0)
        alias TypeTuple!() SecondHalf;
    else
        alias TypeTuple!(T[1], SecondHalf!(T[2..$])) SecondHalf;
}



@property string grammarCode(rules...)() if (rules.length > 0)
{
    alias InsertNoOps!(rules) rulesWithActions;
    alias FirstHalf!rulesWithActions trueRules;
    alias SecondHalf!rulesWithActions actions;
    string result = "";//"class ";
    string namesAA = "\nenum bool[string] ruleNames = [";
    string startName;
	
    foreach(i, rule; trueRules)
    {
        enum parts = parseGlobalRule(rule);
        //pragma(msg, parts);
        static if(!(parts[1].length > 5 && parts[1][0..5] == "ERROR"))
        {
            static if (i == 0)
            {
                startName = parts[0];
                //result ~= startName ~ " {\n";
            }
            
            namesAA ~= "\"" ~ parts[0] ~ "\":true" ~ (i<trueRules.length-1 ? ", " : "");
        }
        else
            static assert(false, "Bad grammar, rule #" ~ to!string(i) ~ ": " ~ parts[1]);
    }
	
	namesAA ~= "];\n";
	alias Seq!(Identifier, Forget!(Lit!"("), Identifier, ZeroOrMore!(Seq!(Forget!(Lit!","), Identifier)), Forget!(Lit!")")) ParamRule; 
	
	// We put the parameterized rules first, to avoid any forward declaration problem.
	foreach(i,rule; trueRules)
	{
		enum parts = parseGlobalRule(rule);
		enum params = ParamRule.parse(parts[0]);
		//writeln(parts[0]);
		static if (params.success)
			result ~= ruleCode(parts[0], parts[1], namesAA, __traits(identifier, actions[i]));
    }
       
	foreach(i,rule; trueRules)
	{
		enum parts = parseGlobalRule(rule);
		enum params = ParamRule.parse(parts[0]);
		//writeln(parts[0]);
		static if (!params.success)
			result ~= ruleCode(parts[0], parts[1], namesAA, __traits(identifier, actions[i]));
    }
    string parse = "\nstatic Output parse(Input input)
    {
        return " ~ __traits(identifier, actions[0]) 
                 ~ "(" ~ startName ~ ".parse(input));
    }";
    
    
    return result;// ~ parse ~ "\n}";
}

mixin template Grammar(rules...)
{
    mixin(grammarCode!(rules)());
}

template isGrammar(T)
{
    enum isGrammar = __traits(compiles,
                              {
                                  void testGrammar(U...)(Grammar!U g) {}
                                  testGrammar(T.init);
                              });
}
