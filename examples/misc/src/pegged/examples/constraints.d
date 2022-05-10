/**
 * This module was used to parse template constraints
 * and instantiate the various constituent,
 * to find which one is blocking a template to be instantiated.
 *
 * I have to code this again, using the D parser.
 */
module pegged.examples.constraints;

import std.conv;
import std.stdio;
import std.traits;

import pegged.grammar;



string constraintToCode(ParseResult p)
{
    string result;
    switch (p.name)
    {
        case "ConstraintExp":
            result = constraintToCode(p.children[0]);
            foreach(child; p.children[1..$])
                result ~= " || " ~ constraintToCode(child);
            return result;
        case "AndExp":
            result = constraintToCode(p.children[0]);
            foreach(child; p.children[1..$])
                result ~= " && " ~ constraintToCode(child);
            return result;
        case "Primary":
            return constraintToCode(p.children[0]);
        case "NotExp":
            return "!" ~ p.matches[0];
        case "Parens":
            return "("~constraintToCode(p.children[0])~")";
        case "Ident":
            return p.matches[0];
        case "IsExpr":
            return p.matches[0];
        default:
            break;
    }
    return result;
}

string[] generateAllConstraints(ParseResult p)
{
    string[] result;
    result ~= constraintToCode(p);
    foreach(child; p.children)
    {
        auto temp = generateAllConstraints(child);
        if (temp[0] != result[$-1])
            result ~= temp;
        else
            result ~= temp[1..$];
    }
    return result;
}

string testAllConstraints(string input)() @property
{
    string result = "string[] testResult;\n";
    string[] constraints = generateAllConstraints(Constraint(input));
    foreach(i, c; constraints)
    {
        auto mock = "mock"~to!string(i);
        result ~= "void "~mock~"(Args args) if (" ~ c ~ ") {};\n";
        result ~= "static if (!__traits(compiles, "~mock~"(args)))\n";
        result ~= "    testResult ~= \""~c~"\";\n";
    }
    return result;
}

string argListAlias(ParseResult p)
{
    string aliases;
    string associations = "enum assoc = `( ` ~ ";
    foreach(i,arg; p.children)
        if (arg.children[0].name != "TupleParameter")
        {
            aliases ~= "alias Args["~to!string(i)~"] " ~ arg.matches[0] ~ ";\n";
            associations ~= "`" ~ arg.matches[0] ~ ": ` ~ Args["~to!string(i)~"].stringof ~ `, ` ~ ";
        }
        else
        {
            aliases ~= "alias Args["~to!string(i)~"..$] " ~ arg.matches[0] ~ ";\n";
            associations ~= "`" ~ arg.matches[0] ~ ": ` ~ Args["~to!string(i)~"..$].stringof ~ `, `";
        }
    return aliases ~ associations[0..$-6]~" ~ `)`;\n";
}

string generateAllMockUps(string argList, string[] constraints)
{
    string result;
    foreach(i,constraint; constraints)
    {
        result ~= "struct Mock"~to!string(i)~argList~" if ("~constraint~") {}\n";
    }
    return result;
}

string generateAllTests(string assoc, string[] constraints)
{
    string result;
    foreach(i,constr; constraints) // to iterate at CT on the right number of mockups
    {
        string si = to!string(i);
        result ~=
"static if (__traits(compiles, Mock"~si~"!(Args)))
    pragma(msg, constraints["~si~"], ` with " ~ assoc ~ ": true`);
else
    pragma(msg, constraints["~si~"], ` with " ~ assoc ~ ": false`);\n";
    }
    return result;
}

auto testInstantiation(alias name, Args...)() @property
{
    enum constraint = getConstraint!name;
    enum argList0 = "("~ExtractTemplateArgumentList(name.stringof).matches[0] ~")";
    enum argList = Type.TemplateParametersList(argList0);
    enum aliases = argListAlias(argList);
    mixin(aliases);
    enum constraints = generateAllConstraints(ConstraintExp.parse(constraint));
    enum mockups = generateAllMockUps(argList0, constraints);
    mixin(mockups);
    enum test = generateAllTests(assoc, constraints);
    mixin(test);
    return argList;
}
