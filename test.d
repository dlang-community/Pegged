/// Testing Pegged modifications.
module test;

import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.typecons;

import pegged.grammar;

enum parser = grammar(`
PROTOBUF:
    ProtoDefinition < (Import / ';')*
    Import          < "import" identifier ';' 
`);

pragma(msg, parser);

mixin(parser);

enum ctParseTree = PROTOBUF(`
    import "path/to/def.proto";

    message ProtoMessage {}
`);

pragma(msg, ctParseTree);

void main()
{
    writeln('\n',ctParseTree);
}