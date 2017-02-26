/**
 * Recreate the EP parser from the grammar.
 */

import pegged.grammar;
import epgrammar;

void main()
{
	auto header = `
PT failOnWordSymbol(PT)(PT p)
{
    import std.uni: sicmp;
    // 6.1.2
    if (sicmp(p.matches[0], "AND") == 0 ||
        sicmp(p.matches[0], "AND_THEN") == 0 ||
        sicmp(p.matches[0], "ARRAY") == 0 ||
        sicmp(p.matches[0], "BEGIN") == 0 ||
        sicmp(p.matches[0], "BINDABLE") == 0 ||
        sicmp(p.matches[0], "CASE") == 0 ||
        sicmp(p.matches[0], "CONST") == 0 ||
        sicmp(p.matches[0], "DIV") == 0 ||
        sicmp(p.matches[0], "DO") == 0 ||
        sicmp(p.matches[0], "DOWNTO") == 0 ||
        sicmp(p.matches[0], "ELSE") == 0 ||
        sicmp(p.matches[0], "END") == 0 ||
        sicmp(p.matches[0], "EXPORT") == 0 ||
        sicmp(p.matches[0], "FILE") == 0 ||
        sicmp(p.matches[0], "FOR") == 0 ||
        sicmp(p.matches[0], "FUNCTION") == 0 ||
        sicmp(p.matches[0], "GOTO") == 0 ||
        sicmp(p.matches[0], "IF") == 0 ||
        sicmp(p.matches[0], "IMPORT") == 0 ||
        sicmp(p.matches[0], "IN") == 0 ||
        sicmp(p.matches[0], "LABEL") == 0 ||
        sicmp(p.matches[0], "MOD") == 0 ||
        sicmp(p.matches[0], "MODULE") == 0 ||
        sicmp(p.matches[0], "NIL") == 0 ||
        sicmp(p.matches[0], "NOT") == 0 ||
        sicmp(p.matches[0], "OF") == 0 ||
        sicmp(p.matches[0], "ONLY") == 0 ||
        sicmp(p.matches[0], "OR") == 0 ||
        sicmp(p.matches[0], "OR_ELSE") == 0 ||
        sicmp(p.matches[0], "OTHERWISE") == 0 ||
        sicmp(p.matches[0], "PACKED") == 0 ||
        sicmp(p.matches[0], "POW") == 0 ||
        sicmp(p.matches[0], "PROCEDURE") == 0 ||
        sicmp(p.matches[0], "PROGRAM") == 0 ||
        sicmp(p.matches[0], "PROTECTED") == 0 ||
        sicmp(p.matches[0], "QUALIFIED") == 0 ||
        sicmp(p.matches[0], "RECORD") == 0 ||
        sicmp(p.matches[0], "REPEAT") == 0 ||
        sicmp(p.matches[0], "RESTRICTED") == 0 ||
        sicmp(p.matches[0], "SET") == 0 ||
        sicmp(p.matches[0], "THEN") == 0 ||
        sicmp(p.matches[0], "TO") == 0 ||
        sicmp(p.matches[0], "TYPE") == 0 ||
        sicmp(p.matches[0], "UNTIL") == 0 ||
        sicmp(p.matches[0], "VALUE") == 0 ||
        sicmp(p.matches[0], "VAR") == 0 ||
        sicmp(p.matches[0], "WHILE") == 0 ||
        sicmp(p.matches[0], "WITH") == 0 ||
    // Prospero extensions:
        sicmp(p.matches[0], "ABSTRACT") == 0 ||
        sicmp(p.matches[0], "CLASS") == 0 ||
        sicmp(p.matches[0], "CONSTRUCTOR") == 0 ||
        sicmp(p.matches[0], "DESTRUCTOR") == 0 ||
        sicmp(p.matches[0], "EXCEPT") == 0 ||
        sicmp(p.matches[0], "IS") == 0 ||
        sicmp(p.matches[0], "PROPERTY") == 0 ||
        sicmp(p.matches[0], "REM") == 0 ||
        sicmp(p.matches[0], "SHL") == 0 ||
        sicmp(p.matches[0], "SHR") == 0 ||
        sicmp(p.matches[0], "TRY") == 0 ||
        sicmp(p.matches[0], "VIEW") == 0 ||
        sicmp(p.matches[0], "XOR") == 0)
    {
        p.successful = false;
    }
    return p;
}
`;
	asModule!()("epparser", "source/epparser", EPgrammar, header);
}
