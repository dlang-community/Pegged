module pegged.examples.python;


import pegged.grammar;

@safe:

// https://docs.python.org/3/reference/grammar.html
// 3.10.3 Documentation » The Python Language Reference » 10. Full Grammar specification
enum string pythonGrammar = `
Python:

# PEG grammar for Python

file <- statements? # ENDMARKER
interactive <  statement_newline
eval <  expressions NEWLINE* # ENDMARKER
func_type <  "(" (type_expressions)? ")" "->" expression NEWLINE* # ENDMARKER
fstring <  star_expressions

# type_expressions allow */** but ignore them
type_expressions < 
      ((expression ("," expression)*) "," "*" expression "," "**" expression)
    / ((expression ("," expression)*) "," "*" expression)
    / ((expression ("," expression)*) "," "**" expression)
    / ("*" expression "," "**" expression)
    / ("*" expression)
    / ("**" expression)
    / (expression ("," expression)*)

statements <  statement+
statement <  compound_stmt  / simple_stmts
statement_newline <
      (compound_stmt NEWLINE)
    / simple_stmts
    / NEWLINE
#   / ENDMARKER
simple_stmts <
      (simple_stmt !";" NEWLINE)  # Not needed, there for speedup
    / ((simple_stmt (";" simple_stmt)*) (";")? NEWLINE)
# NOTE =  assignment MUST precede expression, else parsing a simple assignment
# will throw a SyntaxError.
simple_stmt < 
      assignment
    / star_expressions
    / return_stmt
    / import_stmt
    / raise_stmt
    / "pass"
    / del_stmt
    / yield_stmt
    / assert_stmt
    / "break"
    / "continue"
    / global_stmt
    / nonlocal_stmt
compound_stmt < 
      function_def
    / if_stmt
    / class_def
    / with_stmt
    / for_stmt
    / try_stmt
    / while_stmt
    / match_stmt

# NOTE: annotated_rhs may start with "yield"; yield_expr must start with "yield"
assignment < 
      (NAME ":" expression ("=" annotated_rhs )?)
    / ((("(" single_target ")")
         / single_subscript_attribute_target) ":" expression ("=" annotated_rhs )?)
    / ((star_targets "=" )+ (yield_expr / star_expressions) !"=" (TYPE_COMMENT)?)
    / (single_target augassign   (yield_expr / star_expressions))

augassign < 
      "+="
    / "-="
    / "*="
    / "@="
    / "/="
    / "%="
    / "&="
    / "|="
    / "^="
    / "<<="
    / ">>="
    / "**="
    / "//="

global_stmt <  "global" (NAME ("," NAME)*)
nonlocal_stmt <  "nonlocal" (NAME ("," NAME)*)

yield_stmt <  yield_expr

assert_stmt <  ("assert" expression ("," expression )?)

del_stmt < 
      ("del" del_targets &(";" / NEWLINE))

import_stmt <  import_name / import_from
import_name <  ("import" dotted_as_names)
# note below <  the ("." | "...") is necessary because "..." is tokenized as ELLIPSIS
import_from < 
      ("from" ("..." / ".")* dotted_name "import" import_from_targets)
    / ("from" ("..." / ".")+ "import" import_from_targets)
import_from_targets < 
      ("(" import_from_as_names (",")? ")")
    / (import_from_as_names !",")
    / "*"
import_from_as_names < 
      (import_from_as_name ("," import_from_as_name)*)
import_from_as_name < 
      NAME ("as" NAME )?
dotted_as_names < 
      (dotted_as_name ("," dotted_as_name)*)
dotted_as_name < 
      (dotted_name ("as" NAME )?)
dotted_name < 
      (NAME ("." NAME)+)
    / NAME

if_stmt < 
      ("if" named_expression ":" block elif_stmt)
    / ("if" named_expression ":" block (else_block)?)
elif_stmt < 
      ("elif" named_expression ":" block elif_stmt)
    / ("elif" named_expression ":" block (else_block)?)
else_block < 
      ("else" ":" block)

while_stmt < 
      ("while" named_expression ":" block (else_block)?)

for_stmt < 
      ("for" star_targets "in"   star_expressions ":" (TYPE_COMMENT)? block (else_block)?)
    / (ASYNC "for" star_targets "in"   star_expressions ":" (TYPE_COMMENT)? block (else_block)?)

with_stmt < 
      ("with" "(" (with_item ("," with_item)*) ","? ")" ":" block)
    / ("with" (with_item ("," with_item)*) ":" (TYPE_COMMENT)? block)
    / (ASYNC "with" "(" (with_item ("," with_item)*) ","? ")" ":" block)
    / (ASYNC "with" (with_item ("," with_item)*) ":" (TYPE_COMMENT)? block)

with_item < 
      (expression "as" star_target &("," / ")" / ":"))
    / (expression)

try_stmt < 
      ("try" ":" block finally_block)
    / ("try" ":" block except_block+ (else_block)? (finally_block)?)
except_block < 
      ("except" expression ("as" NAME )? ":" block)
    / ("except" ":" block)
finally_block < 
      ("finally" ":" block)

match_stmt < 
      ("match" subject_expr ":" NEWLINE INDENT case_block+ DEDENT)
subject_expr < 
      (star_named_expression "," star_named_expressions?)
    / (named_expression)
case_block < 
      ("case" patterns guard? ":" block)
guard <  "if" named_expression

patterns < 
      (open_sequence_pattern)
    / (pattern)
pattern < 
      (as_pattern)
    / (or_pattern)
as_pattern < 
      (or_pattern "as" pattern_capture_target)
or_pattern < 
      ((closed_pattern ("|" closed_pattern)*))
closed_pattern < 
      (literal_pattern)
    / (capture_pattern)
    / (wildcard_pattern)
    / (value_pattern)
    / (group_pattern)
    / (sequence_pattern)
    / (mapping_pattern)
    / (class_pattern)

# Literal patterns are used for equality and identity constraints
literal_pattern < 
      (signed_number !("+" / "-"))
    / (complex_number)
    / (strings)
    / ("None")
    / ("True")
    / ("False")

# Literal expressions are used to restrict permitted mapping pattern keys
literal_expr < 
      (signed_number !("+" / "-"))
    / (complex_number)
    / (strings)
    / ("None")
    / ("True")
    / ("False")

complex_number < 
      (signed_real_number "+" imaginary_number)
    / (signed_real_number "-" imaginary_number)

signed_number < 
      (NUMBER)
    / ("-" NUMBER)

signed_real_number < 
      (real_number)
    / ("-" real_number)

real_number < 
      (NUMBER)

imaginary_number < 
      (NUMBER)

capture_pattern < 
      (pattern_capture_target)

pattern_capture_target < 
      (!"_" NAME !("." / "(" / "="))

wildcard_pattern < 
      ("_")

value_pattern < 
      (attr !("." / "(" / "="))
attr < 
      (name_or_attr "." NAME)
name_or_attr < 
      (attr)
    / (NAME)

group_pattern < 
      ("(" pattern ")")

sequence_pattern < 
      ("[" maybe_sequence_pattern? "]")
    / ("(" open_sequence_pattern? ")")
open_sequence_pattern < 
      (maybe_star_pattern "," maybe_sequence_pattern?)
maybe_sequence_pattern < 
      ((maybe_star_pattern ("," maybe_star_pattern)*) ","?)
maybe_star_pattern < 
      (star_pattern)
    / (pattern)
star_pattern < 
      ("*" pattern_capture_target)
    / ("*" wildcard_pattern)

mapping_pattern < 
      ("{" "}")
    / ("{" double_star_pattern ","? "}")
    / ("{" items_pattern "," double_star_pattern ","? "}")
    / ("{" items_pattern ","? "}")
items_pattern < 
      ((key_value_pattern ("," key_value_pattern)*))
key_value_pattern < 
      ((literal_expr / attr) ":" pattern)
double_star_pattern < 
      ("**" pattern_capture_target)

class_pattern < 
      (name_or_attr "(" ")")
    / (name_or_attr "(" positional_patterns ","? ")")
    / (name_or_attr "(" keyword_patterns ","? ")")
    / (name_or_attr "(" positional_patterns "," keyword_patterns ","? ")")
positional_patterns < 
      ((pattern ("," pattern)*))
keyword_patterns < 
      ((keyword_pattern ("," keyword_pattern)*))
keyword_pattern < 
      (NAME "=" pattern)

return_stmt < 
      ("return" (star_expressions)?)

raise_stmt < 
      ("raise" expression ("from" expression )?)
    / ("raise")

function_def < 
      (decorators function_def_raw)
    / (function_def_raw)

function_def_raw < 
      ("def" NAME "(" (params)? ")" ("->" expression )? ":" (func_type_comment)? block)
    / (ASYNC "def" NAME "(" (params)? ")" ("->" expression )? ":" (func_type_comment)? block)
func_type_comment < 
      (NEWLINE TYPE_COMMENT &(NEWLINE INDENT))   # Must be followed by indented block
    / (TYPE_COMMENT)

params < 
      (parameters)

parameters < 
      (slash_no_default param_no_default* param_with_default* (star_etc)?)
    / (slash_with_default param_with_default* (star_etc)?)
    / (param_no_default+ param_with_default* (star_etc)?)
    / (param_with_default+ (star_etc)?)
    / (star_etc)

# Some duplication here because we can't write ("," / &")"),
# which is because we don't support empty alternatives (yet).
#
slash_no_default < 
      (param_no_default+ "/" ",")
    / (param_no_default+ "/" &")")
slash_with_default < 
      (param_no_default* param_with_default+ "/" ",")
    / (param_no_default* param_with_default+ "/" &")")

star_etc < 
      ("*" param_no_default param_maybe_default* (kwds)?)
    / ("*" "," param_maybe_default+ (kwds)?)
    / (kwds)

kwds <  "**" param_no_default

# One parameter.  This *includes* a following comma and type comment.
#
# There are three styles:
# - No default
# - With default
# - Maybe with default
#
# There are two alternative forms of each, to deal with type comments:
# - Ends in a comma followed by an optional type comment
# - No comma, optional type comment, must be followed by close paren
# The latter form is for a final parameter without trailing comma.
#
param_no_default < 
      (param "," TYPE_COMMENT?)
    / (param TYPE_COMMENT? &")")
param_with_default < 
      (param default_expr "," TYPE_COMMENT?)
    / (param default_expr TYPE_COMMENT? &")")
param_maybe_default < 
      (param default_expr? "," TYPE_COMMENT?)
    / (param default_expr? TYPE_COMMENT? &")")
param <  NAME annotation?

annotation <  ":" expression
default_expr <  "=" expression

decorators <  ("@" named_expression NEWLINE )+

class_def < 
      (decorators ClassDeclaration)
    / (ClassDeclaration)

ClassDeclaration < 
      ("class" NAME ("(" arguments? ")")? ":" block)

block < 
      (NEWLINE INDENT statements DEDENT)
    / (simple_stmts)

star_expressions < 
      (star_expression ("," star_expression )+ (",")?)
    / (star_expression ",")
    / (star_expression)
star_expression < 
      ("*" bitwise_or)
    / (expression)

star_named_expressions <  (star_named_expression ("," star_named_expression)*) (",")?
star_named_expression < 
      ("*" bitwise_or)
    / (named_expression)


assignment_expression < 
      (NAME ":="   expression)

named_expression < 
      (assignment_expression)
    / (expression !":=")

annotated_rhs <  yield_expr / star_expressions

expressions < 
      (expression ("," expression )+ (",")?)
    / (expression ",")
    / (expression)
expression < 
      (disjunction "if" disjunction "else" expression)
    / (disjunction)
    / (lambdef)

lambdef < 
      ("lambda" (lambda_params)? ":" expression)

lambda_params < 
      (lambda_parameters)

# lambda_parameters etc. duplicates parameters but without annotations
# or type comments, and if there's no comma after a parameter, we expect
# a colon, not a close parenthesis.  (For more, see parameters above.)
#
lambda_parameters < 
      (lambda_slash_no_default lambda_param_no_default* lambda_param_with_default* (lambda_star_etc)?)
    / (lambda_slash_with_default lambda_param_with_default* (lambda_star_etc)?)
    / (lambda_param_no_default+ lambda_param_with_default* (lambda_star_etc)?)
    / (lambda_param_with_default+ (lambda_star_etc)?)
    / (lambda_star_etc)

lambda_slash_no_default < 
      (lambda_param_no_default+ "/" ",")
    / (lambda_param_no_default+ "/" &":")
lambda_slash_with_default < 
      (lambda_param_no_default* lambda_param_with_default+ "/" ",")
    / (lambda_param_no_default* lambda_param_with_default+ "/" &":")

lambda_star_etc < 
      ("*" lambda_param_no_default lambda_param_maybe_default* (lambda_kwds)?)
    / ("*" "," lambda_param_maybe_default+ (lambda_kwds)?)
    / (lambda_kwds)

lambda_kwds <  "**" lambda_param_no_default

lambda_param_no_default < 
      (lambda_param ",")
    / (lambda_param &":")
lambda_param_with_default < 
      (lambda_param default_expr ",")
    / (lambda_param default_expr &":")
lambda_param_maybe_default < 
      (lambda_param default_expr? ",")
    / (lambda_param default_expr? &":")
lambda_param <  NAME

disjunction < 
      (conjunction ("or" conjunction )+)
    / (conjunction)
conjunction < 
      (inversion ("and" inversion )+)
    / (inversion)
inversion < 
      ("not" inversion)
    / (comparison)
comparison < 
      (bitwise_or compare_op_bitwise_or_pair+)
    / (bitwise_or)
compare_op_bitwise_or_pair < 
      (eq_bitwise_or)
    / (noteq_bitwise_or)
    / (lte_bitwise_or)
    / (lt_bitwise_or)
    / (gte_bitwise_or)
    / (gt_bitwise_or)
    / (notin_bitwise_or)
    / (in_bitwise_or)
    / (isnot_bitwise_or)
    / (is_bitwise_or)
eq_bitwise_or <  "==" bitwise_or
noteq_bitwise_or < 
      (("!=" ) bitwise_or)
lte_bitwise_or <  "<=" bitwise_or
lt_bitwise_or <  "<" bitwise_or
gte_bitwise_or <  ">=" bitwise_or
gt_bitwise_or <  ">" bitwise_or
notin_bitwise_or <  "not" "in" bitwise_or
in_bitwise_or <  "in" bitwise_or
isnot_bitwise_or <  "is" "not" bitwise_or
is_bitwise_or <  "is" bitwise_or

bitwise_or < 
      (bitwise_or "|" bitwise_xor)
    / (bitwise_xor)
bitwise_xor < 
      (bitwise_xor "^" bitwise_and)
    / (bitwise_and)
bitwise_and < 
      (bitwise_and "&" shift_expr)
    / (shift_expr)
shift_expr < 
      (shift_expr "<<" sum)
    / (shift_expr ">>" sum)
    / (sum)

sum < 
      (sum "+" term)
    / (sum "-" term)
    / (term)
term < 
      (term "*" factor)
    / (term "/" factor)
    / (term "//" factor)
    / (term "%" factor)
    / (term "@" factor)
    / (factor)
factor < 
      ("+" factor)
    / ("-" factor)
    / ("~" factor)
    / (power)
power < 
      (await_primary "**" factor)
    / (await_primary)
await_primary <
      ("await" primary)
    / (primary)
primary <
      (primary "." NAME)
    / (primary genexp)
    / (primary "(" (arguments)? ")")
    / (primary "[" slices "]")
    / (atom)

slices < 
      (slice !",")
    / ((slice ("," slice)*) (",")?)
slice < 
      ((expression)? ":" (expression)? (":"  expression? )?)
    / (named_expression)
atom < 
      (NAME)
    / ("True")
    / ("False")
    / ("None")
    / (strings)
    / (NUMBER)
    / ((tuple_expr / group / genexp))
    / ((list / listcomp))
    / ((dict / set / dictcomp / setcomp))
    / ("...")

strings <  STRING+
list < 
      ("[" (star_named_expressions)? "]")
listcomp < 
      ("[" named_expression for_if_clauses "]")
tuple_expr < 
      "(" (star_named_expression ","  star_named_expressions?  )? ")"
group < 
      ("(" (yield_expr / named_expression) ")")
genexp < 
      ("(" ( assignment_expression / (expression !":=")) for_if_clauses ")")
set <  "{" star_named_expressions "}"
setcomp < 
      ("{" named_expression for_if_clauses "}")
dict < 
      ("{" (double_starred_kvpairs)? "}")

dictcomp < 
      ("{" kvpair for_if_clauses "}")
double_starred_kvpairs <  (double_starred_kvpair ("," double_starred_kvpair)*) (",")?
double_starred_kvpair < 
      ("**" bitwise_or)
    / (kvpair)
kvpair <  expression ":" expression
for_if_clauses < 
      (for_if_clause+)
for_if_clause < 
      (ASYNC "for" star_targets "in"   disjunction ("if" disjunction )*)
    / ("for" star_targets "in"   disjunction ("if" disjunction )*)

yield_expr < 
      ("yield" "from" expression)
    / ("yield" (star_expressions)?)

arguments < 
      (args (",")? &")")
args < 
      ((args_helper ("," args_helper)*) ("," kwargs )?)
    / (kwargs)

args_helper < ((starred_expression / ( (assignment_expression / expression) !":=")) !"=")
kwargs < 
      ((kwarg_or_starred ("," kwarg_or_starred)*) "," (kwarg_or_double_starred ("," kwarg_or_double_starred)*))
    / ((kwarg_or_starred ("," kwarg_or_starred)*))
    / ((kwarg_or_double_starred ("," kwarg_or_double_starred)*))
starred_expression < 
      ("*" expression)
kwarg_or_starred < 
      (NAME "=" expression)
    / (starred_expression)
kwarg_or_double_starred < 
      (NAME "=" expression)
    / ("**" expression)

# NOTE: star_targets may contain *bitwise_or, targets may not.
star_targets < 
      (star_target !",")
    / (star_target ("," star_target )* (",")?)
star_targets_list_seq <  (star_target ("," star_target)*) (",")?
star_targets_tuple_seq < 
      (star_target ("," star_target )+ (",")?)
    / (star_target ",")
star_target < 
      ("*" (!"*" star_target))
    / (target_with_star_atom)
target_with_star_atom < 
      (t_primary "." NAME !t_lookahead)
    / (t_primary "[" slices "]" !t_lookahead)
    / (star_atom)
star_atom < 
      (NAME)
    / ("(" target_with_star_atom ")")
    / ("(" (star_targets_tuple_seq)? ")")
    / ("[" (star_targets_list_seq)? "]")

single_target < 
      (single_subscript_attribute_target)
    / (NAME)
    / ("(" single_target ")")
single_subscript_attribute_target < 
      (t_primary "." NAME !t_lookahead)
    / (t_primary "[" slices "]" !t_lookahead)

del_targets <  (del_target ("," del_target)*) (",")?
del_target < 
      (t_primary "." NAME !t_lookahead)
    / (t_primary "[" slices "]" !t_lookahead)
    / (del_t_atom)
del_t_atom < 
      (NAME)
    / ("(" del_target ")")
    / ("(" (del_targets)? ")")
    / ("[" (del_targets)? "]")

t_primary < 
      (t_primary "." NAME &t_lookahead)
    / (t_primary "[" slices "]" &t_lookahead)
    / (t_primary genexp &t_lookahead)
    / (t_primary "(" (arguments)? ")" &t_lookahead)
    / (atom &t_lookahead)
t_lookahead <  "(" / "[" / "."

STRING <- doublequote (DQChar)* doublequote StringPostfix?

DQChar <- EscapeSequence
        / (!doublequote .)

EscapeSequence <- backslash ( quote
    / doublequote
    / backslash
    / [abfnrtv]
    / ('x' HexDigit HexDigit)
    / ('u' HexDigit HexDigit HexDigit HexDigit)
    / ('U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit)
    )

StringPostfix < "c" / "w" / "d"


ASYNC < "async"

NAME <~  (!Keyword [a-zA-Z_][a-zA-Z0-9_]*)
# https://docs.python.org/3/reference/lexical_analysis.html#keywords
Keyword < 
    "False"   / "await"    / "else"    / "import"   / "pass"
  / "None"    / "break"    / "except"  / "in"       / "raise"
  / "True"    / "class"    / "finally" / "is"       / "return"
  / "and"     / "continue" / "for"     / "lambda"   / "try"
  / "as"      / "def"      / "from"    / "nonlocal" / "while"
  / "assert"  / "del"      / "global"  / "not"      / "with"
  / "async"   / "elif"     / "if"      / "or"       / "yield"
  / "type"

NUMBER < IntegerLiteral / FloatLiteral

IntegerLiteral <- DecimalInteger
                / BinaryInteger
                / HexadecimalInteger

DecimalInteger < Integer IntegerSuffix?

Integer <- digit (digit/"_")*

IntegerSuffix < "Lu" / "LU" / "uL" / "UL"
               / "L" / "u" / "U"

BinaryInteger < ("0b" / "0B") [01] ([01] / "_")*

HexadecimalInteger < ("0x"/"0X") HexDigit (HexDigit / "_")*

digit < [0-9]
HexDigit < [0-9a-fA-F]

FloatLiteral < Sign? Integer "." Integer? (("e" / "E") Sign? Integer)?

Sign < ("-" / "+")?

INDENT < ("\t" / " ")+
DEDENT < endOfLine
NEWLINE <- endOfLine  # Comment?

TYPE_COMMENT < "#" "type" ":" NAME endOfLine

Comment <- 
           LineComment
LineComment <~ :'#' (!endOfLine .)* :endOfLine

`;


mixin(grammar(pythonGrammar));

unittest
{
    assert(Python(`m = "Hello";`).toString == q"EOS
Python[0, 0][]
 +-Python.file[0, 0][]
EOS"); // A clear sign that this grammar is malfunctioning!!
}
