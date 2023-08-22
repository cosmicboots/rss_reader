%{
open Value_type
%}

%token BOOLEANLIT
%token NUMBERLIT
%token STRINGLIT
%token <string> STRING
%token PIPE
%token UNDEFINED
%token LBRACKET
%token RBRACKET
%token EOF
%token LPAREN
%token RPAREN
%token ARROW
%token COLON
%token <string> ID

%start <Value_type.value_type> type_

%%

value:
    | l = ID { Const l }
    | s = STRING { String s }
    | NUMBERLIT { NumberT }
    | STRINGLIT { StringT }
    | BOOLEANLIT { BooleanT }
    | UNDEFINED { Const "undefined" }
    | v = value; LBRACKET; RBRACKET { List v }
    | LPAREN; ID; COLON; i = value; RPAREN; ARROW; o = value { Function (i, o) }

array_fields_end:
    | { [] }
    | v = value; PIPE; lst = array_fields_end { v :: lst }
    | v = value; EOF; lst = array_fields_end { v :: lst }

array_fields:
    | v = value; PIPE; lst = array_fields_end { v :: lst }

type_end:
    | BOOLEANLIT; EOF { BooleanT }
    | NUMBERLIT; EOF { NumberT }
    | STRINGLIT; EOF { StringT }
    | x = array_fields; EOF { Union x }
    | v = value; EOF { v }
    ;

type_:
    | PIPE; t = type_end { t }
    | t = type_end { t }
