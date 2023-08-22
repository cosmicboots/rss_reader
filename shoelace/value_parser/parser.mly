%{
open Value_type
%}

%token BOOLEAN
%token NUMBER
%token STRING
%token EOF
%token UNION
%token QUOTE
%token UNDEFINED
%token <string> ID

%start <Value_type.value_type> type_

%%

array_fields:
    | { [] }
    | QUOTE; l = ID; QUOTE; UNION; lst = array_fields { l :: lst }
    | QUOTE; l = ID; QUOTE; EOF; lst = array_fields { l :: lst }
    | UNDEFINED; EOF; lst = array_fields { "undefined" :: lst }

type_:
    | BOOLEAN; EOF { Boolean }
    | NUMBER; EOF { Number }
    | STRING; EOF { String }
    | x = array_fields; EOF { Const x }
    ;
