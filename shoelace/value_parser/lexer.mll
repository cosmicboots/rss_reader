{
open Parser
exception SyntaxError of string
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_' '/' '-']
let id = (letter|digit)+

rule read = 
  parse
  | white { read lexbuf }
  | "'" { read_string (Buffer.create 17) lexbuf }
  | "boolean" { BOOLEANLIT }
  | "string" { STRINGLIT }
  | "number" { NUMBERLIT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ":" { COLON }
  | "=>" { ARROW }
  | "|" { PIPE }
  | "[" { LBRACKET } 
  | "]" { RBRACKET } 
  | "undefined" { UNDEFINED }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }

and read_string buf =
  parse
  | "'"       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
