{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
    parse
      | white { read lexbuf }
      | "boolean" { BOOLEAN }
      | "string" { STRING }
      | "number" { NUMBER }
      | "|" { UNION }
      | "'" { QUOTE }
      | "undefined" { UNDEFINED }
      | id { ID (Lexing.lexeme lexbuf) }
      | eof { EOF }
