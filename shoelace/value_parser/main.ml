let parse (s : string) : Value_type.value_type =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.type_ Lexer.read lexbuf in
  ast
;;
