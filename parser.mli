type token =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQUALS
  | COMMA
  | EOF
  | LITERAL of (int)
  | VARIABLE of (int)

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
