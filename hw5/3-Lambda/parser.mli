type token =
  | LAMBDA
  | DOT
  | ID of (string)
  | LP
  | RP
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.lexp
