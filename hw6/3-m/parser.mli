type token =
  | TRUE
  | FALSE
  | AND
  | OR
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | END
  | FN
  | READ
  | WRITE
  | RARROW
  | EQUAL
  | PLUS
  | MINUS
  | LP
  | RP
  | VAL
  | COLONEQ
  | BANG
  | MALLOC
  | SEMICOLON
  | REC
  | EOF
  | DOT
  | COMMA
  | NUM of (int)
  | ID of (string)
  | STRING of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> M.exp
