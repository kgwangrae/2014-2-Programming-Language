type token =
  | UNIT
  | NUM of (int)
  | TRUE
  | FALSE
  | ID of (string)
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EQUAL
  | LB
  | RB
  | LBLOCK
  | RBLOCK
  | NOT
  | COLONEQ
  | SEMICOLON
  | IF
  | THEN
  | ELSE
  | END
  | WHILE
  | DO
  | FOR
  | TO
  | LET
  | IN
  | READ
  | WRITE
  | PROC
  | LP
  | RP
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> K.K.exp
