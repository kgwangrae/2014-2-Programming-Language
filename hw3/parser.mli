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
  | COMMA
  | PERIOD
  | IF
  | THEN
  | ELSE
  | END
  | WHILE
  | DO
  | LET
  | IN
  | READ
  | WRITE
  | PROC
  | LP
  | RP
  | LC
  | RC
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Hw3.K.exp
