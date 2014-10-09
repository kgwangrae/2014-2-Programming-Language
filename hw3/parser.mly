/*
 * SNU 4190.310 Programming Languages (Fall 2013)
 *
 * Hw3.K- Interpreter
 */
  

%{       
type declLet = Val of string * Hw3.K.exp
             | Fun of string * string list * Hw3.K.exp

exception EmptyBinding
exception ParsingError
let rec desugarLet: declLet * Hw3.K.exp -> Hw3.K.exp  =
  fun (l, e) -> 
  	match l with
		Val(x, e') -> Hw3.K.LETV(x,e',e)
		| Fun(f,x,e') -> Hw3.K.LETF(f,x,e',e)
let rec desugarVars: declLet list -> (Hw3.K.id * Hw3.K.exp) list =
  fun l ->
  	match l with
	  [] -> []
   | a::r -> 
     (match a with
        Val(x, e') -> (x,e')::(desugarVars r)
      | Fun(f,x,e') -> raise ParsingError)

%}

%token UNIT
%token <int> NUM
%token TRUE FALSE
%token <string> ID
%token PLUS MINUS STAR SLASH EQUAL LB RB LBLOCK RBLOCK NOT COLONEQ SEMICOLON COMMA PERIOD IF THEN ELSE END
%token WHILE DO LET IN READ WRITE PROC
%token LP RP LC RC
%token EOF

%nonassoc IN
%left SEMICOLON
%nonassoc DO
%nonassoc THEN
%nonassoc ELSE
%right COLONEQ
%right WRITE         
%left EQUAL LB  
%left PLUS MINUS
%left STAR SLASH
%right NOT
%left PERIOD

%start program
%type <Hw3.K.exp> program

%%

program:
       expr EOF { $1 }
    ;

expr: 
      LP expr RP { $2 }
	| UNIT {Hw3.K.UNIT}
    | MINUS NUM { Hw3.K.NUM (-$2) }
    | NUM { Hw3.K.NUM ($1) }
    | TRUE { Hw3.K.TRUE }
    | FALSE { Hw3.K.FALSE }
    | LP RP { Hw3.K.UNIT }
    | ID { Hw3.K.VAR ($1) }
    | ID LP exprs RP { Hw3.K.CALLV ($1, $3) }
    | expr PLUS expr { Hw3.K.ADD ($1, $3) }
    | expr MINUS expr  {Hw3.K.SUB ($1,$3) }
    | expr STAR expr { Hw3.K.MUL ($1,$3) }
    | expr SLASH expr { Hw3.K.DIV ($1,$3) }
    | expr EQUAL expr { Hw3.K.EQUAL ($1,$3) }
    | expr LB vars RB { match $1 with Hw3.K.VAR(x) -> Hw3.K.CALLR (x, $3) | _ -> raise ParsingError }
	| expr LB ID RB { match $1 with Hw3.K.VAR(x) -> Hw3.K.CALLR (x, [$3]) | _ -> raise ParsingError }
    | expr LB expr { Hw3.K.LESS ($1,$3) }
    | NOT expr { Hw3.K.NOT ($2) }
    | ID COLONEQ expr { Hw3.K.ASSIGN ($1,$3) }
    | expr SEMICOLON expr { Hw3.K.SEQ ($1,$3) }
    | IF expr THEN expr ELSE expr { Hw3.K.IF ($2, $4, $6) }
    | WHILE expr DO expr { Hw3.K.WHILE ($2, $4) }
    | LET decl IN expr { desugarLet($2, $4) }
    | READ ID { Hw3.K.READ ($2) }
    | WRITE expr { Hw3.K.WRITE ($2) }
	| LC RC { Hw3.K.RECORD [] }
	| LC vardecls RC { Hw3.K.RECORD (desugarVars $2) }
	| expr PERIOD ID COLONEQ expr { Hw3.K.ASSIGNF ($1,$3,$5) } 
	| expr PERIOD ID { Hw3.K.FIELD ($1,$3) }
    ;
vardecl: ID COLONEQ expr { Val ($1, $3) }
	;
vardecls: vardecl { [$1] }
    | vardecl COMMA vardecls { $1::$3 }
    ;
decl: vardecl { $1 }
    | PROC ID LP ID RP EQUAL expr {Fun ($2, [$4], $7)}
    | PROC ID LP vars RP EQUAL expr {Fun ($2, $4, $7)}
    ;
exprs: expr { [$1] }
	| expr COMMA exprs { $1::$3 }
	;
vars : ID COMMA ID { [$1; $3] }
	| ID COMMA vars { $1::$3 }
	;
%%
