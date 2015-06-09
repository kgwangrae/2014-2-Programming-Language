/*
 * SNU 4190.310 Programming Languages (Fall 2010)
 *
 * Parser for M
 */

%{ 
exception EmptyBinding
let rec desugarLet =
  function ([], e) -> raise EmptyBinding
   | (a::[], e) -> M.LET(a,e)
   | (a::r, e) -> M.LET(a, desugarLet(r,e))

exception IncorrectSelection
let whichSel = function (e, 1) -> M.SEL1 e
			 | (e, 2) -> M.SEL2 e
			 | _ -> raise IncorrectSelection
%}
%token TRUE FALSE AND OR IF THEN ELSE LET IN END FN READ WRITE RARROW EQUAL
%token PLUS MINUS LP RP VAL COLONEQ BANG MALLOC SEMICOLON REC EOF DOT COMMA
%token <int> NUM
%token <string> ID
%token <string> STRING

%left SEMICOLON         
%right FN RARROW LET 
%right WRITE
%right COLONEQ         
%nonassoc IF THEN ELSE
%left EQUAL 
%left PLUS MINUS OR
%left AND
%right BANG MALLOC
%left DOT 
%nonassoc TRUE FALSE NUM ID STRING READ LP

%start program
%type <M.exp> program
%type <M.exp> expr
%type <M.decl> decl         

%%
program: expr EOF   {$1}
    ;
expr: aexpr {$1}
    | expr aexpr {M.APP($1,$2)}
    | expr PLUS expr {M.BOP(M.ADD,$1,$3)}
    | expr MINUS expr {M.BOP(M.SUB,$1,$3)}
    | expr EQUAL expr {M.BOP(M.EQ,$1,$3)}
    | expr AND expr {M.BOP(M.AND,$1,$3)}
    | expr OR expr {M.BOP(M.OR,$1,$3)}
    | expr SEMICOLON expr {M.SEQ ($1,$3)}
    | expr COLONEQ expr {M.ASSIGN($1,$3)}
    | expr DOT NUM {whichSel ($1,$3)}
    ;
aexpr: LP expr RP {$2}
    | NUM {M.CONST(M.N $1)}
    | STRING {M.CONST(M.S $1)}
    | TRUE {M.CONST(M.B true)}
    | FALSE {M.CONST(M.B false)}
    | ID {M.VAR($1)}
    | READ {M.READ}
    | FN ID RARROW expr {M.FN($2,$4)}
    | LET decls IN expr END {desugarLet($2,$4)}
    | IF expr THEN expr ELSE expr {M.IF($2,$4,$6)}
    | WRITE expr {M.WRITE ($2)}
    | MALLOC expr {M.MALLOC ($2)}
    | BANG expr {M.BANG ($2)}
    | LP expr COMMA expr RP {M.PAIR ($2,$4)}
    ;        
decls: decl {[$1]}
    | decls decl {$1 @ [$2]}
    ;
decl: VAL ID EQUAL expr {M.NREC($2, $4)}
    | REC ID EQUAL FN ID RARROW expr {M.REC($2,M.FN($5,$7))}
    ;
%%
