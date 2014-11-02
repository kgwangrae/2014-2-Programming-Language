/*
 * SNU 4190.310 Programming Languages (Fall 2014)
 * Lambda Calculus
 *
 *	Kihong Heo
 *	khheo@ropas.snu.ac.kr
 */
  

%{
exception EmptyBinding
exception ParsingError
%}

%token LAMBDA DOT
%token <string> ID
%token LP RP
%token EOF

%nonassoc DOT
%left ID LP
%left APP
%right LAMBDA

%start program
%type <Lambda.lexp> program

%%

program: exp EOF{ $1 } 

exp : 
	| ID { Lambda.Id ($1) }
	| LAMBDA ID DOT exp { Lambda.Lam ($2, $4) }
	| LP exp RP { $2 }
	| exp exp %prec APP{ Lambda.App ($1, $2) }
	;

%%
