(*
	SNU 4190.310 Programming Languages (2014 Fall)
	Lambda Calculus

	Kihong Heo
	khheo@ropas.snu.ac.kr
*)
{
 open Parser
 exception Eof
 exception LexicalError
 let verbose1 s =  (* (print_string s; print_newline(); s) *) s
 let verbose2 s =  (* (print_string s; print_newline()) *) ()
 let comment_depth = ref 0
} 

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*

rule start =
 parse blank { start lexbuf }
     | "(*" { comment_depth :=1;
              comment lexbuf;
              start lexbuf }
     | id as id { ID id }
     | "\\" {verbose2 "+"; LAMBDA}
     | "." {verbose2 "-"; DOT}
     | "(" { verbose2 "("; LP}
     | ")" { verbose2 ")"; RP}
     | eof { verbose2 "eof"; EOF}
     | _ { raise LexicalError}

and comment = parse
     "(*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*)" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise Eof}
   | _   {comment lexbuf}
