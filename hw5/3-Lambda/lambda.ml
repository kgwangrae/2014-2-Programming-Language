(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * Lambda Calculus
 *)

type lexp = Id of string
		  | Lam of string * lexp
		  | App of lexp * lexp

let rec pp lexp i =
	match lexp with
	  Id s -> print_string (indent i); print_string "Id "; print_string s
	| Lam (s, e) -> print_string (indent i); print_string ("Lam ("^s^",\n"); pp e (i + 1);print_string ("\n"^(indent i)^")")
	| App (e1, e2) -> print_string (indent i); print_string ("App (\n"); pp e1 (i+1); print_string ", \n"; pp e2 (i+1); print_string ("\n"^(indent i)^")")
and indent i =
	if i = 0 then ""
	else if i = 1 then "  "
	else "  "^(indent (i-1))
