(*
 * SNU 4190.310 Programming Languages (Fall 2010)
 *
 * M: definition of syntax, types, runner, checker and pretty printer
 *)

(* syntax of M *)
type exp = CONST of const
         | VAR of id
         | FN of id * exp
         | APP of exp * exp
         | LET of decl * exp
         | IF of exp * exp * exp
         | BOP of bop * exp * exp
         | READ
         | WRITE of exp
         | MALLOC of exp          (*   malloc e *)
         | ASSIGN of exp * exp    (*   e := e   *)
         | BANG of exp            (*   !e       *)
         | SEQ of exp * exp       (*   e ; e    *)
         | PAIR of exp * exp      (*   (e, e)   *)
         | SEL1 of exp            (*   e.1      *)
         | SEL2 of exp            (*   e.2      *)
and const = S of string | N of int | B of bool
and id = string
and decl = REC of id * exp        (*   recursive function    *)
         | NREC of id * exp       (*   non-recursive function  *)
and bop = ADD | SUB | EQ | AND | OR

(* types in M  *)
type types = TyInt                     (* integer type *)
           | TyBool                    (* boolean type *)
           | TyString                  (* string type *)
           | TyPair of types * types   (* pair type *)
           | TyLoc of types            (* location type *)
           | TyArrow of types * types  (* function type *)

(* errors *)
exception RuntimeError of string
exception TypeError of string




module type M_Runner = sig
    val run: exp -> unit
end

module type M_SimTypeChecker = sig
    val check: exp -> types
end

module type M_PolyChecker = sig
    val check: exp -> types
end

module M_Printer = struct
    let ps = print_string
    let nl = print_newline
    let indent i =
        let rec it = function 0 -> ()
                   |  n -> ps " "; it (n-1)
        in  nl (); it i

    let rec pp n =
        function CONST (S s) -> ps s
         | CONST (N m) -> print_int m
         | CONST (B true) -> ps "true"
         | CONST (B false) -> ps "false"
         | VAR s -> ps s
         | FN (x, e) -> ps ("fn "^x^" -> "); (
                 match e with
                   FN _ -> pp (n+1) e
                 | _ -> indent (n+1); pp (n+1) e
             )
         | APP (e, e') -> pp n e; ps " "; pp n e'
         | IF (e1, e2, e3)-> ps "if "; pp n e1; ps " then ";
                          indent (n+1); pp (n+1) e2;
                          indent (n); ps "else";
                          indent (n+1); pp (n+1) e3
         | READ -> ps "read "
         | WRITE (e) -> ps "write("; pp n e; ps ")"
         | LET (d, e) ->
           let rec sugaring l acc =
		   		match l with
				  LET (d, LET (d', e)) -> sugaring (LET (d', e)) (d::acc)
                | LET (d, e) -> (List.rev (d::acc), e)
                | _  ->  raise (Invalid_argument "impossible")
           in
		   let (decls, body) = sugaring (LET (d, e)) [] in
             ps "let ";
             List.iter (fun x -> (indent(n+1); printDecl (n+1) x)) decls;
             indent n; ps "in";
             indent (n+1); pp (n+1) body;
             indent n; ps "end"
         | MALLOC e -> ps "malloc "; pp (n+1) e
         | ASSIGN (e, e') -> pp n e; ps " := ";  pp n e'
         | BANG e -> ps "!"; pp n e
         | SEQ (e, e') -> pp n e; ps ";"; indent n; pp n e'
         | PAIR (e1, e2) -> ps "("; pp n e1; ps ", "; pp n e2; ps ")"
         | SEL1 e -> pp n e; ps ".1"
         | SEL2 e -> pp n e; ps ".2"
         | BOP (op, e1, e2) -> ps "("; pp n e1;
            ps (" "^(match op with ADD -> "+" | SUB -> "-" | EQ -> "="
                | AND -> "and" | OR -> "or")^" "); pp n e2; ps ")"
    and printDecl n =
        function NREC (x, e) -> ps "val "; ps (x^" = "); pp (n+1) e
         | REC (x, e) -> ps ("rec "^x^" = "); pp (n+1) e
    let rec pp_type ty = match ty
        with TyInt -> ps "int"
         | TyBool -> ps "bool"
         | TyString  -> ps "string"
         | TyPair (tau1, tau2) ->
             ps "("; pp_type tau1; ps " , "; pp_type tau2; ps ")"
         | TyLoc tau1 -> ps "loc("; pp_type tau1; ps ")"
         | TyArrow (tau1, tau2) ->
             ps "("; pp_type tau1; ps ")" ; ps "->";
             ps "("; pp_type tau2; ps ")"

    let print = pp 0
    let printTypes ty = pp_type ty; nl()
end

(* stringifier for printing better type error message *)
module M_String = struct
    let obuf = Buffer.create 80
    let ps = Buffer.add_string obuf
    let pi = fun i -> ps (string_of_int i)
    let nl = fun () -> ps "\n"
    let indent i =
        let rec it = function 0 -> ()
            | n -> ps " "; it (n-1)
        in  nl (); it i
    let rec pp n =
        function CONST (S s) -> ps s
         | CONST (N m) -> pi m
         | CONST (B true) -> ps "true"
         | CONST (B false) -> ps "false"
         | VAR s -> ps s
         | FN (x, e) -> ps ("fn "^x^" -> "); (
                 match e with
                  FN _ -> pp (n+1) e
                 | _ -> indent (n+1); pp (n+1) e
             )
         | APP (e, e') -> pp n e; ps " "; pp n e'
         | IF (e1, e2, e3)-> ps "if "; pp n e1; ps " then ";
                          indent (n+1); pp (n+1) e2;
                          indent (n); ps "else";
                          indent (n+1); pp (n+1) e3
         | READ -> ps "read "
         | WRITE (e) -> ps "write("; pp n e; ps ")"
         | LET (d, e) ->
           let rec sugaring e acc = 
				match e with 
				  LET (d, LET (d', e)) -> sugaring (LET (d', e)) (d::acc)
                | LET (d, e) -> (List.rev (d::acc), e)
                | _ ->  raise (Invalid_argument "impossible")
           in
		   let (decls, body) = sugaring (LET (d, e)) [] in
             ps "let ";
             List.iter (fun x -> (indent(n+1); printDecl (n+1) x)) decls;
             indent n; ps "in";
             indent (n+1); pp (n+1) body;
             indent n; ps "end"
         | MALLOC e -> ps "malloc "; pp (n+1) e
         | ASSIGN (e, e') -> pp n e; ps " := ";  pp n e'
         | BANG e -> ps "!"; pp n e
         | SEQ (e, e') -> pp n e; ps ";"; indent n; pp n e'
         | PAIR (e1, e2) -> ps "("; pp n e1; ps ", "; pp n e2; ps ")"
         | SEL1 e -> pp n e; ps ".1"
         | SEL2 e -> pp n e; ps ".2"
         | BOP (op, e1, e2) -> ps "("; pp n e1;
            ps (" "^(match op with ADD -> "+" | SUB -> "-" | EQ -> "="
                | AND -> "and" | OR -> "or")^" "); pp n e2; ps ")"
    and printDecl n =
        function NREC (x, e) -> ps "val "; ps (x^" = "); pp (n+1) e
         | REC (x, e) -> ps ("rec "^x^" = "); pp (n+1) e
    let rec pp_type ty = 
		 match ty with
           TyInt -> ps "int"
         | TyBool -> ps "bool"
         | TyString  -> ps "string"
         | TyPair (tau1, tau2) ->
             ps "("; pp_type tau1; ps " , "; pp_type tau2; ps ")"
         | TyLoc tau1 -> ps "loc("; pp_type tau1; ps ")"
         | TyArrow (tau1, tau2) ->
             ps "("; pp_type tau1; ps ")" ; ps "->";
             ps "("; pp_type tau2; ps ")"
    let print = pp 0
    let printTypes ty = pp_type ty; nl()
    let string_of e = Buffer.reset obuf; print e; Buffer.contents obuf
    let string_of_types t = Buffer.reset obuf; printTypes t; Buffer.contents obuf
end
