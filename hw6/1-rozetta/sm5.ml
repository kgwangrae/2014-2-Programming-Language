(*
 * SNU 4190.310 Programming Languages 
 *
 * SM5
 *)

module type SM5 = 
sig
  type cmd = 
         PUSH of obj | POP | STORE | LOAD | JTR of command * command
       | MALLOC | BOX of int | UNBOX of string | BIND of string | UNBIND
       | GET | PUT | CALL | ADD | SUB | MUL | DIV | EQ | LESS | NOT
   and obj = Val of value | Id of string | Fn of string * command
   and value = Z of int | B of bool | L of loc | Unit | R of record
   and record
   and loc
   and command = cmd list

  val empty_command : command

  val print : command -> unit
  val run : command -> unit
end

module Sm5 : SM5 =
struct
  type cmd = 
         PUSH of obj | POP | STORE | LOAD | JTR of command * command
       | MALLOC | BOX of int | UNBOX of string | BIND of string | UNBIND
       | GET | PUT | CALL | ADD | SUB | MUL | DIV | EQ | LESS | NOT
   and obj = Val of value | Id of string | Fn of string * command
   and svalue = V of value | P of proc | M of map
   and value = Z of int | B of bool | L of loc | Unit | R of record
   and record = map list
   and loc = int * int
   and map = string * svalue
   and proc = string * command * environment
   and stack = svalue list
   and memory = (loc * value) list
   and environment = map list
   and command = cmd list
   and continuation = (command * environment) list
   
  exception RunError of stack * memory * environment * command * continuation
  exception Unbound_id of string
  exception Unbound_loc of int * int
  exception End

  let empty_command = []

  let (@?) l x = snd (List.find (fun y -> x = fst y) l)
  let fsts l = List.map fst l 
  let rec rangecheck l r1 r2 =
  	match l with
	  [] -> true
	| h::tl -> ((r1 @? h) = (r2 @? h)) && (rangecheck tl r1 r2)

  open Format

  let rec print_seq f g l =
    match l with
      [] -> ()
    | [h] -> f h
    | h::t -> f h; g h; print_seq f g t
  let printid id = print_string "\"";print_string id;print_string "\""
  let rec printv v =
    match v with
      Z i -> print_string "Sm5.Z ";print_int i
    | B b -> print_string "Sm5.B ";if b then print_string "true" else print_string "false"
    | R [] -> print_string "Sm5.R []"
    | R (h::t) ->
        let pf t = 
			match t with
			  (x, V(L(l1, l2))) -> print_string "(";printid x;print_string ",Sm5.V(";printv (L(l1,l2));print_string ")"
   			  | _ -> raise (Invalid_argument "non Loc in Record")
        in  print_string "Sm5.R ["; pf h; List.iter (fun f -> print_string ";"; pf f) t; 
            print_string "]"
    | Unit -> print_string "Sm5.Unit" 
    | L (b,o) -> print_string "Sm5.L(";print_int b;print_string ",";print_int o;print_string ")"
  let rec printp indent p = 
    match p with
      Val v -> printv v
    | Id x -> print_string "Sm5.Id ";printid x
    | Fn(x,c) -> print_string "Sm5.Fn(";printid x;print_string ",\n"; print_cmd (indent^"  ") c; print_string ")"
  and printc indent c = print_string indent;
    (match c with
         PUSH p -> print_string "Sm5.PUSH("; printp indent p;print_string ")"
       | POP -> print_string "Sm5.POP"
       | STORE -> print_string "Sm5.STORE"
       | LOAD -> print_string "Sm5.LOAD"
       | JTR(c1,c2) -> 
           print_string "Sm5.JTR(\n"; print_cmd (indent^"  ") c1; print_string ",\n";
           print_cmd (indent^"  ") c2; print_string ")"
       | MALLOC -> print_string "Sm5.MALLOC"
       | BOX z -> print_string "Sm5.BOX ";print_int z
       | UNBOX x -> print_string "Sm5.UNBOX ";printid x
       | BIND x -> print_string "Sm5.BIND ";printid x
       | UNBIND -> print_string "Sm5.UNBIND"
       | GET -> print_string "Sm5.GET"
       | PUT -> print_string "Sm5.PUT"
       | CALL -> print_string "Sm5.CALL"
       | ADD -> print_string "Sm5.ADD"
       | SUB -> print_string "Sm5.SUB"
       | MUL -> print_string "Sm5.MUL"
       | DIV -> print_string "Sm5.DIV"
       | EQ -> print_string "Sm5.EQ"
       | LESS -> print_string "Sm5.LESS"
       | NOT -> print_string "Sm5.NOT")
  and print_cmd indent l =
    print_string (indent^"[");
    print_seq (printc indent) (fun _ -> print_string ";\n") l;
    print_string (indent^"]")
  and print l = print_cmd "" l;print_newline();flush stdout

  let loccount = ref 0
      (* create new loc *)
  let newl () = loccount := !loccount + 1; (!loccount,0)

  let rec eval (s,m,e,c,k) = 
	eval(
     match (s,m,e,c,k) with
       (_,_,_,PUSH(Val v)::c,_) -> (V v::s, m, e, c, k)
     | (_,_,_,PUSH(Id x)::c, _) ->
	 	(try
        	((e @? x)::s, m, e, c, k) 
        with Not_found -> raise (Unbound_id x))
     | (_,_,_,PUSH(Fn(x,c'))::c,_) -> (P(x,c',e)::s, m, e, c, k)
     | (w::s,_,_,POP::c,k) -> (s, m, e, c, k)
     | (V(L l)::V v::s,_,_,STORE::c,_) -> (s, (l,v)::m, e, c, k)
     | (V(L l)::s,_,_,LOAD::c,_) -> 
	 	(try
        	(V(m @? l)::s, m, e, c, k)
        with Not_found -> 
			let (l1, l2) = l in raise (Unbound_loc (l1, l2)))
     | (V(B b)::s,_,_,JTR(c1,c2)::c,_) -> 
         (s, m, e, (if b then c1@c else (c2@c)), k)
     | (_,_,_,MALLOC::c,_) -> (V(L(newl()))::s, m, e, c, k)
     | (_,_,_,BOX z::c,_) ->
        let rec box b i s =
			if i = 0 then V (R b)::s
			else 
				match s with 
				  (M m::s) -> box (m::b) (i-1) s
              	| _ -> raise (RunError (s,m,e,c,k))
        in  (box [] z s,m,e,c,k)
     | (V (R b)::s,_,_,UNBOX x::c,_) ->
	 	(try
        	((b @? x)::s,m,e,c,k)
		with Not_found -> raise (Unbound_id x))
     | (w::s,_,_,BIND x::c,_) -> (s, m, (x,w)::e, c, k)
     | (_,_,i::e,UNBIND::c,_) -> (M i::s, m, e, c, k)
     | (V(L l)::V v::P(x,c',e')::s,_,_,CALL::c,k) ->
         (s, (l,v)::m, (x,V(L l))::e', c', (c,e)::k)
     | (_,_,_,[],(c,e')::k) -> (s, m, e', c, k)
     | (_,_,_,GET::c,_) -> (V(Z(read_int()))::s, m, e, c, k) 
     | (V(Z z)::s,_,_,PUT::c,_) -> 
          print_int z; print_newline(); (s, m, e, c, k)
     | (V(Z z2)::V(Z z1)::s,_,_,ADD::c,_) -> (V(Z(z1+z2))::s, m, e, c, k)
     | (V(Z z2)::V(L(l1,z1))::s,_,_,ADD::c,_) -> if z1+z2 >= 0 
	 then (V(L(l1,z1+z2))::s, m, e, c, k) 
	 else raise (RunError (s,m,e,c,k))
     | (V(L(l2,z2))::V(Z z1)::s,_,_,ADD::c,_) -> if z1+z2 >= 0 
	 then (V(L(l2,z1+z2))::s, m, e, c, k)
	 else raise (RunError (s,m,e,c,k))
     | (V(Z z2)::V(Z z1)::s,_,_,SUB::c,_) -> (V(Z(z1-z2))::s, m, e, c, k)
     | (V(Z z2)::V(L(l1,z1))::s,_,_,SUB::c,_) -> if z1-z2 >= 0 
	 then (V(L(l1,z1-z2))::s, m, e, c, k)
	 else raise (RunError (s,m,e,c,k))
     | (V(L(l2,z2))::V(L(l1,z1))::s,_,_,SUB::c,_) -> if l1 = l2 then (V(Z(z1-z2))::s, m, e, c, k)
	 												else raise (RunError (s,m,e,c,k))
     | (V(Z z2)::V(Z z1)::s,_,_,MUL::c,_) -> (V(Z(z1*z2))::s, m, e, c, k)
     | (V(Z z2)::V(Z z1)::s,_,_,DIV::c,_) -> if z2 = 0 
	 then raise (RunError (s,m,e,c,k))
	 else (V(Z(z1/z2))::s, m, e, c, k) 
     | (V(Z z2)::V(Z z1)::s,_,_,EQ::c,_) -> (V(B(z1=z2))::s, m, e, c, k)
     | (V(B b2)::V(B b1)::s,_,_,EQ::c,_) -> (V(B(b1=b2))::s, m, e, c, k)
     | (V(R r2)::V(R r1)::s,_,_,EQ::c,_) ->
       (V(B(List.sort compare r1 = List.sort compare r2))::s, m, e, c, k)
    
     | (V Unit::V Unit::s,_,_,EQ::c,_) -> (V(B true)::s, m, e, c, k)
     | (V(L(l2,z2))::V(L(l1,z1))::s,_,_,EQ::c,_) -> (V(B(l1 = l2 && z1 = z2))::s, m, e, c, k)
     | (V _::V _::s,_,_,EQ::c,_) -> (V(B false)::s,m,e,c,k) 
     | (V(Z z2)::V(Z z1)::s,_,_,LESS::c,_) -> (V(B(z1<z2))::s, m, e, c, k)
     | (V(L(z1,z2))::V(L(l1,l2))::s,_,_,LESS::c,_) -> if z1 = l1 then (V(B(l2<z2))::s, m, e, c, k)
	 												else raise (RunError (s,m,e,c,k))
     | (V(B b)::s,_,_,NOT::c,_) -> (V(B(not b))::s, m, e, c, k)
     | (_,_,_,[],[]) -> raise End
     | _ -> raise (RunError (s,m,e,c,k))
	)
  let print_error x = print_string "SM5 evaluation error: ";
    (match x with
       Unbound_id x -> print_string "unbound id '";print_string x;print_endline "'.";flush stdout
     | Unbound_loc (l1,l2) -> print_string "unbound loc (";print_int l1;print_string ",";print_int l2;print_endline ").";flush stdout
     | RunError _ -> print_endline "stuck configuration.";flush stdout
     | x -> raise x);
    flush stdout

  let run c = 
	try
		(ignore (eval ([],[],[],c,[]))) 
    with End -> ()
    | x -> print_error x
end
