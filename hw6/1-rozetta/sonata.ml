(*
 * SNU 4190.310 Programming Languages 
 *
 * SONATA
 *)

module type SONATA = 
sig
  type cmd = 
         PUSH of obj | POP | STORE | LOAD | JTR of command * command
       | MALLOC | BOX of int | UNBOX of string | BIND of string | UNBIND
       | GET | PUT | CALL | ADD | SUB | MUL | DIV | EQ | LESS | NOT
   and obj = Val of value | Id of string | Fn of string * command
   and value = Z of int | B of bool | L of loc | Unit | R of record
   and record
   and loc = int * int
   and command = cmd list

  val empty_command : command

  val print : command -> unit
  val run : command -> unit
end

module Sonata : SONATA =
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
   and memory = (loc * svalue) list
   and environment = map list
   and command = cmd list
   
  exception RunError of stack * memory * environment * command
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
      Z i -> print_string "Sonata.Z ";print_int i
    | B b -> print_string "Sonata.B ";if b then print_string "true" else print_string "false"
    | R [] -> print_string "Sonata.R []"
    | R (h::t) ->
        let pf t = 
			match t with
			  (x, V(L(l1, l2))) -> print_string "(";printid x;print_string ",Sonata.V(";printv (L(l1,l2));print_string ")"
   			  | _ -> raise (Invalid_argument "non Loc in Record")
        in  print_string "Sonata.R ["; pf h; List.iter (fun f -> print_string ";"; pf f) t; 
            print_string "]"
    | Unit -> print_string "Sonata.Unit" 
    | L (b,o) -> print_string "Sonata.L(";print_int b;print_string ",";print_int o;print_string ")"
  let rec printp indent p = 
    match p with
      Val v -> printv v
    | Id x -> print_string "Sonata.Id ";printid x
    | Fn(x,c) -> print_string "Sonata.Fn(";printid x;print_string ",\n"; print_cmd (indent^"  ") c; print_string ")"
  and printc indent c = print_string indent;
    (match c with
         PUSH p -> print_string "Sonata.PUSH("; printp indent p;print_string ")"
       | POP -> print_string "Sonata.POP"
       | STORE -> print_string "Sonata.STORE"
       | LOAD -> print_string "Sonata.LOAD"
       | JTR(c1,c2) -> 
           print_string "Sonata.JTR(\n"; print_cmd (indent^"  ") c1; print_string ",\n";
           print_cmd (indent^"  ") c2; print_string ")"
       | MALLOC -> print_string "Sonata.MALLOC"
       | BOX z -> print_string "Sonata.BOX ";print_int z
       | UNBOX x -> print_string "Sonata.UNBOX ";printid x
       | BIND x -> print_string "Sonata.BIND ";printid x
       | UNBIND -> print_string "Sonata.UNBIND"
       | GET -> print_string "Sonata.GET"
       | PUT -> print_string "Sonata.PUT"
       | CALL -> print_string "Sonata.CALL"
       | ADD -> print_string "Sonata.ADD"
       | SUB -> print_string "Sonata.SUB"
       | MUL -> print_string "Sonata.MUL"
       | DIV -> print_string "Sonata.DIV"
       | EQ -> print_string "Sonata.EQ"
       | LESS -> print_string "Sonata.LESS"
       | NOT -> print_string "Sonata.NOT")
  and print_cmd indent l =
    print_string (indent^"[");
    print_seq (printc indent) (fun _ -> print_string ";\n") l;
    print_string (indent^"]")
  and print l = print_cmd "" l;print_newline();flush stdout
    
  let loccount = ref 0
      (* create new loc *)
  let newl () = loccount := !loccount + 1; (!loccount,0)

  let rec eval (s,m,e,c) = 
	eval(
     match (s,m,e,c) with
       (_,_,_,PUSH(Val v)::c) -> (V v::s, m, e, c)
     | (_,_,_,PUSH(Id x)::c) ->
	 	(try
        	((e @? x)::s, m, e, c) 
        with Not_found -> raise (Unbound_id x))
     | (_,_,_,PUSH(Fn(x,c'))::c) -> (P(x,c',e)::s, m, e, c)
     | (w::s,_,_,POP::c) -> (s, m, e, c)
     | (V(L l)::v::s,_,_,STORE::c) -> (s, (l,v)::m, e, c)
     | (V(L l)::s,_,_,LOAD::c) -> 
	 	(try
        	((m @? l)::s, m, e, c)
        with Not_found -> 
			let (l1, l2) = l in raise (Unbound_loc (l1, l2)))
     | (V(B b)::s,_,_,JTR(c1,c2)::c) -> 
         (s, m, e, (if b then c1@c else (c2@c)))
     | (_,_,_,MALLOC::c) -> (V(L(newl()))::s, m, e, c)
     | (_,_,_,BOX z::c) ->
        let rec box b i s =
			if i = 0 then V (R b)::s
			else 
				match s with 
				  (M m::s) -> box (m::b) (i-1) s
              	| _ -> raise (RunError (s,m,e,c))
        in  (box [] z s,m,e,c)
     | (V (R b)::s,_,_,UNBOX x::c) ->
	 	(try
        	((b @? x)::s,m,e,c)
		with Not_found -> raise (Unbound_id x))
     | (w::s,_,_,BIND x::c) -> (s, m, (x,w)::e, c)
     | (_,_,i::e,UNBIND::c) -> (M i::s, m, e, c)
     | (V(L l)::v::P(x,c',e')::s,_,_,CALL::c) ->
         (s, (l,v)::m, (x,V(L l))::e', c')
     | (_,_,_,GET::c) -> (V(Z(read_int()))::s, m, e, c) 
     | (V(Z z)::s,_,_,PUT::c) -> 
          print_int z; print_newline(); (s, m, e, c)
     | (V(Z z2)::V(Z z1)::s,_,_,ADD::c) -> (V(Z(z1+z2))::s, m, e, c)
     | (V(Z z2)::V(L(l1,z1))::s,_,_,ADD::c) -> if z1+z2 >= 0 
	 then (V(L(l1,z1+z2))::s, m, e, c) 
	 else raise (RunError (s,m,e,c))
     | (V(L(l2,z2))::V(Z z1)::s,_,_,ADD::c) -> if z1+z2 >= 0 
	 then (V(L(l2,z1+z2))::s, m, e, c)
	 else raise (RunError (s,m,e,c))
     | (V(Z z2)::V(Z z1)::s,_,_,SUB::c) -> (V(Z(z1-z2))::s, m, e, c)
     | (V(Z z2)::V(L(l1,z1))::s,_,_,SUB::c) -> if z1-z2 >= 0 
	 then (V(L(l1,z1-z2))::s, m, e, c)
	 else raise (RunError (s,m,e,c))
     | (V(L(l2,z2))::V(L(l1,z1))::s,_,_,SUB::c) -> if l1 = l2 then (V(Z(z1-z2))::s, m, e, c)
	 												else raise (RunError (s,m,e,c))
     | (V(Z z2)::V(Z z1)::s,_,_,MUL::c) -> (V(Z(z1*z2))::s, m, e, c)
     | (V(Z z2)::V(Z z1)::s,_,_,DIV::c) -> if z2 = 0 
	 then raise (RunError (s,m,e,c))
	 else (V(Z(z1/z2))::s, m, e, c) 
     | (V(Z z2)::V(Z z1)::s,_,_,EQ::c) -> (V(B(z1=z2))::s, m, e, c)
     | (V(B b2)::V(B b1)::s,_,_,EQ::c) -> (V(B(b1=b2))::s, m, e, c)
     | (V(R r2)::V(R r1)::s,_,_,EQ::c) ->
       (V(B(List.sort compare r1 = List.sort compare r2))::s, m, e, c)
    
     | (V Unit::V Unit::s,_,_,EQ::c) -> (V(B true)::s, m, e, c)
     | (V(L(l2,z2))::V(L(l1,z1))::s,_,_,EQ::c) -> (V(B(l1 = l2 && z1 = z2))::s, m, e, c)
     | (V _::V _::s,_,_,EQ::c) -> (V(B false)::s,m,e,c) 
     | (V(Z z2)::V(Z z1)::s,_,_,LESS::c) -> (V(B(z1<z2))::s, m, e, c)
     | (V(L(z1,z2))::V(L(l1,l2))::s,_,_,LESS::c) -> if z1 = l1 then (V(B(l2<z2))::s, m, e, c)
	 												else raise (RunError (s,m,e,c))
     | (V(B b)::s,_,_,NOT::c) -> (V(B(not b))::s, m, e, c)
     | (_,_,_,[]) -> raise End
     | _ -> raise (RunError (s,m,e,c))
	)
  let print_error x = printf "SM5 evaluation error: ";
    (match x with
       Unbound_id x -> printf "unbound id '%s'.@." x
     | Unbound_loc (l1,l2) -> printf "unbound loc (%d,%d).@." l1 l2
     | RunError (_, _, _, _) -> printf "stuck configuration.@."
     | x -> raise x);
    print_flush()  

  let run c = 
	try
		(ignore (eval ([],[],[],c))) 
    with End -> ()
    | x -> print_error x
end
