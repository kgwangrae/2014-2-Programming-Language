(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 * k.gwangrae@gmail.com Gwangrae Kim, SNU
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

  exception GC_Failure

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
  
   exception GC_Failure
   exception RunError of stack * memory * environment * command * continuation
   exception Unbound_id of string
   exception Unbound_loc of int * int
   exception End
   exception UnknownError of string 

   let empty_command = []

   let (@?) l x = snd (List.find (fun y -> x = fst y) l)
   let fsts l = List.map fst l 
   
   (* Memory implementation using Hashtable : location (loc) -> validity (bool)
    * Actual binding information is stored as a list of (loc,value)*)
   let mem_used = ref 0
   let mem_size = 128
   let offset = 0
   let mark_mem = Hashtbl.create mem_size
   let marked_some_loc = ref false

   (* Remove invalid entries, reset valid entries' validity and build new memory list*)
   let clear : memory -> memory = fun old_mem ->
     let checker : loc -> bool -> memory -> memory = fun loc is_valid new_mem ->
       if (is_valid)  
       then 
        try ((loc,(old_mem @? loc))::new_mem)
        with Not_found -> new_mem (*Valid location, but not binded with any value yet*)
       else 
         (let _ = (Hashtbl.remove mark_mem loc) in new_mem)
     in
     let resetter : loc -> bool -> unit = fun loc is_valid ->
       (Hashtbl.replace mark_mem loc false)
     in
     let new_mem = Hashtbl.fold checker mark_mem []
     in
     let _ = Hashtbl.iter resetter mark_mem
     in
     let _ = (mem_used := Hashtbl.length mark_mem)
     in
     new_mem
   
   let mark_loc : loc -> unit = fun loc -> 
     if (Hashtbl.find mark_mem loc) then ()
     else
       let _ = (marked_some_loc := true) 
       in 
       Hashtbl.replace mark_mem loc true

   (*Procedures below should be called only when the given parameter itself is reachable*)
   
   let rec probe_rec : record -> unit = fun r ->
     match r with 
     | (_,(V (L l)))::t -> (mark_loc l);(probe_rec t)
     | [] -> ()
     | _ -> raise (UnknownError "Invalid record containing non-loc value") 

   let probe_val : value -> unit = fun v -> 
     match v with
     | L l -> mark_loc l
     | R r -> probe_rec r
     | _ -> ()
   
   (* This should be called at last - when almost all locations have been tested for validity*)
   let rec probe_mem : memory -> unit = fun m -> 
     let rec probe_mem_once : memory -> unit = fun mem -> 
       match mem with
       | (l, v)::t -> (if (Hashtbl.find mark_mem l) then probe_val v else ());(probe_mem_once t)
       | [] -> ()
     in
     let _ = probe_mem_once m
     in
     if (not (!marked_some_loc)) then () (*Base case : nothing was changed*) 
     else ((marked_some_loc := false);probe_mem m)
   
   let rec probe_env : environment -> unit = fun env ->
     match env with
     (*NOTE environment definition in this sm5 interpreter is somewhat strange*)
     | (_,(V (L l)))::t -> (mark_loc l);(probe_env t)
     | (_,(P (_,_,e_in)))::t -> (probe_env e_in);(probe_env t)
       (*NOTE e_in doesn't have P itself by definition, so there's no risk of infinite loop*)
     | [] -> ()
     | _ -> raise (UnknownError "Invalid environment")

   let rec probe_cont : continuation -> unit = fun cont ->
     match cont with
     | (_,env)::t -> (probe_env env);(probe_cont t)
     | [] -> ()
   
   let rec probe_svalue : svalue -> unit = fun sv -> 
     match sv with
     | V v -> probe_val v
     | P (_,_,e) -> probe_env e
     | M (_, sv_in) -> probe_svalue sv_in
     (*
      * | M (_,(V(L l))) -> mark_loc l
      * | _ -> raise (UnknownError "Invalid svalue")*)
   
   let rec probe_stk : stack -> unit = fun stk -> 
     match stk with
     | h::t -> (probe_svalue h);(probe_stk t)
     | [] -> ()

   let gc : (stack * memory * environment * continuation) -> memory = fun (s,m,e,k) ->
     (probe_stk s);(probe_env e);(probe_cont k);(probe_mem m);(clear m) 

   (*NOTE : allocation itself is not always followed by binding*)
   let malloc : (stack * memory * environment * continuation) -> (memory * loc) = fun (s,m,e,k) -> 
     let rec get_free_loc : int -> loc = fun i ->
       if (Hashtbl.mem mark_mem (i,offset))
       then (get_free_loc (i+1))
       else (
         let _ = Hashtbl.add mark_mem (i,offset) false (*Set default as invalid*)
         in
         (i,offset)
       )
     in
     if (!mem_used < mem_size) 
     then ( 
       let _ = (mem_used := !mem_used + 1)
       in
       (m, get_free_loc 0)
     )
     else ( (* no more space : GC required *)
       let new_mem = gc (s,m,e,k)
       in
       if (!mem_used < mem_size)
       then (
         let _ = (mem_used := !mem_used + 1)
         in
         (new_mem, get_free_loc 0) 
       )
       else raise GC_Failure
     )
   
    open Format

    let rec print_seq f g l =
      match l with
        [] -> ()
    | [h] -> f h
      | h::t -> f h; g h; print_seq f g t

    let rec printv v =
      match v with
        Z i -> printf "%d" i
      | B b -> if b then printf "true" else printf "false"
      | R [] -> printf "[]"
      | R (h::t) ->
          let pf t = 
            match t with
          (x, V(L(l1, l2))) -> printf "(%s,<%d,%d>)" x l1 l2
      | _ -> raise (Invalid_argument "non Loc in Record")
          in  printf "["; pf h; List.iter (fun f -> printf ", "; pf f) t; 
              printf "]"
      | Unit -> printf "()" 
      | L (b,o) -> printf "<%d,%d>" b o
    let rec printp p = 
      match p with
    Val v -> printv v
      | Id x -> printf "%s" x
      | Fn(x,c) -> printf "@[<1>(%s,@ " x; print c; printf ")@]"
    and printc c = printf "@[";
      (match c with
           PUSH p -> printf "push "; printp p
      | POP -> printf "pop"
         | STORE -> printf "store"
         | LOAD -> printf "load"
         | JTR(c1,c2) -> 
             printf "@[<5>jtr ("; print c1; printf ",@ ";
             print c2; printf ")@]"
         | MALLOC -> printf "malloc"
         | BOX z -> printf "box %d" z
         | UNBOX x -> printf "unbox %s" x
         | BIND x -> printf "bind %s" x
         | UNBIND -> printf "unbind"
         | GET -> printf "get"
         | PUT -> printf "put"
         | CALL -> printf "call"
         | ADD -> printf "add"
         | SUB -> printf "sub"
         | MUL -> printf "mul"
         | DIV -> printf "div"
         | EQ -> printf "eq"
         | LESS -> printf "less"
         | NOT -> printf "not"); printf "@]"
       and print l =
         printf "@[";
      print_seq printc (fun _ -> printf "@ ") l;
      printf "@]";
      print_flush()


    let rec eval (s,m,e,c,k) = 
      eval(
        match (s,m,e,c,k) with
         (*Memory and stack operations*)
         | (_,_,_,PUSH(Val v)::c,_) -> (V v::s, m, e, c, k)
         | (_,_,_,PUSH(Id x)::c, _) ->
             (try ((e @? x)::s, m, e, c, k) 
              with Not_found -> raise (Unbound_id x))
         | (_,_,_,PUSH(Fn(x,c'))::c,_) -> (P(x,c',e)::s, m, e, c, k)
         | (w::s,_,_,POP::c,k) -> (s, m, e, c, k)
         | (V(L l)::V v::s,_,_,STORE::c,_) -> (s, (l,v)::m, e, c, k)
         | (V(L l)::s,_,_,LOAD::c,_) -> 
             (try (V(m @? l)::s, m, e, c, k)
              with Not_found -> 
                let (l1, l2) = l in raise (Unbound_loc (l1, l2)))
         | (_,_,_,MALLOC::c,_) ->
             let (new_mem, new_loc) = malloc (s,m,e,k) 
             in
             (V(L(new_loc))::s, new_mem, e, c, k)
         
         (*Conditional Branch*)
         | (V(B b)::s,_,_,JTR(c1,c2)::c,_) -> 
             (s, m, e, (if b then c1@c else (c2@c)), k)
         
         (*Box operations*)
         | (_,_,_,BOX z::c,_) ->
           let rec box b i s =
             if i = 0 then V (R b)::s
             else (match s with 
             | (M m::s) -> box (m::b) (i-1) s
             | _ -> raise (RunError (s,m,e,c,k)))
           in  
           (box [] z s,m,e,c,k)
         | (V (R b)::s,_,_,UNBOX x::c,_) ->
             (try ((b @? x)::s,m,e,c,k)
              with Not_found -> raise (Unbound_id x))
         
         (*Env operations*)
         | (w::s,_,_,BIND x::c,_) -> (s, m, (x,w)::e, c, k)
         | (_,_,i::e,UNBIND::c,_) -> (M i::s, m, e, c, k)
         
         (*Call and return*)
         | (V(L l)::V v::P(x,c',e')::s,_,_,CALL::c,k) -> (s, (l,v)::m, (x,V(L l))::e', c', (c,e)::k)
         | (_,_,_,[],(c,e')::k) -> (s, m, e', c, k)
         
         (*IO*)
         | (_,_,_,GET::c,_) -> (V(Z(read_int()))::s, m, e, c, k) 
         | (V(Z z)::s,_,_,PUT::c,_) -> (print_int z; print_newline(); (s, m, e, c, k))
         
         (*Add, Sub*)
         | (V(Z z2)::V(Z z1)::s,_,_,ADD::c,_) -> (V(Z(z1+z2))::s, m, e, c, k)
         | (V(Z z2)::V(L(l1,z1))::s,_,_,ADD::c,_) -> 
             if z1+z2 >= 0 
             then (V(L(l1,z1+z2))::s, m, e, c, k) 
             else raise (RunError (s,m,e,c,k))
         | (V(L(l2,z2))::V(Z z1)::s,_,_,ADD::c,_) -> 
             if z1+z2 >= 0 
             then (V(L(l2,z1+z2))::s, m, e, c, k)
             else raise (RunError (s,m,e,c,k))
         | (V(Z z2)::V(Z z1)::s,_,_,SUB::c,_) -> (V(Z(z1-z2))::s, m, e, c, k)
         | (V(Z z2)::V(L(l1,z1))::s,_,_,SUB::c,_) -> (
             if z1-z2 >= 0 
             then (V(L(l1,z1-z2))::s, m, e, c, k)
             else raise (RunError (s,m,e,c,k)))
         | (V(L(l2,z2))::V(L(l1,z1))::s,_,_,SUB::c,_) -> (
             if l1 = l2 
             then (V(Z(z1-z2))::s, m, e, c, k)
             else raise (RunError (s,m,e,c,k)))
         | (V(Z z2)::V(Z z1)::s,_,_,MUL::c,_) -> (V(Z(z1*z2))::s, m, e, c, k)
         | (V(Z z2)::V(Z z1)::s,_,_,DIV::c,_) -> (
             if z2 = 0 
             then raise (RunError (s,m,e,c,k))
             else (V(Z(z1/z2))::s, m, e, c, k)) 
         
         (*Equality*)
         | (V(Z z2)::V(Z z1)::s,_,_,EQ::c,_) -> (V(B(z1=z2))::s, m, e, c, k)
         | (V(B b2)::V(B b1)::s,_,_,EQ::c,_) -> (V(B(b1=b2))::s, m, e, c, k)
         | (V(R r2)::V(R r1)::s,_,_,EQ::c,_) ->
             (V(B(List.sort compare r1 = List.sort compare r2))::s, m, e, c, k)
         | (V Unit::V Unit::s,_,_,EQ::c,_) -> (V(B true)::s, m, e, c, k)
         | (V(L(l2,z2))::V(L(l1,z1))::s,_,_,EQ::c,_) -> (V(B(l1 = l2 && z1 = z2))::s, m, e, c, k)
         | (V _::V _::s,_,_,EQ::c,_) -> (V(B false)::s,m,e,c,k) 
         
         (*Comparison*)
         | (V(Z z2)::V(Z z1)::s,_,_,LESS::c,_) -> (V(B(z1<z2))::s, m, e, c, k)
         | (V(L(z1,z2))::V(L(l1,l2))::s,_,_,LESS::c,_) -> (
             if z1 = l1 
             then (V(B(l2<z2))::s, m, e, c, k)
             else raise (RunError (s,m,e,c,k)))
         
         (*Not*)
         | (V(B b)::s,_,_,NOT::c,_) -> (V(B(not b))::s, m, e, c, k)
         | (_,_,_,[],[]) -> raise End
         | _ -> raise (RunError (s,m,e,c,k))
      )
    let print_error x = printf "SM5 evaluation error: ";
      (match x with
         Unbound_id x -> printf "unbound id '%s'.@." x
       | Unbound_loc (l1,l2) -> printf "unbound loc (%d,%d).@." l1 l2
       | RunError (_, _, _, _, _) -> printf "stuck configuration.@."
       | x -> raise x);
      print_flush()  

    let run c = 
      try
        (ignore (eval ([],[],[],c,[]))) 
      with End -> ()
      | x -> print_error x
end
