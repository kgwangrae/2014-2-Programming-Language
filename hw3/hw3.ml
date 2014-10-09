(*
   SNU 4190.310 Programming Languages (Fall 2013)
 
   K- Interpreter
*)
(* Location : don't mention it *)
module type LOC =
sig
	type t
	val base : t
	val equal : t -> t -> bool
	val diff : t -> t -> int
	val increase : t -> int -> t
end

module Loc : LOC =
struct
	type t = Location of int
	let base = Location(0)
	let equal (Location(a)) (Location(b)) = (a = b)
	let diff (Location(a)) (Location(b)) = a - b
	let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
	type 'a t
	exception Not_allocated
	exception Not_initialized
	val empty : 'a t (* get empty memory *)
	val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
	val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
	val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
	type ('a, 'b) t
	exception Not_bound
	val empty : ('a, 'b) t (* get empty environment *)
	val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
	val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
	exception Not_allocated
	exception Not_initialized
	type 'a content = V of 'a | U
	type 'a t = M of Loc.t * 'a content list
	let empty = M(Loc.base,[])

	let rec replace_nth = fun l n c -> 
		match l with
		| h::t -> if n = 1 then c::t else h::(replace_nth t (n-1) c)
		| [] -> raise Not_allocated

	let load (M(boundary,storage)) loc =
		match (List.nth storage ((Loc.diff boundary loc) - 1)) with
		| V(v) -> v 
		| U -> raise Not_initialized

	let store (M(boundary,storage)) loc content =
		M(boundary, replace_nth storage (Loc.diff boundary loc) (V(content)))

	let alloc (M(boundary,storage)) = (boundary,M(Loc.increase boundary 1,U::storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
	exception Not_bound
	type ('a, 'b) t = E of ('a -> 'b)
	let empty = E(fun x -> raise Not_bound)
	let lookup (E(env)) id = env id
	let bind (E(env)) id loc = E(fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
	exception Error of string
	type id = string
	type exp =
	| NUM of int | TRUE | FALSE | UNIT
	| VAR of id
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| EQUAL of exp * exp
	| LESS of exp * exp
	| NOT of exp
 	| SEQ of exp * exp            (* sequence *)
 	| IF of exp * exp * exp       (* if-then-else *)
  	| WHILE of exp * exp          (* while loop *)
  	| LETV of id * exp * exp      (* variable binding *)
  	| LETF of id * id list * exp * exp (* procedure binding *)
  	| CALLV of id * exp list      (* call by value *)
  	| CALLR of id * id list       (* call by referenece *)
  	| RECORD of (id * exp) list   (* record construction *)
  	| FIELD of exp * id           (* access record field *)
  	| ASSIGN of id * exp          (* assgin to variable *)
	| ASSIGNF of exp * id * exp   (* assign to record field *)
  	| READ of id
	| WRITE of exp
    
	type program = exp
	type memory
	type env
	type value
	val emptyMemory : memory
	val emptyEnv : env
	val run : memory * env * program -> value
end

module K : KMINUS =
struct
	exception Error of string

	type id = string
	type exp =
	| NUM of int | TRUE | FALSE | UNIT
	| VAR of id
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| EQUAL of exp * exp
	| LESS of exp * exp
  	| NOT of exp
  	| SEQ of exp * exp            (* sequence *)
  	| IF of exp * exp * exp       (* if-then-else *)
  	| WHILE of exp * exp          (* while loop *)
  	| LETV of id * exp * exp      (* variable binding *)
  	| LETF of id * id list * exp * exp (* procedure binding *)
  	| CALLV of id * exp list      (* call by value *)
  	| CALLR of id * id list       (* call by referenece *)
  	| RECORD of (id * exp) list   (* record construction *)
  	| FIELD of exp * id           (* access record field *)
  	| ASSIGN of id * exp          (* assgin to variable *)
	| ASSIGNF of exp * id * exp   (* assign to record field *)
	| READ of id
	| WRITE of exp

	type program = exp

	type value =
	| Num of int
	| Bool of bool
	| Unit
	| Record of (id -> Loc.t)
    
	type memory = value Mem.t
	type env = (id, env_entry) Env.t
	and  env_entry = Addr of Loc.t | Proc of id list * exp * env

	let emptyMemory = Mem.empty
	let emptyEnv = Env.empty

	let value_int v = 
		match v with 
		| Num n -> n
		| Bool _ -> raise (Error "Bool type is used as Num type")
		| Unit -> raise (Error "Unit type is used as Num type")
		| Record _ -> raise (Error "Unit type is used as Num type")

	let value_bool v =
		match v with
		| Bool b -> b
		| Num _ -> raise (Error "Num type is used as Bool type")
		| Unit -> raise (Error "Unit type is used as Bool type")
		| Record _ -> raise (Error "Unit type is used as Bool type")

    let value_unit v =
		match v with 
		| Unit -> ()
		| Num _ -> raise (Error "Num type is used as Unit type")
		| Bool _ -> raise (Error "Bool type is used as Unit type")
		| Record _ -> raise (Error "Bool type is used as Unit type")

	let value_record v =
		match v with
		| Record r -> r
		| Num _ -> raise (Error "Num type is used as Record type")
		| Unit -> raise (Error "Unit type is used as Record type")
		| Bool _ -> raise (Error "Bool type is used as Record type")

	let env_loc e x =
		try
			(match Env.lookup e x with
			| Addr l -> l
			| Proc _ -> raise (Error "not allowed")) 
		with Env.Not_bound -> raise (Error "not bound")

	let env_proc e f =
		try
			(match Env.lookup e f with
  			| Addr _ -> raise (Error "not allowed") 
			| Proc (id, exp, env) -> (id, exp, env))
		with Env.Not_bound -> raise (Error "not bound")
		  
	let rec semantics : memory -> env -> exp -> (value * memory) = 
		fun mem env e -> match e with
		| NUM i -> (Num i, mem)
		| _ -> raise (Error("not implemented")) (* implement it! *)

	let run (mem, env, pgm) = 
		let (v,_) = semantics mem env pgm in
		v
end
