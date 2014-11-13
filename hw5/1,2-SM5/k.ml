(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * K-- Interpreter I
 *)

(* Location *)
module type LOC =
  sig
    (* Base Address *)
    module Base :
      sig
        (* type of bass address *)
        type t

        (* NULL *)
        val null : t

        (* successor *)
        val succ : t -> t

        (* equal b1 b2 tests equality of b1 and b2 *)
        val equal : t -> t -> bool
      end

    (* type of location *)
    type t = Base.t * tyoffset
    and tyoffset = int

    exception Not_allowed

    (* loc_of_base b returns the first (offset 0) location of b *)
    val loc_of_base : Base.t -> t

    (* base l returns the base address of l. *)
    val base : t -> Base.t

    (* offset l return the offset of l. *)
    val offset : t -> int

    (* addi l i adds i to the offset of l. *)
    val addi : t -> int -> t

    (* subi l i subtracts i from the offset of l. *)
    val subi : t -> int -> t

    (* sub x y returns (offset x) - (offset y), or raises Not_allowed if they
       have different base addresses. *)
    val sub : t -> t -> tyoffset

    (* equal l r tests equality of l and r, or raises Not_allowed if they have
       different base addresses. *)
    val equal : t -> t -> bool

    (* less l r returns (offset l) < (offset r), or raises Not_allowed if they
       have different base addresses. *)
    val less : t -> t -> bool
  end

module Loc : LOC =
  struct
    module Base =
      struct
        type t = int
        let null = 0
        let succ b = b + 1
        let equal l r = (l = r)
      end

    type t = Base.t * tyoffset
    and tyoffset = int

    exception Not_allowed

    let loc_of_base base = (base, 0)
    let base (base, _) = base
    let offset (_, offset) = offset
    let addi (base, offset) n = (base, offset + n)
    let subi (base, offset) n = (base, offset - n)
    let sub (base, offset) (base', offset') =
        if Base.equal base base' then offset - offset' else raise Not_allowed
    let equal (base, offset) (base', offset') =
        (Base.equal base base') && (offset = offset')
    let less (base, offset) (base', offset') =
        if Base.equal base base' then offset < offset' else raise Not_allowed
  end

(*
 * Memory
 *)
module type MEM =
  sig
    (* type of memory from Loc.t to 'a *)
    type 'a t

    exception Not_allocated

    (* empty memory *)
    val empty : 'a t

    (* allocate m s returns an Loc.t to a fresh (uninitialized) buffer of size
       s and a memory containing the same bindings as m, plus a binding of
       the Loc.t to the first location of allocated buffer. *)
    val allocate : 'a t -> int -> Loc.t * 'a t

	(* "alloc m" equals to "allocate m 1" *)
	val alloc : 'a t -> Loc.t * 'a t

    (* store m l v returns a memory containing the same bindings as m, with l
       bound to v, or raises Not_allocated if m doesn't contain a binding for
       l. *)
    val store : 'a t -> Loc.t -> 'a -> 'a t

    (* load m l returns the current binding of l in m, or raises Not_found if
       no such binding exists. *)
    val load : 'a t -> Loc.t -> 'a

    (* is_allocated m l returns true if m contains a binding for l, and false
       otherwise. *)
    val is_allocated : 'a t -> Loc.t -> bool
  end

module Mem : MEM =
  struct
    module Map = Map.Make (struct type t = Loc.t let compare = compare end)
    type 'a t = Loc.Base.t * ('a option Map.t)

    exception Not_allocated
	exception Not_initialized

    let empty = (Loc.Base.null, Map.empty)

    let allocate (base, mem) s =
    	let rec range f t = if f < t then f :: (range (f + 1) t) else [] in

        let base' = Loc.Base.succ base in
        let l = Loc.loc_of_base base' in
        	(l, (base', List.fold_left
                        (fun m o -> Map.add (Loc.addi l o) None m)
                        mem
                        (range 0 s)))

	let alloc m = allocate m 1

    let is_allocated (_, mem) l = Map.mem l mem

    let store m l v =
		let (base, mem) = m in
        if is_allocated m l then (base, Map.add l (Some v) mem)
        else raise Not_allocated

    let load (_, mem) l =
		try
	        let content = (Map.find l mem) in 
				match content with
				  None -> raise Not_initialized
				| Some v -> v
		with Not_found -> raise Not_allocated
  end

(*
 * Environment
 *)
module type ENV =
  sig
    (* type of environment from 'a to 'b *)
    type 'a t

    exception Not_bound

    (* empty environment *)
    val empty : 'a t

    (* bind e x l returns an environment containing the same bindings as e,
       plus a binding of x to l. If x was already bound in e, its previous
       binding disappears. *)
    val bind : 'a t -> string -> 'a -> 'a t

    (* lookup e x returns the current binding of x in e, or raises Not_bound if
       no such binding exists. *)
    val lookup : 'a t -> string -> 'a

    (* is_bound e x returns true if e contains a binding for x, and false
       otherwise. *)
    val is_bound : 'a t -> string -> bool
  end

module Env : ENV =
  struct
  	module Map = Map.Make (struct type t = string let compare = compare end)
    type 'a t = 'a Map.t
   
	exception Not_bound

    let empty = Map.empty

    let bind e x a = Map.add x a e

    let lookup e x = try (Map.find x e) with Not_found -> raise Not_bound

    let is_bound e x = Map.mem x e
  end

(*
 * K-- Interpreter
 *)
module type KMINUS =
  sig
    exception Error of string
    type id = string
    type exp =
        NUM of int | TRUE | FALSE | UNIT
      | VAR of id
      | ADD of exp * exp
      | SUB of exp * exp
      | MUL of exp * exp
      | DIV of exp * exp
      | EQUAL of exp * exp
      | LESS of exp * exp
      | NOT of exp
      | ASSIGN of id * exp            (* assgin to variable *)
      | SEQ of exp * exp              (* sequence *)
      | IF of exp * exp * exp         (* if-then-else *)
      | WHILE of exp * exp            (* while loop *)
      | FOR of id * exp * exp * exp   (* for loop *)
      | LETV of id * exp * exp        (* variable binding *)
      | LETF of id * id * exp * exp   (* procedure binding *)
      | CALLV of id * exp             (* call by value *)
      | CALLR of id * id              (* call by referenece *)
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
        NUM of int | TRUE | FALSE | UNIT
      | VAR of id
      | ADD of exp * exp
      | SUB of exp * exp
      | MUL of exp * exp
      | DIV of exp * exp
      | EQUAL of exp * exp
      | LESS of exp * exp
      | NOT of exp
      | ASSIGN of id * exp
      | SEQ of exp * exp
      | IF of exp * exp * exp
      | WHILE of exp * exp
      | FOR of id * exp * exp * exp
      | LETV of id * exp * exp
      | LETF of id * id * exp * exp
      | CALLV of id * exp
      | CALLR of id * id
      | READ of id
      | WRITE of exp
    type program = exp

    type value =
        Num of int
      | Bool of bool
      | Location of Loc.t
      | Unit

    let value_int = function Num n -> n
      | Bool _ -> raise (Error "Bool type is used as Num type")
      | Location _ -> raise (Error "Location type is used as Num type")
      | Unit -> raise (Error "Unit type is used as Num type")

    let value_bool = function Bool b -> b
      | Num _ -> raise (Error "Num type is used as Bool type")
      | Location _ -> raise (Error "Location type is used as Bool type")
      | Unit -> raise (Error "Unit type is used as Bool type")

    let value_unit = function Unit -> ()
      | Num _ -> raise (Error "Num type is used as Unit type")
      | Bool _ -> raise (Error "Bool type is used as Unit type")
      | Location _ -> raise (Error "Location type is used as Unit type")

    (* memory *)
    type memory = value Mem.t

    let emptyMemory = Mem.empty

    (* environment *)
    type env = env_entry Env.t
    and  env_entry = Addr of Loc.t | Proc of id * exp * env

    let emptyEnv = Env.empty

    let env_loc e x =
		try
        (match Env.lookup e x with
           Addr l -> l
         | Proc _ -> raise (Error "not allowed")) 
		with Env.Not_bound -> raise (Error "not bound")

    let env_proc e f = 
		try
        (match Env.lookup e f with
           Addr _ -> raise (Error "not allowed") 
         | Proc (id, exp, env) -> (id, exp, env)) 
		with Env.Not_bound -> raise (Error "not bound")

    let opadd x y =
		match (x, y) with
		  (Num(a), Num(b)) -> Num(a+b)
	  	| (Location(l), Num(o)) -> Location(Loc.addi l o)
	  	| (Num(o), Location(l)) -> Location(Loc.addi l o)
	  	| (_, _) -> raise (Error ("type Error"))
	let opsub x y =
		match (x, y) with
		  (Num(a), Num(b)) -> Num(a-b)
	  	| (Location(l), Num(o)) -> Location(Loc.subi l o) 
	  	| (Location(l1), Location(l2)) -> Num(Loc.sub l1 l2)
	  	| (_, _) -> raise (Error ("type Error"))
	let opmul x y =
		match (x, y) with
		  (Num(i1), Num(i2)) -> Num(i1*i2)
	  	| (_, _) -> raise (Error("type error"))
	let opdiv x y =
		match (x, y) with
		  (Num(i1), Num(0)) -> raise (Error "division by zero")
	  	| (Num(i1), Num(i2)) -> Num(i1/i2)
	  	| (_, _) -> raise (Error"type error")
	let opequ x y =
		match (x, y) with
		  (Num(i1), Num(i2)) -> Bool(i1 = i2)
	  	| (Bool(b1), Bool(b2)) -> Bool(b1 = b2)
	  	| (Unit, Unit) -> Bool(true)
	  	| (Location(l1), Location(l2)) -> Bool(Loc.equal l1 l2) 
	  	| (_, _) -> raise (Error "type error")
	let oples x y = 
		match (x, y) with
		  (Num(i1), Num(i2)) -> Bool(i1 < i2)
	  	| (Location(l1), Location(l2)) -> Bool(Loc.less l1 l2)  
	 	| (Unit, Unit) -> Bool(false)
	  	| (_, _) -> raise (Error"type error")
	let opnot x =
		match x with
		  Bool(b) -> Bool(not b) 
	  	| _ -> raise (Error"type error")

	let rec eval mem env exp = 
		match exp with
		  NUM(i) -> (mem, Num(i))
		| TRUE -> (mem, Bool(true))
		| FALSE -> (mem, Bool(false))
		| UNIT -> (mem, Unit)
		| VAR(id) ->
			let loc = env_loc env id in
			let value = Mem.load mem loc in 
				(mem,value)
		| ADD(e1,e2) -> evalop mem env opadd e1 e2
		| SUB(e1,e2) -> evalop mem env opsub e1 e2
		| MUL(e1,e2) -> evalop mem env opmul e1 e2
 		| DIV(e1,e2) -> evalop mem env opdiv e1 e2
		| EQUAL(e1,e2) -> evalop mem env opequ e1 e2 
		| LESS(e1,e2) -> evalop mem env oples e1 e2 
		| NOT(e) ->
			let (mem',v) = eval mem env e in 
				(mem', opnot v)
		| ASSIGN(id,e) ->
			let (mem',v) = eval mem env e in
			let	loc = env_loc env id in
			let	mem'' = Mem.store mem' loc v in
				(mem'',v) 
		| SEQ(e1,e2) ->
			let (mem',_) = eval mem env e1 in
				eval mem' env e2
		| IF(e1,e2,e3) ->
			let (mem',cond) = eval mem env e1 in
				(match cond with
				  Bool(b) -> if b then eval mem' env e2 else eval mem' env e3
				| _ -> raise (Error"type error"))
		| WHILE(e1,e2) ->
			let (mem',cond) = eval mem env e1 in
				(match cond with
				  Bool(b) -> if b then
								let (mem'',_) = eval mem' env e2 in 
									eval mem'' env exp
							 else (mem',Unit) 
				| _ -> raise (Error "type error"))
			
		| FOR(id,e1,e2,e3) ->
			let (mem',v1) = eval mem env e1 in
			let (mem'',v2) = eval mem' env e2 in
			let n1 = value_int v1 in
			let n2 = value_int v2 in
				evalfor id n1 n2 mem'' env e3
		| LETV(id,e1,e2) ->
			let (mem',v) = eval mem env e1 in
			let	(loc,mem'') = Mem.alloc mem' in
			let env' = Env.bind env id (Addr(loc)) in
			let mem2 = Mem.store mem'' loc v in
				eval mem2 env' e2 
		| LETF(id,param,body,e) ->
			let env' = Env.bind env id (Proc(param,body,env)) in
				eval mem env' e
		| CALLV(id,e) ->
			let (mem',v) = eval mem env e in
			let (param,body,fenv) = env_proc env id in
			let (loc,mem'') = Mem.alloc mem' in
			let mem2 = Mem.store mem'' loc v in
				eval mem2 (Env.bind (Env.bind fenv id (Proc(param,body,fenv))) param (Addr(loc))) body
		| CALLR(fid,aid) ->
			let loc = env_loc env aid in
			let (param,body,fenv) = env_proc env fid in
				eval mem (Env.bind (Env.bind fenv fid (Proc(param,body,fenv))) param (Addr(loc))) body
		| READ(id) ->
			let loc = env_loc env id in
			let value = Num(read_int()) in
			let mem' = Mem.store mem loc value in
				(mem',value)
		| WRITE(exp) ->
			let (mem',v) = eval mem env exp in
			let _ = print_int (value_int v) in
			let _ = print_string "\n" in
				(mem',v)

	(* for-loop evaluation procedure *)
	and evalfor id n1 n2 mem env e =
		if n1 > n2 then (mem, Unit)
		else 
        let loc = env_loc env id in
		let mem' = Mem.store mem loc (Num(n1)) in
		let (mem'',v) = eval mem' env e in
			evalfor id (n1+1) (n2) mem'' env e

	(* operation evaluation procedure *)
	and evalop mem env op e1 e2 =
		let (mem',v1) = eval mem env e1 in
		let (mem'',v2) = eval mem' env e2 in
			(mem'', op v1 v2)

	let run (mem, env, pgm) = let (m,v) = eval mem env pgm in v
  end
