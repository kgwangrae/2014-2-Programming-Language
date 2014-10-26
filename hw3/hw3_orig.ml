(*
   SNU 4190.310 Programming Languages (Fall 2014)
   Gwangrae Kim, k.gwangrae@gmail.com
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
	| Record of (id * Loc.t) list
    
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
			| Proc _ -> raise (Error "not a lcation")) 
		with Env.Not_bound -> raise (Error "location name not bound")

	let env_proc e f =
		try
			(match Env.lookup e f with
  			| Addr _ -> raise (Error "not a procedure") 
			| Proc (id, exp, env) -> (id, exp, env))
		with Env.Not_bound -> raise (Error "procedure name not bound")
    
    let env_bind_loc : env -> id -> Loc.t -> env = 
        fun env id loc ->
            Env.bind env id (Addr (loc))

    let env_bind_proc : env -> id -> (id list * exp * env) -> env = 
        fun env id (id_fun, exp_fun, env_fun) ->
            Env.bind env id (Proc (id_fun, exp_fun, env_fun))

    let rec env_bind_loc_list : env -> (id list) -> (Loc.t list) -> env =
        fun env ids locs ->
           (match ids, locs with
            | curr_id::more_ids, curr_loc::more_locs
              -> (env_bind_loc_list (env_bind_loc env curr_id curr_loc) more_ids more_locs)
            | [], [] -> env
            | _, _ -> raise (Error "Arguments must have the same size."))

    let mem_load : memory -> Loc.t -> value = 
        fun mem loc ->
            try Mem.load mem loc
            with 
            | Mem.Not_initialized -> raise (Error "Cannot access not initialized memory")
            | Failure _ -> raise (Error "Cannot access unallocated memory") 
    
    let mem_alloc_store : memory -> value -> (Loc.t * memory) =
        fun mem v ->
            let (loc_alloc, mem_alloc) = Mem.alloc mem in
            let mem_assign = Mem.store mem_alloc loc_alloc v in
            (loc_alloc, mem_assign)
    
    let rec mem_alloc_store_list : memory -> value list -> (Loc.t list * memory) =
        fun mem v_lst -> (match v_lst with
        | h::t -> let (loc_alloc, mem_alloc) = mem_alloc_store mem h in
            let (locs, mem_final) = mem_alloc_store_list mem_alloc t in
            (loc_alloc::locs, mem_final)
        | [] -> ([], mem)
        )

    let rec lookup record field_name = 
       (match record with 
        | (id, loc)::t -> if (id = field_name) then loc else (lookup t field_name)
        | [] -> raise (Error "No such field name found in the given record!"))

	let rec semantics : memory -> env -> exp -> (value * memory) = 
        let rec seq_calc_r : memory * env * (exp list) * (value list) -> (value list) * memory = 
          fun (mem, env, e_list, v_list) -> 
            (match e_list with
            | h::[] -> let (v_curr, m_curr) = semantics mem env h in
                ((List.rev (v_curr::v_list)), m_curr)
            | h::t -> let (v_curr, m_curr) = semantics mem env h in
                seq_calc_r (m_curr, env, t, v_curr::v_list)
            | [] -> ((List.rev (v_list)), mem))
        in
        let seq_calc mem env e_list = seq_calc_r (mem, env, e_list, []) in
        fun mem env e -> match e with
		| NUM i -> (Num i, mem)
        | TRUE -> (Bool true, mem)
        | FALSE -> (Bool false, mem)
        | UNIT -> (Unit, mem)
        | VAR id -> ((mem_load mem (env_loc env id)),mem)
        (* NOTE : By current definition of this language, procedure is not a first-class citizen!*)
        | ADD (e1, e2) -> 
            (let (v1, mem1) = semantics mem env e1 in
             let (v2, mem2) = semantics mem1 env e2 in
             (Num ((value_int v1)+(value_int v2)),mem2))
        | SUB (e1, e2) -> 
            (let (v1, mem1) = semantics mem env e1 in
             let (v2, mem2) = semantics mem1 env e2 in
             (Num ((value_int v1)-(value_int v2)),mem2))
        | MUL (e1, e2) -> 
            (let (v1, mem1) = semantics mem env e1 in
             let (v2, mem2) = semantics mem1 env e2 in
             (Num ((value_int v1)*(value_int v2)),mem2))
        | DIV (e1, e2) -> 
            (let (v1, mem1) = semantics mem env e1 in
             let (v2, mem2) = semantics mem1 env e2 in
             (Num ((value_int v1)/(value_int v2)),mem2))
        | EQUAL (e1, e2) -> 
            (let (v1, mem1) = semantics mem env e1 in
             let (v2, mem2) = semantics mem1 env e2 in
             match v1, v2 with
             | Record _, _ 
             | _, Record _ -> (Bool false, mem2)
             | _, _ -> (Bool (v1=v2), mem2))
        | LESS (e1, e2) -> 
            (let (v1, mem1) = semantics mem env e1 in
             let (v2, mem2) = semantics mem1 env e2 in
             (Bool ((value_int v1)<(value_int v2)),mem2))
        | NOT e_in -> 
            (let (v,mem1) = semantics mem env e_in in
             (Bool (not (value_bool v)), mem1))
        | SEQ (e1, e2) -> 
            (let (_, mem1) = semantics mem env e1 in
             semantics mem1 env e2)
        | IF (e_cond, e_true, e_false) ->
            (let (v_cond, mem_cond) = semantics mem env e_cond in
             if (value_bool v_cond)
             then (semantics mem_cond env e_true)
             else (semantics mem_cond env e_false))
        | WHILE (e_cond, e_loop) ->
            (let (v_cond, mem_cond) = semantics mem env e_cond in
             if (value_bool v_cond)
             then 
               (let (_, mem_loop) = semantics mem_cond env e_loop in
                semantics mem_loop env e)
             else (Unit, mem_cond))
        | LETV (id, e_var, e_body) ->
            (let (v_var, mem_var) = semantics mem env e_var in
             let (loc_alloc, mem_assign) = mem_alloc_store mem_var v_var in 
             let env_assign = env_bind_loc env id loc_alloc in
             semantics mem_assign env_assign e_body)
        | LETF (id_fun, id_args, e_fun, e_body) ->
            (let env_assign = env_bind_proc env id_fun (id_args, e_fun, env) in
             semantics mem env_assign e_body)
        | CALLV (id_fun, exp_args) ->
            (let (id_args, e_fun, env_fun) = (env_proc env id_fun) in
             if (List.length id_args <> List.length exp_args) 
             then raise (Error "Invalid_argument")
             else 
               let (vals, mem_vals) = seq_calc mem env exp_args in
               let (locs, mem_assign) = mem_alloc_store_list mem_vals vals in
               let env_assign = env_bind_loc_list env_fun id_args locs in
               let env_rec = env_bind_proc env_assign id_fun (id_args, e_fun, env_fun) in
               semantics mem_assign env_rec e_fun) 
        | CALLR (id_fun, ref_args) ->
            (* Syntax : f<y1,y2...> is equal to f(&y1,&y2...) in C language *)
            (let (id_args, e_fun, env_fun) = (env_proc env id_fun) in
             if (List.length id_args <> List.length ref_args) 
             then raise (Error "Invalid_argument")
             else 
               let locs = List.map (env_loc env) ref_args in 
               let env_assign = env_bind_loc_list env_fun id_args locs in
               let env_rec = env_bind_proc env_assign id_fun (id_args, e_fun, env_fun) in
               semantics mem env_rec e_fun)
        | RECORD (id_exp_list) ->
            if (List.length id_exp_list = 0) then (Unit, mem)
            else
            let (id_lst, exp_lst) = List.split id_exp_list in
            let (vals, mem_vals) = seq_calc mem env exp_lst in
            let (locs, mem_assign) = mem_alloc_store_list mem_vals vals in
            ((Record (List.combine id_lst locs)), mem_assign)
        | FIELD (e_record, id_field) ->
            let (v_record, mem_record) = (semantics mem env e_record) in
            let record_in : (id * Loc.t) list = value_record v_record in
            ((mem_load mem_record (lookup record_in id_field)), mem_record)
        | ASSIGN (id_var, e_var) ->
            let (v, mem_var) = semantics mem env e_var in
            let loc = (env_loc env id_var) in
            (* NOTE : loc must not point an unallocated memory, according to the definition of LETV *)
            (v, (Mem.store mem_var loc v))
        | ASSIGNF (e_record, id_field, e_var) -> 
            let (v_record, mem_record) = (semantics mem env e_record) in
            let record_in : (id * Loc.t) list = value_record v_record in
            let (v, mem_var) = semantics mem_record env e_var in
            (v, (Mem.store mem_var (lookup record_in id_field) v))
        | READ (id_var) -> 
            (try
                let v = Num (read_int ()) in
                let loc = (env_loc env id_var) in
                (v, (Mem.store mem loc v))
            with
            | Failure str -> raise (Error str))
        | WRITE (e_var) -> 
            let (v, mem_var) = semantics mem env e_var in
            let _ = print_int (value_int v) in
            (v, mem_var)

	let run (mem, env, pgm) = 
		let (v,_) = semantics mem env pgm in v
end
