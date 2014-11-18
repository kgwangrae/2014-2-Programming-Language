(*
 * SNUCSE 4190.310 Programming Languages 
 * k.gwangrae@gmail.com, Gwangrae Kim
 * Vanilla M: interpreter with dynamic type checking
 *)
open M

module M_Vanilla : M_Runner = struct
  open M

    (* domains *)
    type loc = int
    type value = Num of int
               | String of string
               | Bool of bool
               | Loc of loc
               | Pair of value * value
               | Closure of closure
    and closure = fexpr * env
    and fexpr = Fun of id * exp
              | RecFun of id * id * exp
    and env = id -> value
    type memory = int * (loc -> value)

    (* notations (see 5 page in M.pdf) *)
    (* f @+ (x, v)              f[x |-> v]
     * store M (l, v)           M[l |-> v]
     * fetch M l                M(l)
     *)
    let (@+) f (x, v) = (fun y -> if y = x then v else f y)
    let store (l, m) p = (l, m @+ p)        
    let fetch (_, m) l = m l                
    let malloc (l, m) = (l, (l+1, m))

    (* auxiliary functions *)
    let error msg = raise (RuntimeError msg)
    let getString = function (String s) -> s | _ -> error "not a string value"
    let getNum = function (Num n) -> n | _ -> error "not a number value"
    let getBool = function (Bool b) -> b | _ -> error "not a boolean value"
    let getLoc = function (Loc l) -> l | _ -> error "not a location value"
    let getPair = function (Pair (a,b)) -> (a, b) | _ -> error "not a pair"
    let getClosure = function (Closure c) -> c | _ -> error "not a function"
    let op2fn =
      function ADD -> (fun (v1,v2) -> Num (getNum v1 + getNum v2))
         | SUB -> (fun (v1,v2) -> Num (getNum v1 - getNum v2))
         | AND -> (fun (v1,v2) -> Bool (getBool v1 && getBool v2))
         | OR ->  (fun (v1,v2) -> Bool (getBool v1 || getBool v2))
         | EQ ->  (fun (v1, v2) -> 
             match (v1, v2) with 
             | (Num a, Num b) -> Bool (a=b)
             | (String a, String b) -> Bool (a=b)
             | (Bool a, Bool b) -> Bool (a=b)
             | (Loc a, Loc b) -> Bool (a=b)
             | _ -> error "invalid type of param was used for eq"
             )
    let rec printValue =
      function Num n -> print_int n; print_newline()
         | Bool b -> print_endline (if b then "true" else "false")
         | String s -> print_endline s
         | _ -> error "unprintable"

    let rec eval env mem exp = match exp with 
         | CONST c ->
            ((match c with S s -> String s | N n -> Num n | B b -> Bool b), mem)
         | VAR x -> (env x, mem)
         | FN (x, e) -> (Closure (Fun (x, e), env), mem)
         | APP (e1, e2) ->
             let (v1, m') = eval env mem e1 in
             let (c, env') = getClosure v1 in
             let (v2, m'') = eval env m' e2 in
             (match c with 
               | Fun (x, e) -> eval (env' @+ (x,v2)) m'' e
               | RecFun (f, x, e) -> 
                   let env_fun = env' @+ (x,v2) in
                   let env_arg = env_fun @+ (f, v1) in
                   eval env_arg m'' e
             )
         | LET (NREC (x, e1), e2) ->
             let (v1, m') = eval env mem e1 in
             eval (env @+ (x,v1)) m' e2
         | LET (REC (f, e1), e2) ->
             let (v1, m') = eval env mem e1 in
             let (c, env') = getClosure v1 in
             (match c with
              | Fun (x, e) -> eval (env @+ (f, Closure (RecFun (f, x, e), env'))) m' e2
              | RecFun (_,_,_) -> error "recfun is not allowed here"
             )
         | IF (e1, e2, e3) ->
             let (v1, m') = eval env mem e1 in
             eval env m' (if getBool v1 then e2 else e3)
         | BOP (op, e1, e2) ->
             let (v1, m') = eval env mem e1 in
             let (v2, m'') = eval env m' e2 in
             ((op2fn op) (v1,v2), m'')
         | READ ->
             let n = try read_int () with _ -> error "read error" in  
             (Num n, mem)
         | WRITE e ->
             let (v, m') = eval env mem e in
             printValue v; (v, m')
         | MALLOC e ->
             let (v, m') = eval env mem e in
             let (l, m'') = malloc m' in  
             (Loc l, store m'' (l,v))
         | ASSIGN (e1, e2) -> 
             let (l, m1) = eval env mem e1 in
             let (v, m2) = eval env m1 e2 in
             (v, store m2 ((getLoc l), v))
         | BANG e ->
             let (l, m1) = eval env mem e in
             let v = fetch m1 (getLoc l) in
             (v, m1)
         | SEQ (e1, e2) -> 
             let (v1, m1) = eval env mem e1 in eval env m1 e2
         | PAIR (e1, e2) -> 
             let (v1, m') = eval env mem e1 in
             let (v2, m'') = eval env m' e2 in
             (Pair (v1, v2), m'') 
         | SEL1 e -> 
             let (v, m1) = eval env mem e in
             let (l, r) = getPair v in (l, m1)
         | SEL2 e ->
             let (v, m1) = eval env mem e in
             let (l, r) = getPair v in (r, m1)

    let emptyEnv = (fun x -> error ("unknown id: " ^ x))
    let emptyMem = (0, fun l -> error ("unknown loc: " ^ string_of_int l))

    let run exp = ignore (eval emptyEnv emptyMem exp)
    end
