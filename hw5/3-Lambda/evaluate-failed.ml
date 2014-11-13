(* SNU 4190.310 Programming Languages (Fall 2014)
 * Gwangrae Kim k.gwangrae@gmail.com
 * Lambda Calculus *)
module Evaluator =
  struct
	exception Error of string

  (*new name part : May collide with previously added names*)
  let idx = ref (0) 
  let get_name : unit -> string = fun _ ->
    let name = "u1#aQz" ^ (string_of_int !idx) in let _ = (idx:=!idx+1) in name

  (*env part : May use binary tree for speeding up*)
  type env = (Lambda.lexp * Lambda.lexp) list 
  let empty_env = []
  let add : env -> Lambda.lexp -> Lambda.lexp -> env = fun e k v -> (k,v)::e
  let rec find : env -> Lambda.lexp -> Lambda.lexp = fun e k ->
    match e with 
    | (k_in,v_in)::t -> if (k=k_in) then v_in else (find t k)
    | _ -> k 
  let rec mem : env -> Lambda.lexp -> bool = fun e k ->
    match e with 
    | (k_in,v_in)::t -> if (k=k_in) then true else if (k=v_in) then true else (mem t k)
    | _ -> false
  (*Add all names used in the lexp, without reduction, to the current env
   *used to avoid using duplicate names*) 
  let rec add_names : env -> Lambda.lexp -> env = fun env expr ->
    match expr with
    | Id (str) -> if (mem env expr) then env else add env expr (Id (get_name ()))
    | Lam (str, e_in) -> add_names env e_in
    | App (l, r) -> add_names (add_names env l) r
  let rec rm_dup_names : env -> env -> Lambda.lexp -> Lambda.lexp = fun envl envr expl ->
    match expl with 
    | Id (str) -> if (mem envl expl) then (find envr expl) else expl
    | Lam (str, e_in) -> 
        let result = (find envr (Id str)) in
        (match result with
        | Id (str_in) -> Lam (str_in, rm_dup_names (add_names envl (Id str)) envr e_in)
        | _ -> raise (Error "invalid env err")
        )
    | App (l, r) -> App ((rm_dup_names envl envr l),(rm_dup_names envl envr r))

  let rec my_reduce : env -> Lambda.lexp -> Lambda.lexp = fun env exp ->
    match exp with
    (*Normal-order reduction*)
    | Id (str) -> 
        let ans = (find env exp) in
        if (ans=exp) then ans (*basic constant*)
        else (my_reduce env ans)
    | Lam (str, e_in) -> (
        if (mem env (Id str)) 
        then (
          let name = get_name () in
          Lam (name, (my_reduce (add env (Id str) (Id name)) e_in))
        )
        else Lam (str, (my_reduce env e_in)))
    | App (l, r) ->
        let l_new = rm_dup_names empty_env (add_names empty_env r) (my_reduce env l) in
        (match l_new with
          | Lam (str, e_in) -> my_reduce (add env (Id str) r) e_in
          | _ -> App (l_new, my_reduce env r))
  let reduce : Lambda.lexp -> Lambda.lexp = fun exp -> my_reduce empty_env exp
  end
