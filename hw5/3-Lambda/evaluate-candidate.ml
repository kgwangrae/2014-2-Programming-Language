(* SNU 4190.310 Programming Languages (Fall 2014)
 * Gwangrae Kim k.gwangrae@gmail.com
 * Lambda Calculus *)
module Evaluator =
  struct
	exception Error of string

  (*get_new_name : May collide with previously added names*)
  let idx = ref (0) 
  let get_name : unit -> string = fun _ ->
    let name = "a#b" ^ (string_of_int !idx) in let _ = (idx:=!idx+1) in name

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
  let rec change_names : env -> Lambda.lexp -> Lambda.lexp = fun env exp ->
    match exp with 
    | Id (str) -> (find env exp)
    | Lam (str, e_in) -> 
        let result = (find env (Id str)) in
        (match result with
        | Id (str_in) -> Lam (str_in, change_names env e_in)
        | _ -> raise (Error "invalid env err")
        )
    | App (l, r) -> App ((change_names env l),(change_names env r))


  let rec my_reduce : env -> Lambda.lexp -> bool -> Lambda.lexp = fun env exp stop_at_lambda ->
    match exp with
    (*Normal-order reduction*)
    | Id (str) ->
        let ans = (find env exp) in
        if (ans=exp) then ans (*Case 1 : free variable*)
        else (my_reduce env ans stop_at_lambda) (*Case 2 : switch to other types*)
    | Lam (str, e_in) -> (
        if (stop_at_lambda) 
        then ( (*Just replace names inside without more reduction*)
         if (mem env (Id str)) 
          then (
            let name = get_name () in change_names (add env (Id str) (Id name)) exp
          )
          else change_names env exp
        )
        else (if (mem env (Id str)) 
          then (
            let name = get_name () in
            Lam (name, (my_reduce (add env (Id str) (Id name)) e_in false))
          )
          else Lam (str, (my_reduce env e_in false))))
    | App (l, r) ->
        (match l with (*Don't try to predict too far future*)
         | Id _ -> 
             let left_side = (my_reduce env l true) in
             if (left_side = l) (*str is a free variable.*)
             then (App (l, (my_reduce env r false))) (*reduction of r is completely new*)
             else my_reduce env (App (left_side, r)) stop_at_lambda (*jump to other types*)
         | App (_, _) -> 
             let left_side = (my_reduce env l true) in
             (match left_side with
              | Lam (_, _) -> my_reduce env (App (left_side, r)) stop_at_lambda
              | _ -> (App (left_side, (my_reduce env r false)))
             )
         | Lam (str, _) ->
            let left_names = add_names empty_env l in
            let right_cleared = rm_dup_names empty_env left_names r in
            let right_names = add_names empty_env r in 
            (match (rm_dup_names empty_env right_names l) with 
             | Lam (str_new, e_new) -> 
                 (my_reduce (add env (Id str_new) right_cleared) e_new stop_at_lambda)
             | _ -> raise (Error "invalid expr was returned from dup_names")
            )
        )

  let rec reduce : Lambda.lexp -> Lambda.lexp = fun exp -> 
    let e1 = my_reduce empty_env exp false in
    let e2 = my_reduce empty_env e1 false in
    if (e1=e2) then e1 else reduce e2  
  end
