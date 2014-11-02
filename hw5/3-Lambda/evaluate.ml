(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 * Gwangrae Kim k.gwangrae@gmail.com
 * Lambda Calculus
 *)

module Evaluator =
  struct
	exception Error of string
  
  let idx = ref (0) 
  let get_name : unit -> string = fun _ ->
    let name = "u1AaQz" ^ (string_of_int !idx) in
    let _ = (idx:=!idx+1) in
    name
  (*May collide with previously added names*)

  (*May use hashtables for speeding up*)
  type env = (Lambda.lexp * Lambda.lexp) list 
  let empty_env = []
  let add : env -> Lambda.lexp -> Lambda.lexp -> env = fun e k v -> (k,v)::e
  let rec find : env -> Lambda.lexp -> Lambda.lexp = fun e k ->
    match e with 
    | (k_in,v_in)::t -> if (k=k_in) then v_in else (find t k)
    | _ -> k 
  let rec mem : env -> Lambda.lexp -> bool = fun e k ->
    match e with 
    | (k_in,v_in)::t -> if (k=k_in) then true 
                        else if (k=v_in) then true else (mem t k)
    | _ -> false

  let rec my_reduce : env -> Lambda.lexp -> Lambda.lexp = fun env exp ->
    match exp with 
    | Id (str) -> find env exp
    | Lam (str, e_in) -> (
        if (mem env (Id str)) 
        then (
          let name = get_name () in
          Lam (name, (my_reduce (add env (Id str) (Id name)) e_in))
        )
        else Lam (str, (my_reduce env e_in)))
    | App (l, r) ->
        let l_new = my_reduce env l in
        let r_new = my_reduce env r in
        (match l_new with
          | Lam (str, e_in) -> my_reduce (add env (Id str) r_new) e_in
          | _ -> App (l_new, r_new) 
        )

  let reduce : Lambda.lexp -> Lambda.lexp = fun exp -> my_reduce empty_env exp

  end
