(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 * Gwangrae Kim @SNU CSE, k.gwangrae@gmail.com
 * Low Fat M: static simple type checker
 * Type checking before running a program
 *)

open M
module M_SimChecker : M_SimTypeChecker = struct
  open M
  exception Not_Found
  let tyerr = fun str -> raise (TypeError str)

  type typevar =
    | Int
    | Bool
    | String
    | Pair of typevar * typevar
    | Loc of typevar
    | Fun of typevar * typevar (* Fun type : typevar1 -> typevar2 *)
    | Unknown of int (* Type variables which are 'unknown' at some moments *)
    | Oneof of typevar * (typevar list) 
      (* a = Int | Bool | String ... type like this.*)
      (* NOTE : Nesting Oneof, putting any duplicate into Oneof is not permitted*)
      (* typevar at first is the identifier of this OR type *)
  let var_count = ref 0
  let get_new_var : unit -> typevar = fun _ -> 
    let _ = (var_count := ((!var_count) + 1)) in (Unknown (!var_count))
  
  type typeenv = (id * typevar) list
  let env_add : typeenv -> id -> typevar -> typeenv = fun env id ty -> 
    (id, ty)::env
  let rec ty_equal : typevar -> typevar -> bool = fun ty1 ty2 ->
    let rec list_equal = fun lst1 lst2 ->
      let rec list_equal_rec = fun lst_a ->
        match lst_a with
        | hd::tl -> if (List.exists (fun x -> ty_equal x hd) lst2) 
                    then (list_equal_rec tl)
                    else false
        | [] -> true
      in
      if ((List.length lst1) = (List.length lst2)) then (list_equal_rec lst1)
      else false
    in
    match (ty1, ty2) with
    | (Int, Int) | (Bool, Bool) | (String, String) -> true
    | ((Pair (l1, r1)), Pair (l2, r2)) -> (ty_equal l1 l2) && (ty_equal r1 r2)
    | ((Fun (l1, r1)), Fun (l2, r2)) -> (ty_equal l1 l2) && (ty_equal r1 r2)
    | (Loc l1, Loc l2) -> ty_equal l1 l2 
    | (Unknown i1, Unknown i2) -> i1 = i2
    | ((Oneof (name1,tylst1)), Oneof (name2, tylst2)) -> 
        (list_equal tylst1 tylst2) && (name1 = name2)
    | (Unknown _, Oneof (_, tylst)) -> List.mem ty1 tylst
    | ((Oneof (_, tylst)), Unknown _) -> List.mem ty2 tylst 
    | (_, _) -> false

  let rec get = fun lst k comparator ->
    match lst with 
    | (k_in, v)::tl -> if (comparator k_in k) then v else get tl k comparator
    | [] -> raise Not_Found
  
  let rec rm_dup = fun lst ->
    match lst with
    | hd::tl -> if (List.exists (fun x -> ty_equal x hd) tl) 
                then (rm_dup tl) else hd::(rm_dup tl)
    | [] -> []
  let rec sanitize_oneof : typevar list -> typevar list = fun tylst ->
    let result_unnest = (
      match tylst with
      | hd::tl -> (
          match hd with
          | Oneof (_, tylst_in) -> (sanitize_oneof tylst_in)@(sanitize_oneof tl)
          | _ -> hd::(sanitize_oneof tl)
        )
      | [] -> []
    ) 
    in
    rm_dup result_unnest
  let sanitize : typevar -> typevar = fun (Oneof (name, tylst)) -> 
    let result = sanitize_oneof tylst in
    match result with 
    | [] -> raise (TypeError "No such type is possible")
    | hd::[] -> hd
    | hd::tl -> Oneof (name, result)
  
  (* Substitution : (Unknown _ / Oneof _ -> typevar) list.
   * Used to construct / apply solutions of type equations *)
  type subst = (typevar * typevar) list 
  let subst_add : subst -> typevar -> typevar -> subst = fun subst ty1 ty2 -> 
    (ty1, ty2)::subst
  let rec subst_apply_one : subst -> typevar -> typevar = fun subst ty ->
    match ty with
    | Int | Bool | String -> ty
    | Pair (l, r) -> Pair ((subst_apply_one subst l), subst_apply_one subst r)
    | Loc l -> Loc (subst_apply_one subst l)
    | Fun (arg, ret) -> Fun ((subst_apply_one subst arg), subst_apply_one subst ret)
    | Unknown _ -> ( 
        try get subst ty (fun x y -> x=y)
        with Not_Found -> ty
      )
    | Oneof (name, tylst) -> 
        try (get subst ty ty_equal)
        with Not_Found ->  
        let rec subst_apply_lst = fun lst ->
          match lst with
          | hd::tl -> (subst_apply_one subst hd)::(subst_apply_lst tl)
          | [] -> []
        in
        sanitize(Oneof (name, subst_apply_lst tylst))
  let rec subst_apply_env : subst -> typeenv -> typeenv = fun subst tyenv ->
    match tyenv with
    | (id,ty)::tl -> (id, subst_apply_one subst ty)::(subst_apply_env subst tl)
    | [] -> []

  (* To filter out instances like a = a -> a 
   * target to find : STRICTLY limited to 'Unknown' typevar
   * returns : does the src contain target inside? 
   * NOTE : assumed target != src, target != or (..., target, ...) *)
  let rec contain : typevar -> typevar -> bool = fun target src ->
    match src with
    | Int | Bool | String -> false
    | Pair (l, r) -> (contain target l) || (contain target r)
    | Loc l -> (contain target l)
    | Fun (arg, ret) -> (contain target arg) || (contain target ret)
    | Unknown _ -> target = src
    | Oneof (_, tylst) -> 
        let rec contain_lst = fun lst ->
          match lst with
          | hd::tl -> if (contain target hd) then true else (contain_lst tl)
          | [] -> false
        in
        (contain_lst tylst)

  (* compose two subst into one subst, respecting its order : S2(S1) *)
  let rec compose : subst -> subst -> subst = fun snd fst ->
    match (snd, fst) with
    | ([], _) -> fst
    | (_, []) -> snd
    | (_, (k,v)::tl) -> (k, subst_apply_one snd v)::(compose snd tl)
  
  (* HOW TWO TYPE REQUIREMENTS CAN BE UNIFIED *)
  let rec unify : typevar -> typevar -> subst = fun ty1 ty2 ->
    let rec unify_lst : typevar list -> typevar -> (subst * int) = fun lst ty ->
      (match lst with
      | [] -> ([], 0)
      | hd::tl -> 
          let (subst_result, success_cnt) = unify_lst tl ty in
          try ((unify hd ty), success_cnt+1)
          with TypeError _ -> (subst_result, success_cnt)
      ) 
    in
    let rec unify_lst_lst : typevar list -> typevar list -> (subst * (typevar list)) 
    = fun lst1 lst2 ->
      (match lst1 with
      | [] -> ([], [])
      | hd::tl ->  
          let (curr_subst_result, curr_success_cnt) = unify_lst lst2 hd in
          let (next_subst_result, possible_types) = unify_lst_lst tl lst2 in
          if (curr_success_cnt = 0) then (next_subst_result, possible_types)
          else ((curr_subst_result@next_subst_result), hd::possible_types)
      )
    in
    if (ty_equal ty1 ty2) then []
    else match (ty1, ty2) with
    | (Unknown _, _) ->  
        if (contain ty1 ty2) 
        then raise (TypeError "No such type exists - include self")
        else subst_add [] ty1 ty2
    | (_, Unknown _) -> unify ty2 ty1
    | ((Pair (l1, r1)), Pair (l2, r2)) -> 
        let s1 = unify l1 l2 in
        let s2 = unify (subst_apply_one s1 r1) (subst_apply_one s1 r2) in
        compose s2 s1
    | ((Fun (arg1, ret1)), Fun (arg2, ret2)) -> 
        let s1 = unify arg1 arg2 in
        let s2 = unify (subst_apply_one s1 ret1) (subst_apply_one s1 ret2) in
        compose s2 s1
    | ((Loc l1), Loc l2) -> unify l1 l2
    (* Oneof only contains i, b, s, l (tyvar) *)
    | ((Oneof (_,tylst1)), Oneof (_,tylst2)) -> (
        let (subst_result, possible_types) = unify_lst_lst tylst1 tylst2 in
        let type_count = List.length possible_types in
        if (type_count = 0) then tyerr "unify_fail"
        else if (type_count = 1) then (
          let target_type = List.hd possible_types in
          (subst_add (subst_add subst_result ty1 target_type) ty2 target_type)
        )
        else 
          let new_or_cond = sanitize(Oneof ((get_new_var()), possible_types)) in 
          (subst_add (subst_add subst_result ty1 new_or_cond) ty2 new_or_cond)
      )
    | ((Oneof (_, tylst)), _) ->
        let (subst_result, success_cnt) = unify_lst tylst ty2 in
        if (success_cnt = 0) then tyerr "unify_fail"
        else if (success_cnt = 1) (* Unique matching found *)
          then (subst_add subst_result ty1 ty2)
        else [] (* TODO : not sure - nothing can be determined yet *)
    | (_, Oneof (_,_)) -> unify ty2 ty1
    | (_, _) -> raise (TypeError "No such type exists")

  (* HOW THE GIVEN EXPR CAN BE GIVEN TYPE UNDER CURRENT TYPE ENV 
   * NOTE : if an expression shall be two or more types, 
   * unify them at first and then this algorithm is applied.*)
  let rec m_algo : (typeenv * exp * typevar) -> subst = fun (env, exp, tyvar) -> 
    match exp with
    | CONST n -> (
      match n with
      | S _ -> unify String tyvar
      | N _ -> unify Int tyvar
      | B _ -> unify Bool tyvar
      )
    (* Assuming valid program : No free variable exists *)
    | VAR id -> (
        try (unify (get env id (fun x y -> x = y)) tyvar)
        with Not_Found -> raise (TypeError "Free variable exists")
      )
    | FN (id, e_in) -> (
        let new_var1 = get_new_var () in
        let new_var2 = get_new_var () in
        let s1 = unify (Fun (new_var1, new_var2)) tyvar in
        let s2 = m_algo (
          (env_add (subst_apply_env s1 env) id (subst_apply_one s1 new_var1)),
          e_in, 
          (subst_apply_one s1 new_var2))
        in
        compose s2 s1
      )
    | APP (fun_body, arg) -> (
        let new_var = get_new_var () in
        let s1 = m_algo (env, fun_body, Fun(new_var, tyvar)) in
        let s2 = m_algo ((subst_apply_env s1 env), arg, subst_apply_one s1 new_var) in
        compose s2 s1
      )
    | LET (decl, e_in) -> (
      match decl with
      | REC (id, e_decl) -> ( (*Recursive Functions*)
        let new_var = get_new_var () in
        let new_var2 = get_new_var () in
        let s1 = m_algo (
          (env_add env id (Fun(new_var, new_var2))), 
          e_decl, 
          Fun(new_var, new_var2)
        ) 
        in 
        let s2 = m_algo (
          (subst_apply_env s1 (env_add env id (Fun(new_var, new_var2)))),
          e_in, 
          subst_apply_one s1 tyvar
        ) 
        in
        compose s2 s1 
        )
      | NREC (id, e_decl) -> ( (*Any binding*)
        let new_var = get_new_var () in
        let s1 = m_algo (env, e_decl, new_var) in 
        let s2 = m_algo (
          (subst_apply_env s1 (env_add env id new_var)), 
          e_in, 
          subst_apply_one s1 tyvar
        ) 
        in
        compose s2 s1 
        )
      )
    | IF (e_cond, e_t, e_f) -> (
      (*Check in conservative manner : both e_t and e_f should have correct type*)
      let s1 = m_algo (env, e_cond, Bool) in 
      let env1 = subst_apply_env s1 env in
      let tyvar1 = subst_apply_one s1 tyvar in
      let s2 = m_algo (env1, e_t, tyvar1) in
      let env2 = subst_apply_env s2 env1 in
      let tyvar2 = subst_apply_one s2 tyvar1 in
      let s3 = m_algo (env2, e_f, tyvar2) 
      in
      compose s3 (compose s2 s1)
      )
    | BOP (bop, e_l, e_r) -> (* BInary Operation : ADD, SUB ...  *)
      (*Overloaded function*)
      let m_algo_bop : typevar -> bool -> subst = fun typebase is_eq ->
        let s1 = unify (if (is_eq) then Bool else typebase) tyvar in 
        let env1 = subst_apply_env s1 env in
        let tyvar1 = subst_apply_one s1 typebase in
        let s2 = m_algo (env1, e_l, tyvar1) in
        let env2 = subst_apply_env s2 env1 in
        let tyvar2 = subst_apply_one s2 tyvar1 in
        let s3 = m_algo (env2, e_r, tyvar2) in
        compose s3 (compose s2 s1)
      in
      (match bop with 
       | ADD | SUB -> m_algo_bop Int false
       | AND | OR -> m_algo_bop Bool false
       (* tyvar = typebase = e_l = e_r for above cases! 
        * but tyvar = Bool, typebase = e_l = e_r for EQ. *)
       | EQ -> m_algo_bop (Oneof ((get_new_var ()), [Int;Bool;String;(Loc (get_new_var()))])) true
      )
    | READ -> unify Int tyvar
    | WRITE e_in -> (
      let m_algo_write : typevar -> subst = fun typebase ->
        let s1 = unify typebase tyvar in 
        let env1 = subst_apply_env s1 env in
        let s2 = m_algo (env1, e_in, subst_apply_one s1 tyvar) 
        in
        compose s2 s1
      in
      m_algo_write (Oneof ((get_new_var ()), [Int;Bool;String]))
      )
    | MALLOC e_in -> (
      let new_var = get_new_var () in
      let s1 = unify tyvar (Loc new_var) in 
      let env1 = subst_apply_env s1 env in
      let s2 = m_algo (env1, e_in, (subst_apply_one s1 new_var)) 
      in
      compose s2 s1
      )
    | ASSIGN (e_loc, e_val) ->
      let s1 = m_algo (env, e_loc, (Loc tyvar)) in
      let env1 = subst_apply_env s1 env in
      let s2 = m_algo (env1, e_val, subst_apply_one s1 tyvar) 
      in
      compose s2 s1
    | BANG (e_loc) -> m_algo (env, e_loc, (Loc tyvar))
    | SEQ (e1, e2) -> 
      let s1 = m_algo (env, e1, get_new_var ()) in
      let env1 = subst_apply_env s1 env in
      let s2 = m_algo (env1, e2, subst_apply_one s1 tyvar) 
      in
      compose s2 s1
    | PAIR (e_l, e_r) -> 
      let new_var = get_new_var () in
      let new_var2 = get_new_var () in
      let s1 = unify tyvar (Pair (new_var, new_var2)) in
      let env1 = subst_apply_env s1 env in
      let s2 = m_algo (env1, e_l, subst_apply_one s1 new_var) in
      let env2 = subst_apply_env s2 env in
      let s3 = m_algo (env2, e_r, subst_apply_one s2 new_var2) 
      in
      compose s3 (compose s2 s1)
    | SEL1 (e_pair) -> m_algo (env, e_pair, (Pair (tyvar, get_new_var ())))
    | SEL2 (e_pair) -> m_algo (env, e_pair, (Pair (get_new_var (), tyvar)))
        
  let rec typevar_to_types : typevar -> types = fun tv ->
    match tv with
    | Int -> TyInt
    | Bool -> TyBool
    | String -> TyString
    | Pair (l, r) -> TyPair ((typevar_to_types l), typevar_to_types r)
    | Loc l -> TyLoc (typevar_to_types l)
    | Fun (arg, ret) -> TyArrow ((typevar_to_types arg), typevar_to_types ret)
    | Unknown _ -> raise (TypeError "Programs must not have any free variable")
    | Oneof _ -> raise (TypeError "Programs must not have any free OR type")
    (*NOTE : 'A Program' should yield some value (or function) if it ends
     * FInal Result can't be polymorphic - according to the specification *)
  
  let check : exp -> types = fun pgm ->
    let new_var = get_new_var () in
    let s_result = m_algo ([], pgm, new_var) in
    typevar_to_types (subst_apply_one s_result new_var)
end
