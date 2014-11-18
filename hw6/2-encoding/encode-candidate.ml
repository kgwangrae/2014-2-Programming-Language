(*
 * SNU 4190.310 Programming Languages 
 * k.gwangrae@gmail.com, Gwangrae Kim, Seoul National University CSE
 * M0 (an eager evaluation language) to Lambda expression (for normal order reduction) translator
 *)
open M
module Encoder = 
  struct
  	 exception Error of string
     (* NOTE 
     * Q: Does it correctly translates an eager evaluated language to normal-order-reduced lambda terms?
     * A: According to the assignment specification, 
     * only programs which are able to be eagerly evaluated are eligible for translation.
     * Those programs always have a same normal form when they're reduced in normal order,
     * though converse of this proposition does not hold.
     *)
    let rec encode : M.mexp -> Lambda.lexp = fun pgm -> 
      let bool_t = Lambda.Lam ("###x", Lambda.Lam ("###y", Lambda.Id "###x"))
      in
      let bool_f = Lambda.Lam ("###x", Lambda.Lam ("###y", Lambda.Id "###y"))
      in
      let is_zero = 
        Lambda.Lam ("###n", Lambda.App ((Lambda.App ((Lambda.Id "###n"), Lambda.Lam ("###x", bool_f))), bool_t))
      in
      let is_non_zero = 
        Lambda.Lam ("###n", Lambda.App ((Lambda.App ((Lambda.Id "###n"), Lambda.Lam ("###x", bool_t))), bool_f))
      in
      match pgm with
      | Num n -> (* The value of non-negative n is known at translation time*)
          let rec make_num : int -> Lambda.lexp = fun n ->
            if (n<0) then raise (Error "Negative int is not allowed")
            else if (n=0) 
            then (Lambda.Id "###x")
            else (Lambda.App ((Lambda.Id "###f"), make_num (n-1)))
          in
          Lambda.Lam("###f",Lambda.Lam("###x",make_num n))
      | Var id -> Lambda.Id id
      | Fn (id_arg, e_in) -> Lambda.Lam(id_arg, encode e_in)
      | App (e_l, e_r) -> Lambda.App ((encode e_l), encode e_r)
      | Rec (id_fun, id_arg, e_in) -> 
          let fix_combinator =
            let iter = 
              Lambda.Lam ("###x", Lambda.App ((Lambda.Id "###f"), Lambda.App ((Lambda.Id "###x"), (Lambda.Id "###x"))))
            in
            Lambda.Lam ("###f", Lambda.App (iter, iter))
          in
          Lambda.App (fix_combinator, Lambda.Lam(id_fun, Lambda.Lam(id_arg, encode e_in)))
      | Ifz (e_cond, e_t, e_f) -> (* ifzero *)
          Lambda.App ((Lambda.App ((Lambda.App (is_zero, encode e_cond)), encode e_t)), encode e_f)
      | Add (e1, e2) -> 
          (* The values of terms are not known at translation time.
           * add n m : f^n x = n f x, f^m y = m f y ----> f^(n+m) = m f (n f x) *)
          let add =
            Lambda.Lam ("###m", Lambda.Lam ("###n", Lambda.Lam ("###f", Lambda.Lam ("###x", 
              Lambda.App(
                (Lambda.App ((Lambda.Id "###m"), Lambda.Id "###f")),
                Lambda.App ((Lambda.App ((Lambda.Id "###n"), Lambda.Id "###f")), Lambda.Id "###x")
              )
            ))))
          in
          Lambda.App ((Lambda.App (add, encode e1)), encode e2)
      | Sub (e1, e2) -> 
          let pred = 
            Lambda.Lam ("###n", Lambda.Lam("###f", Lambda.Lam("###x", 
              Lambda.App (
                Lambda.App (
                  Lambda.App ((Lambda.Id "###n")
                    , Lambda.Lam ("###g"
                      , Lambda.Lam ("###h"
                        , Lambda.App (Lambda.Id ("###h")
                          , Lambda.App (Lambda.Id ("###g"), Lambda.Id "###f")
                        )
                      )
                    )
                  )
                  , Lambda.Lam ("###u", Lambda.Id "###x")
                )
                , Lambda.Lam ("###u", Lambda.Id "###u")
              )
            )))
          in
          let sub = 
            Lambda.Lam ("###m", Lambda.Lam ("###n",
              Lambda.App (Lambda.App (Lambda.Id "###n", pred), Lambda.Id "###m")))
          in
          Lambda.App ((Lambda.App (sub, encode e1)), encode e2)
      | And (e1, e2) -> 
          let and_op = 
            Lambda.Lam ("###p", Lambda.Lam ("###q", 
              Lambda.App (
                Lambda.App (Lambda.Id ("###p"), Lambda.Id ("###q"))
                , Lambda.Id "###p"
              ) (* if p then q else p, all boolean *)
            ))
          in
          Lambda.App(
            Lambda.App (
              (Lambda.App (
                (Lambda.App (and_op, Lambda.App (is_non_zero, encode e1)))
                , Lambda.App (is_non_zero, encode e2)))
            , encode (M.Num 1)) (*True*)
          , encode (M.Num 0)) (*False*)
      | Pair (e1, e2) -> 
          let pair =
            Lambda.Lam("###l", (Lambda.Lam ("###r", Lambda.Lam ("###f", 
              Lambda.App (
                Lambda.App (Lambda.Id ("###f"), Lambda.Id "###l")
                , Lambda.Id "###r"
              )
            ))))
          in
          Lambda.App ((Lambda.App (pair, encode e1)), encode e2)
      | Fst (e_pair) -> 
          let car = Lambda.Lam ("###pair", Lambda.App (Lambda.Id "###pair", bool_t))
          in
          Lambda.App (car, encode e_pair)
      | Snd (e_pair) -> 
          let cdr = Lambda.Lam ("###pair", Lambda.App (Lambda.Id "###pair", bool_f))
          in
          Lambda.App (cdr, encode e_pair)
  end
