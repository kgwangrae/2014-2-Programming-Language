(* You should include reduce function in Lambda module and use ./test.sh to use this file*)
open M
;;
open Encode
;;
type nexp = NId of int
          | NLam of int * nexp
          | NApp of nexp * nexp
;;

let normalize e = 
    let count = ref 0 in
    let newN () = count := !count + 1; !count in

    let rec iter e env = 
        match e with
        | Lambda.Id str ->            
            let n = try List.assoc str env with Not_found -> newN () in
            (NId n)

        | Lambda.Lam (x, e') ->
            let n = newN() in
            let ne = iter e' ((x, n)::env) in
            NLam(n, ne)

        | Lambda.App (e1, e2) ->            
            NApp(iter e1 env, iter e2 env)
    in
    iter e []

;;
let equal p1 p2 =
    (normalize p1) = (normalize p2)
;;
let test inputStrM expectStrM = 
    
(*    let _ = print_endline ("Input : "^inputStrM) in*)
    let inputM = Parser.program Lexer.start (Lexing.from_string inputStrM) in
    let inputL = Encoder.encode inputM in

    let expectM = Parser.program Lexer.start (Lexing.from_string expectStrM) in
    let expectL = Encoder.encode expectM in

    if (equal (Lambda.reduce inputL) (Lambda.reduce expectL))
    then (print_endline "O") 
    else (print_endline "X")

;;
test "(fn x => (x.2 - 2)) (1,3)" "1"
;;
test "(fn x => (x + x + x)) 2" "6"     (* 1. fn, app *)
;;
test "((2 + 3) - 2)" "3"           (* 2. add sub  *)
;;
test "ifzero 0 then 1 else 2" "1"  (* 3. ifzero *)
;;
test "(4, 3).2" "3"                (* 6. pair *)
;;
test "ifzero (0 and ((rec f x => (f x)) 1)) then 1 else 0" "1" (* 7. and short-circuit*)
;;
test "(1, (2, 3)).2.1" "2" (* 8. pair pair *)
;;
test "(fn f => (f 3)) (fn x => x + 1)" "4" (* 9. high-order function *)
;;
test "(3 and 4)" "1"               (* 5. and *)
;;
test "(0 and 1)" "0"               (* 5. and *)
;;
test "(1 and 0)" "0"               (* 5. and *)
;;
test "(1 and 1)" "1"               (* 5. and *)
;;
test "((153 and 1) and (0 and 15))" "0"               (* 5. and *)
;;
test "((153 and 12) and (10 and 15))" "1"               (* 5. and *)
;;
test "(rec rrr x => ifzero ((x+1) and (x-0)) then (1,10).2 - 20 else (x+rrr(x-(3 and 5)))) 5" "15"
;;
test "(rec f x => (ifzero x then 0 else x + (f (x - 1)))) 10" "55" (* 4. rec *)
;;
test "(rec f x => ifzero (x - 1) then 1 else ((rec g y => (ifzero y.2 then 0 else ((g (y.1, (y.2 - 1))) + (y.1)))) (x, f (x - 1)) )) 3" "6" (* 10. complicated *)
;;
test "(rec psfibo x => 
    ifzero (x.1 and 1) then x.2+0 
       else ( 
               ifzero ((x.1-1) and 2) then x.2+1 
                     else ( 
                                ifzero (0 and (psfibo (2,3))) 
                                         then ( psfibo(x.1-1,x.2) + psfibo(x.1-2,x.2) ) 
                                                  else 0 (* this case will not be happend. *) 
                                                        ) 
                        ) 
) (4,3)" "18" 
;;
test "(rec psfibo x => 
    ifzero (x.1 and 1) then x.2+0 
       else ( 
               ifzero ((x.1-1) and 2) then x.2+1 
                     else ( 
                                ifzero (0 and (psfibo (2,3))) 
                                         then ( psfibo(x.1-1,x.2) + psfibo(x.1-2,x.2) ) 
                                                  else 0 (* this case will not be happend. *) 
                                                        ) 
                        ) 
) (3,2)" "8"
;;
(* psfibo is fibonacci seq. with some weight *) 
(* a0 = x.2, a1 = x.2+1, an = a(n-1) + a(n-2) *) 
(* if input (3,2), the result is 8, because a0=2, a1=3, a2=5, a3=8. *) 
