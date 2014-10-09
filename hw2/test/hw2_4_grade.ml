open CommonGrade
open Hw2_4

let equals v1 v2 = (abs_float (v1 -. v2)) <= 0.1

let test t answer:bool =
  let v = galculator t in
  (*let _ = print_float v in
  let _ = print_char ' ' in 
  let _ = print_float answer in
  let _ = print_newline() in*)
  (equals v answer);;

let _ = output (fun() ->
    let tE = X in
    try (test tE 0.0)
    with Hw2_4.FreeVariable -> true
)

let _ = output (fun() ->
    let t0 = INT 4 in
    test t0 4.0
)

let _ = output (fun() ->
let t1 = REAL (-3.0) in
test t1 (-3.0)
)




let _ = output (fun() ->
let t2 = ADD(INT 2, REAL 3.5) in
test t2 5.5
)



let _ = output (fun() ->
let t3 = SUB(INT 2, REAL 3.5) in
test t3 (-1.5)
)



let _ = output (fun() ->
let t4 = MUL(INT 2, REAL 5.5) in
test t4 11.0
)



let _ = output (fun() ->
let t5 = DIV(REAL 3.0, INT 2) in
test t5 1.5
)



let _ = output (fun() ->
let t61 = ADD(INT 2, REAL 3.5) in
let t62 = MUL(INT 2, REAL 5.5) in
let t63 = DIV(t62, t61) in
test t63 2.0
)



let _ = output (fun() ->
let t7 = SIGMA (INT 1, INT 10, X) in
test t7 55.0
)

let _ = output (fun() ->
let t7 = SIGMA (INT 1, INT 10, (MUL (INT (-1), X))) in
test t7 (-55.0)
)


let _ = output (fun() ->
let t8 = INTEGRAL(INT 0, INT 10, INT 20) in
test t8 200.
)

let _ = output (fun() ->
let t8 = INTEGRAL(INT 10, INT 0, INT 20) in
test t8 (-200.)
)


let _ = output (fun() ->
    let tE = (ADD (INT 1, X)) in
    try (test tE 0.0)
    with Hw2_4.FreeVariable -> true
)

let _ = output (fun() ->
test (INTEGRAL (REAL 1.3, REAL 1.41,
            SIGMA (INT 1, INT 1, X))) 0.1
)


let _ = output (fun() ->
let t11 = SIGMA (INT 1, INT 5, INTEGRAL (INT 0, ADD (X, REAL 0.01), X)) in
test t11 26.75
)
(* print_char ' '; 
print_float 26.75; *)


let _ = output (fun() ->
let t12 = SIGMA (INT 1, INT 5, SIGMA (X, X, SIGMA (X, ADD (X, INT 1), X))) in
test t12 35.0
)


let _ = output (fun() ->
    let tE = (SIGMA (INT 1, X, INTEGRAL (X, X, SIGMA (X, ADD (X, INT 1), X)))) in
    try (test tE 0.0)
    with Hw2_4.FreeVariable -> true
)


let _ = output (fun() ->
let t14 = SIGMA (INT 1, INT 3, SIGMA (INT 1, INT 5, X)) in
test t14 45.0
)


let _ = output (fun() ->
let t14 = SIGMA (INT 0, INT 10, SIGMA (X, ADD(X, INT 10), INT 3)) in
test t14 363.0
)

let _ = output (fun() ->
let t15 = INTEGRAL (ADD (INT 1, REAL 0.11), INT 1, X) in
test t15 (-0.1)
)



let _ = output (fun() ->
let t16 = ADD (INTEGRAL (INT 1, INT 5, X), INTEGRAL (INT 1, INT 5, SUB (INT 0, X))) in
test t16 0.0
)


let _ = output (fun() ->
    let tE = (ADD (SIGMA (INT 1, INT 5, X), SIGMA (X, INT 5, SUB (INT 0, X)))) in
    try (test tE 0.0)
    with Hw2_4.FreeVariable -> true
)


let _ = output (fun() ->
let t18 = SIGMA (SIGMA (INT 1, INT 1, X), SIGMA (INT 3, INT 3, X), SIGMA (INT 5, INT 5, X)) in
test t18 15.0
)

(*let _ = output (fun() ->
    let t19 = (INTEGRAL(INT 0, INT 100000, DIV (SUB(MUL(INT 16, DIV(X, INT 100000)), INT 16),ADD(SUB(MUL(MUL(DIV(X, INT 100000),DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000))), MUL(MUL(INT 2,DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000)))) , SUB(MUL(INT 4, DIV(X, INT 100000)), INT 4) ) ) )) in
    test t19 314159.46535597858
)*)

(*let _ = output (fun() ->
let t20 = (SIGMA(INT 1, INT 1000000, DIV(INT 8, MUL(SUB(MUL(INT 2, X), INT 1),SUB(MUL(INT 2, X), INT 1))))) in
test t20 9.86960240108985332 
)*)
