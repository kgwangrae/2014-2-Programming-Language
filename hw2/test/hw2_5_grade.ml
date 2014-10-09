open Hw2_5
open CommonGrade

let l1 = [1;5;465546;59]
let l2 = [32;23;1;2;15;2;2]
let l3 = [3;231;5;123131;211;23;7]
let l4 = [4;84]

let empty = IntListQ.emptyQ
let enQ q v = IntListQ.enQ (q, v)
let deQ q = let _, rq = IntListQ.deQ q in rq
let deQV q = let v, _ = IntListQ.deQ q in v


(* 1 *)
let _ = output (fun () -> (
    try 
        (if (deQ empty = empty) then false else false)
    with
    | IntListQ.EMPTY_Q -> true
    | _ -> false
))

(* 2 *)
let _ = output (fun () -> 
    if (l1 = deQV (enQ empty l1)) then true else false
)

(* 3 *)
let _ = output (fun () -> 
    if (l2 = deQV (deQ (enQ (enQ empty l1) l2))) then true else false
)

(* 4 *)
let _ = output (fun () -> 
    if (l3 = deQV (deQ (enQ (enQ (deQ (enQ (enQ empty l1) l2)) l3) l4))) then true else false
)

(* 5 *)
let _ = output (fun () -> 
    if (l4 = deQV (deQ (enQ (deQ (enQ (enQ (deQ (enQ empty l1)) l2) l3)) l4))) then true else false
)
