open CommonGrade
open Hw1_3

let _ = output (fun() ->
natadd (ZERO, ZERO)
= 
ZERO
)
let _ = output (fun() ->
natadd (ZERO, (SUCC (SUCC ZERO)))
= 
(SUCC (SUCC ZERO))
)
let _ = output (fun() ->
natadd ((SUCC (SUCC ZERO)), ZERO)
= 
(SUCC (SUCC ZERO))
)
let _ = output (fun() ->
natadd ((SUCC (SUCC ZERO)), (SUCC (SUCC (SUCC ZERO))))
= 
(SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))
)
let _ = output (fun() ->
natmul (ZERO, ZERO)
= 
ZERO
)
let _ = output (fun() ->
natmul (ZERO, (SUCC (SUCC ZERO)))
= 
ZERO
)
let _ = output (fun() ->
natmul ((SUCC (SUCC ZERO)), ZERO)
= 
ZERO
)
let _ = output (fun() ->
natmul (SUCC ZERO, SUCC ZERO)
= 
(SUCC ZERO)
)
let _ = output (fun() ->
natmul ((SUCC (SUCC ZERO)), (SUCC (SUCC (SUCC ZERO))))
= 
(SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))))
)
let _ = output (fun() ->
natmul ((SUCC (SUCC (SUCC ZERO))), (SUCC (SUCC ZERO)))
= 
(SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))))
