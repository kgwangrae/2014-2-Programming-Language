open CommonGrade
open Hw1_5

let _ = output (fun() ->
let t0 = insert (33, insert (22, insert (5, EMPTY))) in
(findMin t0, findMin (deleteMin t0)) (* 5,22 *)
= (5, 22)
)

let _ = output (fun() ->
let t1 = insert (6, insert (4, insert (3, EMPTY))) in
(findMin t1,findMin (deleteMin t1)) (* 3,4 *)
= (3, 4)
)

let _ = output (fun() ->
let t2 = insert (1, insert (10, insert (30, EMPTY))) in
(findMin t2,findMin (deleteMin t2)) (* 1,10 *)
= (1, 10)
)

let _ = output (fun() ->
let t3 = insert (25, insert (34, insert (9, insert (11, EMPTY)))) in
(findMin t3,findMin (deleteMin t3)) (* 9,11 *)
= (9, 11)
)

let _ = output (fun() ->
let t0 = insert (33, insert (22, insert (5, EMPTY))) in
let t1 = insert (6, insert (4, insert (3, EMPTY))) in
let t4 = merge (t0, t1) in
(findMin t4,findMin (deleteMin t4)) (* 3,4 *)
= (3, 4)
)

let _ = output (fun() ->
let t2 = insert (1, insert (10, insert (30, EMPTY))) in
let t3 = insert (25, insert (34, insert (9, insert (11, EMPTY)))) in
let t5 = merge (t2,t3) in
(findMin t5,findMin (deleteMin t5)) (* 1,9 *)
= (1, 9)
)

let _ = output (fun() ->

let t0 = insert (33, insert (22, insert (5, EMPTY))) in
let t1 = insert (6, insert (4, insert (3, EMPTY))) in
let t2 = insert (1, insert (10, insert (30, EMPTY))) in
let t3 = insert (25, insert (34, insert (9, insert (11, EMPTY)))) in
let t4 = merge (t0,t1) in
let t5 = merge (t2,t3) in
let t6 = merge (t4,t5) in
(findMin t6,findMin (deleteMin t6)) (* 1,3 *)
= (1, 3)
)

let _ = output (fun() ->
let t7 = insert (90, insert (80, insert (70, insert (60, insert (50, (insert (40, EMPTY))))))) in
(findMin t7,findMin (deleteMin t7)) (* 40, 50 *)
= (40, 50)
)

let _ = output (fun() ->
let t0 = insert (33, insert (22 ,insert (5, EMPTY))) in
let t1 = insert (6, insert (4, insert (3, EMPTY))) in
let t2 = insert (1, insert (10, insert (30, EMPTY))) in
let t3 = insert (25, insert (34, insert (9, insert (11, EMPTY)))) in
let t4 = merge (t0,t1) in
let t5 = merge (t2,t3) in
let t6 = merge (t4,t5) in
let t7 = insert (90, insert (80, insert (70, insert (60, insert(50, (insert (40, EMPTY))))))) in
let t8 = merge (t6,t7) in
(findMin t8,findMin (deleteMin t8)) (* 1,3 *)
= (1, 3)
)

let _ = output (fun() ->
let t0 = insert (33, insert (22 ,insert (5, EMPTY))) in
let t1 = insert (6, insert (4, insert (3, EMPTY))) in
let t2 = insert (1, insert (10, insert (30, EMPTY))) in
let t3 = insert (25, insert (34, insert (9, insert (11, EMPTY)))) in
let t4 = merge (t0,t1) in
let t5 = merge (t2,t3) in
let t6 = merge (t4,t5) in
let t7 = insert (90, insert (80, insert (70, insert (60, insert (50, (insert (40, EMPTY))))))) in
let t8 = merge (t6,t7) in
let t9 = merge (t8, insert (0, merge (EMPTY, EMPTY))) in
(findMin t9,findMin (deleteMin t9)) (* 0,1 *)
= (0, 1)
)
