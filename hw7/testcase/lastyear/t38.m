let val swap =
fn order_pair =>
if (order_pair.1) (order_pair.2)
then (order_pair.2)
else (order_pair.2.2, order_pair.2.1)
in
swap(fn pair => pair.1 + 1 = pair.2, (1,2));
swap(fn pair => pair.1 or pair.2, (true, false))
end
(* S K I combinators *)
let val I = fn x => x
val K = fn x => fn y => x
val S = fn x => fn y => fn z => (x z) (y z)
in
S (K (S I)) (S (K K) I) 1 (fn x => x+1)
end