open Hw4_2

let comp = fun (a,b) -> if(a=b) then (print_endline "GOOD") else (print_endline "BAD")

;;
let exactly_has_one e l =
    (List.mem e l) && ((List.length l) = 1)

let exactly_has_two e1 e2 l =
    (List.mem e1 l) && (List.mem e2 l) && ((List.length l) = 2)

let exactly_has_three e1 e2 e3 l =
    (List.mem e1 l) && (List.mem e2 l) && (List.mem e3 l) && ((List.length l) = 3)
;;

comp((exactly_has_two Bar (Node (Bar, Node (Node (Bar, Bar), Node (Bar, Bar)))) (getReady (Guide ("3", Branch (Branch (Branch (End (NameBox "1"), End (NameBox "2")), Guide ("3", End (NameBox "3"))), End (NameBox "4"))) ))), true)
;;
comp((exactly_has_three Bar (Node (Bar, Bar)) (Node (Node (Bar, Bar), Node (Bar, Bar))) (getReady (Guide ("3", Branch (Branch (Branch (Guide ("2", Guide ("1", Branch (End (NameBox "1"), End (NameBox "2")))), Guide ("3", End (NameBox "3"))), Guide ("4", End (NameBox "4"))), End (NameBox "5"))) ))), true)
;;

try(let _ = getReady(Branch(Guide("b",Branch(End (NameBox "a"), End(NameBox "b"))), Guide("a", Branch(End (NameBox "b"), End(NameBox "a"))))) in
  comp (false, true)
)
with IMPOSSIBLE -> comp (true, true)
;;
try(let _ = getReady(Branch (Branch (End (NameBox "1"), Guide ("3", Guide ("3", Guide ("3", Branch (End (StarBox), Guide ("3", Branch (Guide ("2", End (NameBox "2")), Guide ("3", Guide ("3", Guide ("3", End (NameBox "3"))))))))))), End (StarBox))) in comp (false, true))
with IMPOSSIBLE -> comp (true, true)
;;
try(let _ = getReady(Guide ("1", Guide ("1", Branch (End (StarBox), Guide ("1", Guide ("1", Guide ("1", Guide ("1", Guide ("1", End (NameBox "1")))))))))) in comp (false, true))
with IMPOSSIBLE -> comp (true, true)
;;


let a = End (NameBox "x");;
let s = End (StarBox);;
let b = Branch (a,a);;
let c = Guide ("x", a);;
let d = Branch (c, s);;

let y = End (NameBox "y");;
let y_guide = Guide ("y",y);;
let z = End (NameBox "z");;
let a1 = Branch (z, s);;
let a2 = Branch (y, a1);;
let a3 = Branch (a, a2);;
let a4 = Branch (c, y_guide);;
let a5 = Branch (a4, a1);;
let yx = Branch (y, a);;
let xyx = Branch (a, yx);;
let yxyx = Guide ("y", xyx);;

let bb = Node (Bar, Bar);;
let bbb = Node (bb, Bar);;
(*2*)
comp((exactly_has_two bbb bb
(getReady (Guide ("x", yxyx)))),true);;

(*2*)
comp((exactly_has_two Bar (Node (Bar, Bar))
(getReady a3)),true);;

(*2*)
comp((exactly_has_two Bar (Node (Bar, Bar))
(getReady a5)),true);;

(*11*)
comp((exactly_has_one Bar (getReady (a))),true)
;;

(*12*)
comp((exactly_has_one Bar (getReady (c))),true)
;;

(*13*)
comp((exactly_has_one Bar (getReady (d))),true)
;;

(*14*)
try comp((let _ = getReady (b) in true),false)
with IMPOSSIBLE -> comp (true, true);;

(*14-2*)
try comp((let _ = getReady (Branch (s,s)) in true),false)
with IMPOSSIBLE -> comp (true, true);;

let e = Guide ("x",b);;
let f = Branch (e, s);;

(*15*)
try comp((let _ = getReady (f) in true),false)
with IMPOSSIBLE -> comp (true, true);;

(*1*)
comp((exactly_has_one Bar
(getReady (Branch (Guide ("x", End (NameBox "x")), Branch (Guide ("y", End (NameBox "y")), End StarBox))))

),true)
;;
(*2*)
comp((exactly_has_two Bar (Node (Bar, Bar))
(getReady (Branch (Guide ("x", End (NameBox "x")), Guide ("y", End (NameBox "y")))))

),true)
;;


(*3*)
comp((exactly_has_two Bar (Node (Bar, Bar))
(getReady (Branch (End (NameBox "x"), End StarBox)))

),true)
;;

(*4*)
comp((exactly_has_two Bar (Node (Bar, Bar))
(getReady (Branch( End (NameBox "y"), Branch (End (NameBox "x"), End StarBox))))

),true)
;;

(*5*)
comp((exactly_has_three Bar (Node (Bar, Bar)) (Node (Node (Bar, Bar), Bar))
(getReady (Branch( Guide("y", End(NameBox"y")), Guide("x", Branch( End(NameBox"x"), End StarBox )) )))

),true)

;;
(*6*)
comp((exactly_has_three Bar (Node (Bar, Bar)) (Node (Bar, Node (Node (Node (Bar, Bar), Bar), Bar)))
(getReady (Branch (Branch (End (NameBox "y"), End StarBox ), Guide ("x", Branch (End (NameBox "x"), End StarBox)))))

),true)

;;
(*7*)
comp((exactly_has_three Bar (Node (Bar, Bar)) (Node (Node (Node (Bar, Bar), Bar), Bar))
(getReady (Branch (End (NameBox "z"),
	  Guide ("x", Branch( Guide("y", Branch( End (NameBox "x"), End (NameBox "y"))), End StarBox)))))

),true)

;;
(*8*)
comp((exactly_has_two Bar (Node (Bar, Bar))
(getReady (Guide ("x", Guide ("y", Guide ("z", Branch (End (NameBox "x"),
	  Branch (End (NameBox "z"), End (NameBox "y"))))))))

),true)
;;

(*9*)
comp((exactly_has_two Bar (Node (Bar, Node (Bar, Bar)))
(getReady (Branch (Branch (Branch (Guide ("x", Guide ("y", Guide ("z", Branch
	  (Branch (End (NameBox "x"), End (NameBox "y")), End (NameBox "z"))))),
	  End (NameBox "a")), End (NameBox "b")), End (NameBox "c"))))

),true)
;;

(*10*)
comp((exactly_has_two Bar (Node (Bar, Bar))
(getReady (Branch (End (NameBox "x"), Branch (End (NameBox "z"), End (NameBox "y")))))

),true)
;;

