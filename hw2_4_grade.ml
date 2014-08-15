open Hw2_4.Zexpr
open CommonGrade

let (|>) g f = f g

let err_int = -9999999

let test exp : int =
	try (int_of_value (eval (emptyEnv, exp)))
  with 
  |Error str -> let _ = print_string str in err_int

(*1*)
let _ = output (fun () -> ((test (NUM 1)) = 1))

(*2*)
let _ = output (fun() ->((test (PLUS (NUM 2, MINUS (NUM 3, NUM 5)))) = 0))

(*3*)
let _ = output (fun() ->((test (MULT (NUM 3, DIVIDE (NUM 6, NUM 2)))) = 9))

(*4*)
let _ = output (fun() ->((test (MAX [])) = 0))

(*5*)
let _ = output (fun() ->((test (MAX [NUM 1; NUM 3; NUM 2])) = 3))

(*6 - Exception*)
let _ = output (fun() ->((test (VAR "x")) = err_int))

(*7*)
let _ = output (fun() ->((test (LET ("x", NUM 7, VAR "x")))= 7))

(*8*)
let _ = output (fun() ->((test (
  LET ("x", NUM 8, LET ("y", NUM 88, VAR "x")))) = 8))

(*9*)
let _ = output (fun() ->((test (
  LET ("x", NUM 9, LET ("x", NUM 99, VAR "x")))) = 99))

(*10*)
let _ = output (fun() ->((test (
  LET ("x", NUM 10, LET ("y", NUM 100, DIVIDE (VAR "y", VAR "x"))))) = 10))


(*11*)
let _ = output (fun() ->((test (
  MAX [NUM 1; LET ("x", NUM 3, VAR "x"); NUM 2])) = 3))

(*12*)
let _ = output (fun() ->((test (
  LET ("x", NUM 12,
	  PLUS (
		  LET ("x", NUM 121, VAR "x"),
  		LET ("y", NUM 121, VAR "x")
	  )
  ))) = 133))


(*13*)
let _ = output (fun() ->((test (
LET ("x", NUM 13,
	LET ("y", NUM 131,
		MINUS (
			LET ("x", NUM 31, VAR "y"),
			LET ("y", NUM 313, VAR "x")
		)
	)
))) = 118))


(*14*)
let _ = output (fun() ->((test (
MULT (
	LET ("x", NUM 14,
		LET ("y", NUM 41, VAR "x")
	),
	LET ("x", NUM 141,
		LET ("z", NUM 414, VAR "x")
	)
))) = 1974))

(*15*)
let _ = output (fun() ->((test (
LET ("x1",
	MAX [MULT (PLUS (NUM 3, NUM (-6)), NUM 7);
		NUM 3;
		NUM 20;
		NUM 80;
		LET ("y", NUM 3,
			DIVIDE (VAR "y", NUM 3)
		)
	],
	LET ("y2", NUM 6,
		DIVIDE (VAR "x1", PLUS (VAR "y2", NUM 2))
	)
))) = 10))

(*16*)
let _ = output (fun() ->((test (
LET ("x", PLUS (NUM 4, NUM 5),
	LET ("y",
		MAX [NUM 5; NUM 4; NUM 3],
		MULT (VAR "x", VAR "y")
	)
))) = 45))

(*17*)
let _ = output (fun() ->((test (
LET ("x", NUM 1,
	PLUS (MULT (VAR "x", VAR "x"),
	LET ("x", NUM 4,
		MULT (VAR "x", VAR "x"))))
)) = 17))

(*18*)
let _ = output (fun() ->((test (
LET ("x", NUM 3,
	PLUS (
		VAR "x",
		MULT (LET ("x", NUM 4,
			PLUS (
				VAR "x",
				LET ("y", NUM 3,
					MULT (VAR "y", VAR "x")
				)
			)
		),
		VAR "x")
	)
))) = 51))

(*19-Exception*)
let _ = output (fun() ->((test (
LET ("x", PLUS (VAR "x", NUM 1), PLUS (VAR "x", NUM 1))
)) = err_int))

(*20-Exception*)
let _ = output (fun() ->((test (
LET ("x", NUM 20,
	MAX [
		LET ("y", NUM 202, VAR "x");
		LET ("z", NUM 2020, VAR "y")
	]
))) = err_int))
