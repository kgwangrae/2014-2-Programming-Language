open CommonGrade
open Hw2_1

let _ = output (fun() ->
checkMetro (STATION "c")
= false
)


let _ = output (fun() ->
checkMetro (AREA ("e", STATION "e"))
= true
)

let _ = output (fun() ->
checkMetro (AREA ("e", STATION "c"))
= false
)

let _ = output (fun() ->
checkMetro (AREA ("a", CONNECT (AREA ("b", STATION "a"), AREA("c", STATION "c"))))
= true
)


let _ = output (fun() ->
checkMetro (AREA ("a", CONNECT (AREA ("b", STATION "a"), AREA("c", STATION "b"))))
= false
)

let _ = output (fun() ->
checkMetro (AREA ("a", CONNECT (AREA ("b", STATION "a"), AREA("c", STATION "a"))))
= true
)

let _ = output (fun() ->
checkMetro (CONNECT (AREA ("a", STATION "a"), AREA ("b", STATION "b")))
= true
)

let _ = output (fun() ->
checkMetro (AREA ("a", AREA ("b", CONNECT (STATION "b", STATION "a"))))
= true
)

let _ = output (fun() ->
checkMetro (CONNECT (AREA ("b", 
                           CONNECT (STATION "b", 
		                    AREA ("d", STATION "b"))), 
                     AREA ("e", STATION "e")))
= true
)

let _ = output (fun() ->
checkMetro (CONNECT (AREA ("b", 
                           CONNECT (STATION "b", 
		                    AREA ("d", STATION "d"))), 
                     AREA ("e", STATION "b")))
= false
)
