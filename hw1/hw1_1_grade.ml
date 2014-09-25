open CommonGrade
open Hw1_1

let _ = 
    let print_bool x = 
        print_endline (string_of_bool x) in 
    print_bool (385 = sigma (1, 10, (fun x -> x * x))); 
    print_bool (0 = sigma (3, 1, fun x -> x * x)); 
    print_bool (27 = sigma(3, 3, fun x -> x * x * x)); 
    print_bool (385 = sigma(-10, -1, fun x -> x * x))

let _ = output (fun() -> (sigma (3, 3, fun x -> x) = 3)) (*Tuple vs curry is
different!!!!*)
let _ = output (fun() -> (sigma (1, 10, fun x -> x) = 55))
let _ = output (fun() -> (sigma (1, 10, fun x -> if x mod 2 = 0 then 1 else 0)) = 5)
let _ = output (fun() -> (sigma (-3, 3, fun x -> x * x)) = 28)
let _ = output (fun() -> (sigma (3, 1, fun x -> x)) = 0)
