open CommonGrade
open Hw1_2


let _ = output (fun () ->
  (eval (IMPLY (ORELSE (FALSE, LESS (NUM 0, (PLUS (NUM (-1), NUM 0)))), FALSE))))
let _ = output (fun () -> eval TRUE)
let _ = output (fun () -> eval (NOT FALSE))
let _ = output (fun () -> (eval (ANDALSO (TRUE, TRUE))) && (not (eval (ANDALSO (TRUE, FALSE)))) && (not (eval (ANDALSO (FALSE, TRUE)))) && (not (eval (ANDALSO (FALSE, FALSE)))))

let _ = output (fun () -> (eval (ORELSE (TRUE, TRUE))) && (eval (ORELSE (TRUE, FALSE))) && (eval (ORELSE (FALSE, TRUE))) && (not (eval (ORELSE (FALSE, FALSE)))))

let _ = output (fun () -> (eval (IMPLY (TRUE, TRUE))) && (not (eval (IMPLY (TRUE, FALSE)))) && (eval (IMPLY (FALSE, TRUE))) && (eval (IMPLY (FALSE, FALSE))))
let _ = output (fun () -> (eval (LESS (NUM 3, NUM 5))) && (not (eval (LESS (NUM 5, NUM 3)))) && (not (eval (LESS (NUM 3, NUM 3)))))
let _ = output (fun () -> eval (NOT (LESS (PLUS (NUM 3, NUM 5), PLUS (NUM 4, NUM 4)))))
let _ = output (fun () -> eval (LESS (MINUS (NUM 1, NUM 3), MINUS (NUM 1, NUM 2))))
let _ = output (fun () -> eval (NOT (ORELSE (LESS (PLUS (MINUS (NUM 3, NUM 2), NUM 9), NUM 10), FALSE))))

let _ = output (fun () -> eval (IMPLY(LESS (NUM 1, NUM 0), ORELSE(ANDALSO(TRUE, FALSE), ORELSE(NOT TRUE, LESS(NUM 1, NUM 2))))))
let _ = 
    let print_bool x = 
        print_endline (string_of_bool x) in 
    print_bool (true = eval TRUE); 
    print_bool (false = eval FALSE); 
    print_bool (false = eval (NOT TRUE)); 
    print_bool (true = eval (NOT FALSE)); 
    print_bool (true = eval (ANDALSO (TRUE, TRUE))); 
    print_bool (false = eval (ANDALSO (TRUE, FALSE))); 
    print_bool (false = eval (ANDALSO (FALSE, TRUE))); 
    print_bool (false = eval (ANDALSO (FALSE, FALSE))); 
    print_bool (true = eval (ORELSE (TRUE, TRUE))); 
    print_bool (true = eval (ORELSE (TRUE, FALSE))); 
    print_bool (true = eval (ORELSE (FALSE, TRUE))); 
    print_bool (false = eval (ORELSE (FALSE, FALSE))); 
    print_bool (false = eval (IMPLY (TRUE, FALSE))); 
    print_bool (true = eval (IMPLY (TRUE, TRUE))); 
    print_bool (true = eval (IMPLY (FALSE, TRUE))); 
    print_bool (true = eval (IMPLY (FALSE, FALSE))); 
    print_bool (true = eval (LESS (NUM 3, NUM 5))); 
    print_bool (false = eval (LESS (NUM 3, NUM 3))); 
    print_bool (false = eval (LESS (NUM 3, NUM 1))); 
    print_bool (false = eval 
    (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
    print_bool (true = eval 
    (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))))); 

