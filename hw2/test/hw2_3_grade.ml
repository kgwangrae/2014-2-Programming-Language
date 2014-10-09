open CommonGrade
open Hw2_3

let testLoc = LOC( LEAF "*", 
                   HAND( [LEAF "c"], 
                                            HAND( [LEAF "+"; NODE[LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [] ), 
                                                                     [LEAF "d"] 
                                            )) 

let loc1 = goLeft(testLoc)
let _ = output (fun() ->(loc1 = (LOC 
           (LEAF "c", 
            HAND 
             ([], 
              HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), 
                            [LEAF "*"; LEAF "d"]))))) 
let loc2 = goRight(loc1)
let _ = output (fun() -> (loc2 = testLoc))
let _ = output (fun() ->(loc2 = (LOC (LEAF "*", 
            HAND 
                         ([LEAF "c"], 
                                       HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), 
                                                     [LEAF "d"])))
))


let loc3 = goUp(loc2)
let _ = output (fun() -> (loc3 = (LOC 
           (NODE [LEAF "c"; LEAF "*"; LEAF "d"], 
                       HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [])))
))


let loc4 = goDown(loc3)
let _ = output (fun() -> (loc4=loc1))
    
let loc5 = goRight(loc4) 
let _ = output (fun() -> (loc5=loc2))
let loc6 = goUp(loc3) 
let _ = output (fun() -> (loc6 =  
    (LOC 
               (NODE 
                            [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; 
                                          NODE [LEAF "c"; LEAF "*"; LEAF "d"]], 
                                                      TOP)) 
))

let _ = output (fun() -> (loc4 = goDown(goRight(goRight(goDown(loc6))))))
