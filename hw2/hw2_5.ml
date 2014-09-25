module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module type ArgTy = 
sig
  type t
  val is_eq : t -> t -> bool
end

module QueueMake (Arg: ArgTy) : Queue with type element = Arg.t =
struct
    type element = Arg.t
    type queue = (element list) * (element list)
    exception EMPTY_Q
    let emptyq: queue = ([],[])
    let reverse : queue -> queue = 
        fun (inList,outList)-> ([],(List.rev_append inList outList))
    let enq: queue * element -> queue = 
        let isEqual = Arg.is_eq in    
        fun ((inList,outList), elem) -> 
            if (List.exists (isEqual elem) inList) 
               || (List.exists (isEqual elem) outList)
            then (inList,outList) 
            else (elem::inList,outList)
    let rec deq: queue -> element * queue = 
        fun (inList,outList) ->
          match outList with
          | hd::tl -> (hd,(inList,tl))
          | _ -> 
             match inList with
             | [] -> raise EMPTY_Q
             | _ -> deq (reverse (inList,outList))
end


