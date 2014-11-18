(*
 * SNU 4190.310 Programming Languages 
 * k.gwangrae@gmail.com, Gwangrae Kim 
 * Sonata
 *)

open Sm5
open Sonata

module Rozetta = struct
  exception Bad_argument of string

  let interpret_val : Sm5.value -> Sonata.value = fun v ->
    match v with
    | Sm5.Z i -> Sonata.Z i
    | Sm5.B b -> Sonata.B b
    | Sm5.Unit -> Sonata.Unit
    | Sm5.L _ | Sm5.R _ -> raise (Bad_argument "Tried to interpretlate hidden value types")

  let rec interpret : Sm5.command -> Sonata.command = fun cmd ->
    match cmd with
    | [] -> []
    | h::tl -> 
     (match h with
      (*Directly pushing loc and Record by command is forbidden - to abstract memory component
       *NOTE types inside a module can be accessed only when it is defined in its signature*)
      | Sm5.PUSH obj -> 
         [Sonata.PUSH
          (match obj with
            | Sm5.Val v -> Sonata.Val (interpret_val v)
            | Sm5.Id id -> Sonata.Id id
            | Sm5.Fn (id_in, cmd_in) -> 
                if (id_in="#return")
                (*Clean up and restore, NOTE handle recursion with care *)
                then Sonata.Fn (id_in, 
                       ((Sonata.PUSH (Sonata.Id "#return"))::(Sonata.LOAD)
                       ::(Sonata.UNBIND)::(Sonata.UNBIND)::(Sonata.UNBIND)::(Sonata.UNBIND)
                       ::(Sonata.POP)::(Sonata.POP)::(Sonata.POP)::(Sonata.POP)
                       ::(interpret cmd_in))
                       @((Sonata.MALLOC)::(Sonata.BIND "#return")
                       ::(Sonata.PUSH (Sonata.Id "#return"))::(Sonata.STORE)
                       ::(Sonata.PUSH (Sonata.Id "#fn-return"))
                       ::(Sonata.PUSH (Sonata.Id "#return"))::(Sonata.LOAD)
                       ::[(Sonata.MALLOC);(Sonata.CALL)])
                       )
                (*For general functions, the restoration function will be called after them*)
                else Sonata.Fn (id_in, [Sonata.BIND "#fn-return"]@(interpret cmd_in)@
                  [(Sonata.MALLOC);(Sonata.BIND "#result");(Sonata.PUSH (Sonata.Id "#result"))
                  ;(Sonata.STORE);(Sonata.PUSH (Sonata.Id "#fn-return"))
                  ;(Sonata.PUSH (Sonata.Id "#result"));(Sonata.LOAD);(Sonata.MALLOC);(Sonata.CALL)])
          )]@ (interpret tl)
      | Sm5.POP -> [Sonata.POP]@ (interpret tl)
      | Sm5.STORE -> [Sonata.STORE]@ (interpret tl)
      | Sm5.LOAD -> [Sonata.LOAD]@ (interpret tl)
      | Sm5.JTR (l, r)-> [Sonata.JTR ((interpret l), interpret r)]@ (interpret tl)
      | Sm5.MALLOC -> [Sonata.MALLOC]@ (interpret tl)
      | Sm5.BOX n -> [Sonata.BOX n]@ (interpret tl)
      | Sm5.UNBOX id -> [Sonata.UNBOX id]@ (interpret tl)
      | Sm5.BIND id -> [Sonata.BIND id]@ (interpret tl)
      | Sm5.UNBIND -> [Sonata.UNBIND]@ (interpret tl)
      | Sm5.GET -> [Sonata.GET]@ (interpret tl)
      | Sm5.PUT -> [Sonata.PUT]@ (interpret tl)
      | Sm5.CALL -> 
          (*Step 1 : Back up l,v,(x,c',e')*)
        [(Sonata.BIND "#loc")
        ;(Sonata.MALLOC);(Sonata.BIND "#val");(Sonata.PUSH (Sonata.Id "#val"));(Sonata.STORE)
        ;(Sonata.BIND "#fun")]
          (*Step 2 : Backup current environment and command list in a 'restoration' procedure*)
        @(interpret [Sm5.PUSH (Sm5.Fn ("#return", tl))])
          (*Step 3 : Restore l,v,(x,c',e')*)
        @[(Sonata.PUSH (Sonata.Id "#fun"));(Sonata.PUSH (Sonata.Id "#val"));(Sonata.LOAD);(Sonata.PUSH (Sonata.Id "#loc"))
          (*Step 4 : Call (x,c',e'), then result will appear on stack top*)
        ;(Sonata.CALL)]
      | Sm5.ADD -> [Sonata.ADD]@ (interpret tl)
      | Sm5.SUB -> [Sonata.SUB]@ (interpret tl)
      | Sm5.MUL -> [Sonata.MUL]@ (interpret tl)
      | Sm5.DIV -> [Sonata.DIV]@ (interpret tl)
      | Sm5.EQ -> [Sonata.EQ]@ (interpret tl)
      | Sm5.LESS -> [Sonata.LESS]@ (interpret tl)
      | Sm5.NOT -> [Sonata.NOT]@ (interpret tl)
     )  
  let rec trans : Sm5.command -> Sonata.command = fun cmd ->
    match cmd with
    | [] -> []
    | h::tl -> 
     [(Sonata.PUSH (Sonata.Fn ("#return",[])));(Sonata.BIND "#fn-return")] (*Base case for recursion*)
     @ interpret cmd 
end
