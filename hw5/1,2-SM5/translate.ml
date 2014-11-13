(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 * k.gwangrae@gmail.com, Gwangrae Kim
 * K-- to SM5 translator : Assume k-- and SM5 just work as expected!
 * Then tell SM5 interpreter do what K-- interpreter does.
 * NOTE : Stack should only include the final result value at last.
 *)
open K
open Sm5
module Translator = struct

let rec trans : K.program -> Sm5.command
= fun pgm -> 
  match pgm with
  | K.UNIT -> [Sm5.PUSH(Sm5.Val(Sm5.Unit))]
  | K.NUM i -> [Sm5.PUSH(Sm5.Val(Sm5.Z i))]  
  | K.TRUE -> [Sm5.PUSH(Sm5.Val(Sm5.B true))] 
  | K.FALSE -> [Sm5.PUSH(Sm5.Val(Sm5.B false))]
  | K.VAR id -> [Sm5.PUSH(Sm5.Id id);Sm5.LOAD]
  | K.ADD(e1,e2) -> (trans e1)@(trans e2)@[Sm5.ADD]
  | K.SUB(e1,e2) -> (trans e1)@(trans e2)@[Sm5.SUB]
  | K.MUL(e1,e2) -> (trans e1)@(trans e2)@[Sm5.MUL]
  | K.DIV(e1,e2) -> (trans e1)@(trans e2)@[Sm5.DIV]
  | K.EQUAL(e1,e2) -> (trans e1)@(trans e2)@[Sm5.EQ]
  | K.LESS(e1,e2) -> (trans e1)@(trans e2)@[Sm5.LESS]
  | K.NOT e -> (trans e)@[Sm5.NOT]
  | K.ASSIGN(id,e) -> 
      (* Global id assignment - should be stored in memory, 
       * assuming id is already declared in the environment*)
      (trans e)@[(Sm5.PUSH (Sm5.Id id));(Sm5.STORE);(Sm5.PUSH (Sm5.Id id));(Sm5.LOAD)]
  | K.SEQ(e1,e2) -> (trans e1)@[Sm5.POP]@(trans e2)
  | K.IF(cond,e_t,e_f) -> (trans cond)@[Sm5.JTR ((trans e_t), (trans e_f))]
  | K.READ id -> 
      [Sm5.GET;(Sm5.PUSH (Sm5.Id id));(Sm5.STORE);(Sm5.PUSH (Sm5.Id id));(Sm5.LOAD)]
      (* Assume SM5 will raise an exception when non-integer value is given 
       * trans e will be popped after PUT - 
       * but it should remain in stack with the same memory and environment state*) 
  | K.WRITE e -> 
      [(Sm5.MALLOC);(Sm5.BIND "tmp")]
      @(trans (K.ASSIGN ("tmp", e)))
      @[(Sm5.PUSH (Sm5.Id "tmp"));Sm5.LOAD;Sm5.PUT;Sm5.UNBIND;Sm5.POP]
  | K.CALLV(f,e_val) -> 
      [Sm5.PUSH (Sm5.Id f)]@ (* Adding f again for binding itself after call - for recursion*)
      [Sm5.PUSH (Sm5.Id f)]@(trans e_val)@[Sm5.MALLOC;Sm5.CALL]
  | K.CALLR(f,ref_id) -> 
      [(Sm5.PUSH (Sm5.Id f));(Sm5.PUSH (Sm5.Id f));(Sm5.PUSH (Sm5.Id ref_id));
        Sm5.LOAD;(Sm5.PUSH (Sm5.Id ref_id));Sm5.CALL]
  | K.LETV(id,e1,e2) -> 
      [(Sm5.MALLOC);(Sm5.BIND id)]@(trans (K.ASSIGN(id,e1)))
      @[Sm5.POP]@(trans e2)@[Sm5.UNBIND;Sm5.POP]
  | K.LETF(f,v,e1,e2) ->
      let e1_t = [(Sm5.BIND f)]@(trans e1) in 
      (* env inside will be thrown away after execution. So there's no need to unbind it*)
      [(Sm5.PUSH(Sm5.Fn(v,e1_t)));(Sm5.BIND f)]@(trans e2)@[Sm5.UNBIND;Sm5.POP]
  | K.WHILE(cond,body) -> (* Recursive version of loop.*)
      let k_while = K.LETF("while","cond",
        K.IF (cond,K.SEQ (body,K.CALLR("while","cond")),K.UNIT)
        , K.CALLV("while", K.UNIT)) in
      trans k_while
  | K.FOR(x,bot,top,body) -> 
      let k_cond = K.NOT (K.LESS (top, K.VAR x)) in
      let k_inc_x = K.SEQ(body,
                        (K.SEQ((K.ASSIGN (x, K.ADD(K.VAR "backup!x", K.NUM 1))),
                               (K.ASSIGN ("backup!x", (K.VAR x)))))) in
      let k_dec_x = K.ASSIGN (x, K.SUB(K.VAR "backup!x", K.NUM 1)) in
      let k_init_x = K.ASSIGN (x, (K.VAR "init!x")) in
      let k_mark_inc = K.SEQ(k_inc_x,(K.ASSIGN ("temporal!increment",K.TRUE))) in
      let k_for_body = K.IF (k_cond,
                             K.SEQ (k_mark_inc,K.CALLR("for",x)), 
                             (K.IF ((K.VAR "temporal!increment"),
                                    (K.SEQ(k_dec_x,K.UNIT)),
                                    (K.SEQ(k_init_x,K.UNIT))
                                   )
                             )
                            ) 
      in
      (* The value of x may be changed inside body, so CALLR is appropriate. 
       * BUT according to the weird semantics definition, those changes should be ignored *)
      let k_for = K.LETF("for","idx",k_for_body,K.CALLR("for",x)) in
      trans (K.LETV ("init!x", (K.VAR x),
                    K.SEQ((K.ASSIGN (x, bot)),             
                    (K.LETV("backup!x",(K.VAR x),
                    (K.LETV("temporal!increment",K.FALSE,k_for)))))))
end
