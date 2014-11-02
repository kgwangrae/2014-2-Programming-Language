(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * SM5
 *)
open K
open Sm5
module Translator = struct

let rec trans : K.program -> Sm5.command
= fun pgm -> 
  match pgm with
    | K.NUM n -> Sm5.empty_command (* Implement this. *)
    | _ -> Sm5.empty_command

end
