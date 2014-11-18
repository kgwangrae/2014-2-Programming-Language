(*
 * SNU 4190.310 Programming Languages 
 *
 * M0: definition of syntax, types, runner, checker and pretty printer
 *)

type mexp = Num of int
          | Var of id
          | Fn of id * mexp
          | App of mexp * mexp
          | Rec of id * id * mexp
          | Ifz of mexp * mexp * mexp
          | Add of mexp * mexp
          | Sub of mexp * mexp
          | And of mexp * mexp
          | Pair of mexp * mexp      (* (e, e) *)
          | Fst of mexp            (*   e.1  *)
          | Snd of mexp            (*   e.2  *)
and id = string

exception RuntimeError of string
exception TypeError of string

module M_Printer = struct
  let ps = print_string
    let nl = print_newline
    let indent i =
      let rec it = function 0 -> ()
               | n -> ps " "; it (n-1)
      in  nl (); it i

    let rec pp n =
      function 
        | Num i -> print_int i
         | Var s -> ps s
         | Fn (x, e) -> ps ("fn "^x^" => "); (
           match e with
                   Fn _ -> pp (n+1) e
         | Rec _ -> pp (n+1) e
                 | _ -> indent (n+1); pp (n+1) e
                 )
                 | App (e, e') -> pp n e; ps " "; pp n e'
         | Ifz (e1, e2, e3)-> ps "ifzero "; pp n e1; ps " then ";
                          indent (n+1); pp (n+1) e2;
                          indent (n); ps "else";
                          indent (n+1); pp (n+1) e3
         | Pair (e1, e2) -> ps "("; pp n e1; ps ", "; pp n e2; ps ")"
         | Fst e -> pp n e; ps ".1"
         | Snd e -> pp n e; ps ".2"
         | Add (e1, e2) -> ps "("; pp n e1; ps " + "; pp n e2; ps ")"
         | Sub (e1, e2) -> ps "("; pp n e1; ps " - "; pp n e2; ps ")"
         | And (e1, e2) -> ps "("; pp n e1; ps " and "; pp n e2; ps ")"
     | Rec (f, x, e) -> ps ("rec "^f^" "^x^" => "); (
       match e with
                   Fn _ -> pp (n+1) e
     | Rec _ -> pp (n+1) e
                 | _ -> indent (n+1); pp (n+1) e
                 )

    let print = pp 0
end
