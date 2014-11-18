(*
 * SNU 4190.310 Programming Languages 
 *
 * SM5
 *)

open Rozetta
open Pp
open K
open Sm5
open Sonata

let main () =
    let pp = ref false in
	let psm5 = ref false in
	let psonata = ref false in
	let sm5 = ref false in
	let k = ref false in
    let src = ref "" in
    let _ =
        Arg.parse
          [("-pp", Arg.Set pp, "display parse tree");
		   ("-psm5", Arg.Set psm5, "print translated sm5 code");
		   ("-psonata", Arg.Set psonata, "print translated sonata code");
           ("-k", Arg.Set k, "run using k interpreter");
		   ("-sm5", Arg.Set sm5, "translate k-- to sm5 and run using sm5 interpreter")]
          (fun x -> src := x)
          ("Usage: " ^ (Filename.basename Sys.argv.(0)) ^ " [-pp | -psm5 | -psonata | -k | -sm5] [file]")
    in
    let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
    let pgm = Parser.program Lexer.start lexbuf in
   
	if !pp then (KParseTreePrinter.print pgm)
	else if !psm5 then ignore (Sm5.print (Trans_k.trans pgm))
	else if !psonata then ignore (Sonata.print (Rozetta.trans (Trans_k.trans pgm)))
	else if !sm5 then ignore (Sm5.run (Trans_k.trans pgm))
	else if !k then ignore (K.run (K.emptyMemory, K.emptyEnv, pgm))
    else ignore (Sonata.run(Rozetta.trans (Trans_k.trans pgm)))

let _ = main ()
