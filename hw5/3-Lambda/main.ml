(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 * Lambda Calculus
 *
 * Kihong Heo
 * khheo@ropas.snu.ac.kr
 *)

open Evaluate
open Pp 
let main () =
    let pp = ref false in
    let src = ref "" in
    let _ =
        Arg.parse
          [("-pp", Arg.Set pp, "display parse tree")]
          (fun x -> src := x)
          ("Usage: " ^ (Filename.basename Sys.argv.(0)) ^ " [-ptree] [file]")
    in
    let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
    let pgm = Parser.program Lexer.start lexbuf in

	if !pp then(
		Lambda.pp pgm 0;
		print_newline();
	)
	else(
		print_string "=============\n";
		print_string "input program\n";
		print_string "=============\n";
		Pp.pp pgm;
		print_string "\n\n\n============\n";
		print_string "output program\n";
		print_string "=============\n";
		Pp.pp (Evaluator.reduce pgm))
let _ = main ()
