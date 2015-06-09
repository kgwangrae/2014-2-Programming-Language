(*
 * SNU 4190.310 Programming Languages (Fall 2010)
 *
 * Main Interface for M
 *)

open M
open M_lowfat
open Hw7_1
open Hw7_2

let print_m_code = ref false
let poly_ty_chk = ref false

let spec = [
    ("-pp", Arg.Set print_m_code, "Print M program");
    ("-poly", Arg.Set poly_ty_chk, "Use a polymorphic type system before running (with low fat M)")
]
let usage = "Usage: run [<options>] <M file> \n<options> are: ";;

(*
print_m_code := true;;
*)
poly_ty_chk := true;;

let run () =
    let src = ref "" in
    let _ = Arg.parse spec
           (fun x -> if Sys.file_exists x then src := x
                    else raise (Arg.Bad (x^": No such file"))) usage in  
	try
        Error.init ();
        let lexbuf =
            Lexing.from_channel (if !src = "" then stdin else open_in !src) in
        let pgm = Parser.program Lexer.start lexbuf in  
			if !print_m_code then (
                print_string "== Input Program ==\n"; 
                M_Printer.print pgm;
                print_newline()
            );
            if !poly_ty_chk then (
              print_string "== Poly ==\n";
            	M_Printer.printTypes (M_PolyChecker.check pgm)
              (*
                print_string "== Running with Low Fat M ==\n";
                M_LowFat.run pgm
                *)
            )
            else (
                M_Printer.printTypes (M_SimChecker.check pgm);
            )
   with v -> Error.handle_exn v

let _ = Printexc.catch run () 
