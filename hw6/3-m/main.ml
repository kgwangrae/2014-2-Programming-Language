(*
 * SNU 4190.310 Programming Languages 
 *
 * Main Interface for M
 *)

open M
open M_vanilla

let print_m_code = ref false
let spec = [
    ("-pp", Arg.Set print_m_code, "Print M program");
]
let usage = "Usage: run [<options>] <M file> \n<options> are: "

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
            (
                print_string "== Running with Vanilla M ==\n";
                M_Vanilla.run pgm
            )
   with v -> Error.handle_exn v

let _ = Printexc.catch run () 
