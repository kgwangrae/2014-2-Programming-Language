(*
 * SNU 4190.310 Programming Languages (Fall 2013)
 *
 * K- Interpreter I
 *)
open Pp
open Hw3
let main () =
    let print_code = ref false in
    let src = ref "" in
    let spec = [("-pp", Arg.Set print_code, "�Է� K- ���α׷� ����")] in
    let usage = "����: run <options> <file> \n��� ������ �ɼǵ�: " in
    let _ = Arg.parse spec
                (fun
                   x ->
                     if Sys.file_exists x then src := x
                     else raise (Arg.Bad (x ^ ": ������ ����")))
                usage
    in
    
	if !src = "" then Arg.usage spec usage
    else
    	let file_channel = open_in !src in
    	let lexbuf = Lexing.from_channel file_channel in
    	let pgm = Parser.program Lexer.start lexbuf in
		try
       		if !print_code then (
              print_endline "== �Է� ���α׷� ==";
              Kminus_PP.pp pgm
          	) else (
				try
          		   (ignore (K.run (K.emptyMemory, K.emptyEnv, pgm)))
				with
                	K.Error s -> print_endline ("����: " ^ s)
          )
		with Lexer.LexicalError -> print_endline (!src ^ ": ���� ����")

let _ = main ()
