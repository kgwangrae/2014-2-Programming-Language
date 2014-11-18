ocamlc -c m.ml
ocamlc -c lambda.ml
ocamlc -c encode.ml
ocamlc -c pp.ml
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c main.ml
ocamlc -o run.exe pp.cmo parser.cmo lexer.cmo m.cmo lambda.cmo encode.cmo main.cmo
