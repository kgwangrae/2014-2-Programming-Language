all: run

run: lexer.cmo parser.cmo m.cmo encode.cmo main.cmo
	ocamlc -o run lexer.cmo parser.cmo encode.cmo m.cmo pp.cmo main.cmo

lambda.cmo : lambda.ml
	ocamlc -c lambda.ml

m.cmo : m.ml lambda.cmo
	ocamlc -c m.ml

pp.cmo : pp.ml
	ocamlc -c pp.ml

encode.cmo : encode.ml m.cmo
	ocamlc -c encode.ml

parser.ml: parser.mly m.cmo 
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : encode.cmo main.ml pp.cmo
	ocamlc -c main.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
