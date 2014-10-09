all: run

run: lexer.cmo parser.cmo hw3.cmo pp.cmo main.cmo
	ocamlc -o run lexer.cmo pp.cmo parser.cmo hw3.cmo main.cmo

hw3.cmo : hw3.ml
	ocamlc -c hw3.ml

pp.cmo : pp.ml hw3.cmo
	ocamlc -c pp.ml

parser.ml: parser.mly hw3.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : hw3.cmo main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
