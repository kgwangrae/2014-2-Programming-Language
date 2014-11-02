all: run

run: lexer.cmo parser.cmo evaluate.cmo lambda.cmo pp.cmo main.cmo
	ocamlc -o run lexer.cmo parser.cmo evaluate.cmo lambda.cmo pp.cmo main.cmo

lambda.cmo : lambda.ml
	ocamlc -c lambda.ml

evaluate.cmo : evaluate.ml lambda.cmo 
	ocamlc -c evaluate.ml

pp.cmo : pp.ml evaluate.cmo
	ocamlc -c pp.ml

parser.ml: parser.mly lambda.cmo
	ocamlyacc -v parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : evaluate.cmo pp.cmo main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo parser.output
