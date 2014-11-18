all: run

run: lexer.cmo parser.cmo sm5.cmo sonata.cmo k.cmo trans_k.cmo rozetta.cmo pp.cmo main.cmo
	ocamlc -o run lexer.cmo pp.cmo parser.cmo sm5.cmo sonata.cmo trans_k.cmo rozetta.cmo k.cmo main.cmo

k.cmo : k.ml
	ocamlc -c k.ml

sm5.cmo : sm5.ml
	ocamlc -c sm5.ml

sonata.cmo : sonata.ml
	ocamlc -c sonata.ml

trans_k.cmo : trans_k.ml k.cmo sm5.cmo
	ocamlc -c trans_k.ml

rozetta.cmo : rozetta.ml sm5.cmo sonata.cmo
	ocamlc -c rozetta.ml

pp.cmo : pp.ml trans_k.cmo
	ocamlc -c pp.ml

parser.ml: parser.mly trans_k.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : trans_k.cmo rozetta.ml main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
