all: run

run: m.cmo error.cmo lexer.cmo parser.cmo m_vanilla.cmo main.cmo
	ocamlc -o run m.cmo error.cmo lexer.cmo parser.cmo m_vanilla.cmo main.cmo

error.cmo : error.ml
	ocamlc -c error.ml

m.cmo : m.ml
	ocamlc -c m.ml

m_vanilla.cmo : m_vanilla.ml m.cmo
	ocamlc -c m_vanilla.ml


parser.ml: parser.mly m.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : m_vanilla.cmo main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml error.cmo
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
