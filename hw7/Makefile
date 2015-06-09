all: run

run: m.cmo error.cmo lexer.cmo parser.cmo m_lowfat.cmo hw7_1.cmo hw7_2.cmo main.cmo test.cmo
	ocamlc -o run m.cmo error.cmo lexer.cmo parser.cmo m_lowfat.cmo hw7_1.cmo hw7_2.cmo main.cmo
	ocamlc -o test m.cmo error.cmo lexer.cmo parser.cmo m_lowfat.cmo hw7_1.cmo hw7_2.cmo test.cmo


error.cmo : error.ml
	ocamlc -c error.ml

m.cmo : m.ml
	ocamlc -c m.ml

m_lowfat.cmo : m.ml m_lowfat.ml
	ocamlc -c m_lowfat.ml

hw7_1.cmo : hw7_1.ml m.cmo
	ocamlc -c hw7_1.ml

hw7_2.cmo : hw7_2.ml m.cmo
	ocamlc -c hw7_2.ml

parser.ml: parser.mly m.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : m.ml m_lowfat.ml hw7_1.cmo hw7_2.cmo main.ml
	ocamlc -c main.ml

test.cmo: m.ml hw7_1.cmo hw7_2.cmo test.ml
	ocamlc -c test.ml

lexer.cmo: lexer.ml error.cmo
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo test
