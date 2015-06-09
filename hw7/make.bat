ocamlc -c m.ml
ocamlc -c error.ml
ocamlc -c m_lowfat.ml
ocamlc -c hw7_1.ml
ocamlc -c hw7_2.ml
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c main.ml
ocamlc -o run.exe m.cmo error.cmo lexer.cmo parser.cmo m_lowfat.cmo hw7_1.cmo hw7_2.cmo main.cmo
