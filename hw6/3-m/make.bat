ocamlc -c m.ml
ocamlc -c error.ml
ocamlc -c m_vanilla.ml
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c main.ml
ocamlc -o run m.cmo error.cmo lexer.cmo parser.cmo m_vanilla.cmo main.cmo
