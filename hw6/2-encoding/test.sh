make clean
make
ocamlc encode.ml parser.ml lexer.ml lambda.ml test.ml
./a.out
rm a.out
