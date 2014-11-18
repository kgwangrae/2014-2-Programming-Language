#!/bin/zsh
echo 'Some tests require your input'
cd ..
make clean
make
cd test
for i in $(ls); do
	echo --------------FILE : $i --------------------
	echo 'K--'
	../run -k $i
	echo 'Sm5'
	../run -sm5 $i
	echo 'Sonata'
	../run $i
done
