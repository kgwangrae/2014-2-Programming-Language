#!/bin/zsh
echo 'Some tests require your input'
cd ..
make
cd test
for i in $(ls); do
	echo FILE : $i
	echo 'k--'
	../run -k $i
	echo 'sm5'
	../run $i
done
