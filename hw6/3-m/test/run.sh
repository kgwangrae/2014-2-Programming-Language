#!/bin/zsh
echo 'Some tests require your input'
cd ..
make clean
make
cd test
for i in $(ls); do
	echo --------------FILE : $i --------------------
	../run $i
done
