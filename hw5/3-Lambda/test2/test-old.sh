#!/bin/zsh
echo 'Some tests require your input'
for i in $(ls); do
	echo FILE : $i
	../run-old $i
	echo ''
done
