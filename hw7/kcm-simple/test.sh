cd ..
make clean
make
cd kcm-simple

for i in $(ls); do
	echo ''
	echo --------------FILE : $i --------------------
	# cat $i
	#echo ''
	../test $i
done
cd ..
make clean
