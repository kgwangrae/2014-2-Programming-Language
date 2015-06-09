cd ..
make clean
make
cd kcm-poly

for i in $(ls); do
	#echo ''
	echo --------------FILE : $i --------------------
	cat $i
	echo ''
	../test $i
done
cd ..
make clean
