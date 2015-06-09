#!/bin/bash

make

printf "1: Should be { int }\n"
SRC="./testcase/lastyear/sort.m"
./test $SRC
printf "\n"


printf "2: Should be { int }\n"
SRC="./testcase/lastyear/t2.m"
./test $SRC
printf "\n"

printf "3: Should be { int }\n"
SRC="./testcase/lastyear/t3.m"
./test $SRC
printf "\n"

printf "4: Should be { bool }\n"
SRC="./testcase/lastyear/t4.m"
./test $SRC
printf "\n"

printf "5: Should be { exception }\n"
SRC="./testcase/lastyear/t5.m"
./test $SRC
printf "\n"

printf "6: Should be { bool }\n"
SRC="./testcase/lastyear/t6.m"
./test $SRC
printf "\n"

printf "7: Should be { int }\n"
SRC="./testcase/lastyear/t7.m"
./test $SRC
printf "\n"

printf "8: Should be { exception }\n"
SRC="./testcase/lastyear/t8.m"
./test $SRC
printf "\n"

printf "9: Should be { exception }\n"
SRC="./testcase/lastyear/t9.m"
./test $SRC
printf "\n"


printf "10: Should be { Int }\n"
SRC="./testcase/lastyear/t10.m"
./test $SRC
printf "\n"

printf "11: Should be { bool }\n"
SRC="./testcase/lastyear/t11.m"
./test $SRC
printf "\n"

printf "12: Should be { string }\n"
SRC="./testcase/lastyear/t12.m"
./test $SRC
printf "\n"

printf "13: Should be { bool }\n"
SRC="./testcase/lastyear/t13.m"
./test $SRC
printf "\n"

printf "14: Should be { pair(int,bool) }\n"
SRC="./testcase/lastyear/t14.m"
./test $SRC
printf "\n"

printf "15: Should be { int }\n"
SRC="./testcase/lastyear/t15.m"
./test $SRC
printf "\n"

printf "16: Should be { int }\n"
SRC="./testcase/lastyear/t16.m"
./test $SRC
printf "\n"

printf "17: Should be { int }\n"
SRC="./testcase/lastyear/t17.m"
./test $SRC
printf "\n"

printf "18: Should be { loc(int) }\n"
SRC="./testcase/lastyear/t18.m"
./test $SRC
printf "\n"

printf "19: Should be { exception }\n"
SRC="./testcase/lastyear/t19.m"
./test $SRC
printf "\n"

printf "20: Should be { int }\n"
SRC="./testcase/lastyear/t20.m"
./test $SRC
printf "\n"

printf "21: Should be { exception }\n"
SRC="./testcase/lastyear/t21.m"
./test $SRC
printf "\n"

printf "22: Should be { int }\n"
SRC="./testcase/lastyear/t22.m"
./test $SRC
printf "\n"

printf "23: Should be { exception }\n"
SRC="./testcase/lastyear/t23.m"
./test $SRC
printf "\n"

printf "24: Should be { int }\n"
SRC="./testcase/lastyear/t24.m"
./test $SRC
printf "\n"

printf "25: Should be { int }\n"
SRC="./testcase/lastyear/t25.m"
./test $SRC
printf "\n"

printf "26: Should be { int }\n"
SRC="./testcase/lastyear/t26.m"
./test $SRC
printf "\n"

printf "26-2: Should be { bool }\n"
SRC="./testcase/lastyear/t26-2.m"
./test $SRC
printf "\n"

printf "27: Should be { int }\n"
SRC="./testcase/lastyear/t27.m"
./test $SRC
printf "\n"

printf "28: Should be { int }\n"
SRC="./testcase/lastyear/t28.m"
./test $SRC
printf "\n"

printf "29: Should be { exception }\n"
SRC="./testcase/lastyear/t29.m"
./test $SRC
printf "\n"

printf "30: Should be { int }\n"
SRC="./testcase/lastyear/t30.m"
./test $SRC
printf "\n"

printf "31: Should be { exception }\n"
SRC="./testcase/lastyear/t31.m"
./test $SRC
printf "\n"

printf "32: Should be { exception }\n"
SRC="./testcase/lastyear/t32.m"
./test $SRC
printf "\n"

printf "33: Should be { pair(string,bool) }\n"
SRC="./testcase/lastyear/t33.m"
./test $SRC
printf "\n"

printf "34: Should be { exception }\n"
SRC="./testcase/lastyear/t34.m"
./test $SRC
printf "\n"


printf "35: Should be { exception }\n"
SRC="./testcase/lastyear/t35.m"
./test $SRC
printf "\n"


printf "36: Should be { loc(pair(bool,bool)) }\n"
SRC="./testcase/lastyear/t36.m"
./test $SRC
printf "\n"

printf "37: Should be { int }\n"
SRC="./testcase/lastyear/t37.m"
./test $SRC
printf "\n"

printf "38: Should be { exception }\n"
SRC="./testcase/lastyear/t38.m"
./test $SRC
printf "\n"

make clean
