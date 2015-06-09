#!/bin/bash



counter=0
while read line
do
  counter=$[counter+1]
  echo -e "The result should be { $line }"
  ./test ./testcase/lastyear2/t$counter.m
  printf "\n"
done < "7-2.txt"

echo -e "Finished\n"


