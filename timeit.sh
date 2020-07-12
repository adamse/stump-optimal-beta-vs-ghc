#!/bin/bash

# usage:
# ./timeit.sh Example1
# ./timeit.sh Example2
# ./timeit.sh Example3

reps=${2:-100000}

ghc -O2 -fforce-recomp --make $1.hs -o $1

echo
echo time to compute just m ^ n
time ./$1 1000001 100000 1

echo
echo "time to compute 1 ^ 1 many (${reps}) times"
time ./$1 1 1 $reps

echo
echo "time to compute m ^ n many (${reps}) times"
time ./$1 1000001 100000 $reps
