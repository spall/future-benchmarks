#!/bin/bash

# build (make; make base) | version (6.10 etc) | CPU count | rtime | utime | stime

# args
nthunks=$1

tstamp=$(date +%T)
#outfile="$(pwd)/results/$1_$tstamp.out"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
#mkdir -p $(pwd)/results

#touch $outfile
# write first line to file
#echo "type | n | #-of-futures | cpu | real | gc | $threads" >> $outfile

echo "Running scheme thread-thunk benchmarks"

echo "Running with 1 thread"
./thread-test.ss --thread-count 1 --number-thunks $nthunks

echo "Running with 2 threads"
./thread-test.ss --thread-count 2 --number-thunks $nthunks

echo "Running with 4 threads"
./thread-test.ss --thread-count 4 --number-thunks $nthunks

echo "Running wiht 8 threads"
./thread-test.ss --thread-count 8 --number-thunks $nthunks

echo "Running with 16 threads"
./thread-test.ss --thread-count 16 --number-thunks $nthunks

echo "done"

    

