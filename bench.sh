#!/bin/bash

# build (make; make base) | version (6.10 etc) | CPU count | rtime | utime | stime

# args
name=$1
MAX=$2       # need to do this better
bench=$3
racket7=$4  # directory to racket7 makefile
racketbin=$5   # directory to racket binary

tstamp=$(date +%T)
outfile="$(pwd)/results/$1_$tstamp.out"  # for some reason using version here doesnt work.

# create results directory if it doesnt exist
mkdir -p $(pwd)/results

touch $outfile
# write first line to file
echo "type | n | #-of-futures | cpu | real | gc" >> $outfile

cd $racket7

echo "Running racket7"

make run ARGS="$bench $MAX $outfile racket7"

echo "Running racket"
$racketbin/racket $bench $MAX $outfile racket

echo "done"

    

