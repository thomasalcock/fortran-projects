#!/bin/bash

SRC="src/*.f90"
EXE=build/fortran_utils_test
FLAGS="-std=f2018 -Wextra -Wall -Wconversion"
RUN_FLAGS="--filepath output/results.csv --number 30"

if [ -z $1 ]; then
    echo "usage: ./build.sh <debug / release>"
    exit 1
elif [ $1 = "debug" ]; then
    FLAGS="$FLAGS -g"
elif [ $1 = "release" ]; then
    FLAGS="$FLAGS -O3 -funit-at-a-time"
elif [ $1 = "run" ]; then
    ./$EXE $RUN_FLAGS
fi

gfortran $FLAGS -o $EXE $SRC

