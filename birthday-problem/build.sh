#!/bin/bash

SRC="birthdayproblem.f90 utils.f90"
EXE=birthdayproblem
FLAGS="-fsanitize=address -std=f2018 -Wextra -Wall -Wconversion"
if [ $1 = "debug" ]; then
    FLAGS="$FLAGS -g"
elif [ $1 = "release" ]; then
    FLAGS="$FLAGS -O3 -funit-at-a-time"
fi

gfortran $FLAGS -o $EXE $SRC