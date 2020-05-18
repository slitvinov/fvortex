#!/bin/sh

: ${FC=gfortran}
: ${FFLAGS=-g -Wall -Wimplicit-interface}

$FC $FFLAGS sizeof.f95 -o sizeof
$FC $FFLAGS f.f95 -c -o f.o
$FC $FFLAGS main.f95 -c -o main.o
$FC $FFLAGS main.o f.o -o main
