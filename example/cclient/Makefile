.POSIX:
.SUFFIXES:
.SUFFIXES: .c
.SUFFIXES: .f
.SUFFIXES: .o
include ../../conf.mk

O = \
lib.o \
main.o \

M = main

all: $M
.f.c:; f2c -I$(PREFIX)/include $<
.c.o:; $(CC) $< -c -I$(PREFIX)/include
main: $O; $(CC) $O -o $@ -L$(PREFIX)/lib -lfvortex_c -lf2c -lm -lgfortran
clean:; rm -f -- $M $O
