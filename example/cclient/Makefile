.POSIX:
.SUFFIXES:
.SUFFIXES: .c
.SUFFIXES: .f
include ../../conf.mk

FVORTEX_FLAGS = -L$(PREFIX)/lib -lfvortex_c -lf2c -lm -lgfortran

M = \
main\

all: $M
.f.c:; f2c -I$(PREFIX)/include $<
.c:; $(CC) $< -o $@ $(FVORTEX_FLAGS)
clean:; rm -f -- $M
