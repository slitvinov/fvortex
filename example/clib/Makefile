.POSIX:
.SUFFIXES:
.SUFFIXES: .f
include ../../conf.mk

FVORTEX_FLAGS = -I$(PREFIX)/include -ff2c -L$(PREFIX)/lib -lfvortex_c -lf2c

M = \
main\

all: $M
.f:; $(FC) $< -o $@ $(FCSTD) $(FCFLAGS) $(FVORTEX_FLAGS)
clean:; rm -f -- $M
