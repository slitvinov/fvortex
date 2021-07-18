.POSIX:
.SUFFIXES:
.SUFFIXES: .f90
include ../../conf.mk

FVORTEX_FLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib -lfvortex -lm

M = \
main\

all: $M
.f90:; $(LINK) $< -o $@ $(FCSTD) $(FCFLAGS) $(FVORTEX_FLAGS)
clean:; rm -f -- $M
