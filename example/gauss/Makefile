.POSIX:
.SUFFIXES:
.SUFFIXES: .f90
.SUFFIXES: .f
include ../../conf.mk

FVORTEX_FLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib -lfvortex -lm

M = \
main\
f77\

all: $M
.f90:; $(LINK) $< -o $@ $(FCSTD) $(FCFLAGS) $(FVORTEX_FLAGS)
.f:; $(F77) $< -o $@ $(FCFLAGS) $(FVORTEX_FLAGS)
clean:; rm -f -- $M
