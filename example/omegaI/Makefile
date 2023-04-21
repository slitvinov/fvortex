.POSIX:
.SUFFIXES:
.SUFFIXES: .f
include ../../conf.mk

FVORTEX_FLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib -lfvortex

M = \
main\

all: $M
.f:; $(FC) $< -o $@ $(FCSTD) $(FCFLAGS) $(FVORTEX_FLAGS)
clean:; rm -f -- $M
