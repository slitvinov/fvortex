.POSIX:
.SUFFIXES:
.SUFFIXES: .f
include ../../conf.mk

FVORTEX_FLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib -lfvortex
M = \
box_dim\
box_1\
diagnos\
make_box\

all: $M
.f:; $(FC) $< -o $@ $(FCSTD) $(FCFLAGS) $(FVORTEX_FLAGS)
clean:; rm -f -- $M
