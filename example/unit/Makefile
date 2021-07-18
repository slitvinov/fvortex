.POSIX:
.SUFFIXES:
.SUFFIXES: .o .f90
include ../../conf.mk

M_FCFLAGS = -I$(PREFIX)/include
M_LDFLAGS = -L$(PREFIX)/lib -lpetros -lm

M = \
box_dim\
box_1\
make_box\

all: $M
.f90.o:; $(FC) $(FCSTD) -c $(FCFLAGS) $(M_FCFLAGS) $<
.o:; $(LINK) $< $(LDFLAGS) $(M_LDFLAGS) -o $@

lint:; make FCFLAGS='-Wall -Wimplicit-interface -Wimplicit-procedure -g -O2'

.PHONY: clean lint
clean:; rm -f -- $M
