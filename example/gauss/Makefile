.POSIX:
.SUFFIXES: .o .f90
include ../../conf.mk

M_FFLAGS = -I$(PREFIX)/include
M_LDFLAGS = -L$(PREFIX)/lib -lpetros -lm

M = \
main\

all: $M
.f90.o:; $(FC) -c $(FFLAGS) $(M_FFLAGS) $< -o $@
.o:; $(LINK) $< $(LDFLAGS) $(M_LDFLAGS) -o $@
%: %.f90

lint:; make FFLAGS='-Wall -Wimplicit-interface -Wimplicit-procedure -g -O2'

.PHONY: clean lint
clean:; rm -f -- $M
