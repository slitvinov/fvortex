.POSIX:
.SUFFIXES: .o .f95
include ../conf.mk

M_FFLAGS = -I$(PREFIX)/include
M_LDFLAGS = -L$(PREFIX)/lib -lpetros -lgfortran -lm

M = \
main\

all: $M
.f95.o:; $(FC) -c $(FFLAGS) $(M_FFLAGS) $< -o $@
.o:; $(LINK) $< $(LDFLAGS) $(M_LDFLAGS) -o $@
%: %.f95

lint:; make FFLAGS='-Wall -Wimplicit-interface -Wimplicit-procedure -g -O2'

.PHONY: clean lint
clean:; rm -f -- $M
