.POSIX:
.SUFFIXES: .o .f
include ../conf.mk

M_FFLAGS = -I$(PREFIX)/include -std=f95
M_LDFLAGS = -L$(PREFIX)/lib -lpetros -lgfortran -lm

M = \
main\

all: $M
.f.o:; $(FC) -c $(FFLAGS) $(M_FFLAGS) $< -o $@
.o:; $(LINK) $< $(LDFLAGS) $(M_LDFLAGS) -o $@
%: %.f

lint:; make FFLAGS='-Wall -Wextra -g -O2'

.PHONY: clean lint
clean:; rm -f -- $M
