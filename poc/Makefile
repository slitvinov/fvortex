.POSIX:
.SUFFIXES:
.SUFFIXES: .f
include ../conf.mk

M = \
ppm\
circle\
remesh\

all: $M
.f:; $(FC) $< -o $@ $(FCSTD) $(FCFLAGS)
clean:; rm -f -- $M
