.POSIX:
.SUFFIXES:
.SUFFIXES: .f
include ../conf.mk

M = \
ppm\
circle\

all: $M
.f:; $(FC) $< -o $@ $(FCSTD) $(FCFLAGS)
clean:; rm -f -- $M
