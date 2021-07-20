.POSIX:
.SUFFIXES:
.SUFFIXES: .c
.SUFFIXES: .f
.SUFFIXES: .o
include ../../conf.mk
F2C_CFLAGS = -I/usr/include
F2C_LDFLAGS = ../../libf2c/libf2c.wasm.a
FVORTEX_LDLAGS = ../../lib/c/libfvortex.wasm.a

O = \
lib.o \
main.o \

M = main

all: $M
lib.c: lib.f
.f.c:; f2c -I$(PREFIX)/include $<
.c.o:; $(CC) $(CFLAGS) $(F2C_CFLAGS) $< -c -I$(PREFIX)/include
main: $O; $(CC) $(LDFLAGS) $O -o $@ -L$(PREFIX)/lib $(FVORTEX_LDLAGS) $(F2C_LDFLAGS) -lm
clean:; rm -f -- $M $O
