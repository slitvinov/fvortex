.POSIX:
.SUFFIXES:
.SUFFIXES: .c
.SUFFIXES: .o
.PHONY: clean lint install dep int
include ../../conf.mk

L = libfvortex_c.a
O = \
box_1.o\
box9_to_part.o\
box_dim.o\
box_to_part.o\
check_box.o\
childless9.o\
ch_to_par.o\
condiff.o\
diagnos.o\
exp_chdless.o\
gaussian.o\
int_box.o\
int_box_part.o\
int_chless1.o\
int_chless2.o\
int_chless3.o\
int_chless4.o\
int_chless5.o\
int_chless6.o\
int_chless7.o\
int_chless8.o\
int_chless9.o\
int_part1.o\
int_part2.o\
int_part_box.o\
int_rest2.o\
int_rest3.o\
int_rest4.o\
int_rest5.o\
int_rest6.o\
int_rest7.o\
int_rest8.o\
int_rest9.o\
make_box.o\
mv_ab.o\
mv_eul.o\
mv_rk.o\
near_far.o\
omega.o\
par_to_ch.o\
read_restart.o\
remesh.o\
update_position.o\
vel_ext.o\
velocity_x.o\
velocity_y.o\
vort_field.o\
wheneq.o\
write_restart.o\
zeroall.o\

install: $L
	mkdir -p -- "$(PREFIX)/lib"
	cp -- $L "$(PREFIX)/lib"
$L: $O; ar rv $@ $O && ranlib $@
.c.o:; $(CC) -c $(CFLAGS) $<
clean:; rm -f -- $O $L
