.POSIX:
.SUFFIXES: .o .f
.PHONY: clean lint install
include ../conf.mk

M_FFLAGS = -std=f95

H = \
main_dim.h\
part.h\

L = libpetros.a
O = \
acc_theta.o\
acc_x.o\
acc_y.o\
angle.o\
box_1.o\
box9_to_part.o\
box_dim.o\
box_to_part.o\
build_tree.o\
check_box.o\
childless9.o\
ch_to_par.o\
condiff.o\
diagnos.o\
exp_chdless.o\
far_cls_cyl.o\
far_cls_ubd.o\
gaussian.o\
initial.o\
input.o\
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
max_rad.o\
mv_ab.o\
mv_eul.o\
mv_rk.o\
near_far.o\
omega.o\
par_to_ch.o\
pathlines.o\
pot_box_cyl.o\
pot_box_ubd.o\
potential.o\
pot_part_cyl.o\
pot_part_ubd.o\
read_restart.o\
remesh.o\
str_box.o\
streamfunction.o\
str_part.o\
update_position.o\
vel_box.o\
vel_ext.o\
velocity.o\
velocity_x.o\
velocity_y.o\
vel_part.o\
vel_tree.o\
vort_field.o\
vort_field_ta.o\
wheneq.o\
write_restart.o\
zeroall.o\

install: $L $H
	mkdir -p -- "$(PREFIX)/lib" "$(PREFIX)/include"
	cp -- $H "$(PREFIX)/include"
	cp -- $L "$(PREFIX)/lib"
$L: $O; ar rv $@ $O && ranlib $@
.f.o:; $(FC) -c $(FFLAGS) $(M_FFLAGS) $< -o $@
include dep.mk

lint:; make FFLAGS='-Wall -Wextra -g -O2'
dep:; for i in *.f; do $(FC) -MM -cpp $$i; done > dep.mk
clean:; rm -f -- $O $L
