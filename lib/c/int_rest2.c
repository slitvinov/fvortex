/* ../int_rest2.f -- translated by f2c (version 20160102).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real xbox[60001], ybox[60001], prbox[480008]	/* was [60001][8] */, 
	    pibox[480008]	/* was [60001][8] */, xt[3450000], yt[3450000]
	    , gt[3450000];
    integer it[3450000];
} tempor_;

#define tempor_1 tempor_

struct {
    real xp[5000000], yp[5000000], gp[5000000];
} vort1_;

#define vort1_1 vort1_

struct {
    real xn[5000000], yn[5000000], gn[5000000];
} vort2_;

#define vort2_1 vort2_

struct {
    real uu[5000000], vv[5000000];
} vel_;

#define vel_1 vel_

struct {
    real uold[5000000], vold[5000000];
} oldvel_;

#define oldvel_1 oldvel_

struct {
    real gdiff[5000000], gdold[5000000];
} diff_;

#define diff_1 diff_

struct {
    integer kp1;
    real ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8, ds9;
} times_;

#define times_1 times_

struct {
    real xc1[4], yc1[4], xc2[16], yc2[16], xc3[64], yc3[64], xc4[256], yc4[
	    256], xc5[1024], yc5[1024], xc6[4096], yc6[4096], xc7[16384], yc7[
	    16384], xc8[65536], yc8[65536], xc9[262144], yc9[262144];
} centre_;

#define centre_1 centre_

struct {
    integer npb1[8]	/* was [4][2] */, ic1[4], jc1[4], npb2[32]	/* 
	    was [16][2] */, ic2[16], jc2[16], npb3[128]	/* was [64][2] */, 
	    ic3[64], jc3[64], npb4[512]	/* was [256][2] */, ic4[256], jc4[256]
	    , npb5[2048]	/* was [1024][2] */, ic5[1024], jc5[1024], 
	    npb6[8192]	/* was [4096][2] */, ic6[4096], jc6[4096], npb7[32768]
	    	/* was [16384][2] */, ic7[16384], jc7[16384], npb8[131072]	
	    /* was [65536][2] */, ic8[65536], jc8[65536], npb9[524288]	/* 
	    was [262144][2] */, ic9[262144], jc9[262144];
} index11_;

#define index11_1 index11_

struct {
    integer ipar1ch2[16]	/* was [4][4] */, ipar2ch3[64]	/* was [16][4]
	     */, ipar3ch4[256]	/* was [64][4] */, ipar4ch5[1024]	/* 
	    was [256][4] */, ipar5ch6[4096]	/* was [1024][4] */, ipar6ch7[
	    16384]	/* was [4096][4] */, ipar7ch8[65536]	/* was [16384]
	    [4] */, ipar8ch9[262144]	/* was [65536][4] */;
} index22_;

#define index22_1 index22_

struct {
    integer ichildless8[65536], iparent8[65536], imark8[65536], ichildless7[
	    16384], iparent7[16384], imark7[16384], ichildless6[4096], 
	    iparent6[4096], imark6[4096], ichildless5[1024], iparent5[1024], 
	    imark5[1024], ichildless4[256], iparent4[256], imark4[256], 
	    ichildless3[64], iparent3[64], imark3[64], ichildless2[16], 
	    iparent2[16], imark2[16], ichildless1[4], iparent1[4], imark1[4], 
	    liststart[4];
} index33_;

#define index33_1 index33_

struct {
    real pr9[2097152]	/* was [262144][8] */, pi9[2097152]	/* was [
	    262144][8] */, pr8[524288]	/* was [65536][8] */, pi8[524288]	
	    /* was [65536][8] */, pr7[131072]	/* was [16384][8] */, pi7[
	    131072]	/* was [16384][8] */, pr6[32768]	/* was [4096][
	    8] */, pi6[32768]	/* was [4096][8] */, pr5[8192]	/* was [1024][
	    8] */, pi5[8192]	/* was [1024][8] */, pr4[2048]	/* was [256][
	    8] */, pi4[2048]	/* was [256][8] */, pr3[512]	/* was [64][8]
	     */, pi3[512]	/* was [64][8] */, pr2[128]	/* was [16][8]
	     */, pi2[128]	/* was [16][8] */, pr1[32]	/* was [4][8] 
	    */, pi1[32]	/* was [4][8] */;
} poles_;

#define poles_1 poles_

struct {
    real br9[1835008]	/* was [262144][7] */, bi9[1835008]	/* was [
	    262144][7] */, br8[458752]	/* was [65536][7] */, bi8[458752]	
	    /* was [65536][7] */, br7[114688]	/* was [16384][7] */, bi7[
	    114688]	/* was [16384][7] */, br6[28672]	/* was [4096][
	    7] */, bi6[28672]	/* was [4096][7] */, br5[7168]	/* was [1024][
	    7] */, bi5[7168]	/* was [1024][7] */, br4[1792]	/* was [256][
	    7] */, bi4[1792]	/* was [256][7] */, br3[448]	/* was [64][7]
	     */, bi3[448]	/* was [64][7] */, br2[112]	/* was [16][7]
	     */, bi2[112]	/* was [16][7] */, br1[28]	/* was [4][7] 
	    */, bi1[28]	/* was [4][7] */;
} boxexp_;

#define boxexp_1 boxexp_

struct {
    real x0, y0;
    integer limpar;
} geom_;

#define geom_1 geom_

/* Table of constant values */

static integer c__4 = 4;
static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__16 = 16;

/* Subroutine */ int int_rest2__(integer *kp2)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    double atan(doublereal);
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    extern /* Subroutine */ int near_far__(integer *, integer *, integer *, 
	    real *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *);
    static integer listexam[5000000];
    static real dyopiinv;
    static integer listpart[5000000], i__, k;
    extern /* Subroutine */ int check_box__(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    static integer n1, n2, n4, listclose[5000000], ib, jb, kb, id;
    static real r11, r21, r22, xb, yb;
    static integer np, kbb, kfar, ipar, jpar;
    extern /* Subroutine */ int int_box_part__(integer *, integer *, real *, 
	    real *, integer *, real *, real *);
    static integer kexam, kpart, kclose;
    extern /* Subroutine */ int int_box__(integer *, integer *, real *, real *
	    , integer *, real *, real *);
    static integer listfar[5000000];

    /* Fortran I/O blocks */
    static cilist io___27 = { 0, 6, 0, 0, 0 };
    static cilist io___29 = { 0, 6, 0, 0, 0 };


/*     This routine figures box-box interactions for level 2 boxes as well as */
/*     interacting with the particles at higher level childless boxes that */
/*     interacted as particle-box previously. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* --------------------------------------------------------------------- */
    dyopiinv = 1.f / (atan(1.f) * 8.f);
    r21 = .5f;
    r11 = 1.f;
    r22 = 1.f;
    i__1 = *kp2;
    for (kb = 1; kb <= i__1; ++kb) {
/* All level 2 boxes, Childless & Parents */
	ib = index11_1.ic2[kb - 1];
	jb = index11_1.jc2[kb - 1];
	xb = centre_1.xc2[kb - 1];
	yb = centre_1.yc2[kb - 1];
/* Step 1 : Find i,j of your parent. */
	ipar = (xb - geom_1.x0) / times_1.ds1 + 1;
	jpar = (yb - geom_1.y0) / times_1.ds1 + 1;
	i__2 = times_1.kp1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    kexam = times_1.kp1;
	    listexam[i__ - 1] = index33_1.liststart[i__ - 1];
/* L1: */
	}
/* Step 2 : Find boxes adjacent to the parents */
/*          ( Start at coarsest  level ) */
/* -> 1st level */
	near_far__(&c__4, &ipar, &jpar, &r11, index11_1.ic1, index11_1.jc1, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
/* Examine boxes that are close for being childless or not. */
	check_box__(&c__4, &kclose, listclose, &kexam, listexam, &kpart, 
		listpart, index22_1.ipar1ch2, index33_1.imark1);
/* Boxes that are childless and close to the parents are examined to see if */
/* they are close to the box itself. */
	near_far__(&c__4, &ib, &jb, &r21, index11_1.ic1, index11_1.jc1, &
		kpart, listpart, &kfar, listfar, &kclose, listclose);
/* Boxes that are far from the child had interacted particle-box earlier. */
/* Box kb now needs to interact with their particles. */
	n4 = 0;
	i__2 = kfar;
	for (k = 1; k <= i__2; ++k) {
	    id = listfar[k - 1];
	    n1 = index11_1.npb1[id - 1];
	    n2 = index11_1.npb1[id + 3];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++n4;
		tempor_1.xt[n4 - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[n4 - 1] = vort2_1.yn[np - 1];
		tempor_1.gt[n4 - 1] = vort2_1.gn[np - 1];
/* L210: */
	    }
/* L21: */
	}
	if (n4 == 0) {
	    goto L88;
	}
	if (n4 > 3450000) {
	    s_wsle(&io___27);
	    do_lio(&c__9, &c__1, "error in rest2b", (ftnlen)15);
	    do_lio(&c__3, &c__1, (char *)&n4, (ftnlen)sizeof(integer));
	    e_wsle();
	}
	int_box_part__(&c__16, &kb, &xb, &yb, &n4, boxexp_1.br2, boxexp_1.bi2)
		;
/*  Now at level of child (box kb), look for boxes far enough away to */
/*  interact as box-box */
L88:
	near_far__(&c__16, &ib, &jb, &r22, index11_1.ic2, index11_1.jc2, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
	i__2 = kfar;
	for (kbb = 1; kbb <= i__2; ++kbb) {
	    id = listfar[kbb - 1];
	    tempor_1.xbox[kbb - 1] = centre_1.xc2[id - 1];
	    tempor_1.ybox[kbb - 1] = centre_1.yc2[id - 1];
	    tempor_1.prbox[kbb - 1] = poles_1.pr2[id - 1];
	    tempor_1.pibox[kbb - 1] = poles_1.pi2[id - 1];
	    tempor_1.prbox[kbb + 60000] = poles_1.pr2[id + 15];
	    tempor_1.pibox[kbb + 60000] = poles_1.pi2[id + 15];
	    tempor_1.prbox[kbb + 120001] = poles_1.pr2[id + 31];
	    tempor_1.pibox[kbb + 120001] = poles_1.pi2[id + 31];
	    tempor_1.prbox[kbb + 180002] = poles_1.pr2[id + 47];
	    tempor_1.pibox[kbb + 180002] = poles_1.pi2[id + 47];
	    tempor_1.prbox[kbb + 240003] = poles_1.pr2[id + 63];
	    tempor_1.pibox[kbb + 240003] = poles_1.pi2[id + 63];
	    tempor_1.prbox[kbb + 300004] = poles_1.pr2[id + 79];
	    tempor_1.pibox[kbb + 300004] = poles_1.pi2[id + 79];
	    tempor_1.prbox[kbb + 360005] = poles_1.pr2[id + 95];
	    tempor_1.pibox[kbb + 360005] = poles_1.pi2[id + 95];
	    tempor_1.prbox[kbb + 420006] = poles_1.pr2[id + 111];
	    tempor_1.pibox[kbb + 420006] = poles_1.pi2[id + 111];
/* L24: */
	}
	if (kfar > 60001) {
	    s_wsle(&io___29);
	    do_lio(&c__9, &c__1, "error in rest2", (ftnlen)14);
	    do_lio(&c__3, &c__1, (char *)&kbb, (ftnlen)sizeof(integer));
	    e_wsle();
	}
	int_box__(&c__16, &kb, &xb, &yb, &kfar, boxexp_1.br2, boxexp_1.bi2);
/* L20: */
    }
    return 0;
} /* int_rest2__ */

