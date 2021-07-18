/* ../int_chless9.f -- translated by f2c (version 20160102).
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

static integer c_b6 = 262144;
static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;

/* Subroutine */ int int_chless9__(integer *kp8, integer *kp9)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

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
    static integer i__, j, k, m, n;
    extern /* Subroutine */ int int_part1__(real *, real *, real *, real *, 
	    real *, real *, integer *);
    static integer n1, n2, listclose[5000000], ib, jb, kb, kc, id, km, nn, ks,
	     np;
    static real r99;
    static integer nns, nb1, nb2, kfp;
    static real up1, vp1, gp1;
    static integer lclg[10], kfar, ipar, jpar, kexam, kclose, listfar[5000000]
	    ;

    /* Fortran I/O blocks */
    static cilist io___31 = { 0, 6, 0, 0, 0 };


/*     Same idea as in int_chless2 but now for level 9 childless boxes. */
/*     For descriptive comments, go back to int_chless1&2 */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ---------------------------------------------------------------------------- */
    dyopiinv = 1.f / (atan(1.f) * 8.f);
    r99 = 1.f;
    i__1 = *kp9;
    for (kb = 1; kb <= i__1; ++kb) {
	nns = 0;
/* List 1 (same level) */
	nn = 0;
/* List 1 (finer levels) */
	kfp = 0;
/* List 3 */
	ib = index11_1.ic9[kb - 1];
	jb = index11_1.jc9[kb - 1];
	nb1 = index11_1.npb9[kb - 1];
	nb2 = index11_1.npb9[kb + 262143];
	ipar = (centre_1.xc9[kb - 1] - geom_1.x0) / times_1.ds8 + 1;
	jpar = (centre_1.yc9[kb - 1] - geom_1.y0) / times_1.ds8 + 1;
	kc = 0;
	i__2 = *kp8;
	for (k = 1; k <= i__2; ++k) {
/* Loop over boxes in parents level. */
	    i__ = index11_1.ic8[k - 1];
	    j = index11_1.jc8[k - 1];
	    if ((i__3 = i__ - ipar, abs(i__3)) > 1 || (i__4 = j - jpar, abs(
		    i__4)) > 1) {
		goto L21;
	    }
	    ++kc;
	    lclg[kc - 1] = k;
L21:
	    ;
	}
	kexam = 0;
	for (m = 1; m <= 4; ++m) {
	    i__2 = kc;
	    for (k = 1; k <= i__2; ++k) {
		ks = lclg[k - 1];
		km = index22_1.ipar8ch9[ks + (m << 16) - 65537];
		if (km == 0) {
		    goto L23;
		}
		++kexam;
		listexam[kexam - 1] = km;
L23:
		;
	    }
/* L22: */
	}
	near_far__(&c_b6, &ib, &jb, &r99, index11_1.ic9, index11_1.jc9, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
	i__2 = kclose;
	for (k = 1; k <= i__2; ++k) {
	    id = listclose[k - 1];
	    n1 = index11_1.npb9[id - 1];
	    n2 = index11_1.npb9[id + 262143];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++nns;
		tempor_1.xt[nns - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[nns - 1] = vort2_1.yn[np - 1];
/* childless boxes same level */
		tempor_1.gt[nns - 1] = vort2_1.gn[np - 1];
/* L250: */
	    }
/* L25: */
	}
	if (nns > 3450000) {
	    s_wsle(&io___31);
	    do_lio(&c__9, &c__1, "error in int_chless8", (ftnlen)20);
	    do_lio(&c__3, &c__1, (char *)&nns, (ftnlen)sizeof(integer));
	    e_wsle();
	}
	i__2 = nb2;
	for (n = nb1; n <= i__2; ++n) {
	    int_part1__(&vort2_1.xn[n - 1], &vort2_1.yn[n - 1], &vort2_1.gn[n 
		    - 1], &up1, &vp1, &gp1, &nns);
	    vel_1.uu[n - 1] += up1 * dyopiinv;
	    vel_1.vv[n - 1] += vp1 * dyopiinv;
	    diff_1.gdiff[n - 1] += gp1;
/* L251: */
	}
/* L20: */
    }
    return 0;
} /* int_chless9__ */

