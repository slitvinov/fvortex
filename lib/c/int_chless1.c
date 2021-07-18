/* ../int_chless1.f -- translated by f2c (version 20160102).
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

/* Table of constant values */

static integer c__4 = 4;
static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__16 = 16;
static integer c__64 = 64;
static integer c__256 = 256;
static integer c__1024 = 1024;
static integer c__4096 = 4096;
static integer c__16384 = 16384;
static integer c__65536 = 65536;
static integer c_b48 = 262144;

/* Subroutine */ int int_chless1__(integer *kchildless1)
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
    static integer listpart[5000000], i__, k, n;
    extern /* Subroutine */ int int_part1__(real *, real *, real *, real *, 
	    real *, real *, integer *), int_part2__(real *, real *, real *, 
	    real *, real *, real *, integer *), check_box__(integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *);
    static integer n1, n2, listclose[5000000], ib, jb, kb, id, kh;
    static real r12, r13, r14, r15;
    static integer nn, np;
    static real r16, r17, r18, r19;
    static integer nb1, nb2, nns, kfp;
    static real xnn, ynn, gnn, up1, vp1, gp1, up2, vp2, gp2;
    static integer kfar;
    static real ubox, vbox;
    extern /* Subroutine */ int int_part_box__(real *, real *, real *, real *,
	     integer *);
    static integer kexam, level, kpart, kclose, listfar[5000000];

    /* Fortran I/O blocks */
    static cilist io___31 = { 0, 6, 0, 0, 0 };
    static cilist io___39 = { 0, 6, 0, 0, 0 };
    static cilist io___40 = { 0, 6, 0, 0, 0 };


/*     These subroutines determine the hierarchy of interactions and call for the */
/*     interactions. Particle interactions are assigned in the int_chless routines */
/*     as they only happen for childless boxes (otherwise you go to the next level */
/*     in hopes of geting some box interactions). Box interactions are taken care */
/*     of by the int_rest routines. Particle-box interactions occur in both, with */
/*     the particle end of these interactions in int_rest and box in int_chless. */
/* ------------------------------------------------------------------------- */
/*     This subroutine figures out the interactions for childless boxes at */
/*     level 1 (the highest level of the tree).  They can interact as */
/*     particle-particle with nearby childless boxes and particle-box with */
/*     finer boxes which are far enough away (bigger box must be considered as */
/*     particles for the smaller box in this case but smaller box as a box to */
/*     bigger box). Box-box interactions cannot happen for a level 1 box and are */
/*     handled by the int_rest arrays rather than int_chless. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ------------------------------------------------------------------------- */
    dyopiinv = 1.f / (atan(1.f) * 8.f);
    r12 = 2.f;
    r13 = 4.f;
    r14 = 8.f;
    r15 = 16.f;
    r16 = 32.f;
    r17 = 64.f;
    r18 = 128.f;
    r19 = 256.f;
    kexam = 0;
    kfar = 0;
    kclose = 0;
    for (i__ = 1; i__ <= 5000000; ++i__) {
	listexam[i__ - 1] = 0;
	listfar[i__ - 1] = 0;
	listclose[i__ - 1] = 0;
    }
    i__1 = *kchildless1;
    for (kh = 1; kh <= i__1; ++kh) {
	kb = index33_1.ichildless1[kh - 1];
	ib = index11_1.ic1[kb - 1];
	jb = index11_1.jc1[kb - 1];
	nb1 = index11_1.npb1[kb - 1];
	nb2 = index11_1.npb1[kb + 3];
	i__2 = times_1.kp1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    ++kclose;
	    listclose[i__ - 1] = index33_1.liststart[i__ - 1];
/* L1: */
	}
/*  Construct the interaction list with particles and boxes that belong */
/*  to finer levels than the ** 1st **. */
/*  First find all childless boxes on level 1. */
/*  Note that the box will find and interact with itself (as particles) */
	check_box__(&c__4, &kclose, listclose, &kexam, listexam, &kpart, 
		listpart, index22_1.ipar1ch2, index33_1.imark1);
	nns = 0;
	i__2 = kpart;
	for (k = 1; k <= i__2; ++k) {
	    id = listpart[k - 1];
	    n1 = index11_1.npb1[id - 1];
	    n2 = index11_1.npb1[id + 3];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++nns;
		tempor_1.xt[nns - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[nns - 1] = vort2_1.yn[np - 1];
		tempor_1.gt[nns - 1] = vort2_1.gn[np - 1];
/* L111: */
	    }
/* L11: */
	}
	if (nns > 3450000) {
	    s_wsle(&io___31);
	    do_lio(&c__9, &c__1, "error in int_chless1", (ftnlen)20);
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
/* ______________________________ */
	level = 2;
/*  Find which level 2 boxes are far enough away to interact as a box with */
/*  level 1 particles (level 2 boxes are the 4 subdivisions of a level 1 box). */
	near_far__(&c__16, &ib, &jb, &r12, index11_1.ic2, index11_1.jc2, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
	kfp = 0;
	i__2 = kfar;
	for (k = 1; k <= i__2; ++k) {
	    ++kfp;
	    id = listfar[k - 1];
	    tempor_1.xbox[kfp - 1] = centre_1.xc2[id - 1];
	    tempor_1.ybox[kfp - 1] = centre_1.yc2[id - 1];
	    tempor_1.prbox[kfp - 1] = poles_1.pr2[id - 1];
	    tempor_1.pibox[kfp - 1] = poles_1.pi2[id - 1];
	    tempor_1.prbox[kfp + 60000] = poles_1.pr2[id + 15];
	    tempor_1.pibox[kfp + 60000] = poles_1.pi2[id + 15];
	    tempor_1.prbox[kfp + 120001] = poles_1.pr2[id + 31];
	    tempor_1.pibox[kfp + 120001] = poles_1.pi2[id + 31];
	    tempor_1.prbox[kfp + 180002] = poles_1.pr2[id + 47];
	    tempor_1.pibox[kfp + 180002] = poles_1.pi2[id + 47];
	    tempor_1.prbox[kfp + 240003] = poles_1.pr2[id + 63];
	    tempor_1.pibox[kfp + 240003] = poles_1.pi2[id + 63];
	    tempor_1.prbox[kfp + 300004] = poles_1.pr2[id + 79];
	    tempor_1.pibox[kfp + 300004] = poles_1.pi2[id + 79];
	    tempor_1.prbox[kfp + 360005] = poles_1.pr2[id + 95];
	    tempor_1.pibox[kfp + 360005] = poles_1.pi2[id + 95];
	    tempor_1.prbox[kfp + 420006] = poles_1.pr2[id + 111];
	    tempor_1.pibox[kfp + 420006] = poles_1.pi2[id + 111];
/* L12: */
	}
/*  Check the remaining level 2 boxes for childless boxes. Since they didn't */
/*  interact as a box above and further subdivisions don't exist for the */
/*  box, it must now interact as particles. */
	check_box__(&c__16, &kclose, listclose, &kexam, listexam, &kpart, 
		listpart, index22_1.ipar2ch3, index33_1.imark2);
	nn = 0;
	i__2 = kpart;
	for (k = 1; k <= i__2; ++k) {
	    id = listpart[k - 1];
	    n1 = index11_1.npb2[id - 1];
	    n2 = index11_1.npb2[id + 15];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++nn;
		tempor_1.xt[nn - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[nn - 1] = vort2_1.yn[np - 1];
		tempor_1.gt[nn - 1] = vort2_1.gn[np - 1];
		tempor_1.it[nn - 1] = np;
/* L250: */
	    }
/* L25: */
	}
/* All remaining boxes (those which have not yet interacted in some way) are */
/* parents, thus go to their level 3 children. Process of level 2 repeats */
/* for all subsequent levels. */
/* _____________________________________ */
	level = 3;
	near_far__(&c__64, &ib, &jb, &r13, index11_1.ic3, index11_1.jc3, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
	i__2 = kfar;
	for (k = 1; k <= i__2; ++k) {
	    ++kfp;
	    id = listfar[k - 1];
	    tempor_1.xbox[kfp - 1] = centre_1.xc3[id - 1];
	    tempor_1.ybox[kfp - 1] = centre_1.yc3[id - 1];
	    tempor_1.prbox[kfp - 1] = poles_1.pr3[id - 1];
	    tempor_1.pibox[kfp - 1] = poles_1.pi3[id - 1];
	    tempor_1.prbox[kfp + 60000] = poles_1.pr3[id + 63];
	    tempor_1.pibox[kfp + 60000] = poles_1.pi3[id + 63];
	    tempor_1.prbox[kfp + 120001] = poles_1.pr3[id + 127];
	    tempor_1.pibox[kfp + 120001] = poles_1.pi3[id + 127];
	    tempor_1.prbox[kfp + 180002] = poles_1.pr3[id + 191];
	    tempor_1.pibox[kfp + 180002] = poles_1.pi3[id + 191];
	    tempor_1.prbox[kfp + 240003] = poles_1.pr3[id + 255];
	    tempor_1.pibox[kfp + 240003] = poles_1.pi3[id + 255];
	    tempor_1.prbox[kfp + 300004] = poles_1.pr3[id + 319];
	    tempor_1.pibox[kfp + 300004] = poles_1.pi3[id + 319];
	    tempor_1.prbox[kfp + 360005] = poles_1.pr3[id + 383];
	    tempor_1.pibox[kfp + 360005] = poles_1.pi3[id + 383];
	    tempor_1.prbox[kfp + 420006] = poles_1.pr3[id + 447];
	    tempor_1.pibox[kfp + 420006] = poles_1.pi3[id + 447];
/* L26: */
	}
	check_box__(&c__64, &kclose, listclose, &kexam, listexam, &kpart, 
		listpart, index22_1.ipar3ch4, index33_1.imark3);
	i__2 = kpart;
	for (k = 1; k <= i__2; ++k) {
	    id = listpart[k - 1];
	    n1 = index11_1.npb3[id - 1];
	    n2 = index11_1.npb3[id + 63];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++nn;
		tempor_1.xt[nn - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[nn - 1] = vort2_1.yn[np - 1];
		tempor_1.gt[nn - 1] = vort2_1.gn[np - 1];
		tempor_1.it[nn - 1] = np;
/* L270: */
	    }
/* L27: */
	}
/* ____________________ */
	level = 4;
	near_far__(&c__256, &ib, &jb, &r14, index11_1.ic4, index11_1.jc4, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
	i__2 = kfar;
	for (k = 1; k <= i__2; ++k) {
	    ++kfp;
	    id = listfar[k - 1];
	    tempor_1.xbox[kfp - 1] = centre_1.xc4[id - 1];
	    tempor_1.ybox[kfp - 1] = centre_1.yc4[id - 1];
	    tempor_1.prbox[kfp - 1] = poles_1.pr4[id - 1];
	    tempor_1.pibox[kfp - 1] = poles_1.pi4[id - 1];
	    tempor_1.prbox[kfp + 60000] = poles_1.pr4[id + 255];
	    tempor_1.pibox[kfp + 60000] = poles_1.pi4[id + 255];
	    tempor_1.prbox[kfp + 120001] = poles_1.pr4[id + 511];
	    tempor_1.pibox[kfp + 120001] = poles_1.pi4[id + 511];
	    tempor_1.prbox[kfp + 180002] = poles_1.pr4[id + 767];
	    tempor_1.pibox[kfp + 180002] = poles_1.pi4[id + 767];
	    tempor_1.prbox[kfp + 240003] = poles_1.pr4[id + 1023];
	    tempor_1.pibox[kfp + 240003] = poles_1.pi4[id + 1023];
	    tempor_1.prbox[kfp + 300004] = poles_1.pr4[id + 1279];
	    tempor_1.pibox[kfp + 300004] = poles_1.pi4[id + 1279];
	    tempor_1.prbox[kfp + 360005] = poles_1.pr4[id + 1535];
	    tempor_1.pibox[kfp + 360005] = poles_1.pi4[id + 1535];
	    tempor_1.prbox[kfp + 420006] = poles_1.pr4[id + 1791];
	    tempor_1.pibox[kfp + 420006] = poles_1.pi4[id + 1791];
/* L28: */
	}
	check_box__(&c__256, &kclose, listclose, &kexam, listexam, &kpart, 
		listpart, index22_1.ipar4ch5, index33_1.imark4);
	i__2 = kpart;
	for (k = 1; k <= i__2; ++k) {
	    id = listpart[k - 1];
	    n1 = index11_1.npb4[id - 1];
	    n2 = index11_1.npb4[id + 255];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++nn;
		tempor_1.xt[nn - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[nn - 1] = vort2_1.yn[np - 1];
		tempor_1.gt[nn - 1] = vort2_1.gn[np - 1];
		tempor_1.it[nn - 1] = np;
/* L290: */
	    }
/* L29: */
	}
/* ____________________ */
	level = 5;
	near_far__(&c__1024, &ib, &jb, &r15, index11_1.ic5, index11_1.jc5, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
	i__2 = kfar;
	for (k = 1; k <= i__2; ++k) {
	    ++kfp;
	    id = listfar[k - 1];
	    tempor_1.xbox[kfp - 1] = centre_1.xc5[id - 1];
	    tempor_1.ybox[kfp - 1] = centre_1.yc5[id - 1];
	    tempor_1.prbox[kfp - 1] = poles_1.pr5[id - 1];
	    tempor_1.pibox[kfp - 1] = poles_1.pi5[id - 1];
	    tempor_1.prbox[kfp + 60000] = poles_1.pr5[id + 1023];
	    tempor_1.pibox[kfp + 60000] = poles_1.pi5[id + 1023];
	    tempor_1.prbox[kfp + 120001] = poles_1.pr5[id + 2047];
	    tempor_1.pibox[kfp + 120001] = poles_1.pi5[id + 2047];
	    tempor_1.prbox[kfp + 180002] = poles_1.pr5[id + 3071];
	    tempor_1.pibox[kfp + 180002] = poles_1.pi5[id + 3071];
	    tempor_1.prbox[kfp + 240003] = poles_1.pr5[id + 4095];
	    tempor_1.pibox[kfp + 240003] = poles_1.pi5[id + 4095];
	    tempor_1.prbox[kfp + 300004] = poles_1.pr5[id + 5119];
	    tempor_1.pibox[kfp + 300004] = poles_1.pi5[id + 5119];
	    tempor_1.prbox[kfp + 360005] = poles_1.pr5[id + 6143];
	    tempor_1.pibox[kfp + 360005] = poles_1.pi5[id + 6143];
	    tempor_1.prbox[kfp + 420006] = poles_1.pr5[id + 7167];
	    tempor_1.pibox[kfp + 420006] = poles_1.pi5[id + 7167];
/* L30: */
	}
	check_box__(&c__1024, &kclose, listclose, &kexam, listexam, &kpart, 
		listpart, index22_1.ipar5ch6, index33_1.imark5);
	i__2 = kpart;
	for (k = 1; k <= i__2; ++k) {
	    id = listpart[k - 1];
	    n1 = index11_1.npb5[id - 1];
	    n2 = index11_1.npb5[id + 1023];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++nn;
		tempor_1.xt[nn - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[nn - 1] = vort2_1.yn[np - 1];
		tempor_1.gt[nn - 1] = vort2_1.gn[np - 1];
		tempor_1.it[nn - 1] = np;
/* L310: */
	    }
/* L31: */
	}
/* ____________________ */
	level = 6;
	near_far__(&c__4096, &ib, &jb, &r16, index11_1.ic6, index11_1.jc6, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
	i__2 = kfar;
	for (k = 1; k <= i__2; ++k) {
	    ++kfp;
	    id = listfar[k - 1];
	    tempor_1.xbox[kfp - 1] = centre_1.xc6[id - 1];
	    tempor_1.ybox[kfp - 1] = centre_1.yc6[id - 1];
	    tempor_1.prbox[kfp - 1] = poles_1.pr6[id - 1];
	    tempor_1.pibox[kfp - 1] = poles_1.pi6[id - 1];
	    tempor_1.prbox[kfp + 60000] = poles_1.pr6[id + 4095];
	    tempor_1.pibox[kfp + 60000] = poles_1.pi6[id + 4095];
	    tempor_1.prbox[kfp + 120001] = poles_1.pr6[id + 8191];
	    tempor_1.pibox[kfp + 120001] = poles_1.pi6[id + 8191];
	    tempor_1.prbox[kfp + 180002] = poles_1.pr6[id + 12287];
	    tempor_1.pibox[kfp + 180002] = poles_1.pi6[id + 12287];
	    tempor_1.prbox[kfp + 240003] = poles_1.pr6[id + 16383];
	    tempor_1.pibox[kfp + 240003] = poles_1.pi6[id + 16383];
	    tempor_1.prbox[kfp + 300004] = poles_1.pr6[id + 20479];
	    tempor_1.pibox[kfp + 300004] = poles_1.pi6[id + 20479];
	    tempor_1.prbox[kfp + 360005] = poles_1.pr6[id + 24575];
	    tempor_1.pibox[kfp + 360005] = poles_1.pi6[id + 24575];
	    tempor_1.prbox[kfp + 420006] = poles_1.pr6[id + 28671];
	    tempor_1.pibox[kfp + 420006] = poles_1.pi6[id + 28671];
/* L32: */
	}
	check_box__(&c__4096, &kclose, listclose, &kexam, listexam, &kpart, 
		listpart, index22_1.ipar6ch7, index33_1.imark6);
	i__2 = kpart;
	for (k = 1; k <= i__2; ++k) {
	    id = listpart[k - 1];
	    n1 = index11_1.npb6[id - 1];
	    n2 = index11_1.npb6[id + 4095];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++nn;
		tempor_1.xt[nn - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[nn - 1] = vort2_1.yn[np - 1];
		tempor_1.gt[nn - 1] = vort2_1.gn[np - 1];
		tempor_1.it[nn - 1] = np;
/* L330: */
	    }
/* L33: */
	}
/* ____________________ */
	level = 7;
	near_far__(&c__16384, &ib, &jb, &r17, index11_1.ic7, index11_1.jc7, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
	i__2 = kfar;
	for (k = 1; k <= i__2; ++k) {
	    ++kfp;
	    id = listfar[k - 1];
	    tempor_1.xbox[kfp - 1] = centre_1.xc7[id - 1];
	    tempor_1.ybox[kfp - 1] = centre_1.yc7[id - 1];
	    tempor_1.prbox[kfp - 1] = poles_1.pr7[id - 1];
	    tempor_1.pibox[kfp - 1] = poles_1.pi7[id - 1];
	    tempor_1.prbox[kfp + 60000] = poles_1.pr7[id + 16383];
	    tempor_1.pibox[kfp + 60000] = poles_1.pi7[id + 16383];
	    tempor_1.prbox[kfp + 120001] = poles_1.pr7[id + 32767];
	    tempor_1.pibox[kfp + 120001] = poles_1.pi7[id + 32767];
	    tempor_1.prbox[kfp + 180002] = poles_1.pr7[id + 49151];
	    tempor_1.pibox[kfp + 180002] = poles_1.pi7[id + 49151];
	    tempor_1.prbox[kfp + 240003] = poles_1.pr7[id + 65535];
	    tempor_1.pibox[kfp + 240003] = poles_1.pi7[id + 65535];
	    tempor_1.prbox[kfp + 300004] = poles_1.pr7[id + 81919];
	    tempor_1.pibox[kfp + 300004] = poles_1.pi7[id + 81919];
	    tempor_1.prbox[kfp + 360005] = poles_1.pr7[id + 98303];
	    tempor_1.pibox[kfp + 360005] = poles_1.pi7[id + 98303];
	    tempor_1.prbox[kfp + 420006] = poles_1.pr7[id + 114687];
	    tempor_1.pibox[kfp + 420006] = poles_1.pi7[id + 114687];
/* L34: */
	}
	check_box__(&c__16384, &kclose, listclose, &kexam, listexam, &kpart, 
		listpart, index22_1.ipar7ch8, index33_1.imark7);
	i__2 = kpart;
	for (k = 1; k <= i__2; ++k) {
	    id = listpart[k - 1];
	    n1 = index11_1.npb7[id - 1];
	    n2 = index11_1.npb7[id + 16383];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++nn;
		tempor_1.xt[nn - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[nn - 1] = vort2_1.yn[np - 1];
		tempor_1.gt[nn - 1] = vort2_1.gn[np - 1];
		tempor_1.it[nn - 1] = np;
/* L350: */
	    }
/* L35: */
	}
/* ____________________ */
	level = 8;
	near_far__(&c__65536, &ib, &jb, &r18, index11_1.ic8, index11_1.jc8, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
	i__2 = kfar;
	for (k = 1; k <= i__2; ++k) {
	    ++kfp;
	    id = listfar[k - 1];
	    tempor_1.xbox[kfp - 1] = centre_1.xc8[id - 1];
	    tempor_1.ybox[kfp - 1] = centre_1.yc8[id - 1];
	    tempor_1.prbox[kfp - 1] = poles_1.pr8[id - 1];
	    tempor_1.pibox[kfp - 1] = poles_1.pi8[id - 1];
	    tempor_1.prbox[kfp + 60000] = poles_1.pr8[id + 65535];
	    tempor_1.pibox[kfp + 60000] = poles_1.pi8[id + 65535];
	    tempor_1.prbox[kfp + 120001] = poles_1.pr8[id + 131071];
	    tempor_1.pibox[kfp + 120001] = poles_1.pi8[id + 131071];
	    tempor_1.prbox[kfp + 180002] = poles_1.pr8[id + 196607];
	    tempor_1.pibox[kfp + 180002] = poles_1.pi8[id + 196607];
	    tempor_1.prbox[kfp + 240003] = poles_1.pr8[id + 262143];
	    tempor_1.pibox[kfp + 240003] = poles_1.pi8[id + 262143];
	    tempor_1.prbox[kfp + 300004] = poles_1.pr8[id + 327679];
	    tempor_1.pibox[kfp + 300004] = poles_1.pi8[id + 327679];
	    tempor_1.prbox[kfp + 360005] = poles_1.pr8[id + 393215];
	    tempor_1.pibox[kfp + 360005] = poles_1.pi8[id + 393215];
	    tempor_1.prbox[kfp + 420006] = poles_1.pr8[id + 458751];
	    tempor_1.pibox[kfp + 420006] = poles_1.pi8[id + 458751];
/* L36: */
	}
	check_box__(&c__65536, &kclose, listclose, &kexam, listexam, &kpart, 
		listpart, index22_1.ipar8ch9, index33_1.imark8);
	i__2 = kpart;
	for (k = 1; k <= i__2; ++k) {
	    id = listpart[k - 1];
	    n1 = index11_1.npb8[id - 1];
	    n2 = index11_1.npb8[id + 65535];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++nn;
		tempor_1.xt[nn - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[nn - 1] = vort2_1.yn[np - 1];
		tempor_1.gt[nn - 1] = vort2_1.gn[np - 1];
		tempor_1.it[nn - 1] = np;
/* L370: */
	    }
/* L37: */
	}
/* ____________________ */
	level = 9;
	near_far__(&c_b48, &ib, &jb, &r19, index11_1.ic9, index11_1.jc9, &
		kexam, listexam, &kfar, listfar, &kclose, listclose);
	i__2 = kfar;
	for (k = 1; k <= i__2; ++k) {
	    ++kfp;
	    id = listfar[k - 1];
	    tempor_1.xbox[kfp - 1] = centre_1.xc9[id - 1];
	    tempor_1.ybox[kfp - 1] = centre_1.yc9[id - 1];
	    tempor_1.prbox[kfp - 1] = poles_1.pr9[id - 1];
	    tempor_1.pibox[kfp - 1] = poles_1.pi9[id - 1];
	    tempor_1.prbox[kfp + 60000] = poles_1.pr9[id + 262143];
	    tempor_1.pibox[kfp + 60000] = poles_1.pi9[id + 262143];
	    tempor_1.prbox[kfp + 120001] = poles_1.pr9[id + 524287];
	    tempor_1.pibox[kfp + 120001] = poles_1.pi9[id + 524287];
	    tempor_1.prbox[kfp + 180002] = poles_1.pr9[id + 786431];
	    tempor_1.pibox[kfp + 180002] = poles_1.pi9[id + 786431];
	    tempor_1.prbox[kfp + 240003] = poles_1.pr9[id + 1048575];
	    tempor_1.pibox[kfp + 240003] = poles_1.pi9[id + 1048575];
	    tempor_1.prbox[kfp + 300004] = poles_1.pr9[id + 1310719];
	    tempor_1.pibox[kfp + 300004] = poles_1.pi9[id + 1310719];
	    tempor_1.prbox[kfp + 360005] = poles_1.pr9[id + 1572863];
	    tempor_1.pibox[kfp + 360005] = poles_1.pi9[id + 1572863];
	    tempor_1.prbox[kfp + 420006] = poles_1.pr9[id + 1835007];
	    tempor_1.pibox[kfp + 420006] = poles_1.pi9[id + 1835007];
	}
	i__2 = kclose;
	for (k = 1; k <= i__2; ++k) {
/* All close boxes are now childless */
	    id = listclose[k - 1];
	    n1 = index11_1.npb9[id - 1];
	    n2 = index11_1.npb9[id + 262143];
	    i__3 = n2;
	    for (np = n1; np <= i__3; ++np) {
		++nn;
		tempor_1.xt[nn - 1] = vort2_1.xn[np - 1];
		tempor_1.yt[nn - 1] = vort2_1.yn[np - 1];
		tempor_1.gt[nn - 1] = vort2_1.gn[np - 1];
		tempor_1.it[nn - 1] = np;
	    }
	}
	if (nn > 3450000) {
	    s_wsle(&io___39);
	    do_lio(&c__9, &c__1, "error in int_chless1p", (ftnlen)21);
	    do_lio(&c__3, &c__1, (char *)&nn, (ftnlen)sizeof(integer));
	    e_wsle();
	}
	if (kfp > 60001) {
	    s_wsle(&io___40);
	    do_lio(&c__9, &c__1, "error in int_chless1b", (ftnlen)21);
	    do_lio(&c__3, &c__1, (char *)&kfp, (ftnlen)sizeof(integer));
	    e_wsle();
	}
	i__2 = nb2;
	for (n = nb1; n <= i__2; ++n) {
	    xnn = vort2_1.xn[n - 1];
	    ynn = vort2_1.yn[n - 1];
	    gnn = vort2_1.gn[n - 1];
	    int_part2__(&gnn, &xnn, &ynn, &up2, &vp2, &gp2, &nn);
	    int_part_box__(&xnn, &ynn, &ubox, &vbox, &kfp);
	    vel_1.uu[n - 1] += (up2 + ubox) * dyopiinv;
	    vel_1.vv[n - 1] += (vp2 + vbox) * dyopiinv;
	    diff_1.gdiff[n - 1] += gp2;
/* L351: */
	}
/* L10: */
    }
    return 0;
} /* int_chless1__ */

