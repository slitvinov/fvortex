/* ../childless9.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int childless9_(integer *kp9)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer j;
    static real x, y;
    static integer n1, n2, nb;
    static real xm, ym, fa1, fa2, fa3, fa4, tr0, ti0, re0, tr1, ti1, re1, tr2,
	     ti2, re2, tr3, ti3, re3, tr4, ti4, re4, tr5, ti5, re5, fa5, tr6, 
	    ti6, re6, fa6, tr7, ti7, re7, fa7;

/*     This subroutine computes the MULTIPOLE EXPANSIONS of */
/*     the childless boxes on level 9. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ----------------------------------------------------------------------- */
    i__1 = *kp9;
    for (nb = 1; nb <= i__1; ++nb) {
	n1 = index11_1.npb9[nb - 1];
	n2 = index11_1.npb9[nb + 262143];
	xm = centre_1.xc9[nb - 1];
	ym = centre_1.yc9[nb - 1];
	tr0 = 0.f;
	ti0 = 0.f;
	tr1 = 0.f;
	ti1 = 0.f;
	tr2 = 0.f;
	ti2 = 0.f;
	tr3 = 0.f;
	ti3 = 0.f;
	tr4 = 0.f;
	ti4 = 0.f;
	tr5 = 0.f;
	ti5 = 0.f;
	tr6 = 0.f;
	ti6 = 0.f;
	tr7 = 0.f;
	ti7 = 0.f;
	i__2 = n2;
	for (j = n1; j <= i__2; ++j) {
	    x = vort2_1.xn[j - 1] - xm;
	    y = ym - vort2_1.yn[j - 1];
	    re0 = vort2_1.gn[j - 1];
	    re1 = re0 * x;
	    fa1 = re0 * y;
	    re2 = re1 * x - fa1 * y;
	    fa2 = re1 * y + fa1 * x;
	    re3 = re2 * x - fa2 * y;
	    fa3 = re2 * y + fa2 * x;
	    re4 = re3 * x - fa3 * y;
	    fa4 = re3 * y + fa3 * x;
	    re5 = re4 * x - fa4 * y;
	    fa5 = re4 * y + fa4 * x;
	    re6 = re5 * x - fa5 * y;
	    fa6 = re5 * y + fa5 * x;
	    re7 = re6 * x - fa6 * y;
	    fa7 = re6 * y + fa6 * x;
	    tr0 += re0;
	    tr1 += re1;
	    ti1 += fa1;
	    tr2 += re2;
	    ti2 += fa2;
	    tr3 += re3;
	    ti3 += fa3;
	    tr4 += re4;
	    ti4 += fa4;
	    tr5 += re5;
	    ti5 += fa5;
	    tr6 += re6;
	    ti6 += fa6;
	    tr7 += re7;
	    ti7 += fa7;
/* L100: */
	}
	poles_1.pr9[nb - 1] = tr0;
	poles_1.pi9[nb - 1] = ti0;
	poles_1.pr9[nb + 262143] = tr1;
	poles_1.pi9[nb + 262143] = ti1;
	poles_1.pr9[nb + 524287] = tr2;
	poles_1.pi9[nb + 524287] = ti2;
	poles_1.pr9[nb + 786431] = tr3;
	poles_1.pi9[nb + 786431] = ti3;
	poles_1.pr9[nb + 1048575] = tr4;
	poles_1.pi9[nb + 1048575] = ti4;
	poles_1.pr9[nb + 1310719] = tr5;
	poles_1.pi9[nb + 1310719] = ti5;
	poles_1.pr9[nb + 1572863] = tr6;
	poles_1.pi9[nb + 1572863] = ti6;
	poles_1.pr9[nb + 1835007] = tr7;
	poles_1.pi9[nb + 1835007] = ti7;
/* L10: */
    }
    return 0;
} /* childless9_ */

