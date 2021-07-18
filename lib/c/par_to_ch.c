/* ../par_to_ch.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int par_to_ch__(integer *nmax, real *dch, integer *kp, real *
	br2, real *bi2, real *br3, real *bi3, integer *ich3par2, integer *
	ipar2ch3)
{
    /* System generated locals */
    integer ipar2ch3_dim1, ipar2ch3_offset, br2_dim1, br2_offset, bi2_dim1, 
	    bi2_offset, br3_dim1, br3_offset, bi3_dim1, bi3_offset, i__1;

    /* Local variables */
    static real f1, f2, f3, f4, f5, f6, f7;
    static integer m1, m2, m3, m4;
    static real p1, p2, p3, p4, p5, p6, p7, r1, r2, r3, r4, r5, r6, r7;
    static integer nb;
    static real p12, p22, p32, p13, p14;
    static integer km;
    static real p44, p54, p26, p38, p68, p78, p15, p16, p17, p18, p220, p212, 
	    p420, p320, p460, p524, p340, p230, p370, p242, p256, p656, p764, 
	    p584, p3112, p4140, p5224, p6224, p4280;

/*     This subroutine computes the MULTIPOLE EXPANSIONS of */
/*     the CHILDREN boxes from the multipole expansions of their */
/*     parents. */
/* --------------------------------------------------------------- */
    /* Parameter adjustments */
    ipar2ch3_dim1 = *nmax;
    ipar2ch3_offset = 1 + ipar2ch3_dim1;
    ipar2ch3 -= ipar2ch3_offset;
    --ich3par2;
    bi3_dim1 = 4 * *nmax;
    bi3_offset = 1 + bi3_dim1;
    bi3 -= bi3_offset;
    br3_dim1 = 4 * *nmax;
    br3_offset = 1 + br3_dim1;
    br3 -= br3_offset;
    bi2_dim1 = *nmax;
    bi2_offset = 1 + bi2_dim1;
    bi2 -= bi2_offset;
    br2_dim1 = *nmax;
    br2_offset = 1 + br2_dim1;
    br2 -= br2_offset;

    /* Function Body */
    p1 = *dch * .5f;
    p2 = p1 * p1;
    p3 = p1 * p2;
    p4 = p2 * p2;
    p5 = p2 * p3;
    p6 = p3 * p3;
    p7 = p3 * p4;
/*     Other constants */
    p78 = p7 * 8.f;
    p68 = p6 * 8.f;
    p54 = p5 * 4.f;
    p44 = p4 * 4.f;
    p32 = p3 * 2.f;
    p22 = p2 * 2.f;
    p764 = p7 * 64.f;
    p656 = p6 * 56.f;
    p524 = p5 * 24.f;
    p420 = p4 * 20.f;
    p38 = p3 * 8.f;
    p26 = p2 * 6.f;
    p12 = p1 * 2.f;
    p6224 = p6 * 224.f;
    p584 = p5 * 84.f;
    p460 = p4 * 60.f;
    p320 = p3 * 20.f;
    p212 = p2 * 12.f;
    p13 = p1 * 3.f;
    p5224 = p5 * 224.f;
    p4140 = p4 * 140.f;
    p340 = p3 * 40.f;
    p220 = p2 * 20.f;
    p14 = p1 * 4.f;
    p4280 = p4 * 280.f;
    p370 = p3 * 70.f;
    p230 = p2 * 30.f;
    p15 = p1 * 5.f;
    p3112 = p3 * 112.f;
    p242 = p2 * 42.f;
    p16 = p1 * 6.f;
    p256 = p2 * 56.f;
    p17 = p1 * 7.f;
    p18 = p1 * 8.f;
/* ----------------------------------------------------------------------------- */
    i__1 = *kp;
    for (nb = 1; nb <= i__1; ++nb) {
	km = ich3par2[nb];
/* REAL VALUES */
	r1 = br2[km + br2_dim1];
	r2 = br2[km + (br2_dim1 << 1)];
	r3 = br2[km + br2_dim1 * 3];
	r4 = br2[km + (br2_dim1 << 2)];
	r5 = br2[km + br2_dim1 * 5];
	r6 = br2[km + br2_dim1 * 6];
	r7 = br2[km + br2_dim1 * 7];
/* IMAGINARY VALUES */
	f1 = bi2[km + bi2_dim1];
	f2 = bi2[km + (bi2_dim1 << 1)];
	f3 = bi2[km + bi2_dim1 * 3];
	f4 = bi2[km + (bi2_dim1 << 2)];
	f5 = bi2[km + bi2_dim1 * 5];
	f6 = bi2[km + bi2_dim1 * 6];
	f7 = bi2[km + bi2_dim1 * 7];
/* Contribution to 1st Child (if any) */
	m1 = ipar2ch3[km + ipar2ch3_dim1];
/* index of child (if any) box 1 at level */
	m2 = ipar2ch3[km + (ipar2ch3_dim1 << 1)];
/* index of child (if any) box 2 at level */
	m3 = ipar2ch3[km + ipar2ch3_dim1 * 3];
/* index of child (if any) box 3 at level */
	m4 = ipar2ch3[km + (ipar2ch3_dim1 << 2)];
/* index of child (if any) box 4 at level */
	if (nb == m1) {
/* z */
/* -------- */
/* REAL and IMAGINARY PART OF EXPANSIONS */
/* -------- */
/* 1 - Order */
	    br3[nb + br3_dim1] = br3[nb + br3_dim1] - p68 * f7 + p54 * (r6 + 
		    f6) - p44 * r5 + p32 * (r4 - f4) + p22 * f3 - p1 * (r2 + 
		    f2) + r1;
	    bi3[nb + bi3_dim1] = bi3[nb + bi3_dim1] + p68 * r7 + p54 * (f6 - 
		    r6) - p44 * f5 + p32 * (f4 + r4) - p22 * r3 - p1 * (f2 - 
		    r2) + f1;
/* 2 - Order */
	    br3[nb + (br3_dim1 << 1)] = br3[nb + (br3_dim1 << 1)] + p524 * (
		    r7 + f7) - p420 * r6 + p38 * (r5 - f5) + p26 * f4 - p12 * 
		    (r3 + f3) + r2;
	    bi3[nb + (bi3_dim1 << 1)] = bi3[nb + (bi3_dim1 << 1)] + p524 * (
		    f7 - r7) - p420 * f6 + p38 * (f5 + r5) - p26 * r4 - p12 * 
		    (f3 - r3) + f2;
/* 3 - Order */
	    br3[nb + br3_dim1 * 3] = br3[nb + br3_dim1 * 3] - p460 * r7 + 
		    p320 * (r6 - f6) + p212 * f5 - p13 * (r4 + f4) + r3;
	    bi3[nb + bi3_dim1 * 3] = bi3[nb + bi3_dim1 * 3] - p460 * f7 + 
		    p320 * (f6 + r6) - p212 * r5 - p13 * (f4 - r4) + f3;
/* 4 - Order */
	    br3[nb + (br3_dim1 << 2)] = br3[nb + (br3_dim1 << 2)] + p340 * (
		    r7 - f7) + p220 * f6 - p14 * (r5 + f5) + r4;
	    bi3[nb + (bi3_dim1 << 2)] = bi3[nb + (bi3_dim1 << 2)] + p340 * (
		    f7 + r7) - p220 * r6 - p14 * (f5 - r5) + f4;
/* 5 - Order */
	    br3[nb + br3_dim1 * 5] = br3[nb + br3_dim1 * 5] + p230 * f7 - p15 
		    * (r6 + f6) + r5;
	    bi3[nb + bi3_dim1 * 5] = bi3[nb + bi3_dim1 * 5] - p230 * r7 - p15 
		    * (f6 - r6) + f5;
/* 6 - Order */
	    br3[nb + br3_dim1 * 6] = br3[nb + br3_dim1 * 6] - p16 * (r7 + f7) 
		    + r6;
	    bi3[nb + bi3_dim1 * 6] = bi3[nb + bi3_dim1 * 6] - p16 * (f7 - r7) 
		    + f6;
/* 7 - Order */
	    br3[nb + br3_dim1 * 7] += r7;
	    bi3[nb + bi3_dim1 * 7] += f7;
	} else if (nb == m2) {
/* ***************************************************************************** */
/*                                                                            * */
/*                    Contribution to 2nd Child (if any)                      * */
/*                                                                            * */
/* ***************************************************************************** */
/* -------- */
/* REAL and IMAGINARY PART OF EXPANSIONS */
/* -------- */
/* 1 - Order */
	    br3[nb + br3_dim1] = br3[nb + br3_dim1] + p68 * f7 + p54 * (r6 - 
		    f6) - p44 * r5 + p32 * (r4 + f4) - p22 * f3 - p1 * (r2 - 
		    f2) + r1;
	    bi3[nb + bi3_dim1] = bi3[nb + bi3_dim1] - p68 * r7 + p54 * (f6 + 
		    r6) - p44 * f5 + p32 * (f4 - r4) + p22 * r3 - p1 * (f2 + 
		    r2) + f1;
/* 2 - Order */
	    br3[nb + (br3_dim1 << 1)] = br3[nb + (br3_dim1 << 1)] + p524 * (
		    r7 - f7) - p420 * r6 + p38 * (r5 + f5) - p26 * f4 - p12 * 
		    (r3 - f3) + r2;
	    bi3[nb + (bi3_dim1 << 1)] = bi3[nb + (bi3_dim1 << 1)] + p524 * (
		    f7 + r7) - p420 * f6 + p38 * (f5 - r5) + p26 * r4 - p12 * 
		    (f3 + r3) + f2;
/* 3 - Order */
	    br3[nb + br3_dim1 * 3] = br3[nb + br3_dim1 * 3] - p460 * r7 + 
		    p320 * (r6 + f6) - p212 * f5 - p13 * (r4 - f4) + r3;
	    bi3[nb + bi3_dim1 * 3] = bi3[nb + bi3_dim1 * 3] - p460 * f7 + 
		    p320 * (f6 - r6) + p212 * r5 - p13 * (f4 + r4) + f3;
/* 4 - Order */
	    br3[nb + (br3_dim1 << 2)] = br3[nb + (br3_dim1 << 2)] + p340 * (
		    r7 + f7) - p220 * f6 - p14 * (r5 - f5) + r4;
	    bi3[nb + (bi3_dim1 << 2)] = bi3[nb + (bi3_dim1 << 2)] + p340 * (
		    f7 - r7) + p220 * r6 - p14 * (f5 + r5) + f4;
/* 5 - Order */
	    br3[nb + br3_dim1 * 5] = br3[nb + br3_dim1 * 5] - p230 * f7 - p15 
		    * (r6 - f6) + r5;
	    bi3[nb + bi3_dim1 * 5] = bi3[nb + bi3_dim1 * 5] + p230 * r7 - p15 
		    * (f6 + r6) + f5;
/* 6 - Order */
	    br3[nb + br3_dim1 * 6] = br3[nb + br3_dim1 * 6] - p16 * (r7 - f7) 
		    + r6;
	    bi3[nb + bi3_dim1 * 6] = bi3[nb + bi3_dim1 * 6] - p16 * (f7 + r7) 
		    + f6;
/* 7 - Order */
	    br3[nb + br3_dim1 * 7] += r7;
	    bi3[nb + bi3_dim1 * 7] += f7;
	} else if (nb == m3) {
/* ***************************************************************************** */
/*                                                                            * */
/*                    Contribution to 3rd Child (if any)                      * */
/*                                                                            * */
/* ***************************************************************************** */
/* -------- */
/* REAL and IMAGINARY PART OF EXPANSIONS */
/* -------- */
/* 1 - Order */
	    br3[nb + br3_dim1] = br3[nb + br3_dim1] + p68 * f7 - p54 * (r6 - 
		    f6) - p44 * r5 - p32 * (r4 + f4) - p22 * f3 + p1 * (r2 - 
		    f2) + r1;
	    bi3[nb + bi3_dim1] = bi3[nb + bi3_dim1] - p68 * r7 - p54 * (f6 + 
		    r6) - p44 * f5 - p32 * (f4 - r4) + p22 * r3 + p1 * (f2 + 
		    r2) + f1;
/* 2 - Order */
	    br3[nb + (br3_dim1 << 1)] = br3[nb + (br3_dim1 << 1)] - p524 * (
		    r7 - f7) - p420 * r6 - p38 * (r5 + f5) - p26 * f4 + p12 * 
		    (r3 - f3) + r2;
	    bi3[nb + (bi3_dim1 << 1)] = bi3[nb + (bi3_dim1 << 1)] - p524 * (
		    f7 + r7) - p420 * f6 - p38 * (f5 - r5) + p26 * r4 + p12 * 
		    (f3 + r3) + f2;
/* 3 - Order */
	    br3[nb + br3_dim1 * 3] = br3[nb + br3_dim1 * 3] - p460 * r7 - 
		    p320 * (r6 + f6) - p212 * f5 + p13 * (r4 - f4) + r3;
	    bi3[nb + bi3_dim1 * 3] = bi3[nb + bi3_dim1 * 3] - p460 * f7 - 
		    p320 * (f6 - r6) + p212 * r5 + p13 * (f4 + r4) + f3;
/* 4 - Order */
	    br3[nb + (br3_dim1 << 2)] = br3[nb + (br3_dim1 << 2)] - p340 * (
		    r7 + f7) - p220 * f6 + p14 * (r5 - f5) + r4;
	    bi3[nb + (bi3_dim1 << 2)] = bi3[nb + (bi3_dim1 << 2)] - p340 * (
		    f7 - r7) + p220 * r6 + p14 * (f5 + r5) + f4;
/* 5 - Order */
	    br3[nb + br3_dim1 * 5] = br3[nb + br3_dim1 * 5] - p230 * f7 + p15 
		    * (r6 - f6) + r5;
	    bi3[nb + bi3_dim1 * 5] = bi3[nb + bi3_dim1 * 5] + p230 * r7 + p15 
		    * (f6 + r6) + f5;
/* 6 - Order */
	    br3[nb + br3_dim1 * 6] = br3[nb + br3_dim1 * 6] + p16 * (r7 - f7) 
		    + r6;
	    bi3[nb + bi3_dim1 * 6] = bi3[nb + bi3_dim1 * 6] + p16 * (f7 + r7) 
		    + f6;
/* 7 - Order */
	    br3[nb + br3_dim1 * 7] += r7;
	    bi3[nb + bi3_dim1 * 7] += f7;
	} else if (nb == m4) {
/* ***************************************************************************** */
/*                                                                            * */
/*                    Contribution to 4th Child (if any)                      * */
/*                                                                            * */
/* ***************************************************************************** */
/* -------- */
/* REAL and IMAGINARY PART OF EXPANSIONS */
/* -------- */
/* 1 - Order */
	    br3[nb + br3_dim1] = br3[nb + br3_dim1] - p68 * f7 - p54 * (r6 + 
		    f6) - p44 * r5 - p32 * (r4 - f4) + p22 * f3 + p1 * (r2 + 
		    f2) + r1;
	    bi3[nb + bi3_dim1] = bi3[nb + bi3_dim1] + p68 * r7 - p54 * (f6 - 
		    r6) - p44 * f5 - p32 * (f4 + r4) - p22 * r3 + p1 * (f2 - 
		    r2) + f1;
/* 2 - Order */
	    br3[nb + (br3_dim1 << 1)] = br3[nb + (br3_dim1 << 1)] - p524 * (
		    r7 + f7) - p420 * r6 - p38 * (r5 - f5) + p26 * f4 + p12 * 
		    (r3 + f3) + r2;
	    bi3[nb + (bi3_dim1 << 1)] = bi3[nb + (bi3_dim1 << 1)] - p524 * (
		    f7 - r7) - p420 * f6 - p38 * (f5 + r5) - p26 * r4 + p12 * 
		    (f3 - r3) + f2;
/* 3 - Order */
	    br3[nb + br3_dim1 * 3] = br3[nb + br3_dim1 * 3] - p460 * r7 - 
		    p320 * (r6 - f6) + p212 * f5 + p13 * (r4 + f4) + r3;
	    bi3[nb + bi3_dim1 * 3] = bi3[nb + bi3_dim1 * 3] - p460 * f7 - 
		    p320 * (f6 + r6) - p212 * r5 + p13 * (f4 - r4) + f3;
/* 4 - Order */
	    br3[nb + (br3_dim1 << 2)] = br3[nb + (br3_dim1 << 2)] - p340 * (
		    r7 - f7) + p220 * f6 + p14 * (r5 + f5) + r4;
	    bi3[nb + (bi3_dim1 << 2)] = bi3[nb + (bi3_dim1 << 2)] - p340 * (
		    f7 + r7) - p220 * r6 + p14 * (f5 - r5) + f4;
/* 5 - Order */
	    br3[nb + br3_dim1 * 5] = br3[nb + br3_dim1 * 5] + p230 * f7 + p15 
		    * (r6 + f6) + r5;
	    bi3[nb + bi3_dim1 * 5] = bi3[nb + bi3_dim1 * 5] - p230 * r7 + p15 
		    * (f6 - r6) + f5;
/* 6 - Order */
	    br3[nb + br3_dim1 * 6] = br3[nb + br3_dim1 * 6] + p16 * (r7 + f7) 
		    + r6;
	    bi3[nb + bi3_dim1 * 6] = bi3[nb + bi3_dim1 * 6] + p16 * (f7 - r7) 
		    + f6;
/* 7 - Order */
	    br3[nb + br3_dim1 * 7] += r7;
	    bi3[nb + bi3_dim1 * 7] += f7;
	}
/* L20: */
    }
    return 0;
} /* par_to_ch__ */

