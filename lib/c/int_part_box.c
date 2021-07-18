/* ../int_part_box.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int int_part_box__(real *xtest, real *ytest, real *ubox, 
	real *vbox, integer *kbox)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static real f1, f2, f3, f4, f5, f6, f7, f8, r1, r2, r3, r4, r5, r6, r7, 
	    r8;
    static integer ks;
    static real xx, yy, c0i, c1i, c2i, c3i, c4i, c5i, c6i, c7i, c0r, c1r, c2r,
	     c3r, c4r, c5r, c6r, c7r, pil, prl, veli, velr, r2inv;
    static integer level;

/*     This  subroutine calculates the velocities induced by boxes */
/*     on individual particles. */
/* ----------------------------------------------------------- */
    *ubox = 0.f;
    *vbox = 0.f;
    i__1 = *kbox;
    for (ks = 1; ks <= i__1; ++ks) {
	level = 0;
	xx = *xtest - tempor_1.xbox[ks - 1];
	yy = *ytest - tempor_1.ybox[ks - 1];
	r2inv = 1.f / (xx * xx + yy * yy);
	r1 = xx * r2inv;
	f1 = yy * r2inv;
	c0r = f1 * tempor_1.prbox[ks + level * 60001 - 1];
	c0i = r1 * tempor_1.prbox[ks + level * 60001 - 1];
	level = 1;
	prl = tempor_1.prbox[ks + level * 60001 - 1];
	pil = tempor_1.pibox[ks + level * 60001 - 1];
	r2 = r1 * r1 - f1 * f1;
	f2 = r1 * f1 + f1 * r1;
	c1r = f2 * prl + r2 * pil;
	c1i = r2 * prl - f2 * pil;
	level = 2;
	prl = tempor_1.prbox[ks + level * 60001 - 1];
	pil = tempor_1.pibox[ks + level * 60001 - 1];
	r3 = r2 * r1 - f2 * f1;
	f3 = r2 * f1 + f2 * r1;
	c2r = f3 * prl + r3 * pil;
	c2i = r3 * prl - f3 * pil;
	level = 3;
	prl = tempor_1.prbox[ks + level * 60001 - 1];
	pil = tempor_1.pibox[ks + level * 60001 - 1];
	r4 = r3 * r1 - f3 * f1;
	f4 = r3 * f1 + f3 * r1;
	c3r = f4 * prl + r4 * pil;
	c3i = r4 * prl - f4 * pil;
	level = 4;
	prl = tempor_1.prbox[ks + level * 60001 - 1];
	pil = tempor_1.pibox[ks + level * 60001 - 1];
	r5 = r4 * r1 - f4 * f1;
	f5 = r4 * f1 + f4 * r1;
	c4r = f5 * prl + r5 * pil;
	c4i = r5 * prl - f5 * pil;
	level = 5;
	prl = tempor_1.prbox[ks + level * 60001 - 1];
	pil = tempor_1.pibox[ks + level * 60001 - 1];
	r6 = r5 * r1 - f5 * f1;
	f6 = r5 * f1 + f5 * r1;
	c5r = f6 * prl + r6 * pil;
	c5i = r6 * prl - f6 * pil;
	level = 6;
	prl = tempor_1.prbox[ks + level * 60001 - 1];
	pil = tempor_1.pibox[ks + level * 60001 - 1];
	r7 = r6 * r1 - f6 * f1;
	f7 = r6 * f1 + f6 * r1;
	c6r = f7 * prl + r7 * pil;
	c6i = r7 * prl - f7 * pil;
	level = 7;
	prl = tempor_1.prbox[ks + level * 60001 - 1];
	pil = tempor_1.pibox[ks + level * 60001 - 1];
	r8 = r7 * r1 - f7 * f1;
	f8 = r7 * f1 + f7 * r1;
	c7r = f8 * prl + r8 * pil;
	c7i = r8 * prl - f8 * pil;
/*   Sum all the terms in the series */
	velr = c0r + c1r + c2r + c3r + c4r + c5r + c6r + c7r;
	veli = c0i + c1i + c2i + c3i + c4i + c5i + c6i + c7i;
/*  Calculate the velocity induced by the group "k" on particle "i" */
	*ubox -= velr;
	*vbox += veli;
/* L2: */
    }
    return 0;
} /* int_part_box__ */

