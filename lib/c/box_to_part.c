/* ../box_to_part.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int box_to_part__(integer *nmax, integer *kchildless, 
	integer *ichildless, real *xc, real *yc, integer *npb, real *br, real 
	*bi)
{
    /* System generated locals */
    integer npb_dim1, npb_offset, br_dim1, br_offset, bi_dim1, bi_offset, 
	    i__1, i__2;

    /* Builtin functions */
    double atan(doublereal);

    /* Local variables */
    static real dyopiinv, f;
    static integer k, n;
    static real p, f1, f2, f3, f4, f5, f6;
    static integer n1, n2;
    static real r1, r2, r3, r4, r5, r6, ci;
    static integer id;
    static real cr, xb, yb, xx, yy, c1i, c2i, c3i, c4i, c5i, c6i, c7i, c1r, 
	    c2r, c3r, c4r, c5r, c6r, c7r, bib[7], brb[7];

/*     This subroutine calculates the velocities induced by a box */
/*     on its own particles from the box's interactions. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ----------------------------------------------------------- */
    /* Parameter adjustments */
    bi_dim1 = *nmax;
    bi_offset = 1 + bi_dim1;
    bi -= bi_offset;
    br_dim1 = *nmax;
    br_offset = 1 + br_dim1;
    br -= br_offset;
    npb_dim1 = *nmax;
    npb_offset = 1 + npb_dim1;
    npb -= npb_offset;
    --yc;
    --xc;
    --ichildless;

    /* Function Body */
    dyopiinv = 1.f / (atan(1.f) * 8.f);
    i__1 = *kchildless;
    for (k = 1; k <= i__1; ++k) {
/* all childless boxes on level */
	id = ichildless[k];
	xb = xc[id];
	yb = yc[id];
	n1 = npb[id + npb_dim1];
	n2 = npb[id + (npb_dim1 << 1)];
	brb[0] = br[id + br_dim1];
	bib[0] = bi[id + bi_dim1];
	brb[1] = br[id + (br_dim1 << 1)];
	bib[1] = bi[id + (bi_dim1 << 1)];
	brb[2] = br[id + br_dim1 * 3];
	bib[2] = bi[id + bi_dim1 * 3];
	brb[3] = br[id + (br_dim1 << 2)];
	bib[3] = bi[id + (bi_dim1 << 2)];
	brb[4] = br[id + br_dim1 * 5];
	bib[4] = bi[id + bi_dim1 * 5];
	brb[5] = br[id + br_dim1 * 6];
	bib[5] = bi[id + bi_dim1 * 6];
	brb[6] = br[id + br_dim1 * 7];
	bib[6] = bi[id + bi_dim1 * 7];
	i__2 = n2;
	for (n = n1; n <= i__2; ++n) {
/* all particles in each box */
	    xx = vort2_1.xn[n - 1] - xb;
	    yy = yb - vort2_1.yn[n - 1];
/* Use multipole expansions to compute the forces on the box */
	    p = brb[0];
	    f = bib[0];
	    c1r = p;
	    c1i = f;
/*             level = 2 */
	    r1 = xx;
	    f1 = yy;
	    p = brb[1];
	    f = bib[1];
	    c2r = r1 * p - f1 * f;
	    c2i = r1 * f + f1 * p;
/*             level = 3 */
	    r2 = r1 * r1 - f1 * f1;
	    f2 = r1 * f1 + f1 * r1;
	    p = brb[2];
	    f = bib[2];
	    c3r = r2 * p - f2 * f;
	    c3i = r2 * f + f2 * p;
/*             level = 4 */
	    r3 = r2 * r1 - f2 * f1;
	    f3 = r2 * f1 + f2 * r1;
	    p = brb[3];
	    f = bib[3];
	    c4r = r3 * p - f3 * f;
	    c4i = r3 * f + f3 * p;
/*             level = 5 */
	    r4 = r3 * r1 - f3 * f1;
	    f4 = r3 * f1 + f3 * r1;
	    p = brb[4];
	    f = bib[4];
	    c5r = r4 * p - f4 * f;
	    c5i = r4 * f + f4 * p;
/*             level = 6 */
	    r5 = r4 * r1 - f4 * f1;
	    f5 = r4 * f1 + f4 * r1;
	    p = brb[5];
	    f = bib[5];
	    c6r = r5 * p - f5 * f;
	    c6i = r5 * f + f5 * p;
/*             level = 7 */
	    r6 = r5 * r1 - f5 * f1;
	    f6 = r5 * f1 + f5 * r1;
	    p = brb[6];
	    f = bib[6];
	    c7r = r6 * p - f6 * f;
	    c7i = r6 * f + f6 * p;
/*   Sum all the terms in the series */
	    cr = c1r + c2r + c3r + c4r + c5r + c6r + c7r;
	    ci = c1i + c2i + c3i + c4i + c5i + c6i + c7i;
/*  Calculate the velocity induced by the group "k" on particle "i" */
	    vel_1.uu[n - 1] -= ci * dyopiinv;
	    vel_1.vv[n - 1] += cr * dyopiinv;
/* L2: */
	}
/* L90: */
    }
    return 0;
} /* box_to_part__ */

