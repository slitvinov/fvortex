/* ../exp_chdless.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int exp_chdless__(integer *nmax, integer *kchildless, real *
	xc, real *yc, integer *ichildless, integer *npb, real *pr, real *pi)
{
    /* System generated locals */
    integer npb_dim1, npb_offset, pr_dim1, pr_offset, pi_dim1, pi_offset, 
	    i__1, i__2;

    /* Local variables */
    static integer j, k;
    static real x, y;
    static integer n1, n2, nb;
    static real xm, ym, fa1, fa2, fa3, fa4, fa5, fa6, fa7, re0, re1, re2, re3,
	     re4, re5, ti0, ti1, ti2, ti3, ti4, ti5, ti6, ti7, re6, tr0, tr1, 
	    tr2, tr3, tr4, tr5, tr6, tr7, re7;

/*     This subroutine computes the MULTIPOLE EXPANSIONS of */
/*     the childless boxes at a level. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ----------------------------------------------------------------- */
    /* Parameter adjustments */
    pi_dim1 = *nmax;
    pi_offset = 1 + pi_dim1 * 0;
    pi -= pi_offset;
    pr_dim1 = *nmax;
    pr_offset = 1 + pr_dim1 * 0;
    pr -= pr_offset;
    npb_dim1 = *nmax;
    npb_offset = 1 + npb_dim1;
    npb -= npb_offset;
    --ichildless;
    --yc;
    --xc;

    /* Function Body */
    i__1 = *kchildless;
    for (k = 1; k <= i__1; ++k) {
	nb = ichildless[k];
	n1 = npb[nb + npb_dim1];
	n2 = npb[nb + (npb_dim1 << 1)];
	xm = xc[nb];
	ym = yc[nb];
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
	pr[nb] = tr0;
	pi[nb] = ti0;
	pr[nb + pr_dim1] = tr1;
	pi[nb + pi_dim1] = ti1;
	pr[nb + (pr_dim1 << 1)] = tr2;
	pi[nb + (pi_dim1 << 1)] = ti2;
	pr[nb + pr_dim1 * 3] = tr3;
	pi[nb + pi_dim1 * 3] = ti3;
	pr[nb + (pr_dim1 << 2)] = tr4;
	pi[nb + (pi_dim1 << 2)] = ti4;
	pr[nb + pr_dim1 * 5] = tr5;
	pi[nb + pi_dim1 * 5] = ti5;
	pr[nb + pr_dim1 * 6] = tr6;
	pi[nb + pi_dim1 * 6] = ti6;
	pr[nb + pr_dim1 * 7] = tr7;
	pi[nb + pi_dim1 * 7] = ti7;
/* L10: */
    }
    return 0;
} /* exp_chdless__ */

