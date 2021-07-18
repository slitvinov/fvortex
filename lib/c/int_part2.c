/* ../int_part2.f -- translated by f2c (version 20160102).
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
    integer np;
    real s2, ovrlp, gnu;
} part_;

#define part_1 part_

struct {
    real gdelta, gauss[15001];
} gauss_table__;

#define gauss_table__1 gauss_table__

struct {
    real visc_cutoff__;
} cutoff_;

#define cutoff_1 cutoff_

/* Subroutine */ int int_part2__(real *gtest, real *xtest, real *ytest, real *
	upart, real *vpart, real *gpart, integer *kpart)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double atan(doublereal);

    /* Local variables */
    static real dyopiinv, a, c__;
    static integer i__, m, n;
    static real r2, gg, fm, fn, xx, yy, arg, svl, s2inv2;

/*     This  subroutine calculates the velocities induced on the */
/*     particle located at *xtest*,*ytest*, by the particles in its */
/*     interaction list *XT,YT,GT*; it also does the reverse influence of */
/*     xtest,ytest on the particles XT,YT. Thus it is only used when xtest, */
/*     ytest is in a higher level box than XT,YT to keep things straight. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ------------------------------------------------------------------------ */
    dyopiinv = 1.f / (atan(1.f) * 8.f);
    s2inv2 = .5f / part_1.s2;
    *upart = 0.f;
    *vpart = 0.f;
    gg = *gtest * dyopiinv;
    *gpart = 0.f;
/*     FPP$PERMUTATION(IT) */
    i__1 = *kpart;
    for (m = 1; m <= i__1; ++m) {
	xx = *xtest - tempor_1.xt[m - 1];
	yy = tempor_1.yt[m - 1] - *ytest;
	r2 = xx * xx + yy * yy;
	arg = r2 * s2inv2;
	if (arg < 16.f) {
	    i__ = arg / gauss_table__1.gdelta + 1;
	    c__ = gauss_table__1.gauss[i__ - 1] * (i__ * 
		    gauss_table__1.gdelta + 1 - arg);
/* includes first error ter */
	} else {
	    c__ = 0.f;
	}
	svl = (1.f - c__) / r2;
	fm = tempor_1.gt[m - 1] * svl;
	*upart += yy * fm;
/* velocity on particle LP */
	*vpart += xx * fm;
/* Calculation is made symmetric here */
	n = tempor_1.it[m - 1];
	fn = gg * svl;
	vel_1.uu[n - 1] -= yy * fn;
	vel_1.vv[n - 1] -= xx * fn;
/* *          rad1 = xtest*xtest + ytest*ytest */
/* *          rad2 = xt(m)*xt(m) + yt(m)*yt(m) */
/* *          if((rad1.lt.visc_cutoff).and.(rad2.lt.visc_cutoff))then */
	a = (tempor_1.gt[m - 1] - *gtest) * c__;
	*gpart += a;
	diff_1.gdiff[n - 1] -= a;
/* *         endif */
/* Strength of n */
/* L4: */
    }
    return 0;
} /* int_part2__ */

