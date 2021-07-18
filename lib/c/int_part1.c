/* ../int_part1.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int int_part1__(real *xtest, real *ytest, real *gtest, real *
	upart, real *vpart, real *gpart, integer *kpart)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static real c__;
    static integer i__, m;
    static real r2, fc, xx, yy, arg, s2inv2;

/*     This set of subroutines performs the various types of interactions */
/*     between particles and boxes that are called for in the tree routine. */
/*     --------------------------------------------------------------------- */
/*     This  subroutine calculates the velocities induced on the */
/*     particle located at *xtest*,*ytest*, by the particles in its */
/*     interaction list *XT,YT,GT*. */
/* ------------------------------------------------------------------------------ */
    s2inv2 = .5f / part_1.s2;
    *upart = 0.f;
    *vpart = 0.f;
    *gpart = 0.f;
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
	fc = tempor_1.gt[m - 1] * (1.f - c__) / (r2 + 1e-9f);
	*upart += yy * fc;
	*vpart += xx * fc;
/* * rad1 = xtest*xtest + ytest*ytest */
/* * if(rad1.lt.visc_cutoff)then */
	*gpart += (tempor_1.gt[m - 1] - *gtest) * c__;
/* *         endif */
/* L4: */
    }
    return 0;
} /* int_part1__ */

