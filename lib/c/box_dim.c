/* ../box_dim.f -- translated by f2c (version 20160102).
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
    real xp[5000000], yp[5000000];
} vort1_;

#define vort1_1 vort1_

/* Subroutine */ int box_dim__(integer *npart, real *xmin, real *xmax, real *
	ymin, real *ymax)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    static real x, y;

/*     This subroutine finds the bounding x and y coordinates in the domain. */
/* ------------------------------------------------------------------ */
    *xmin = vort1_1.xp[0];
    *xmax = vort1_1.xp[0];
    *ymin = vort1_1.yp[0];
    *ymax = vort1_1.yp[0];
    i__1 = *npart;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x = vort1_1.xp[i__ - 1];
	y = vort1_1.yp[i__ - 1];
	*xmin = dmin(*xmin,x);
	*xmax = dmax(*xmax,x);
	*ymin = dmin(*ymin,y);
	*ymax = dmax(*ymax,y);
/* L10: */
    }
    return 0;
} /* box_dim__ */

