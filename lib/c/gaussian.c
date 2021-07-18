/* ../gaussian.f -- translated by f2c (version 20160102).
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
    real gdelta, gauss[15001];
} gauss_table__;

#define gauss_table__1 gauss_table__

/* Subroutine */ int gaussian_(void)
{
    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static integer i__;
    static real x;

/*     Computes a table of values for the gaussian, which is used in the */
/*     formulas for convection and diffusion between particles. Table just */
/*     has the Guassian for argument -r**2/s2, starting at 0 and going out */
/*     to whatever cutoff value we choose for particle interactions. */
/* ------------------------------------------------------------------ */
    gauss_table__1.gdelta = .0010666666666666667f;
    for (i__ = 1; i__ <= 15001; ++i__) {
	x = (i__ - 1) * gauss_table__1.gdelta;
	gauss_table__1.gauss[i__ - 1] = exp(-x);
/* L1: */
    }
    return 0;
} /* gaussian_ */

