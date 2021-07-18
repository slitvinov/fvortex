/* ../int_box_part.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int int_box_part__(integer *nmax, integer *kb, real *xb, 
	real *yb, integer *k4, real *br, real *bi)
{
    /* System generated locals */
    integer br_dim1, br_offset, bi_dim1, bi_offset, i__1;

    /* Local variables */
    static real f, g, p, s0, t0, s1, t1, s2, t2, s3, t3, s4, t4, s5, t5, s6, 
	    t6;
    static integer nb;
    static real xx, yy, bi1, bi2, bi3, bi4, bi5, bi6, bi7, br1, br2, br3, br4,
	     br5, br6, br7, r2inv;

/*     This subroutine computes the influence of the particles of childless */
/*     boxes on far boxes of a lower level, as these are particle-box interactions. */
/* ----------------------------------------------------------- */
    /* Parameter adjustments */
    bi_dim1 = *nmax;
    bi_offset = 1 + bi_dim1;
    bi -= bi_offset;
    br_dim1 = *nmax;
    br_offset = 1 + br_dim1;
    br -= br_offset;

    /* Function Body */
    br1 = 0.f;
    bi1 = 0.f;
    br2 = 0.f;
    bi2 = 0.f;
    br3 = 0.f;
    bi3 = 0.f;
    br4 = 0.f;
    bi4 = 0.f;
    br5 = 0.f;
    bi5 = 0.f;
    br6 = 0.f;
    bi6 = 0.f;
    br7 = 0.f;
    bi7 = 0.f;
    i__1 = *k4;
    for (nb = 1; nb <= i__1; ++nb) {
	g = tempor_1.gt[nb - 1];
	xx = *xb - tempor_1.xt[nb - 1];
	yy = *yb - tempor_1.yt[nb - 1];
	r2inv = 1.f / (xx * xx + yy * yy);
	p = xx * r2inv;
	f = yy * r2inv;
/* level = 1 */
	s0 = p * g;
	t0 = f * g;
/* level = 2 */
	s1 = s0 * p - t0 * f;
	t1 = s0 * f + t0 * p;
/* level = 3 */
	s2 = s1 * p - t1 * f;
	t2 = s1 * f + t1 * p;
/* level = 4 */
	s3 = s2 * p - t2 * f;
	t3 = s2 * f + t2 * p;
/* level = 5 */
	s4 = s3 * p - t3 * f;
	t4 = s3 * f + t3 * p;
/* level = 6 */
	s5 = s4 * p - t4 * f;
	t5 = s4 * f + t4 * p;
/* level = 7 */
	s6 = s5 * p - t5 * f;
	t6 = s5 * f + t5 * p;
	br1 += s0;
	bi1 += t0;
	br2 -= s1;
	bi2 -= t1;
	br3 += s2;
	bi3 += t2;
	br4 -= s3;
	bi4 -= t3;
	br5 += s4;
	bi5 += t4;
	br6 -= s5;
	bi6 -= t5;
	br7 += s6;
	bi7 += t6;
/* L1: */
    }
    br[*kb + br_dim1] += br1;
    bi[*kb + bi_dim1] += bi1;
    br[*kb + (br_dim1 << 1)] += br2;
    bi[*kb + (bi_dim1 << 1)] += bi2;
    br[*kb + br_dim1 * 3] += br3;
    bi[*kb + bi_dim1 * 3] += bi3;
    br[*kb + (br_dim1 << 2)] += br4;
    bi[*kb + (bi_dim1 << 2)] += bi4;
    br[*kb + br_dim1 * 5] += br5;
    bi[*kb + bi_dim1 * 5] += bi5;
    br[*kb + br_dim1 * 6] += br6;
    bi[*kb + bi_dim1 * 6] += bi6;
    br[*kb + br_dim1 * 7] += br7;
    bi[*kb + bi_dim1 * 7] += bi7;
    return 0;
} /* int_box_part__ */

