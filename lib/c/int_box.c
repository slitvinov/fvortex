/* ../int_box.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int int_box__(integer *nmax, integer *id, real *xtest, real *
	ytest, integer *kbb, real *br, real *bi)
{
    /* System generated locals */
    integer br_dim1, br_offset, bi_dim1, bi_offset, i__1;

    /* Local variables */
    static real a, b, s1, t1, s2, t2, s3, t3, s4, t4, s5, t5, s6, t6, s7, t7;
    static integer ni;
    static real xx, yy, bi0, bi1, bi2, bi3, bi4, bi5, bi6, bi7, ci0, br0, br1,
	     br2, br3, br4, br5, br6, br7, cr0, cr1, ci1, cr2, ci2, cr3, ci3, 
	    cr4, ci4, cr5, ci5, cr6, ci6, cr7, ci7, pil, prl, r2inv;
    static integer level;

/*     This subroutine computes BOX-BOX interactions. */
/* ----------------------------------------------------------- */
    /* Parameter adjustments */
    bi_dim1 = *nmax;
    bi_offset = 1 + bi_dim1;
    bi -= bi_offset;
    br_dim1 = *nmax;
    br_offset = 1 + br_dim1;
    br -= br_offset;

    /* Function Body */
    br0 = 0.f;
    bi0 = 0.f;
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
    i__1 = *kbb;
    for (ni = 1; ni <= i__1; ++ni) {
	level = 0;
	xx = *xtest - tempor_1.xbox[ni - 1];
	yy = *ytest - tempor_1.ybox[ni - 1];
	r2inv = 1.f / (xx * xx + yy * yy);
	s1 = xx * r2inv;
	t1 = yy * r2inv;
	prl = tempor_1.prbox[ni - 1];
	pil = tempor_1.pibox[ni - 1];
	cr0 = prl;
	ci0 = pil;
	level = 1;
	prl = tempor_1.prbox[ni + level * 60001 - 1];
	pil = tempor_1.pibox[ni + level * 60001 - 1];
	cr1 = s1 * prl - t1 * pil;
	ci1 = s1 * pil + t1 * prl;
	level = 2;
	prl = tempor_1.prbox[ni + level * 60001 - 1];
	pil = tempor_1.pibox[ni + level * 60001 - 1];
	s2 = s1 * s1 - t1 * t1;
	t2 = s1 * t1 + t1 * s1;
	cr2 = s2 * prl - t2 * pil;
	ci2 = s2 * pil + t2 * prl;
	level = 3;
	prl = tempor_1.prbox[ni + level * 60001 - 1];
	pil = tempor_1.pibox[ni + level * 60001 - 1];
	s3 = s2 * s1 - t2 * t1;
	t3 = s2 * t1 + t2 * s1;
	cr3 = s3 * prl - t3 * pil;
	ci3 = s3 * pil + t3 * prl;
	level = 4;
	prl = tempor_1.prbox[ni + level * 60001 - 1];
	pil = tempor_1.pibox[ni + level * 60001 - 1];
	s4 = s3 * s1 - t3 * t1;
	t4 = s3 * t1 + t3 * s1;
	cr4 = s4 * prl - t4 * pil;
	ci4 = s4 * pil + t4 * prl;
	level = 5;
	prl = tempor_1.prbox[ni + level * 60001 - 1];
	pil = tempor_1.pibox[ni + level * 60001 - 1];
	s5 = s4 * s1 - t4 * t1;
	t5 = s4 * t1 + t4 * s1;
	cr5 = s5 * prl - t5 * pil;
	ci5 = s5 * pil + t5 * prl;
	level = 6;
	prl = tempor_1.prbox[ni + level * 60001 - 1];
	pil = tempor_1.pibox[ni + level * 60001 - 1];
	s6 = s5 * s1 - t5 * t1;
	t6 = s5 * t1 + t5 * s1;
	cr6 = s6 * prl - t6 * pil;
	ci6 = s6 * pil + t6 * prl;
	level = 7;
	prl = tempor_1.prbox[ni + level * 60001 - 1];
	pil = tempor_1.pibox[ni + level * 60001 - 1];
	s7 = s6 * s1 - t6 * t1;
	t7 = s6 * t1 + t6 * s1;
	cr7 = s7 * prl - t7 * pil;
	ci7 = s7 * pil + t7 * prl;
	a = cr0 + cr1 + cr2 + cr3 + cr4 + cr5 + cr6 + cr7;
	b = ci0 + ci1 + ci2 + ci3 + ci4 + ci5 + ci6 + ci7;
	br1 = br1 + s1 * a - t1 * b;
	bi1 = bi1 + s1 * b + t1 * a;
	a = cr0 + cr1 * 2.f + cr2 * 3.f + cr3 * 4.f + cr4 * 5.f + cr5 * 6.f + 
		cr6 * 7.f + cr7 * 8.f;
	b = ci0 + ci1 * 2.f + ci2 * 3.f + ci3 * 4.f + ci4 * 5.f + ci5 * 6.f + 
		ci6 * 7.f + ci7 * 8.f;
	br2 = br2 + s2 * a - t2 * b;
	bi2 = bi2 + s2 * b + t2 * a;
	a = cr0 + cr1 * 3.f + cr2 * 6.f + cr3 * 10.f + cr4 * 15.f + cr5 * 
		21.f + cr6 * 28.f + cr7 * 36.f;
	b = ci0 + ci1 * 3.f + ci2 * 6.f + ci3 * 10.f + ci4 * 15.f + ci5 * 
		21.f + ci6 * 28.f + ci7 * 36.f;
	br3 = br3 + s3 * a - t3 * b;
	bi3 = bi3 + s3 * b + t3 * a;
	a = cr0 + cr1 * 4.f + cr2 * 10.f + cr3 * 20.f + cr4 * 35.f + cr5 * 
		56.f + cr6 * 84.f + cr7 * 120.f;
	b = ci0 + ci1 * 4.f + ci2 * 10.f + ci3 * 20.f + ci4 * 35.f + ci5 * 
		56.f + ci6 * 84.f + ci7 * 120.f;
	br4 = br4 + s4 * a - t4 * b;
	bi4 = bi4 + s4 * b + t4 * a;
	a = cr0 + cr1 * 5.f + cr2 * 15.f + cr3 * 35.f + cr4 * 70.f + cr5 * 
		126.f + cr6 * 210.f + cr7 * 330.f;
	b = ci0 + ci1 * 5.f + ci2 * 15.f + ci3 * 35.f + ci4 * 70.f + ci5 * 
		126.f + ci6 * 210.f + ci7 * 330.f;
	br5 = br5 + s5 * a - t5 * b;
	bi5 = bi5 + s5 * b + t5 * a;
	a = cr0 + cr1 * 6.f + cr2 * 21.f + cr3 * 56.f + cr4 * 126.f + cr5 * 
		252.f + cr6 * 462.f + cr7 * 792.f;
	b = ci0 + ci1 * 6.f + ci2 * 21.f + ci3 * 56.f + ci4 * 126.f + ci5 * 
		252.f + ci6 * 462.f + ci7 * 792.f;
	br6 = br6 + s6 * a - t6 * b;
	bi6 = bi6 + s6 * b + t6 * a;
	a = cr0 + cr1 * 7.f + cr2 * 28.f + cr3 * 84.f + cr4 * 210.f + cr5 * 
		462.f + cr6 * 924.f + cr7 * 1716.f;
	b = ci0 + ci1 * 7.f + ci2 * 28.f + ci3 * 84.f + ci4 * 210.f + ci5 * 
		462.f + ci6 * 924.f + ci7 * 1716.f;
	br7 = br7 + s7 * a - t7 * b;
	bi7 = bi7 + s7 * b + t7 * a;
/* L2: */
    }
    br[*id + br_dim1] += br1;
    bi[*id + bi_dim1] += bi1;
    br[*id + (br_dim1 << 1)] -= br2;
    bi[*id + (bi_dim1 << 1)] -= bi2;
    br[*id + br_dim1 * 3] += br3;
    bi[*id + bi_dim1 * 3] += bi3;
    br[*id + (br_dim1 << 2)] -= br4;
    bi[*id + (bi_dim1 << 2)] -= bi4;
    br[*id + br_dim1 * 5] += br5;
    bi[*id + bi_dim1 * 5] += bi5;
    br[*id + br_dim1 * 6] -= br6;
    bi[*id + bi_dim1 * 6] -= bi6;
    br[*id + br_dim1 * 7] += br7;
    bi[*id + bi_dim1 * 7] += bi7;
    return 0;
} /* int_box__ */

