/* ../ch_to_par.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int ch_to_par__(integer *nmax1, real *dch, integer *kparent, 
	integer *iparent, integer *iparichj, real *pr, real *pi, real *pgr, 
	real *pgi, real *gr, real *gi)
{
    /* System generated locals */
    integer iparichj_dim1, iparichj_offset, pr_dim1, pr_offset, pi_dim1, 
	    pi_offset, pgr_dim1, pgr_offset, pgi_dim1, pgi_offset, gr_dim1, 
	    gr_offset, gi_dim1, gi_offset, i__1;

    /* Local variables */
    static integer k, m;
    static real f0, f1, f2, f3, f4, f5, f6, f7, p1, p2, p3, p4, p5, p6, p7, 
	    r0, r1, r2, r3, r4, r5, r6, r7;
    static integer kb, nb;
    static real p13, p32, p14, p15;
    static integer km;
    static real p26, p44, p38, p54, p68, p16, p78, p17, p220, p212, p420, 
	    p320, p340, p230, p460, p524, p370, p242, p656, p584, p4140;

/*     This subroutine computes the MULTIPOLE EXPANSIONS of */
/*     the PARENT boxes once the multipole expansions of their */
/*     children has been computed. */
/* ----------------------------------------------------------------------------- */
    /* Parameter adjustments */
    gi_dim1 = *nmax1 / 4;
    gi_offset = 1 + gi_dim1 * 0;
    gi -= gi_offset;
    gr_dim1 = *nmax1 / 4;
    gr_offset = 1 + gr_dim1 * 0;
    gr -= gr_offset;
    pgi_dim1 = *nmax1 / 4;
    pgi_offset = 1 + pgi_dim1 * 0;
    pgi -= pgi_offset;
    pgr_dim1 = *nmax1 / 4;
    pgr_offset = 1 + pgr_dim1 * 0;
    pgr -= pgr_offset;
    pi_dim1 = *nmax1;
    pi_offset = 1 + pi_dim1 * 0;
    pi -= pi_offset;
    pr_dim1 = *nmax1;
    pr_offset = 1 + pr_dim1 * 0;
    pr -= pr_offset;
    iparichj_dim1 = *nmax1 / 4;
    iparichj_offset = 1 + iparichj_dim1;
    iparichj -= iparichj_offset;
    --iparent;

    /* Function Body */
    p1 = *dch * .5f;
    p2 = p1 * p1;
    p3 = p1 * p2;
    p4 = p2 * p2;
    p5 = p2 * p3;
    p6 = p3 * p3;
    p7 = p3 * p4;
/*     3-order */
    p32 = p3 * 2.f;
    p26 = p2 * 6.f;
    p13 = p1 * 3.f;
/*     4-order */
    p44 = p4 * 4.f;
    p38 = p3 * 8.f;
    p212 = p2 * 12.f;
    p14 = p1 * 4.f;
/*     5-order */
    p54 = p5 * 4.f;
    p420 = p4 * 20.f;
    p320 = p3 * 20.f;
    p220 = p2 * 20.f;
    p15 = p1 * 5.f;
/*     6-order */
    p68 = p6 * 8.f;
    p524 = p5 * 24.f;
    p460 = p4 * 60.f;
    p340 = p3 * 40.f;
    p230 = p2 * 30.f;
    p16 = p1 * 6.f;
/*     7-order */
    p78 = p7 * 8.f;
    p656 = p6 * 56.f;
    p584 = p5 * 84.f;
    p4140 = p4 * 140.f;
    p370 = p3 * 70.f;
    p242 = p2 * 42.f;
    p17 = p1 * 7.f;
    i__1 = *kparent;
    for (nb = 1; nb <= i__1; ++nb) {
	gr[nb] = 0.f;
	gr[nb + gr_dim1] = 0.f;
	gr[nb + (gr_dim1 << 1)] = 0.f;
	gr[nb + gr_dim1 * 3] = 0.f;
	gr[nb + (gr_dim1 << 2)] = 0.f;
	gr[nb + gr_dim1 * 5] = 0.f;
	gr[nb + gr_dim1 * 6] = 0.f;
	gr[nb + gr_dim1 * 7] = 0.f;
	gi[nb] = 0.f;
	gi[nb + gi_dim1] = 0.f;
	gi[nb + (gi_dim1 << 1)] = 0.f;
	gi[nb + gi_dim1 * 3] = 0.f;
	gi[nb + (gi_dim1 << 2)] = 0.f;
	gi[nb + gi_dim1 * 5] = 0.f;
	gi[nb + gi_dim1 * 6] = 0.f;
	gi[nb + gi_dim1 * 7] = 0.f;
/*     Contribution of 1st Child (if any) */
	km = iparent[nb];
	m = iparichj[km + iparichj_dim1];
/* index of child (if any) box 1 at level */
	if (m == 0) {
	    goto L2;
	}
/*     REAL VALUES */
	r0 = pr[m];
	r1 = pr[m + pr_dim1];
	r2 = pr[m + (pr_dim1 << 1)];
	r3 = pr[m + pr_dim1 * 3];
	r4 = pr[m + (pr_dim1 << 2)];
	r5 = pr[m + pr_dim1 * 5];
	r6 = pr[m + pr_dim1 * 6];
	r7 = pr[m + pr_dim1 * 7];
/*     IMAGINARY VALUES */
	f0 = pi[m];
	f1 = pi[m + pi_dim1];
	f2 = pi[m + (pi_dim1 << 1)];
	f3 = pi[m + pi_dim1 * 3];
	f4 = pi[m + (pi_dim1 << 2)];
	f5 = pi[m + pi_dim1 * 5];
	f6 = pi[m + pi_dim1 * 6];
	f7 = pi[m + pi_dim1 * 7];
/*     REAL and IMAGINARY PART OF EXPANSIONS */
/*     0 - Order */
	gr[nb] = r0;
	gi[nb] = 0.f;
/*     1 - Order */
	gr[nb + gr_dim1] = -p1 * r0 + r1;
	gi[nb + gi_dim1] = p1 * r0 + f1;
/*     2 - Order */
	gr[nb + (gr_dim1 << 1)] = p1 * -2.f * (r1 + f1) + r2;
	gi[nb + (gi_dim1 << 1)] = (-p2 * r0 + p1 * (r1 - f1)) * 2.f + f2;
/*     3 - Order */
	gr[nb + gr_dim1 * 3] = p32 * r0 + p26 * f1 - p13 * (r2 + f2) + r3;
	gi[nb + gi_dim1 * 3] = p32 * r0 - p26 * r1 + p13 * (r2 - f2) + f3;
/*     4 - Order */
	gr[nb + (gr_dim1 << 2)] = -p44 * r0 + p38 * (r1 - f1) + p212 * f2 - 
		p14 * (r3 + f3) + r4;
	gi[nb + (gi_dim1 << 2)] = p38 * (r1 + f1) - p212 * r2 + p14 * (r3 - 
		f3) + f4;
/*     5 - Order */
	gr[nb + gr_dim1 * 5] = p54 * r0 - p420 * r1 + p320 * (r2 - f2) + p220 
		* f3 - p15 * (r4 + f4) + r5;
	gi[nb + gi_dim1 * 5] = -p54 * r0 - p420 * f1 + p320 * (r2 + f2) - 
		p220 * r3 + p15 * (r4 - f4) + f5;
/*     6 - Order */
	gr[nb + gr_dim1 * 6] = p524 * (r1 + f1) - p460 * r2 + p340 * (r3 - f3)
		 + p230 * f4 - p16 * (r5 + f5) + r6;
	gi[nb + gi_dim1 * 6] = p68 * r0 + p524 * (f1 - r1) - p460 * f2 + p3 * 
		40 * (r3 + f3) - p230 * r4 + p16 * (r5 - f5) + f6;
/*     7 - Order */
	gr[nb + gr_dim1 * 7] = -p78 * r0 - p656 * f1 + p584 * (r2 + f2) - 
		p4140 * r3 + p370 * (r4 - f4) + p242 * f5 - p17 * (r6 + f6) + 
		r7;
	gi[nb + gi_dim1 * 7] = -p78 * r0 + p656 * r1 + p584 * (f2 - r2) - 
		p4140 * f3 + p370 * (r4 + f4) - p242 * r5 + p17 * (r6 - f6) + 
		f7;
/*     -  Contribution of 2nd Child (if any) */
L2:
	m = iparichj[km + (iparichj_dim1 << 1)];
/* index of child (if any) box 2 at level */
	if (m == 0) {
	    goto L3;
	}
/*     REAL VALUES */
	r0 = pr[m];
	r1 = pr[m + pr_dim1];
	r2 = pr[m + (pr_dim1 << 1)];
	r3 = pr[m + pr_dim1 * 3];
	r4 = pr[m + (pr_dim1 << 2)];
	r5 = pr[m + pr_dim1 * 5];
	r6 = pr[m + pr_dim1 * 6];
	r7 = pr[m + pr_dim1 * 7];
/*     IMAGINARY VALUES */
	f0 = pi[m];
	f1 = pi[m + pi_dim1];
	f2 = pi[m + (pi_dim1 << 1)];
	f3 = pi[m + pi_dim1 * 3];
	f4 = pi[m + (pi_dim1 << 2)];
	f5 = pi[m + pi_dim1 * 5];
	f6 = pi[m + pi_dim1 * 6];
	f7 = pi[m + pi_dim1 * 7];
/*     REAL and IMAGINARY PARTS OF EXPANSIONS */
/*     0 - Order */
	gr[nb] += r0;
	gi[nb] = 0.f;
/*     1 - Order */
	gr[nb + gr_dim1] = gr[nb + gr_dim1] - p1 * r0 + r1;
	gi[nb + gi_dim1] = gi[nb + gi_dim1] - p1 * r0 + f1;
/*     2 - Order */
	gr[nb + (gr_dim1 << 1)] = gr[nb + (gr_dim1 << 1)] + p1 * 2.f * (f1 - 
		r1) + r2;
	gi[nb + (gi_dim1 << 1)] = gi[nb + (gi_dim1 << 1)] + (p2 * r0 - p1 * (
		r1 + f1)) * 2.f + f2;
/*     3 - Order */
	gr[nb + gr_dim1 * 3] = gr[nb + gr_dim1 * 3] + p32 * r0 - p26 * f1 + 
		p13 * (f2 - r2) + r3;
	gi[nb + gi_dim1 * 3] = gi[nb + gi_dim1 * 3] - p32 * r0 + p26 * r1 - 
		p13 * (f2 + r2) + f3;
/*     4 - Order */
	gr[nb + (gr_dim1 << 2)] = gr[nb + (gr_dim1 << 2)] - p44 * r0 + p38 * (
		r1 + f1) - p212 * f2 + p14 * (f3 - r3) + r4;
	gi[nb + (gi_dim1 << 2)] = gi[nb + (gi_dim1 << 2)] + p38 * (f1 - r1) + 
		p212 * r2 - p14 * (r3 + f3) + f4;
/*     5 - Order */
	gr[nb + gr_dim1 * 5] = gr[nb + gr_dim1 * 5] + p54 * r0 - p420 * r1 + 
		p320 * (r2 + f2) - p220 * f3 + p15 * (f4 - r4) + r5;
	gi[nb + gi_dim1 * 5] = gi[nb + gi_dim1 * 5] + p54 * r0 - p420 * f1 + 
		p320 * (f2 - r2) + p220 * r3 - p15 * (r4 + f4) + f5;
/*     6 - Order */
	gr[nb + gr_dim1 * 6] = gr[nb + gr_dim1 * 6] + p524 * (r1 - f1) - p460 
		* r2 + p340 * (r3 + f3) - p230 * f4 + p16 * (f5 - r5) + r6;
	gi[nb + gi_dim1 * 6] = gi[nb + gi_dim1 * 6] - p68 * r0 + p524 * (f1 + 
		r1) - p460 * f2 + p3 * 40 * (f3 - r3) + p230 * r4 - p16 * (r5 
		+ f5) + f6;
/*     7 - Order */
	gr[nb + gr_dim1 * 7] = gr[nb + gr_dim1 * 7] - p78 * r0 + p656 * f1 + 
		p584 * (r2 - f2) - p4140 * r3 + p370 * (r4 + f4) - p242 * f5 
		+ p17 * (f6 - r6) + r7;
	gi[nb + gi_dim1 * 7] = gi[nb + gi_dim1 * 7] + p78 * r0 - p656 * r1 + 
		p584 * (f2 + r2) - p4140 * f3 + p370 * (f4 - r4) + p242 * r5 
		- p17 * (r6 + f6) + f7;
/*     -  Contribution of 3rd Child (if any) */
L3:
	m = iparichj[km + iparichj_dim1 * 3];
/* index of child (if any) box 3 at level */
	if (m == 0) {
	    goto L4;
	}
/*     REAL VALUES */
	r0 = pr[m];
	r1 = pr[m + pr_dim1];
	r2 = pr[m + (pr_dim1 << 1)];
	r3 = pr[m + pr_dim1 * 3];
	r4 = pr[m + (pr_dim1 << 2)];
	r5 = pr[m + pr_dim1 * 5];
	r6 = pr[m + pr_dim1 * 6];
	r7 = pr[m + pr_dim1 * 7];
/*     IMAGINARY VALUES */
	f0 = pi[m];
	f1 = pi[m + pi_dim1];
	f2 = pi[m + (pi_dim1 << 1)];
	f3 = pi[m + pi_dim1 * 3];
	f4 = pi[m + (pi_dim1 << 2)];
	f5 = pi[m + pi_dim1 * 5];
	f6 = pi[m + pi_dim1 * 6];
	f7 = pi[m + pi_dim1 * 7];
/*     REAL and IMAGINARY PARTS OF EXPANSIONS */
/*     0 - Order */
	gr[nb] += r0;
	gi[nb] = 0.f;
/*     1 - Order */
	gr[nb + gr_dim1] = gr[nb + gr_dim1] + p1 * r0 + r1;
	gi[nb + gi_dim1] = gi[nb + gi_dim1] + p1 * r0 + f1;
/*     2 - Order */
	gr[nb + (gr_dim1 << 1)] = gr[nb + (gr_dim1 << 1)] - p1 * 2.f * (f1 - 
		r1) + r2;
	gi[nb + (gi_dim1 << 1)] = gi[nb + (gi_dim1 << 1)] + (p2 * r0 + p1 * (
		r1 + f1)) * 2.f + f2;
/*     3 - Order */
	gr[nb + gr_dim1 * 3] = gr[nb + gr_dim1 * 3] - p32 * r0 - p26 * f1 - 
		p13 * (f2 - r2) + r3;
	gi[nb + gi_dim1 * 3] = gi[nb + gi_dim1 * 3] + p32 * r0 + p26 * r1 + 
		p13 * (f2 + r2) + f3;
/*     4 - Order */
	gr[nb + (gr_dim1 << 2)] = gr[nb + (gr_dim1 << 2)] - p44 * r0 - p38 * (
		r1 + f1) - p212 * f2 - p14 * (f3 - r3) + r4;
	gi[nb + (gi_dim1 << 2)] = gi[nb + (gi_dim1 << 2)] - p38 * (f1 - r1) + 
		p212 * r2 + p14 * (r3 + f3) + f4;
/*     5 - Order */
	gr[nb + gr_dim1 * 5] = gr[nb + gr_dim1 * 5] - p54 * r0 - p420 * r1 - 
		p320 * (r2 + f2) - p220 * f3 - p15 * (f4 - r4) + r5;
	gi[nb + gi_dim1 * 5] = gi[nb + gi_dim1 * 5] - p54 * r0 - p420 * f1 - 
		p320 * (f2 - r2) + p220 * r3 + p15 * (r4 + f4) + f5;
/*     6 - Order */
	gr[nb + gr_dim1 * 6] = gr[nb + gr_dim1 * 6] - p524 * (r1 - f1) - p460 
		* r2 - p340 * (r3 + f3) - p230 * f4 - p16 * (f5 - r5) + r6;
	gi[nb + gi_dim1 * 6] = gi[nb + gi_dim1 * 6] - p68 * r0 - p524 * (f1 + 
		r1) - p460 * f2 - p3 * 40 * (f3 - r3) + p230 * r4 + p16 * (r5 
		+ f5) + f6;
/*     7 - Order */
	gr[nb + gr_dim1 * 7] = gr[nb + gr_dim1 * 7] + p78 * r0 + p656 * f1 - 
		p584 * (r2 - f2) - p4140 * r3 - p370 * (r4 + f4) - p242 * f5 
		- p17 * (f6 - r6) + r7;
	gi[nb + gi_dim1 * 7] = gi[nb + gi_dim1 * 7] - p78 * r0 - p656 * r1 - 
		p584 * (f2 + r2) - p4140 * f3 - p370 * (f4 - r4) + p242 * r5 
		+ p17 * (r6 + f6) + f7;
/*     -   Contribution of 4th Child (if any) */
L4:
	m = iparichj[km + (iparichj_dim1 << 2)];
/* index of child (if any) box 4 at level */
	if (m == 0) {
	    goto L20;
	}
/*     REAL VALUES */
/* Non-Empty box. */
	r0 = pr[m];
	r1 = pr[m + pr_dim1];
	r2 = pr[m + (pr_dim1 << 1)];
	r3 = pr[m + pr_dim1 * 3];
	r4 = pr[m + (pr_dim1 << 2)];
	r5 = pr[m + pr_dim1 * 5];
	r6 = pr[m + pr_dim1 * 6];
	r7 = pr[m + pr_dim1 * 7];
/*     IMAGINARY VALUES */
	f0 = pi[m];
	f1 = pi[m + pi_dim1];
	f2 = pi[m + (pi_dim1 << 1)];
	f3 = pi[m + pi_dim1 * 3];
	f4 = pi[m + (pi_dim1 << 2)];
	f5 = pi[m + pi_dim1 * 5];
	f6 = pi[m + pi_dim1 * 6];
	f7 = pi[m + pi_dim1 * 7];
/*     REAL and IMAGINARY PART OF EXPANSIONS */
/*     0 - Order */
	gr[nb] += r0;
	gi[nb] = 0.f;
/*     1 - Order */
	gr[nb + gr_dim1] = gr[nb + gr_dim1] + p1 * r0 + r1;
	gi[nb + gi_dim1] = gi[nb + gi_dim1] - p1 * r0 + f1;
/*     2 - Order */
	gr[nb + (gr_dim1 << 1)] = gr[nb + (gr_dim1 << 1)] + p1 * 2.f * (r1 + 
		f1) + r2;
	gi[nb + (gi_dim1 << 1)] = gi[nb + (gi_dim1 << 1)] + (-p2 * r0 - p1 * (
		r1 - f1)) * 2.f + f2;
/*     3 - Order */
	gr[nb + gr_dim1 * 3] = gr[nb + gr_dim1 * 3] - p32 * r0 + p26 * f1 + 
		p13 * (r2 + f2) + r3;
	gi[nb + gi_dim1 * 3] = gi[nb + gi_dim1 * 3] - p32 * r0 - p26 * r1 - 
		p13 * (r2 - f2) + f3;
/*     4 - Order */
	gr[nb + (gr_dim1 << 2)] = gr[nb + (gr_dim1 << 2)] - p44 * r0 - p38 * (
		r1 - f1) + p212 * f2 + p14 * (r3 + f3) + r4;
	gi[nb + (gi_dim1 << 2)] = gi[nb + (gi_dim1 << 2)] - p38 * (r1 + f1) - 
		p212 * r2 - p14 * (r3 - f3) + f4;
/*     5 - Order */
	gr[nb + gr_dim1 * 5] = gr[nb + gr_dim1 * 5] - p54 * r0 - p420 * r1 - 
		p320 * (r2 - f2) + p220 * f3 + p15 * (r4 + f4) + r5;
	gi[nb + gi_dim1 * 5] = gi[nb + gi_dim1 * 5] + p54 * r0 - p420 * f1 - 
		p320 * (r2 + f2) - p220 * r3 - p15 * (r4 - f4) + f5;
/*     6 - Order */
	gr[nb + gr_dim1 * 6] = gr[nb + gr_dim1 * 6] - p524 * (r1 + f1) - p460 
		* r2 - p340 * (r3 - f3) + p230 * f4 + p16 * (r5 + f5) + r6;
	gi[nb + gi_dim1 * 6] = gi[nb + gi_dim1 * 6] + p68 * r0 - p524 * (f1 - 
		r1) - p460 * f2 - p3 * 40 * (r3 + f3) - p230 * r4 - p16 * (r5 
		- f5) + f6;
/*     7 - Order */
	gr[nb + gr_dim1 * 7] = gr[nb + gr_dim1 * 7] + p78 * r0 - p656 * f1 - 
		p584 * (r2 + f2) - p4140 * r3 - p370 * (r4 - f4) + p242 * f5 
		+ p17 * (r6 + f6) + r7;
	gi[nb + gi_dim1 * 7] = gi[nb + gi_dim1 * 7] + p78 * r0 + p656 * r1 - 
		p584 * (f2 - r2) - p4140 * f3 - p370 * (r4 + f4) - p242 * r5 
		- p17 * (r6 - f6) + f7;
L20:
	;
    }
    i__1 = *kparent;
    for (k = 1; k <= i__1; ++k) {
	kb = iparent[k];
	pgr[kb] = gr[k];
	pgr[kb + pgr_dim1] = gr[k + gr_dim1];
	pgr[kb + (pgr_dim1 << 1)] = gr[k + (gr_dim1 << 1)];
	pgr[kb + pgr_dim1 * 3] = gr[k + gr_dim1 * 3];
	pgr[kb + (pgr_dim1 << 2)] = gr[k + (gr_dim1 << 2)];
	pgr[kb + pgr_dim1 * 5] = gr[k + gr_dim1 * 5];
	pgr[kb + pgr_dim1 * 6] = gr[k + gr_dim1 * 6];
	pgr[kb + pgr_dim1 * 7] = gr[k + gr_dim1 * 7];
	pgi[kb] = gi[k];
	pgi[kb + pgi_dim1] = gi[k + gi_dim1];
	pgi[kb + (pgi_dim1 << 1)] = gi[k + (gi_dim1 << 1)];
	pgi[kb + pgi_dim1 * 3] = gi[k + gi_dim1 * 3];
	pgi[kb + (pgi_dim1 << 2)] = gi[k + (gi_dim1 << 2)];
	pgi[kb + pgi_dim1 * 5] = gi[k + gi_dim1 * 5];
	pgi[kb + pgi_dim1 * 6] = gi[k + gi_dim1 * 6];
	pgi[kb + pgi_dim1 * 7] = gi[k + gi_dim1 * 7];
/* L30: */
    }
    return 0;
} /* ch_to_par__ */

