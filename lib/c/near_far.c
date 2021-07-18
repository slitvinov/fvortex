/* ../near_far.f -- translated by f2c (version 20160102).
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

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;

/* Subroutine */ int near_far__(integer *nmax, integer *ib, integer *jb, real 
	*r__, integer *ic, integer *jc, integer *kexam, integer *listexam, 
	integer *kfar, integer *listfar, integer *kclose, integer *listclose)
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    static integer k;
    static real fi, fj, cr, si;
    static integer ks;
    static real sj;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, 0, 0 };


/*     This  subroutine finds all the far & close boxes at a certain */
/*     level associated with a certain particle. */
/* ------------------------------------------------------------------------------ */
    /* Parameter adjustments */
    --jc;
    --ic;
    --listexam;
    --listfar;
    --listclose;

    /* Function Body */
    if (*kexam > 5000000) {
	s_wsle(&io___1);
	do_lio(&c__9, &c__1, "error in near_far,", (ftnlen)18);
	do_lio(&c__3, &c__1, (char *)&(*kexam), (ftnlen)sizeof(integer));
	e_wsle();
    }
    fi = *r__ * (*ib - .5f) + .5f;
    fj = *r__ * (*jb - .5f) + .5f;
    cr = (*r__ + 1.f) * .500001f;
    *kfar = 0;
    *kclose = 0;
    i__1 = *kexam;
    for (k = 1; k <= i__1; ++k) {
	ks = listexam[k];
	si = (real) ic[ks];
	sj = (real) jc[ks];
	if ((r__1 = fi - si, dabs(r__1)) < cr && (r__2 = fj - sj, dabs(r__2)) 
		< cr) {
	    ++(*kclose);
	    listclose[*kclose] = ks;
/* close */
	} else {
	    ++(*kfar);
	    listfar[*kfar] = ks;
/* far away */
	}
/* L2: */
    }
    return 0;
} /* near_far__ */

