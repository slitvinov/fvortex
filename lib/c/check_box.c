/* ../check_box.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int check_box__(integer *nmax1, integer *kclose, integer *
	listclose, integer *kexam, integer *listexam, integer *kpart, integer 
	*listpart, integer *ipar1ch2, integer *imark1)
{
    /* System generated locals */
    integer ipar1ch2_dim1, ipar1ch2_offset, i__1;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    static integer i__, k, m, ks, kcheck, icheck4[5000000];

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, 0, 0 };


/*     This  subroutine examines the nearby boxes of a certain particle. */
/*     If they are parents then their children are placed in the list */
/*     *listexam* so that they are eaxmined at the lower level. */
/*     If they are childless then their particles are placed in list */
/*     *Listpart* so that they interact directly with the particle. */
/* ------------------------------------------------------ */
    /* Parameter adjustments */
    --imark1;
    ipar1ch2_dim1 = *nmax1;
    ipar1ch2_offset = 1 + ipar1ch2_dim1;
    ipar1ch2 -= ipar1ch2_offset;
    --listclose;
    --listexam;
    --listpart;

    /* Function Body */
    if (*kclose > 5000000) {
	s_wsle(&io___1);
	do_lio(&c__9, &c__1, "error in check_box,", (ftnlen)19);
	do_lio(&c__3, &c__1, (char *)&(*kclose), (ftnlen)sizeof(integer));
	e_wsle();
    }
    *kexam = 0;
    *kpart = 0;
    kcheck = 0;
    i__1 = *kclose;
    for (k = 1; k <= i__1; ++k) {
	ks = listclose[k];
	if (imark1[ks] == 0) {
/* childless */
	    ++(*kpart);
	    listpart[*kpart] = ks;
	} else {
	    ++kcheck;
	    icheck4[kcheck - 1] = ks;
	}
/* L2: */
    }
    for (i__ = 1; i__ <= 4; ++i__) {
	i__1 = kcheck;
	for (k = 1; k <= i__1; ++k) {
	    ks = icheck4[k - 1];
	    m = ipar1ch2[ks + i__ * ipar1ch2_dim1];
	    if (m != 0) {
/* box not empty, examine at lower */
		++(*kexam);
		listexam[*kexam] = m;
	    }
/* L30: */
	}
/* L3: */
    }
    return 0;
} /* check_box__ */

