/* ../streamfunction.f -- translated by f2c (version 20160102).
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

static integer c__0 = 0;
static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;

/* Subroutine */ int streamfunction_(real *xbc, real *ybc, real *sp, real *sb)
{
    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int str_part__(real *, real *, integer *, real *);
    static integer nn;
    extern /* Subroutine */ int build_tree__(integer *, real *, real *, 
	    integer *, integer *);
    static integer kfp;
    extern /* Subroutine */ int str_box__(real *, real *, integer *, real *);

    /* Fortran I/O blocks */
    static cilist io___3 = { 0, 6, 0, 0, 0 };
    static cilist io___4 = { 0, 6, 0, 0, 0 };


/*     This subroutine finds the streamfunction value at (xbc,ybc) induced */
/*     by the vorticity field, sending back contributions from particle */
/*     interactions (Sp) and box interactions (Sb). */
/* --------------------------------------------------------------------------- */
    build_tree__(&c__0, xbc, ybc, &nn, &kfp);
    str_part__(xbc, ybc, &nn, sp);
    if (nn > 3450000) {
	s_wsle(&io___3);
	do_lio(&c__9, &c__1, "error in str_part, nn=", (ftnlen)22);
	do_lio(&c__3, &c__1, (char *)&nn, (ftnlen)sizeof(integer));
	e_wsle();
	s_stop("", (ftnlen)0);
    }
    str_box__(xbc, ybc, &kfp, sb);
    if (kfp > 60001) {
	s_wsle(&io___4);
	do_lio(&c__9, &c__1, "error in str_box, kfp=", (ftnlen)22);
	do_lio(&c__3, &c__1, (char *)&kfp, (ftnlen)sizeof(integer));
	e_wsle();
	s_stop("", (ftnlen)0);
    }
/* L900: */
    return 0;
} /* streamfunction_ */

