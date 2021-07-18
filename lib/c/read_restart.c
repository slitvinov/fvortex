/* ../read_restart.f -- translated by f2c (version 20160102).
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

/* Table of constant values */

static integer c__4 = 4;
static integer c__1 = 1;
static integer c__3 = 3;

/* Subroutine */ int read_restart__(real *time, integer *np, real *s2, real *
	ovrlp, integer *nvort, real *xp, real *yp, real *gp)
{
    /* System generated locals */
    integer xp_dim1, yp_dim1, gp_dim1, i__1;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open(olist *), s_rsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_rsle(void), f_clos(cllist *), s_rsue(cilist *),
	     do_uio(integer *, char *, ftnlen), e_rsue(void);

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 1, 0, 0, 0 };
    static cilist io___2 = { 0, 1, 0, 0, 0 };
    static cilist io___3 = { 0, 1, 0, 0, 0 };
    static cilist io___4 = { 0, 1, 0, 0, 0 };
    static cilist io___5 = { 0, 1, 0, 0, 0 };
    static cilist io___6 = { 0, 1, 0, 0, 0 };
    static cilist io___7 = { 0, 1, 0, 0, 0 };


/*     Reads the files for restarting the simulation from a saved state. */
/*     Values for u and v are freestream velocity, thus (-) body vel. */
/* ---------------------------------------------------------------------- */
    /* Parameter adjustments */
    gp_dim1 = *nvort;
    --gp;
    yp_dim1 = *nvort;
    --yp;
    xp_dim1 = *nvort;
    --xp;

    /* Function Body */
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 11;
    o__1.ofnm = "params.cont";
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsle(&io___1);
    do_lio(&c__4, &c__1, (char *)&(*time), (ftnlen)sizeof(real));
    e_rsle();
    s_rsle(&io___2);
    do_lio(&c__3, &c__1, (char *)&(*np), (ftnlen)sizeof(integer));
    e_rsle();
    s_rsle(&io___3);
    do_lio(&c__4, &c__1, (char *)&(*s2), (ftnlen)sizeof(real));
    e_rsle();
    s_rsle(&io___4);
    do_lio(&c__4, &c__1, (char *)&(*ovrlp), (ftnlen)sizeof(real));
    e_rsle();
    cl__1.cerr = 0;
    cl__1.cunit = 1;
    cl__1.csta = 0;
    f_clos(&cl__1);
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 6;
    o__1.ofnm = "x.cont";
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsue(&io___5);
    i__1 = 1 * xp_dim1;
    do_uio(&i__1, (char *)&xp[1], (ftnlen)sizeof(real));
    e_rsue();
    cl__1.cerr = 0;
    cl__1.cunit = 1;
    cl__1.csta = 0;
    f_clos(&cl__1);
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 6;
    o__1.ofnm = "y.cont";
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsue(&io___6);
    i__1 = 1 * yp_dim1;
    do_uio(&i__1, (char *)&yp[1], (ftnlen)sizeof(real));
    e_rsue();
    cl__1.cerr = 0;
    cl__1.cunit = 1;
    cl__1.csta = 0;
    f_clos(&cl__1);
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 6;
    o__1.ofnm = "g.cont";
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsue(&io___7);
    i__1 = 1 * gp_dim1;
    do_uio(&i__1, (char *)&gp[1], (ftnlen)sizeof(real));
    e_rsue();
    cl__1.cerr = 0;
    cl__1.cunit = 1;
    cl__1.csta = 0;
    f_clos(&cl__1);
    return 0;
} /* read_restart__ */

