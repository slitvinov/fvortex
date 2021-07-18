/* ../diagnos.f -- translated by f2c (version 20160102).
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
    real xp[5000000], yp[5000000], gp[5000000];
} vort1_;

#define vort1_1 vort1_

struct {
    real xn[5000000], yn[5000000], gn[5000000];
} vort2_;

#define vort2_1 vort2_

struct {
    real uu[5000000], vv[5000000];
} vel_;

#define vel_1 vel_

struct {
    real uold[5000000], vold[5000000];
} oldvel_;

#define oldvel_1 oldvel_

struct {
    real gdiff[5000000], gdold[5000000];
} diff_;

#define diff_1 diff_

struct {
    integer n;
    real time, dt;
} params_;

#define params_1 params_

struct {
    integer np;
    real s2, ovrlp, gnu;
} part_;

#define part_1 part_

/* Table of constant values */

static integer c__4 = 4;
static integer c__1 = 1;

/* Subroutine */ int diagnos_(void)
{
    /* Format strings */
    static char fmt_100[] = "(f10.4,2x,e13.6)";

    /* System generated locals */
    integer i__1;
    cllist cl__1;

    /* Builtin functions */
    double atan(doublereal);
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void), f_clos(cllist *), s_wsfe(cilist *), do_fio(integer *
	    , char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static real g;
    static integer i__;
    static real x, y, pi, ang, circ, xmom, ymom;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 21, 0, 0, 0 };
    static cilist io___11 = { 0, 10, 0, fmt_100, 0 };
    static cilist io___12 = { 0, 11, 0, fmt_100, 0 };
    static cilist io___13 = { 0, 12, 0, fmt_100, 0 };


/*     Calculates the linear/angular impulse and circulation of the flow. */
/*     Differentiation of the impulse will give drag and lift. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ----------------------------------------------------- */
    pi = atan(1.f) * 4.f;
    circ = 0.f;
    xmom = 0.f;
    ymom = 0.f;
    ang = 0.f;
    i__1 = part_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x = vort1_1.xp[i__ - 1];
	y = vort1_1.yp[i__ - 1];
	g = vort1_1.gp[i__ - 1];
	circ += g;
	xmom -= g * y;
	ymom += g * x;
	ang += (x * x + y * y + part_1.s2) * .5f * g;
	s_wsle(&io___10);
	do_lio(&c__4, &c__1, (char *)&x, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&y, (ftnlen)sizeof(real));
	e_wsle();
/* L2: */
    }
    cl__1.cerr = 0;
    cl__1.cunit = 21;
    cl__1.csta = 0;
    f_clos(&cl__1);
    s_wsfe(&io___11);
    do_fio(&c__1, (char *)&params_1.time, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&xmom, (ftnlen)sizeof(real));
    e_wsfe();
    s_wsfe(&io___12);
    do_fio(&c__1, (char *)&params_1.time, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&ymom, (ftnlen)sizeof(real));
    e_wsfe();
    s_wsfe(&io___13);
    do_fio(&c__1, (char *)&params_1.time, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&circ, (ftnlen)sizeof(real));
    e_wsfe();
    cl__1.cerr = 0;
    cl__1.cunit = 10;
    cl__1.csta = 0;
    f_clos(&cl__1);
    cl__1.cerr = 0;
    cl__1.cunit = 11;
    cl__1.csta = 0;
    f_clos(&cl__1);
    cl__1.cerr = 0;
    cl__1.cunit = 12;
    cl__1.csta = 0;
    f_clos(&cl__1);
    return 0;
} /* diagnos_ */

