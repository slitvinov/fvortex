/* ../vort_field.f -- translated by f2c (version 20160102).
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
    integer nx, ny;
    real xmax, xmin, ymax, ymin;
} vort_avg__;

#define vort_avg__1 vort_avg__

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

static integer c__1 = 1;

/* Subroutine */ int vort_field__(integer *iframe)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    double atan(doublereal);
    integer i_nint(real *);
    double exp(doublereal);
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    , f_open(olist *), s_wsfe(cilist *), e_wsfe(void), f_clos(cllist *
	    );

    /* Local variables */
    static real twopiinv, c__, g;
    static integer i__, j, m;
    static real x, y;
    static integer m0;
    static real x0, y0, gg[250000];
    static integer in;
    static real pi, dx, dy, xg[250000], yg[250000];
    static integer ix, iy;
    static real xx, r2s;
    static char vortoutfile[256];
    static integer ngrid;
    static real s2inv2, dxinv, dyinv, s2piinv;

    /* Fortran I/O blocks */
    static icilist io___28 = { 0, vortoutfile, 0, "(A,I8.8,A)", 256, 1 };
    static cilist io___29 = { 0, 1, 0, "(A)", 0 };
    static cilist io___30 = { 0, 1, 0, "(A,I8,A,I8)", 0 };
    static cilist io___32 = { 0, 1, 0, "(3E17.9)", 0 };


/*     outputs the vorticity field on a grid. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ---------------------------------------------------------------- */
    pi = atan(1.f) * 4.f;
    twopiinv = .5f / pi;
    dx = (r__1 = (vort_avg__1.xmax - vort_avg__1.xmin) / vort_avg__1.nx, dabs(
	    r__1));
    dy = (r__1 = (vort_avg__1.ymax - vort_avg__1.ymin) / vort_avg__1.ny, dabs(
	    r__1));
    x0 = vort_avg__1.xmin;
    y0 = vort_avg__1.ymin;
    in = 0;
    i__1 = vort_avg__1.nx;
    for (ix = 1; ix <= i__1; ++ix) {
	i__2 = vort_avg__1.ny;
	for (iy = 1; iy <= i__2; ++iy) {
	    ++in;
	    gg[in - 1] = 0.f;
/* L2: */
	}
/* L1: */
    }
    in = 0;
    i__1 = vort_avg__1.nx;
    for (ix = 1; ix <= i__1; ++ix) {
	xx = x0 + (ix - 1) * dx;
	i__2 = vort_avg__1.ny;
	for (iy = 1; iy <= i__2; ++iy) {
	    ++in;
	    xg[in - 1] = xx;
	    yg[in - 1] = y0 + (iy - 1) * dy;
/* L11: */
	}
/* L10: */
    }
    ngrid = vort_avg__1.nx * vort_avg__1.ny;
/*     Determination of  the  circulation  of each particle */
    s2inv2 = .5f / part_1.s2;
    s2piinv = twopiinv / part_1.s2;
    dxinv = 1.f / dx;
    dyinv = 1.f / dy;
    i__1 = part_1.np;
    for (j = 1; j <= i__1; ++j) {
	g = vort1_1.gp[j - 1] * s2piinv;
	x = vort1_1.xp[j - 1];
	y = vort1_1.yp[j - 1];
	r__1 = (x - x0) * dxinv;
	ix = i_nint(&r__1) + 1;
	r__1 = (y - y0) * dyinv;
	iy = i_nint(&r__1) + 1;
	if (ix > -3 && iy > -3 && ix < vort_avg__1.nx + 3 && iy < 
		vort_avg__1.ny + 3) {
/* --   loop over grid in expnaded code in order to vectorize */
/*     ---- Points on column IX */
	    m0 = (ix - 1) * vort_avg__1.ny + iy;
	    m = m0;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
/*     ---- Points on column IX - 3 */
	    m0 = (ix - 4) * vort_avg__1.ny + iy;
	    m = m0;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
/*     ---- Points on column IX - 2 */
	    m0 = (ix - 3) * vort_avg__1.ny + iy;
	    m = m0;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
/*     ---- Points on column IX - 1 */
	    m0 = (ix - 2) * vort_avg__1.ny + iy;
	    m = m0;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
/*     ---- Points on column IX + 1 */
	    m0 = ix * vort_avg__1.ny + iy;
	    m = m0;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
/*     ---- Points on column IX + 2 */
	    m0 = (ix + 1) * vort_avg__1.ny + iy;
	    m = m0;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
/*     ---- Points on column IX + 3 */
	    m0 = (ix + 2) * vort_avg__1.ny + iy;
	    m = m0;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 - 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 1;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 2;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	    m = m0 + 3;
	    if (m > 0 && m < ngrid + 1) {
/* Computing 2nd power */
		r__1 = xg[m - 1] - x;
/* Computing 2nd power */
		r__2 = yg[m - 1] - y;
		r2s = (r__1 * r__1 + r__2 * r__2) * s2inv2;
		c__ = exp(-r2s);
		gg[m - 1] += g * c__;
	    }
	}
/* L20: */
    }
    s_wsfi(&io___28);
    do_fio(&c__1, "w.", (ftnlen)2);
    do_fio(&c__1, (char *)&(*iframe), (ftnlen)sizeof(integer));
    do_fio(&c__1, ".dat", (ftnlen)4);
    e_wsfi();
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 256;
    o__1.ofnm = vortoutfile;
    o__1.orl = 0;
    o__1.osta = "replace";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsfe(&io___29);
    do_fio(&c__1, "variables=x,y,w", (ftnlen)15);
    e_wsfe();
    s_wsfe(&io___30);
    do_fio(&c__1, "zone i=", (ftnlen)7);
    do_fio(&c__1, (char *)&vort_avg__1.nx, (ftnlen)sizeof(integer));
    do_fio(&c__1, ", j=", (ftnlen)4);
    do_fio(&c__1, (char *)&vort_avg__1.ny, (ftnlen)sizeof(integer));
    e_wsfe();
    in = 0;
    i__1 = vort_avg__1.nx;
    for (j = 1; j <= i__1; ++j) {
	i__2 = vort_avg__1.ny;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    ++in;
	    s_wsfe(&io___32);
	    do_fio(&c__1, (char *)&xg[in - 1], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&yg[in - 1], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&gg[in - 1], (ftnlen)sizeof(real));
	    e_wsfe();
	}
    }
    cl__1.cerr = 0;
    cl__1.cunit = 1;
    cl__1.csta = 0;
    f_clos(&cl__1);
    return 0;
} /* vort_field__ */

