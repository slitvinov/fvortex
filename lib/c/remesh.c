/* ../remesh.f -- translated by f2c (version 20160102).
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
    integer np;
    real s2, ovrlp, gnu;
} part_;

#define part_1 part_

struct {
    integer n;
    real time, dt;
} params_;

#define params_1 params_

struct {
    real vortlim;
} rems_;

#define rems_1 rems_

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__800 = 800;
static integer c_n800 = -800;
static integer c__4 = 4;

/* Subroutine */ int remesh_(void)
{
    /* Format strings */
    static char fmt_89[] = "(3x,\002New Total :\002,i8,3x,\002INSIDE :\002,i"
	    "8,2x,\002OUTSIDE :\002,i8,2x)";

    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double atan(doublereal), sqrt(doublereal);
    integer i_nint(real *), s_wsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_wsle(void);
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static real twopiinv, g;
    static integer i__;
    static real u, v, x, y, ag;
    static integer k00, k10, k20, k01, k11, ig, k21, k02, k12, k22;
    static real dh;
    static integer in;
    static real pi, yb, cx, gg[2563201], xg[2563201], yg[2563201];
    static integer ix, iy;
    static real xl, xr, yt, xx, yy, fx0, fy0, fy1;
    static integer ix0, ix1, ix2, iy0, iy1, iy2;
    static real fy2, fx1, fx2, circ;
    static integer ifar;
    static real cold;
    static integer ny_b__;
    static real cnew;
    static integer nx_l__, indx[2563201]	/* was [1601][1601] */, 
	    nx_r__, ny_t__;
    static real xmin, ymin, xmax, ymax;
    static integer iback;
    static real dhhaf, dhinv;
    static integer nmesh;
    static real ndist, sdist, twopi, cutoff;
    extern /* Subroutine */ int box_dim__(integer *, real *, real *, real *, 
	    real *);
    static real cut_far__;

    /* Fortran I/O blocks */
    static cilist io___18 = { 0, 6, 0, 0, 0 };
    static cilist io___19 = { 0, 6, 0, 0, 0 };
    static cilist io___20 = { 0, 6, 0, 0, 0 };
    static cilist io___21 = { 0, 6, 0, 0, 0 };
    static cilist io___22 = { 0, 6, 0, 0, 0 };
    static cilist io___23 = { 0, 6, 0, 0, 0 };
    static cilist io___35 = { 0, 6, 0, 0, 0 };
    static cilist io___36 = { 0, 6, 0, 0, 0 };
    static cilist io___40 = { 0, 6, 0, 0, 0 };
    static cilist io___77 = { 0, 6, 0, 0, 0 };
    static cilist io___78 = { 0, 6, 0, fmt_89, 0 };


/*     This subroutine remeshes the vortex field onto a uniform Cartesian */
/*     grid. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ---------------------------------------------------------------- */
    pi = atan(1.f) * 4.f;
    twopi = pi * 2.f;
    twopiinv = 1.f / twopi;
    dh = part_1.ovrlp * 2.f * sqrt(part_1.s2 / pi);
    dhinv = 1.f / dh;
/* ---  FIRST ESTABLISH THE NEW GRID TO BE MAPPED INTO */
/* ---  Find the edge of the grid of particles */
    box_dim__(&part_1.np, &xmin, &xmax, &ymin, &ymax);
    xr = xmax + dh * 5.f;
    xl = xmin - dh * 5.f;
    yt = ymax + dh * 5.f;
    yb = ymin - dh * 5.f;
    r__1 = xr / dh;
    nx_r__ = i_nint(&r__1);
    r__1 = xl / dh;
    nx_l__ = i_nint(&r__1);
    r__1 = yt / dh;
    ny_t__ = i_nint(&r__1);
    r__1 = yb / dh;
    ny_b__ = i_nint(&r__1) - 1;
    s_wsle(&io___18);
    do_lio(&c__9, &c__1, "nx_l,nx_r", (ftnlen)9);
    do_lio(&c__3, &c__1, (char *)&nx_l__, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&nx_r__, (ftnlen)sizeof(integer));
    e_wsle();
    s_wsle(&io___19);
    do_lio(&c__9, &c__1, "ny_t,ny_b", (ftnlen)9);
    do_lio(&c__3, &c__1, (char *)&ny_t__, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&ny_b__, (ftnlen)sizeof(integer));
    e_wsle();
    if (nx_r__ > 800) {
	s_wsle(&io___20);
	do_lio(&c__9, &c__1, "PROBLEM :nx_right =", (ftnlen)19);
	do_lio(&c__3, &c__1, (char *)&nx_r__, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, " nx_max = ", (ftnlen)10);
	do_lio(&c__3, &c__1, (char *)&c__800, (ftnlen)sizeof(integer));
	e_wsle();
    }
    if (nx_l__ < -800) {
	s_wsle(&io___21);
	do_lio(&c__9, &c__1, "PROBLEM :nx_left =", (ftnlen)18);
	do_lio(&c__3, &c__1, (char *)&nx_l__, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, " nx_min=", (ftnlen)8);
	do_lio(&c__3, &c__1, (char *)&c_n800, (ftnlen)sizeof(integer));
	e_wsle();
    }
    if (ny_t__ > 800) {
	s_wsle(&io___22);
	do_lio(&c__9, &c__1, "PROBLEM :ny_top =", (ftnlen)17);
	do_lio(&c__3, &c__1, (char *)&ny_t__, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, "ny_max=", (ftnlen)7);
	do_lio(&c__3, &c__1, (char *)&c__800, (ftnlen)sizeof(integer));
	e_wsle();
    }
    if (ny_b__ < -800) {
	s_wsle(&io___23);
	do_lio(&c__9, &c__1, "PROBLEM :ny_bottom =", (ftnlen)20);
	do_lio(&c__3, &c__1, (char *)&ny_b__, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, "ny_min=", (ftnlen)7);
	do_lio(&c__3, &c__1, (char *)&c_n800, (ftnlen)sizeof(integer));
	e_wsle();
    }
    if (nx_r__ > 800 || nx_l__ < -800 || ny_t__ > 800 || ny_b__ < -800) {
	s_stop("", (ftnlen)0);
    }
/* ---  Establish the new grid for the remeshed field */
    dhhaf = dh * .5f;
    ig = 0;
    i__1 = nx_r__;
    for (ix = nx_l__; ix <= i__1; ++ix) {
	xx = dhhaf + ix * dh;
	i__2 = ny_t__;
	for (iy = ny_b__; iy <= i__2; ++iy) {
	    yy = dhhaf + iy * dh;
	    ++ig;
	    xg[ig - 1] = xx;
	    yg[ig - 1] = yy;
	    gg[ig - 1] = 0.f;
	    indx[ix + iy * 1601 + 1281600] = ig;
/* We can avoid this big array */
/* L11: */
	}
/* L10: */
    }
    nmesh = ig;
    s_wsle(&io___35);
    do_lio(&c__9, &c__1, "Nmesh = ", (ftnlen)8);
    do_lio(&c__3, &c__1, (char *)&nmesh, (ftnlen)sizeof(integer));
    e_wsle();
    if (nmesh > 2563201) {
/* overran array dimensions */
	s_wsle(&io___36);
	do_lio(&c__9, &c__1, "nmesh too large in remesh, stopping", (ftnlen)
		35);
	e_wsle();
	s_stop("", (ftnlen)0);
    }
/* ---  check diagnostics, pre-remesh - they should be conserved through remesh */
    cold = 0.f;
    cx = 0.f;
    i__1 = part_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	cold += vort1_1.gp[i__ - 1];
/* total circulation */
	cx += vort1_1.gp[i__ - 1] * vort1_1.yp[i__ - 1];
/* x-impulse */
/* L71: */
    }
    s_wsle(&io___40);
    do_lio(&c__9, &c__1, "pre-remesh, circulation:", (ftnlen)24);
    do_lio(&c__4, &c__1, (char *)&cold, (ftnlen)sizeof(real));
    do_lio(&c__9, &c__1, "  x-impulse: ", (ftnlen)13);
    do_lio(&c__4, &c__1, (char *)&cx, (ftnlen)sizeof(real));
    e_wsle();
/* --   set cutoff values to throw out particles */
/* Computing 2nd power */
    r__1 = part_1.ovrlp;
    cutoff = rems_1.vortlim * part_1.s2 * (r__1 * r__1);
    cut_far__ = cutoff * 10.f;
/* ---  NEW GRID IN PLACE, REMESH OLD FIELD NOW */
    in = 0;
    ifar = 0;
    i__1 = part_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* loop through, mapping particles to mes */
	g = vort1_1.gp[i__ - 1];
	x = vort1_1.xp[i__ - 1];
	y = vort1_1.yp[i__ - 1];
	ndist = y;
	sdist = x;
	r__1 = sdist * dhinv - .5f;
	ix = i_nint(&r__1);
	r__1 = ndist * dhinv - .5f;
	iy = i_nint(&r__1);
/* ---- Do not remesh the particles outside the established grid */
	if (ix > nx_r__ || ix < nx_l__ || iy > ny_t__ || iy < ny_b__) {
	    if (dabs(g) >= cut_far__) {
		++ifar;
		vort1_1.xp[ifar - 1] = x;
		vort1_1.yp[ifar - 1] = y;
		vort1_1.gp[ifar - 1] = g;
	    }
/* -- all other particles are in the inner grid */
	} else if (ix == nx_r__ || ix == nx_l__ || iy == ny_t__ || iy == 
		ny_b__) {
/* ---------------------------------------------------------------- */
/* - Category 0_1 : PARTICLES at the FAR interface (NGP remeshing) */
/* ---------------------------------------------------------------- */
	    ++in;
	    ig = indx[ix + iy * 1601 + 1281600];
	    gg[ig - 1] += g;
/* NGP   Interpolation */
	} else {
/* ---------------------------------------------------------------- */
/* - Category 0_1 : ALL other PARTICLES in the domain */
/* ---------------------------------------------------------------- */
	    ++in;
	    ix0 = ix;
	    ix1 = ix - 1;
	    ix2 = ix + 1;
	    iy0 = iy;
	    iy1 = iy - 1;
	    iy2 = iy + 1;
	    k00 = indx[ix0 + iy0 * 1601 + 1281600];
	    k10 = indx[ix1 + iy0 * 1601 + 1281600];
	    k20 = indx[ix2 + iy0 * 1601 + 1281600];
	    k01 = indx[ix0 + iy1 * 1601 + 1281600];
	    k11 = indx[ix1 + iy1 * 1601 + 1281600];
	    k21 = indx[ix2 + iy1 * 1601 + 1281600];
	    k02 = indx[ix0 + iy2 * 1601 + 1281600];
	    k12 = indx[ix1 + iy2 * 1601 + 1281600];
	    k22 = indx[ix2 + iy2 * 1601 + 1281600];
	    u = (sdist - xg[k00 - 1]) * dhinv;
	    v = (ndist - yg[k00 - 1]) * dhinv;
	    fy0 = 1.f - v * v;
	    fy1 = v * .5f * (v - 1.f);
	    fy2 = v * .5f * (v + 1.f);
	    fx0 = g * (1.f - u * u);
	    fx1 = g * (u * .5f * (u - 1.f));
	    fx2 = g * (u * .5f * (u + 1.f));
	    gg[k00 - 1] += fx0 * fy0;
	    gg[k01 - 1] += fx0 * fy1;
	    gg[k02 - 1] += fx0 * fy2;
	    gg[k10 - 1] += fx1 * fy0;
	    gg[k11 - 1] += fx1 * fy1;
	    gg[k12 - 1] += fx1 * fy2;
	    gg[k20 - 1] += fx2 * fy0;
	    gg[k21 - 1] += fx2 * fy1;
	    gg[k22 - 1] += fx2 * fy2;
	}
/* L40: */
    }
/* ---  put remeshed particles into arrays, using cutoffs determined earlier */
    iback = ifar;
    i__1 = nmesh;
    for (i__ = 1; i__ <= i__1; ++i__) {
	g = gg[i__ - 1];
	ag = dabs(g);
/* -- only cutoff if below threshold AND away from domain center */
	if (ag > cutoff || (r__1 = yg[i__ - 1], dabs(r__1)) * (r__2 = yg[i__ 
		- 1], dabs(r__2)) + (r__3 = xg[i__ - 1], dabs(r__3)) * (r__4 =
		 xg[i__ - 1], dabs(r__4)) < 1.f) {
	    ++iback;
	    vort1_1.xp[iback - 1] = xg[i__ - 1];
	    vort1_1.yp[iback - 1] = yg[i__ - 1];
	    vort1_1.gp[iback - 1] = g;
	}
/* L29: */
    }
/* ---  check diagnostics */
    part_1.np = iback;
    cnew = 0.f;
    cx = 0.f;
    i__1 = part_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	circ = vort1_1.gp[i__ - 1];
	cnew += circ;
	cx += circ * vort1_1.yp[i__ - 1];
/* L72: */
    }
    s_wsle(&io___77);
    do_lio(&c__9, &c__1, "post-remesh, circulation:", (ftnlen)25);
    do_lio(&c__4, &c__1, (char *)&cnew, (ftnlen)sizeof(real));
    do_lio(&c__9, &c__1, "  x-impulse: ", (ftnlen)13);
    do_lio(&c__4, &c__1, (char *)&cx, (ftnlen)sizeof(real));
    e_wsle();
    s_wsfe(&io___78);
    do_fio(&c__1, (char *)&part_1.np, (ftnlen)sizeof(integer));
    i__1 = iback - ifar;
    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ifar, (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* remesh_ */

