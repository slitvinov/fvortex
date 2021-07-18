/* ../box_1.f -- translated by f2c (version 20160102).
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
    real x0, y0;
    integer limpar;
} geom_;

#define geom_1 geom_

struct {
    real dragbf, liftbf, mombf, xsumbf, ysumbf, rsumbf;
    integer bf_marker__[5000000];
} bf_;

#define bf_1 bf_

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__4 = 4;

/* Subroutine */ int box_1__(integer *npart, real *s0, real *xc1, real *yc1, 
	integer *ic1, integer *jc1, integer *npb1, real *ds1, integer *kp1, 
	integer *liststart)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, n, np, ix;
    static real lx, ly;
    static integer nb1, nb2, nb3, nb4, nbx, ixy[5000000];
    static real xst, yst;
    static integer ibox, jbox, inew;
    static real ds1inv;
    extern /* Subroutine */ int wheneq_(integer *, integer *, integer *, 
	    integer *, integer *, integer *);
    static integer idummy[5000000], bf_marker_temp__[5000000];

/*     This subroutine sorts the particles into four boxes and provides the */
/*     necessary identification arrays for this top level of the interaction tree. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ------------------------------------------------------------------------ */
    /* Parameter adjustments */
    --liststart;
    npb1 -= 5;
    --jc1;
    --ic1;
    --yc1;
    --xc1;

    /* Function Body */
    for (i__ = 1; i__ <= 5000000; ++i__) {
	ixy[i__ - 1] = 0;
	idummy[i__ - 1] = 0;
	bf_marker_temp__[i__ - 1] = bf_1.bf_marker__[i__ - 1];
    }
    *ds1 = *s0 * .5f;
    xst = geom_1.x0 - *ds1 * .5f;
    yst = geom_1.y0 - *ds1 * .5f;
    ds1inv = 1.f / *ds1;
/* --   Identify each particle with one of the boxes */
    i__1 = *npart;
    for (n = 1; n <= i__1; ++n) {
	lx = (vort1_1.xp[n - 1] - geom_1.x0) * ds1inv;
	ly = (vort1_1.yp[n - 1] - geom_1.y0) * ds1inv;
	ibox = lx + 1;
/* indices of the box where the particles */
	jbox = ly + 1;
/* reside */
	ixy[n - 1] = 1;
/* IXY(n) has as value */
	if (ibox == 1 && jbox == 2) {
/* the index (1,2,3,4) */
	    ixy[n - 1] = 2;
/* of the box that */
	} else if (ibox == 2 && jbox == 1) {
/* particle n is in */
	    ixy[n - 1] = 3;
	} else if (ibox == 2 && jbox == 2) {
	    ixy[n - 1] = 4;
	}
/* L1: */
    }
/*     Find  how  many  particles  are  in  each subbox */
/*     and store  the  particles  in their new sorted  locations */
    wheneq_(npart, ixy, &c__1, &c__1, idummy, &nb1);
    i__1 = nb1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	inew = idummy[i__ - 1];
	vort2_1.xn[i__ - 1] = vort1_1.xp[inew - 1];
	vort2_1.yn[i__ - 1] = vort1_1.yp[inew - 1];
	vort2_1.gn[i__ - 1] = vort1_1.gp[inew - 1];
	oldvel_1.uold[i__ - 1] = vel_1.uu[inew - 1];
	oldvel_1.vold[i__ - 1] = vel_1.vv[inew - 1];
	diff_1.gdold[i__ - 1] = diff_1.gdiff[inew - 1];
	bf_1.bf_marker__[i__ - 1] = bf_marker_temp__[inew - 1];
/* L211: */
    }
    wheneq_(npart, ixy, &c__1, &c__2, idummy, &nb2);
    nbx = nb1;
    i__1 = nb2;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ix = i__ + nbx;
	inew = idummy[i__ - 1];
	vort2_1.xn[ix - 1] = vort1_1.xp[inew - 1];
	vort2_1.yn[ix - 1] = vort1_1.yp[inew - 1];
	vort2_1.gn[ix - 1] = vort1_1.gp[inew - 1];
	oldvel_1.uold[ix - 1] = vel_1.uu[inew - 1];
	oldvel_1.vold[ix - 1] = vel_1.vv[inew - 1];
	diff_1.gdold[ix - 1] = diff_1.gdiff[inew - 1];
	bf_1.bf_marker__[ix - 1] = bf_marker_temp__[inew - 1];
/* L212: */
    }
    wheneq_(npart, ixy, &c__1, &c__3, idummy, &nb3);
    nbx += nb2;
    i__1 = nb3;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ix = i__ + nbx;
	inew = idummy[i__ - 1];
	vort2_1.xn[ix - 1] = vort1_1.xp[inew - 1];
	vort2_1.yn[ix - 1] = vort1_1.yp[inew - 1];
	vort2_1.gn[ix - 1] = vort1_1.gp[inew - 1];
	oldvel_1.uold[ix - 1] = vel_1.uu[inew - 1];
	oldvel_1.vold[ix - 1] = vel_1.vv[inew - 1];
	diff_1.gdold[ix - 1] = diff_1.gdiff[inew - 1];
	bf_1.bf_marker__[ix - 1] = bf_marker_temp__[inew - 1];
/* L213: */
    }
    wheneq_(npart, ixy, &c__1, &c__4, idummy, &nb4);
    nbx += nb3;
    i__1 = nb4;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ix = i__ + nbx;
	inew = idummy[i__ - 1];
	vort2_1.xn[ix - 1] = vort1_1.xp[inew - 1];
	vort2_1.yn[ix - 1] = vort1_1.yp[inew - 1];
	vort2_1.gn[ix - 1] = vort1_1.gp[inew - 1];
	oldvel_1.uold[ix - 1] = vel_1.uu[inew - 1];
	oldvel_1.vold[ix - 1] = vel_1.vv[inew - 1];
	diff_1.gdold[ix - 1] = diff_1.gdiff[inew - 1];
	bf_1.bf_marker__[ix - 1] = bf_marker_temp__[inew - 1];
/* L214: */
    }
/*     NPBk(kp1,1)  -> index of first particle in box kp1 at level k */
/*     NPBk(kp1,2)  -> index of last particle in box kp1 at level k */
/*     kpi is the number of boxes that contain particles at level i */
/*     Now make necessary identification arrays for the boxes */
/*     Box 1 */
    *kp1 = 0;
    for (i__ = 1; i__ <= 4; ++i__) {
	liststart[i__] = 0;
    }
    np = 0;
    if (nb1 > 0) {
	++(*kp1);
	liststart[*kp1] = *kp1;
	npb1[*kp1 + 4] = np + 1;
	np = nb1;
	npb1[*kp1 + 8] = np;
	ic1[*kp1] = 1;
	jc1[*kp1] = 1;
	xc1[*kp1] = xst + *ds1;
	yc1[*kp1] = yst + *ds1;
    }
/*     Box 2 */
    if (nb2 > 0) {
	++(*kp1);
	liststart[*kp1] = *kp1;
	npb1[*kp1 + 4] = np + 1;
	np += nb2;
	npb1[*kp1 + 8] = np;
	ic1[*kp1] = 1;
	jc1[*kp1] = 2;
	xc1[*kp1] = xst + *ds1;
	yc1[*kp1] = yst + *ds1 * 2.f;
    }
/*     Box 3 */
    if (nb3 > 0) {
	++(*kp1);
	liststart[*kp1] = *kp1;
	npb1[*kp1 + 4] = np + 1;
	np += nb3;
	npb1[*kp1 + 8] = np;
	ic1[*kp1] = 2;
	jc1[*kp1] = 1;
	xc1[*kp1] = xst + *ds1 * 2.f;
	yc1[*kp1] = yst + *ds1;
    }
/*     Box 4 */
    if (nb4 > 0) {
	++(*kp1);
	liststart[*kp1] = *kp1;
	npb1[*kp1 + 4] = np + 1;
	np += nb4;
	npb1[*kp1 + 8] = np;
	ic1[*kp1] = 2;
	jc1[*kp1] = 2;
	xc1[*kp1] = xst + *ds1 * 2.f;
	yc1[*kp1] = yst + *ds1 * 2.f;
    }
    return 0;
} /* box_1__ */

