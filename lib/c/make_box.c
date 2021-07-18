/* ../make_box.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int make_box__(integer *nmax, real *ds1, real *ds2, integer *
	kp1, integer *kp2, integer *kparent1, integer *kchildless1, integer *
	ic1, integer *jc1, integer *npb1, integer *iparent1, integer *imark1, 
	integer *ipar1ch2, integer *ich2par1, integer *npb2, integer *ic2, 
	integer *jc2, real *xc2, real *yc2, integer *ichildless1)
{
    /* System generated locals */
    integer npb1_dim1, npb1_offset, ipar1ch2_dim1, ipar1ch2_offset, npb2_dim1,
	     npb2_offset, i__1, i__2;

    /* Local variables */
    static integer i__, n, n1, n2, jn, ip, jp, np, ix, lx, ly, nb1, nb2, nb3, 
	    nb4, n1m1, nbx, ixy[5000000];
    static real xst, yst;
    static integer ich1, jch1, ich2, jch2, ich3, jch3, ich4, jch4;
    static real si1d, sj1d, si2d, sj2d, si3d, sj3d, si4d, sj4d;
    static integer ipar, jpar, ibox, jbox, kbox, inew, npbox;
    static real ds2inv;
    extern /* Subroutine */ int wheneq_(integer *, integer *, integer *, 
	    integer *, integer *, integer *);
    static integer idummy[5000000], bf_marker_temp__[5000000];

/*     This subroutine takes each box on the previous level and splits it into */
/*     four boxes, creating all the necessary indentification arrays to relate */
/*     the levels.  It mostly parallels the BOX_1 subroutine. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* --------------------------------------------------------------------- */
    /* Parameter adjustments */
    --ichildless1;
    --yc2;
    --xc2;
    --jc2;
    --ic2;
    npb2_dim1 = *nmax;
    npb2_offset = 1 + npb2_dim1;
    npb2 -= npb2_offset;
    --ich2par1;
    ipar1ch2_dim1 = *nmax / 4;
    ipar1ch2_offset = 1 + ipar1ch2_dim1;
    ipar1ch2 -= ipar1ch2_offset;
    --imark1;
    --iparent1;
    npb1_dim1 = *nmax / 4;
    npb1_offset = 1 + npb1_dim1;
    npb1 -= npb1_offset;
    --jc1;
    --ic1;

    /* Function Body */
    for (i__ = 1; i__ <= 5000000; ++i__) {
	ixy[i__ - 1] = 0;
	idummy[i__ - 1] = 0;
	bf_marker_temp__[i__ - 1] = bf_1.bf_marker__[i__ - 1];
    }
/*     Find childless & parent boxes. Subdivide parent boxes in 4 squares. */
/*     (Parent boxes  are those  that contain more than LIMPAR particles ) */
    *ds2 = *ds1 * .5f;
    ds2inv = 1.f / *ds2;
    xst = geom_1.x0 - *ds2 * .5f;
    yst = geom_1.y0 - *ds2 * .5f;
    np = 0;
    *kp2 = 0;
    *kparent1 = 0;
    *kchildless1 = 0;
    i__1 = *kp1;
    for (kbox = 1; kbox <= i__1; ++kbox) {
	ip = ic1[kbox];
	jp = jc1[kbox];
	n1 = npb1[kbox + npb1_dim1];
	n2 = npb1[kbox + (npb1_dim1 << 1)];
	npbox = n2 - n1 + 1;
	if (npbox > geom_1.limpar) {
/* *  Parent Box */
	    ++(*kparent1);
	    iparent1[*kparent1] = kbox;
/* index of parent box */
	    imark1[kbox] = 1;
	    ipar = ip - 1 << 1;
	    jpar = jp - 1 << 1;
	    ich1 = ipar + 1;
/* 1st subbox */
	    jch1 = jpar + 1;
	    ich2 = ich1;
/* 2nd subbox */
	    jch2 = jpar + 2;
	    ich3 = ipar + 2;
/* 3rd  subbox */
	    jch3 = jch1;
	    ich4 = ich3;
/* 4th  subbox */
	    jch4 = jch2;
	    si1d = ich1 * *ds2;
	    sj1d = jch1 * *ds2;
	    si2d = si1d;
	    sj2d = jch2 * *ds2;
	    si3d = ich3 * *ds2;
	    sj3d = sj1d;
	    si4d = si3d;
	    sj4d = sj2d;
	    jn = 0;
	    i__2 = n2;
	    for (n = n1; n <= i__2; ++n) {
		lx = (vort2_1.xn[n - 1] - geom_1.x0) * ds2inv;
		ly = (vort2_1.yn[n - 1] - geom_1.y0) * ds2inv;
		ibox = lx + 1;
		jbox = ly + 1;
		++jn;
		ixy[jn - 1] = 1;
		if (ibox == ich2 && jbox == jch2) {
		    ixy[jn - 1] = 2;
		} else if (ibox == ich3 && jbox == jch3) {
		    ixy[jn - 1] = 3;
		} else if (ibox == ich4 && jbox == jch4) {
		    ixy[jn - 1] = 4;
		}
/* L20: */
	    }
/*   Find  how  many  particles  are  in  each subbox */
/*  and store  the  particles  in their new sorted  locations */
	    n1m1 = n1 - 1;
	    nbx = n1m1;
	    wheneq_(&npbox, ixy, &c__1, &c__1, idummy, &nb1);
	    i__2 = nb1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		ix = i__ + nbx;
		inew = idummy[i__ - 1] + n1m1;
		vort1_1.xp[ix - 1] = vort2_1.xn[inew - 1];
		vort1_1.yp[ix - 1] = vort2_1.yn[inew - 1];
		vort1_1.gp[ix - 1] = vort2_1.gn[inew - 1];
		vel_1.uu[ix - 1] = oldvel_1.uold[inew - 1];
		vel_1.vv[ix - 1] = oldvel_1.vold[inew - 1];
		diff_1.gdiff[ix - 1] = diff_1.gdold[inew - 1];
		bf_1.bf_marker__[ix - 1] = bf_marker_temp__[inew - 1];
/* L211: */
	    }
	    wheneq_(&npbox, ixy, &c__1, &c__2, idummy, &nb2);
	    nbx += nb1;
	    i__2 = nb2;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		ix = i__ + nbx;
		inew = idummy[i__ - 1] + n1m1;
		vort1_1.xp[ix - 1] = vort2_1.xn[inew - 1];
		vort1_1.yp[ix - 1] = vort2_1.yn[inew - 1];
		vort1_1.gp[ix - 1] = vort2_1.gn[inew - 1];
		vel_1.uu[ix - 1] = oldvel_1.uold[inew - 1];
		vel_1.vv[ix - 1] = oldvel_1.vold[inew - 1];
		diff_1.gdiff[ix - 1] = diff_1.gdold[inew - 1];
		bf_1.bf_marker__[ix - 1] = bf_marker_temp__[inew - 1];
/* L212: */
	    }
	    wheneq_(&npbox, ixy, &c__1, &c__3, idummy, &nb3);
	    nbx += nb2;
	    i__2 = nb3;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		ix = i__ + nbx;
		inew = idummy[i__ - 1] + n1m1;
		vort1_1.xp[ix - 1] = vort2_1.xn[inew - 1];
		vort1_1.yp[ix - 1] = vort2_1.yn[inew - 1];
		vort1_1.gp[ix - 1] = vort2_1.gn[inew - 1];
		vel_1.uu[ix - 1] = oldvel_1.uold[inew - 1];
		vel_1.vv[ix - 1] = oldvel_1.vold[inew - 1];
		diff_1.gdiff[ix - 1] = diff_1.gdold[inew - 1];
		bf_1.bf_marker__[ix - 1] = bf_marker_temp__[inew - 1];
/* L213: */
	    }
	    wheneq_(&npbox, ixy, &c__1, &c__4, idummy, &nb4);
	    nbx += nb3;
	    i__2 = nb4;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		ix = i__ + nbx;
		inew = idummy[i__ - 1] + n1m1;
		vort1_1.xp[ix - 1] = vort2_1.xn[inew - 1];
		vort1_1.yp[ix - 1] = vort2_1.yn[inew - 1];
		vort1_1.gp[ix - 1] = vort2_1.gn[inew - 1];
		vel_1.uu[ix - 1] = oldvel_1.uold[inew - 1];
		vel_1.vv[ix - 1] = oldvel_1.vold[inew - 1];
		diff_1.gdiff[ix - 1] = diff_1.gdold[inew - 1];
		bf_1.bf_marker__[ix - 1] = bf_marker_temp__[inew - 1];
/* L214: */
	    }
/* Box 1 */
	    np = n1 - 1;
	    if (nb1 > 0) {
		++(*kp2);
		ipar1ch2[kbox + ipar1ch2_dim1] = *kp2;
		ich2par1[*kp2] = kbox;
		npb2[*kp2 + npb2_dim1] = np + 1;
		np += nb1;
		npb2[*kp2 + (npb2_dim1 << 1)] = np;
		ic2[*kp2] = ich1;
		jc2[*kp2] = jch1;
		xc2[*kp2] = xst + si1d;
		yc2[*kp2] = yst + sj1d;
	    } else {
		ipar1ch2[kbox + ipar1ch2_dim1] = 0;
	    }
/* Box 2 */
	    if (nb2 > 0) {
		++(*kp2);
		ipar1ch2[kbox + (ipar1ch2_dim1 << 1)] = *kp2;
		ich2par1[*kp2] = kbox;
		npb2[*kp2 + npb2_dim1] = np + 1;
		np += nb2;
		npb2[*kp2 + (npb2_dim1 << 1)] = np;
		ic2[*kp2] = ich2;
		jc2[*kp2] = jch2;
		xc2[*kp2] = xst + si2d;
		yc2[*kp2] = yst + sj2d;
	    } else {
		ipar1ch2[kbox + (ipar1ch2_dim1 << 1)] = 0;
	    }
/* Box 3 */
	    if (nb3 > 0) {
		++(*kp2);
		ipar1ch2[kbox + ipar1ch2_dim1 * 3] = *kp2;
		ich2par1[*kp2] = kbox;
		npb2[*kp2 + npb2_dim1] = np + 1;
		np += nb3;
		npb2[*kp2 + (npb2_dim1 << 1)] = np;
		ic2[*kp2] = ich3;
		jc2[*kp2] = jch3;
		xc2[*kp2] = xst + si3d;
		yc2[*kp2] = yst + sj3d;
	    } else {
		ipar1ch2[kbox + ipar1ch2_dim1 * 3] = 0;
	    }
/* Box 4 */
	    if (nb4 > 0) {
		++(*kp2);
		ipar1ch2[kbox + (ipar1ch2_dim1 << 2)] = *kp2;
		ich2par1[*kp2] = kbox;
		npb2[*kp2 + npb2_dim1] = np + 1;
		np += nb4;
		npb2[*kp2 + (npb2_dim1 << 1)] = np;
		ic2[*kp2] = ich4;
		jc2[*kp2] = jch4;
		xc2[*kp2] = xst + si4d;
		yc2[*kp2] = yst + sj4d;
	    } else {
		ipar1ch2[kbox + (ipar1ch2_dim1 << 2)] = 0;
	    }
	} else {
/*  * Box is childless */
	    ++(*kchildless1);
	    ichildless1[*kchildless1] = kbox;
	    imark1[kbox] = 0;
	    i__2 = n2;
	    for (i__ = n1; i__ <= i__2; ++i__) {
		vort1_1.xp[i__ - 1] = vort2_1.xn[i__ - 1];
		vort1_1.yp[i__ - 1] = vort2_1.yn[i__ - 1];
		vort1_1.gp[i__ - 1] = vort2_1.gn[i__ - 1];
		vel_1.uu[i__ - 1] = oldvel_1.uold[i__ - 1];
		vel_1.vv[i__ - 1] = oldvel_1.vold[i__ - 1];
		diff_1.gdiff[i__ - 1] = diff_1.gdold[i__ - 1];
		bf_1.bf_marker__[i__ - 1] = bf_marker_temp__[i__ - 1];
/* L22: */
	    }
	}
/* L2: */
    }
    for (i__ = 1; i__ <= 5000000; ++i__) {
	vort2_1.xn[i__ - 1] = vort1_1.xp[i__ - 1];
	vort2_1.yn[i__ - 1] = vort1_1.yp[i__ - 1];
	vort2_1.gn[i__ - 1] = vort1_1.gp[i__ - 1];
	oldvel_1.uold[i__ - 1] = vel_1.uu[i__ - 1];
	oldvel_1.vold[i__ - 1] = vel_1.vv[i__ - 1];
	diff_1.gdold[i__ - 1] = diff_1.gdiff[i__ - 1];
    }
    return 0;
} /* make_box__ */

