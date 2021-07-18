/* ../vel_ext.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int vel_ext__(real *tm)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    extern doublereal velocity_x__(real *), velocity_y__(real *);
    static real xni, yni, arn2, uext, vext, alpha;
    extern doublereal omega_(real *);
    static real x_vel__, y_vel__;

/*     This routine adds the irrotational components of the velocity field */
/*     (which are not represented by the vorticity field) such as a free stream. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ---------------------------------------- */
    alpha = omega_(tm);
    x_vel__ = velocity_x__(tm);
    y_vel__ = velocity_y__(tm);
    i__1 = part_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	xni = vort2_1.xn[i__ - 1];
	yni = vort2_1.yn[i__ - 1];
	arn2 = alpha / (xni * xni + yni * yni);
/* from rotation */
	uext = -yni * arn2 + x_vel__;
	vext = xni * arn2 + y_vel__;
	vel_1.uu[i__ - 1] += uext;
	vel_1.vv[i__ - 1] += vext;
/* L1: */
    }
    return 0;
} /* vel_ext__ */

