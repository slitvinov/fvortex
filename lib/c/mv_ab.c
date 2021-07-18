/* ../mv_ab.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int mv_ab__(integer *irk)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Builtin functions */
    double atan(doublereal);

    /* Local variables */
    static integer i__;
    static real c1, c2;
    static integer in;
    static real pi, const__;

/*     Advance the particle positions using an Adams Bashforth scheme */
/*     based on velocities calculated in CONDIFF and VEL_EXT. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* -------------------------------------------------------------------- */
    in = 0;
    pi = atan(1.f) * 4.f;
/* Computing 2nd power */
    r__1 = part_1.ovrlp;
    const__ = part_1.gnu * (r__1 * r__1) / (pi * part_1.s2);
    if (*irk == 0) {
	c1 = 1.5f;
	c2 = .5f;
    } else {
	c1 = 2.f;
	c2 = 1.f;
    }
    i__1 = part_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vort1_1.xp[i__ - 1] = vort2_1.xn[i__ - 1] + params_1.dt * (c1 * 
		vel_1.uu[i__ - 1] - c2 * oldvel_1.uold[i__ - 1]);
	vort1_1.yp[i__ - 1] = vort2_1.yn[i__ - 1] + params_1.dt * (c1 * 
		vel_1.vv[i__ - 1] - c2 * oldvel_1.vold[i__ - 1]);
	vort1_1.gp[i__ - 1] = vort2_1.gn[i__ - 1] + params_1.dt * const__ * (
		c1 * diff_1.gdiff[i__ - 1] - c2 * diff_1.gdold[i__ - 1]);
    }
    return 0;
} /* mv_ab__ */

