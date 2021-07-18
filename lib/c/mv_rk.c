/* ../mv_rk.f -- translated by f2c (version 20160102).
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

struct {
    integer xfree;
    real xmass, xdamp, xspring, xforce, x_const__, x_amp__, x_freq__, 
	    x_phase__;
    integer yfree;
    real ymass, ydamp, yspring, yforce, y_amp__, y_freq__, y_phase__;
    integer wfree;
    real wmass, wdamp, wspring, w_fixed__, w_const__, w_amp__, w_freq__, 
	    w_phase__, last_x__, last_u__, last_udot__, last_y__, last_v__, 
	    last_vdot__, last_th__, last_w__, last_wdot__;
} motion_;

#define motion_1 motion_

/* Table of constant values */

static integer c__0 = 0;
static real c_b4 = 9999.f;
static integer c__1 = 1;

/* Subroutine */ int mv_rk__(real *visc_rmax__)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Builtin functions */
    double atan(doublereal);

    /* Local variables */
    extern /* Subroutine */ int update_position__(real *);
    static integer i__, in;
    static real pi, const__;
    extern /* Subroutine */ int condiff_(integer *, integer *, real *, 
	    integer *), vel_ext__(real *);

/*     Advance the particle positions using a Runge-Kutta scheme. */
/*     Must do after a remesh rather than Adams Bashforth because */
/*     velocities from the previous step are not available. */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ------------------------------------------------------------------ */
    in = 0;
    pi = atan(1.f) * 4.f;
/* Computing 2nd power */
    r__1 = part_1.ovrlp;
    const__ = part_1.gnu * (r__1 * r__1) / (pi * part_1.s2);
/* ---  1st Substep, predictor step, goes to dt/2 */
    i__1 = part_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vort1_1.xp[i__ - 1] = vort2_1.xn[i__ - 1] + params_1.dt * .5f * 
		vel_1.uu[i__ - 1];
	vort1_1.yp[i__ - 1] = vort2_1.yn[i__ - 1] + params_1.dt * .5f * 
		vel_1.vv[i__ - 1];
	vort1_1.gp[i__ - 1] = vort2_1.gn[i__ - 1] + params_1.dt * .5f * 
		const__ * diff_1.gdiff[i__ - 1];
/* L1: */
    }
/* --   If any free motion, need to update now */
    if (motion_1.xfree == 1 || motion_1.yfree == 1 || motion_1.wfree == 1) {
/* -- Rebuild the tree */
	condiff_(&part_1.np, &c__0, &c_b4, &c__0);
	r__1 = params_1.dt * .5f;
	update_position__(&r__1);
    }
/* ---  2nd Substep, corrector step, uses dt/2 values to get to dt */
    in = 0;
/* --   Need to rebuild the interaction tree */
    condiff_(&part_1.np, &c__1, visc_rmax__, &c__0);
    r__1 = params_1.time + params_1.dt * .5f;
    vel_ext__(&r__1);
    i__1 = part_1.np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vort1_1.xp[i__ - 1] = vort2_1.xn[i__ - 1] + params_1.dt * (vel_1.uu[
		i__ - 1] - oldvel_1.uold[i__ - 1] * .5f);
	vort1_1.yp[i__ - 1] = vort2_1.yn[i__ - 1] + params_1.dt * (vel_1.vv[
		i__ - 1] - oldvel_1.vold[i__ - 1] * .5f);
	vort1_1.gp[i__ - 1] = vort2_1.gn[i__ - 1] + params_1.dt * const__ * (
		diff_1.gdiff[i__ - 1] - diff_1.gdold[i__ - 1] * .5f);
/* L2: */
    }
    return 0;
} /* mv_rk__ */

