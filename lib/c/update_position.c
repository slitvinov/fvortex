/* ../update_position.f -- translated by f2c (version 20160102).
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
    real last_cdp__, last_cdf__, last_clp__, last_clf__, last_cm__;
} force_;

#define force_1 force_

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

/* Subroutine */ int update_position__(real *delta_t__)
{
    /* Builtin functions */
    double atan(doublereal);

    /* Local variables */
    static real th_tilde__, old_sheet__, cd, cl, pi, x_tilde__, u_tilde__, 
	    y_tilde__, v_tilde__, w_tilde__;

/*     This routine updates the motion of the body in all directions in */
/*     which it is not considered fixed. */
/*     Signs get a bit messy becuase u,v,udot,vdot are for the freestream, */
/*     which is (-) the body motion. */
/* ---------------------------------------- */
    pi = atan(1.f) * 4.f;
    if (motion_1.xfree == 1) {
	x_tilde__ = motion_1.last_x__ - *delta_t__ * motion_1.last_u__;
	u_tilde__ = motion_1.last_u__ + *delta_t__ * motion_1.last_udot__;
	old_sheet__ = pi * -2.f * motion_1.last_udot__;
	cd = force_1.last_cdp__ + force_1.last_cdf__ + old_sheet__;
	motion_1.last_udot__ = -(1.f / (motion_1.xmass + pi)) * (cd + (
		motion_1.xmass - pi) * motion_1.xforce + motion_1.xdamp * 
		u_tilde__ - motion_1.xspring * x_tilde__);
	motion_1.last_u__ += *delta_t__ * motion_1.last_udot__;
	motion_1.last_x__ -= *delta_t__ * motion_1.last_u__;
    }
    if (motion_1.yfree == 1) {
	y_tilde__ = motion_1.last_y__ - *delta_t__ * motion_1.last_v__;
	v_tilde__ = motion_1.last_v__ + *delta_t__ * motion_1.last_vdot__;
	old_sheet__ = pi * -2.f * motion_1.last_vdot__;
	cl = force_1.last_clp__ + force_1.last_clf__ + old_sheet__;
	motion_1.last_vdot__ = -(1.f / (motion_1.ymass + pi)) * (cl + (
		motion_1.ymass - pi) * motion_1.yforce + motion_1.ydamp * 
		v_tilde__ - motion_1.yspring * y_tilde__);
	motion_1.last_v__ += *delta_t__ * motion_1.last_vdot__;
	motion_1.last_y__ -= *delta_t__ * motion_1.last_v__;
    }
    if (motion_1.wfree == 1) {
	th_tilde__ = motion_1.last_th__ + *delta_t__ * motion_1.last_w__;
	w_tilde__ = motion_1.last_w__ + *delta_t__ * motion_1.last_wdot__;
	motion_1.last_wdot__ = 1.f / motion_1.wmass * (force_1.last_cm__ - 
		motion_1.wdamp * w_tilde__ - motion_1.wspring * (th_tilde__ - 
		motion_1.w_fixed__));
	motion_1.last_w__ += *delta_t__ * motion_1.last_wdot__;
	motion_1.last_th__ += *delta_t__ * motion_1.last_w__;
    }
    return 0;
} /* update_position__ */

