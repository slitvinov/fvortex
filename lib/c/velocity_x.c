/* ../velocity_x.f -- translated by f2c (version 20160102).
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

doublereal velocity_x__(real *time)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double sin(doublereal);

/*     Gives the freestream velocity in the x-direction at time */
/* ----------------------------------------------- */
    if (motion_1.xfree == 1) {
	ret_val = motion_1.last_u__;
    } else {
	ret_val = motion_1.x_const__ + motion_1.x_amp__ * sin(
		motion_1.x_freq__ * *time + motion_1.x_phase__);
    }
    return ret_val;
} /* velocity_x__ */

