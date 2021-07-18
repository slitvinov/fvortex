/* ../wheneq.f -- translated by f2c (version 20160102).
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

/* Subroutine */ int wheneq_(integer *npart, integer *ibox, integer *itoss, 
	integer *imatch, integer *idummy, integer *nmatch)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;

/*     This routine finds elements of an array which match some key and saves */
/*     their indicies in a new array. */
/* ---------------------------------------------------------- */
    /* Parameter adjustments */
    --idummy;
    --ibox;

    /* Function Body */
    *nmatch = 0;
    i__1 = *npart;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (ibox[i__] == *imatch) {
	    ++(*nmatch);
	    idummy[*nmatch] = i__;
	}
/* L10: */
    }
    return 0;
} /* wheneq_ */

