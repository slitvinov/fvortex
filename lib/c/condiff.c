/* ../condiff.f -- translated by f2c (version 20160102).
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
    integer kp1;
    real ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8, ds9;
} times_;

#define times_1 times_

struct {
    real xc1[4], yc1[4], xc2[16], yc2[16], xc3[64], yc3[64], xc4[256], yc4[
	    256], xc5[1024], yc5[1024], xc6[4096], yc6[4096], xc7[16384], yc7[
	    16384], xc8[65536], yc8[65536], xc9[262144], yc9[262144];
} centre_;

#define centre_1 centre_

struct {
    integer npb1[8]	/* was [4][2] */, ic1[4], jc1[4], npb2[32]	/* 
	    was [16][2] */, ic2[16], jc2[16], npb3[128]	/* was [64][2] */, 
	    ic3[64], jc3[64], npb4[512]	/* was [256][2] */, ic4[256], jc4[256]
	    , npb5[2048]	/* was [1024][2] */, ic5[1024], jc5[1024], 
	    npb6[8192]	/* was [4096][2] */, ic6[4096], jc6[4096], npb7[32768]
	    	/* was [16384][2] */, ic7[16384], jc7[16384], npb8[131072]	
	    /* was [65536][2] */, ic8[65536], jc8[65536], npb9[524288]	/* 
	    was [262144][2] */, ic9[262144], jc9[262144];
} index11_;

#define index11_1 index11_

struct {
    integer ipar1ch2[16]	/* was [4][4] */, ipar2ch3[64]	/* was [16][4]
	     */, ipar3ch4[256]	/* was [64][4] */, ipar4ch5[1024]	/* 
	    was [256][4] */, ipar5ch6[4096]	/* was [1024][4] */, ipar6ch7[
	    16384]	/* was [4096][4] */, ipar7ch8[65536]	/* was [16384]
	    [4] */, ipar8ch9[262144]	/* was [65536][4] */;
} index22_;

#define index22_1 index22_

struct {
    integer ichildless8[65536], iparent8[65536], imark8[65536], ichildless7[
	    16384], iparent7[16384], imark7[16384], ichildless6[4096], 
	    iparent6[4096], imark6[4096], ichildless5[1024], iparent5[1024], 
	    imark5[1024], ichildless4[256], iparent4[256], imark4[256], 
	    ichildless3[64], iparent3[64], imark3[64], ichildless2[16], 
	    iparent2[16], imark2[16], ichildless1[4], iparent1[4], imark1[4], 
	    liststart[4];
} index33_;

#define index33_1 index33_

struct {
    real pr9[2097152]	/* was [262144][8] */, pi9[2097152]	/* was [
	    262144][8] */, pr8[524288]	/* was [65536][8] */, pi8[524288]	
	    /* was [65536][8] */, pr7[131072]	/* was [16384][8] */, pi7[
	    131072]	/* was [16384][8] */, pr6[32768]	/* was [4096][
	    8] */, pi6[32768]	/* was [4096][8] */, pr5[8192]	/* was [1024][
	    8] */, pi5[8192]	/* was [1024][8] */, pr4[2048]	/* was [256][
	    8] */, pi4[2048]	/* was [256][8] */, pr3[512]	/* was [64][8]
	     */, pi3[512]	/* was [64][8] */, pr2[128]	/* was [16][8]
	     */, pi2[128]	/* was [16][8] */, pr1[32]	/* was [4][8] 
	    */, pi1[32]	/* was [4][8] */;
} poles_;

#define poles_1 poles_

struct {
    real br9[1835008]	/* was [262144][7] */, bi9[1835008]	/* was [
	    262144][7] */, br8[458752]	/* was [65536][7] */, bi8[458752]	
	    /* was [65536][7] */, br7[114688]	/* was [16384][7] */, bi7[
	    114688]	/* was [16384][7] */, br6[28672]	/* was [4096][
	    7] */, bi6[28672]	/* was [4096][7] */, br5[7168]	/* was [1024][
	    7] */, bi5[7168]	/* was [1024][7] */, br4[1792]	/* was [256][
	    7] */, bi4[1792]	/* was [256][7] */, br3[448]	/* was [64][7]
	     */, bi3[448]	/* was [64][7] */, br2[112]	/* was [16][7]
	     */, bi2[112]	/* was [16][7] */, br1[28]	/* was [4][7] 
	    */, bi1[28]	/* was [4][7] */;
} boxexp_;

#define boxexp_1 boxexp_

struct {
    real x0, y0;
    integer limpar;
} geom_;

#define geom_1 geom_

struct {
    real visc_cutoff__;
} cutoff_;

#define cutoff_1 cutoff_

/* Table of constant values */

static integer c__16 = 16;
static integer c__64 = 64;
static integer c__256 = 256;
static integer c__1024 = 1024;
static integer c__4096 = 4096;
static integer c__16384 = 16384;
static integer c__65536 = 65536;
static integer c_b10 = 262144;
static integer c__4 = 4;
static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;

/* Subroutine */ int condiff_(integer *npart, integer *islip, real *
	visc_rmax__, integer *istats)
{
    /* System generated locals */
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    static integer kparent1, kparent2, kparent3, kparent4, kparent5, kparent6,
	     kparent7, kparent8;
    extern /* Subroutine */ int make_box__(integer *, real *, real *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, real *, real *, integer *);
    static integer i__, j;
    extern /* Subroutine */ int int_rest2__(integer *), int_rest3__(integer *)
	    , int_rest4__(integer *), int_rest5__(integer *), int_rest6__(
	    integer *), int_rest7__(integer *), int_rest8__(integer *), 
	    int_rest9__(integer *), ch_to_par__(integer *, real *, integer *, 
	    integer *, integer *, real *, real *, real *, real *, real *, 
	    real *), par_to_ch__(integer *, real *, integer *, real *, real *,
	     real *, real *, integer *, integer *);
    static real s0;
    extern /* Subroutine */ int childless9_(integer *);
    static integer kp2, kp3, kp4, kp5, kp6, kp7, kp8, kp9, kchildless1, 
	    kchildless2, kchildless3, kchildless4, kchildless5, kchildless6, 
	    kchildless7, kchildless8;
    static real gr1[32]	/* was [4][8] */, gi1[32]	/* was [4][8] */, gr2[
	    128]	/* was [16][8] */, gi2[128]	/* was [16][8] */, 
	    gr3[512]	/* was [64][8] */, gi3[512]	/* was [64][8] */, 
	    gr4[2048]	/* was [256][8] */, gi4[2048]	/* was [256][8] */, 
	    gr5[8192]	/* was [1024][8] */, gi5[8192]	/* was [1024][8] */, 
	    gr6[32768]	/* was [4096][8] */, gi6[32768]	/* was [4096][8] */, 
	    gr7[131072]	/* was [16384][8] */, gi7[131072]	/* was [16384]
	    [8] */, gr8[524288]	/* was [65536][8] */, gi8[524288]	/* 
	    was [65536][8] */, gr9[2097152]	/* was [262144][8] */, gi9[
	    2097152]	/* was [262144][8] */;
    extern /* Subroutine */ int exp_chdless__(integer *, integer *, real *, 
	    real *, integer *, integer *, real *, real *), int_chless1__(
	    integer *), int_chless2__(integer *), int_chless3__(integer *, 
	    integer *), int_chless4__(integer *, integer *), int_chless5__(
	    integer *, integer *), int_chless6__(integer *, integer *), 
	    int_chless7__(integer *, integer *), int_chless8__(integer *, 
	    integer *), int_chless9__(integer *, integer *), box_to_part__(
	    integer *, integer *, integer *, real *, real *, integer *, real *
	    , real *), box9_to_part__(integer *, integer *, real *, real *, 
	    integer *, real *, real *);
    static real xmin, ymin, xmax, ymax;
    extern /* Subroutine */ int box_1__(integer *, real *, real *, real *, 
	    integer *, integer *, integer *, real *, integer *, integer *);
    static integer ich2par1[16], ich3par2[64], ich4par3[256], ich5par4[1024], 
	    ich6par5[4096], ich7par6[16384], ich8par7[65536], ich9par8[262144]
	    ;
    extern /* Subroutine */ int box_dim__(integer *, real *, real *, real *, 
	    real *), zeroall_(void);

    /* Fortran I/O blocks */
    static cilist io___58 = { 0, 6, 0, 0, 0 };
    static cilist io___59 = { 0, 6, 0, 0, 0 };
    static cilist io___60 = { 0, 6, 0, 0, 0 };
    static cilist io___61 = { 0, 6, 0, 0, 0 };
    static cilist io___62 = { 0, 6, 0, 0, 0 };
    static cilist io___63 = { 0, 6, 0, 0, 0 };
    static cilist io___64 = { 0, 6, 0, 0, 0 };
    static cilist io___65 = { 0, 6, 0, 0, 0 };
    static cilist io___66 = { 0, 6, 0, 0, 0 };
    static cilist io___67 = { 0, 6, 0, 0, 0 };


/*     This subroutine is the driver of an implementation of an O(N) method for */
/*     fast computation of velocities and diffusion of a field of vortex blobs */
/*     using a box-box scheme. */
/* ***************************************************************************** */
/*     Coming in, data should be in XP,YP,GP,UU,VV. */
/*     At the end the particles have locations at XN,YN, GN                      * */
/*     and the initial velocity field is stored in: Uold,Vold.                 * */
/*     The new velocity field is placed in: UU,VV.                               * */
/*     The changed to the circulations due to diffusion are put in Gdiff.        * */
/*     The minimum number of particles for a box is given by limpar.             * */
/*     * */
/* ***************************************************************************** */
/* -----------------LOCATIONS -------------------- */
/* -----------------VELOCITIES -------------------- */
/* -----------------DIFFUSION -------------------- */
/* ----------------------------------------------------------------------------- */
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._. */
/*     _. */
/*     STAGE 0 : Zero all relevant arrays.                        _. */
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._. */
    cutoff_1.visc_cutoff__ = *visc_rmax__ * *visc_rmax__;
    zeroall_();
/* handles common blocks */
    times_1.kp1 = 0.f;
    kp2 = 0.f;
    kp3 = 0.f;
    kp4 = 0.f;
    kp5 = 0.f;
    kp6 = 0.f;
    kp7 = 0.f;
    kp8 = 0.f;
    kp9 = 0.f;
    kchildless1 = 0.f;
    kchildless2 = 0.f;
    kchildless3 = 0.f;
    kchildless4 = 0.f;
    kchildless5 = 0.f;
    kchildless6 = 0.f;
    kchildless7 = 0.f;
    kchildless8 = 0.f;
    kparent1 = 0.f;
    kparent2 = 0.f;
    kparent3 = 0.f;
    kparent4 = 0.f;
    kparent5 = 0.f;
    kparent6 = 0.f;
    kparent7 = 0.f;
    kparent8 = 0.f;
    for (i__ = 1; i__ <= 16; ++i__) {
	ich2par1[i__ - 1] = 0.f;
    }
    for (i__ = 1; i__ <= 64; ++i__) {
	ich3par2[i__ - 1] = 0.f;
    }
    for (i__ = 1; i__ <= 256; ++i__) {
	ich4par3[i__ - 1] = 0.f;
    }
    for (i__ = 1; i__ <= 1024; ++i__) {
	ich5par4[i__ - 1] = 0.f;
    }
    for (i__ = 1; i__ <= 4096; ++i__) {
	ich6par5[i__ - 1] = 0.f;
    }
    for (i__ = 1; i__ <= 16384; ++i__) {
	ich7par6[i__ - 1] = 0.f;
    }
    for (i__ = 1; i__ <= 65536; ++i__) {
	ich8par7[i__ - 1] = 0.f;
    }
    for (i__ = 1; i__ <= 262144; ++i__) {
	ich9par8[i__ - 1] = 0.f;
    }
    for (i__ = 0; i__ <= 7; ++i__) {
	for (j = 1; j <= 4; ++j) {
	    gr1[j + (i__ << 2) - 1] = 0.f;
	    gi1[j + (i__ << 2) - 1] = 0.f;
	}
	for (j = 1; j <= 16; ++j) {
	    gr2[j + (i__ << 4) - 1] = 0.f;
	    gi2[j + (i__ << 4) - 1] = 0.f;
	}
	for (j = 1; j <= 64; ++j) {
	    gr3[j + (i__ << 6) - 1] = 0.f;
	    gi3[j + (i__ << 6) - 1] = 0.f;
	}
	for (j = 1; j <= 256; ++j) {
	    gr4[j + (i__ << 8) - 1] = 0.f;
	    gi4[j + (i__ << 8) - 1] = 0.f;
	}
	for (j = 1; j <= 1024; ++j) {
	    gr5[j + (i__ << 10) - 1] = 0.f;
	    gi5[j + (i__ << 10) - 1] = 0.f;
	}
	for (j = 1; j <= 4096; ++j) {
	    gr6[j + (i__ << 12) - 1] = 0.f;
	    gi6[j + (i__ << 12) - 1] = 0.f;
	}
	for (j = 1; j <= 16384; ++j) {
	    gr7[j + (i__ << 14) - 1] = 0.f;
	    gi7[j + (i__ << 14) - 1] = 0.f;
	}
	for (j = 1; j <= 65536; ++j) {
	    gr8[j + (i__ << 16) - 1] = 0.f;
	    gi8[j + (i__ << 16) - 1] = 0.f;
	}
	for (j = 1; j <= 262144; ++j) {
	    gr9[j + (i__ << 18) - 1] = 0.f;
	    gi9[j + (i__ << 18) - 1] = 0.f;
	}
    }
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._. */
/*     _. */
/*     STAGE 1 : Divide the domain containing particles           _. */
/*     into a hierarchy of cells.                       _. */
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._. */
/*     Determine the  region  of  the main square */
    box_dim__(npart, &xmin, &xmax, &ymin, &ymax);
/* Computing MAX */
    r__3 = (r__1 = xmax - xmin, dabs(r__1)), r__4 = (r__2 = ymax - ymin, dabs(
	    r__2));
    s0 = dmax(r__3,r__4);
/* Side of square */
    geom_1.x0 = xmin - s0 * .01f;
/* Coords. of lower */
    geom_1.y0 = ymin - s0 * .01f;
/* left corner of square (origin) */
    s0 *= 1.02f;
/*     Level 1 */
/*     Divide the domain into 4 squares and find the particles in them */
    box_1__(npart, &s0, centre_1.xc1, centre_1.yc1, index11_1.ic1, 
	    index11_1.jc1, index11_1.npb1, &times_1.ds1, &times_1.kp1, 
	    index33_1.liststart);
/*     Level 2 - divide level 1 boxes into four squares */
    make_box__(&c__16, &times_1.ds1, &times_1.ds2, &times_1.kp1, &kp2, &
	    kparent1, &kchildless1, index11_1.ic1, index11_1.jc1, 
	    index11_1.npb1, index33_1.iparent1, index33_1.imark1, 
	    index22_1.ipar1ch2, ich2par1, index11_1.npb2, index11_1.ic2, 
	    index11_1.jc2, centre_1.xc2, centre_1.yc2, index33_1.ichildless1);
/*     Level 3 */
    if (kp2 == 0) {
	goto L1;
    }
    make_box__(&c__64, &times_1.ds2, &times_1.ds3, &kp2, &kp3, &kparent2, &
	    kchildless2, index11_1.ic2, index11_1.jc2, index11_1.npb2, 
	    index33_1.iparent2, index33_1.imark2, index22_1.ipar2ch3, 
	    ich3par2, index11_1.npb3, index11_1.ic3, index11_1.jc3, 
	    centre_1.xc3, centre_1.yc3, index33_1.ichildless2);
/*     Level 4 */
    if (kp3 == 0) {
	goto L1;
    }
    make_box__(&c__256, &times_1.ds3, &times_1.ds4, &kp3, &kp4, &kparent3, &
	    kchildless3, index11_1.ic3, index11_1.jc3, index11_1.npb3, 
	    index33_1.iparent3, index33_1.imark3, index22_1.ipar3ch4, 
	    ich4par3, index11_1.npb4, index11_1.ic4, index11_1.jc4, 
	    centre_1.xc4, centre_1.yc4, index33_1.ichildless3);
/*     Level 5 */
    if (kp4 == 0) {
	goto L1;
    }
    make_box__(&c__1024, &times_1.ds4, &times_1.ds5, &kp4, &kp5, &kparent4, &
	    kchildless4, index11_1.ic4, index11_1.jc4, index11_1.npb4, 
	    index33_1.iparent4, index33_1.imark4, index22_1.ipar4ch5, 
	    ich5par4, index11_1.npb5, index11_1.ic5, index11_1.jc5, 
	    centre_1.xc5, centre_1.yc5, index33_1.ichildless4);
/*     Level 6 */
    if (kp5 == 0) {
	goto L1;
    }
    make_box__(&c__4096, &times_1.ds5, &times_1.ds6, &kp5, &kp6, &kparent5, &
	    kchildless5, index11_1.ic5, index11_1.jc5, index11_1.npb5, 
	    index33_1.iparent5, index33_1.imark5, index22_1.ipar5ch6, 
	    ich6par5, index11_1.npb6, index11_1.ic6, index11_1.jc6, 
	    centre_1.xc6, centre_1.yc6, index33_1.ichildless5);
/*     Level 7 */
    if (kp6 == 0) {
	goto L1;
    }
    make_box__(&c__16384, &times_1.ds6, &times_1.ds7, &kp6, &kp7, &kparent6, &
	    kchildless6, index11_1.ic6, index11_1.jc6, index11_1.npb6, 
	    index33_1.iparent6, index33_1.imark6, index22_1.ipar6ch7, 
	    ich7par6, index11_1.npb7, index11_1.ic7, index11_1.jc7, 
	    centre_1.xc7, centre_1.yc7, index33_1.ichildless6);
/*     Level 8 */
    if (kp7 == 0) {
	goto L1;
    }
    make_box__(&c__65536, &times_1.ds7, &times_1.ds8, &kp7, &kp8, &kparent7, &
	    kchildless7, index11_1.ic7, index11_1.jc7, index11_1.npb7, 
	    index33_1.iparent7, index33_1.imark7, index22_1.ipar7ch8, 
	    ich8par7, index11_1.npb8, index11_1.ic8, index11_1.jc8, 
	    centre_1.xc8, centre_1.yc8, index33_1.ichildless7);
/*     Level 9 (finest boxes) */
    if (kp8 == 0) {
	goto L1;
    }
    make_box__(&c_b10, &times_1.ds8, &times_1.ds9, &kp8, &kp9, &kparent8, &
	    kchildless8, index11_1.ic8, index11_1.jc8, index11_1.npb8, 
	    index33_1.iparent8, index33_1.imark8, index22_1.ipar8ch9, 
	    ich9par8, index11_1.npb9, index11_1.ic9, index11_1.jc9, 
	    centre_1.xc9, centre_1.yc9, index33_1.ichildless8);
L1:
    times_1.ds2 = times_1.ds1 * .5f;
    times_1.ds3 = times_1.ds2 * .5f;
    times_1.ds4 = times_1.ds3 * .5f;
    times_1.ds5 = times_1.ds4 * .5f;
    times_1.ds6 = times_1.ds5 * .5f;
    times_1.ds7 = times_1.ds6 * .5f;
    times_1.ds8 = times_1.ds7 * .5f;
    times_1.ds9 = times_1.ds8 * .5f;
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._. */
/*     C */
/*     STAGE 2 : FORM THE MULTIPOLE EXPANSIONS  FOR EVERY  BOX             C */
/*     AT EACH  LEVEL                                            C */
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C */
/*     Form the expansions of the childless boxes first as these are finest levels */
/*     Level 1 (Coarsest) */
    if (kchildless1 != 0) {
	exp_chdless__(&c__4, &kchildless1, centre_1.xc1, centre_1.yc1, 
		index33_1.ichildless1, index11_1.npb1, poles_1.pr1, 
		poles_1.pi1);
    }
/*     Level 2 */
    if (kchildless2 != 0) {
	exp_chdless__(&c__16, &kchildless2, centre_1.xc2, centre_1.yc2, 
		index33_1.ichildless2, index11_1.npb2, poles_1.pr2, 
		poles_1.pi2);
    }
/*     Level 3 */
    if (kchildless3 != 0) {
	exp_chdless__(&c__64, &kchildless3, centre_1.xc3, centre_1.yc3, 
		index33_1.ichildless3, index11_1.npb3, poles_1.pr3, 
		poles_1.pi3);
    }
/*     Level 4 */
    if (kchildless4 != 0) {
	exp_chdless__(&c__256, &kchildless4, centre_1.xc4, centre_1.yc4, 
		index33_1.ichildless4, index11_1.npb4, poles_1.pr4, 
		poles_1.pi4);
    }
/*     Level 5 */
    if (kchildless5 != 0) {
	exp_chdless__(&c__1024, &kchildless5, centre_1.xc5, centre_1.yc5, 
		index33_1.ichildless5, index11_1.npb5, poles_1.pr5, 
		poles_1.pi5);
    }
/*     Level 6 */
    if (kchildless6 != 0) {
	exp_chdless__(&c__4096, &kchildless6, centre_1.xc6, centre_1.yc6, 
		index33_1.ichildless6, index11_1.npb6, poles_1.pr6, 
		poles_1.pi6);
    }
/*     Level 7 */
    if (kchildless7 != 0) {
	exp_chdless__(&c__16384, &kchildless7, centre_1.xc7, centre_1.yc7, 
		index33_1.ichildless7, index11_1.npb7, poles_1.pr7, 
		poles_1.pi7);
    }
/*     Level 8 */
    if (kchildless8 != 0) {
	exp_chdless__(&c__65536, &kchildless8, centre_1.xc8, centre_1.yc8, 
		index33_1.ichildless8, index11_1.npb8, poles_1.pr8, 
		poles_1.pi8);
    }
/*     Level 9  (finest) */
    if (kp9 != 0) {
	childless9_(&kp9);
    }
/*     Form the expansions of the parent boxes now */
/*     Level 8 */
    if (kparent8 != 0) {
	ch_to_par__(&c_b10, &times_1.ds9, &kparent8, index33_1.iparent8, 
		index22_1.ipar8ch9, poles_1.pr9, poles_1.pi9, poles_1.pr8, 
		poles_1.pi8, gr8, gi8);
    }
/*     Level 7 */
    if (kparent7 != 0) {
	ch_to_par__(&c__65536, &times_1.ds8, &kparent7, index33_1.iparent7, 
		index22_1.ipar7ch8, poles_1.pr8, poles_1.pi8, poles_1.pr7, 
		poles_1.pi7, gr7, gi7);
    }
/*     Level 6 */
    if (kparent6 != 0) {
	ch_to_par__(&c__16384, &times_1.ds7, &kparent6, index33_1.iparent6, 
		index22_1.ipar6ch7, poles_1.pr7, poles_1.pi7, poles_1.pr6, 
		poles_1.pi6, gr6, gi6);
    }
/*     Level 5 */
    if (kparent5 != 0) {
	ch_to_par__(&c__4096, &times_1.ds6, &kparent5, index33_1.iparent5, 
		index22_1.ipar5ch6, poles_1.pr6, poles_1.pi6, poles_1.pr5, 
		poles_1.pi5, gr5, gi5);
    }
/*     Level 4 */
    if (kparent4 != 0) {
	ch_to_par__(&c__1024, &times_1.ds5, &kparent4, index33_1.iparent4, 
		index22_1.ipar4ch5, poles_1.pr5, poles_1.pi5, poles_1.pr4, 
		poles_1.pi4, gr4, gi4);
    }
/*     Level 3 */
    if (kparent3 != 0) {
	ch_to_par__(&c__256, &times_1.ds4, &kparent3, index33_1.iparent3, 
		index22_1.ipar3ch4, poles_1.pr4, poles_1.pi4, poles_1.pr3, 
		poles_1.pi3, gr3, gi3);
    }
/*     Level 2 */
    if (kparent2 != 0) {
	ch_to_par__(&c__64, &times_1.ds3, &kparent2, index33_1.iparent2, 
		index22_1.ipar2ch3, poles_1.pr3, poles_1.pi3, poles_1.pr2, 
		poles_1.pi2, gr2, gi2);
    }
/*     Level 1 */
    if (kparent1 != 0) {
	ch_to_par__(&c__16, &times_1.ds2, &kparent1, index33_1.iparent1, 
		index22_1.ipar1ch2, poles_1.pr2, poles_1.pi2, poles_1.pr1, 
		poles_1.pi1, gr1, gi1);
    }
/* --   stop here if only building the interaction tree */
    if (*islip == 0) {
	return 0;
    }
    for (i__ = 1; i__ <= 5000000; ++i__) {
	vel_1.uu[i__ - 1] = 0.f;
	vel_1.vv[i__ - 1] = 0.f;
	diff_1.gdiff[i__ - 1] = 0.f;
    }
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._. */
/*     C */
/*     STAGE 3 : Now allow the particles and boxes to interact,            C */
/*     finding the induced velocities and circulation exchange   C */
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C */
/* L107: */
    if (*istats == 1) {
	s_wsle(&io___58);
	do_lio(&c__9, &c__1, "************TREE STATS*************", (ftnlen)
		35);
	e_wsle();
	s_wsle(&io___59);
	do_lio(&c__9, &c__1, "Level 1: total boxes=", (ftnlen)21);
	do_lio(&c__3, &c__1, (char *)&times_1.kp1, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ", childless=", (ftnlen)12);
	do_lio(&c__3, &c__1, (char *)&kchildless1, (ftnlen)sizeof(integer));
	e_wsle();
	s_wsle(&io___60);
	do_lio(&c__9, &c__1, "Level 2: total boxes=", (ftnlen)21);
	do_lio(&c__3, &c__1, (char *)&kp2, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ", childless=", (ftnlen)12);
	do_lio(&c__3, &c__1, (char *)&kchildless2, (ftnlen)sizeof(integer));
	e_wsle();
	s_wsle(&io___61);
	do_lio(&c__9, &c__1, "Level 3: total boxes=", (ftnlen)21);
	do_lio(&c__3, &c__1, (char *)&kp3, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ", childless=", (ftnlen)12);
	do_lio(&c__3, &c__1, (char *)&kchildless3, (ftnlen)sizeof(integer));
	e_wsle();
	s_wsle(&io___62);
	do_lio(&c__9, &c__1, "Level 4: total boxes=", (ftnlen)21);
	do_lio(&c__3, &c__1, (char *)&kp4, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ", childless=", (ftnlen)12);
	do_lio(&c__3, &c__1, (char *)&kchildless4, (ftnlen)sizeof(integer));
	e_wsle();
	s_wsle(&io___63);
	do_lio(&c__9, &c__1, "Level 5: total boxes=", (ftnlen)21);
	do_lio(&c__3, &c__1, (char *)&kp5, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ", childless=", (ftnlen)12);
	do_lio(&c__3, &c__1, (char *)&kchildless5, (ftnlen)sizeof(integer));
	e_wsle();
	s_wsle(&io___64);
	do_lio(&c__9, &c__1, "Level 6: total boxes=", (ftnlen)21);
	do_lio(&c__3, &c__1, (char *)&kp6, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ", childless=", (ftnlen)12);
	do_lio(&c__3, &c__1, (char *)&kchildless6, (ftnlen)sizeof(integer));
	e_wsle();
	s_wsle(&io___65);
	do_lio(&c__9, &c__1, "Level 7: total boxes=", (ftnlen)21);
	do_lio(&c__3, &c__1, (char *)&kp7, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ", childless=", (ftnlen)12);
	do_lio(&c__3, &c__1, (char *)&kchildless7, (ftnlen)sizeof(integer));
	e_wsle();
	s_wsle(&io___66);
	do_lio(&c__9, &c__1, "Level 8: total boxes=", (ftnlen)21);
	do_lio(&c__3, &c__1, (char *)&kp8, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ", childless=", (ftnlen)12);
	do_lio(&c__3, &c__1, (char *)&kchildless8, (ftnlen)sizeof(integer));
	e_wsle();
	s_wsle(&io___67);
	do_lio(&c__9, &c__1, "Level 9: total boxes=", (ftnlen)21);
	do_lio(&c__3, &c__1, (char *)&kp9, (ftnlen)sizeof(integer));
	do_lio(&c__9, &c__1, ", childless=", (ftnlen)12);
	do_lio(&c__3, &c__1, (char *)&kp9, (ftnlen)sizeof(integer));
	e_wsle();
    }
/*     Level 1 */
    if (kchildless1 != 0) {
	int_chless1__(&kchildless1);
    }
/*     2nd level */
    if (kchildless2 != 0) {
	int_chless2__(&kchildless2);
    }
    if (kp2 != 0) {
	int_rest2__(&kp2);
    }
/*     3rd level */
    if (kchildless3 != 0) {
	int_chless3__(&kp2, &kchildless3);
    }
    if (kp3 != 0) {
	int_rest3__(&kp3);
    }
/*     4th level */
    if (kchildless4 != 0) {
	int_chless4__(&kp3, &kchildless4);
    }
    if (kp4 != 0) {
	int_rest4__(&kp4);
    }
/*     5th level */
    if (kchildless5 != 0) {
	int_chless5__(&kp4, &kchildless5);
    }
    if (kp5 != 0) {
	int_rest5__(&kp5);
    }
/*     6th level */
    if (kchildless6 != 0) {
	int_chless6__(&kp5, &kchildless6);
    }
    if (kp6 != 0) {
	int_rest6__(&kp6);
    }
/*     7th level */
    if (kchildless7 != 0) {
	int_chless7__(&kp6, &kchildless7);
    }
    if (kp7 != 0) {
	int_rest7__(&kp7);
    }
/*     8th level */
    if (kchildless8 != 0) {
	int_chless8__(&kp7, &kchildless8);
    }
    if (kp8 != 0) {
	int_rest8__(&kp8);
    }
/*     9th level (finest) */
    if (kp9 != 0) {
	int_chless9__(&kp8, &kp9);
    }
    if (kp9 != 0) {
	int_rest9__(&kp9);
    }
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._. */
/*     C */
/*     STAGE 4 : TRANSFER THE EXPANSIONS OF BOXES FROM                     C */
/*     COARSER TO FINER LEVELS                                   C */
/*     C */
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C */
/* ***  Level 2 -> Level 3 */
    if (kp3 != 0) {
	par_to_ch__(&c__16, &times_1.ds3, &kp3, boxexp_1.br2, boxexp_1.bi2, 
		boxexp_1.br3, boxexp_1.bi3, ich3par2, index22_1.ipar2ch3);
    }
/* ***  Level 3 -> Level 4 */
    if (kp4 != 0) {
	par_to_ch__(&c__64, &times_1.ds4, &kp4, boxexp_1.br3, boxexp_1.bi3, 
		boxexp_1.br4, boxexp_1.bi4, ich4par3, index22_1.ipar3ch4);
    }
/* ***  Level 4 -> Level 5 */
    if (kp5 != 0) {
	par_to_ch__(&c__256, &times_1.ds5, &kp5, boxexp_1.br4, boxexp_1.bi4, 
		boxexp_1.br5, boxexp_1.bi5, ich5par4, index22_1.ipar4ch5);
    }
/* ***  Level 5 -> Level 6 */
    if (kp6 != 0) {
	par_to_ch__(&c__1024, &times_1.ds6, &kp6, boxexp_1.br5, boxexp_1.bi5, 
		boxexp_1.br6, boxexp_1.bi6, ich6par5, index22_1.ipar5ch6);
    }
/* ***  Level 6 -> Level 7 */
    if (kp7 != 0) {
	par_to_ch__(&c__4096, &times_1.ds7, &kp7, boxexp_1.br6, boxexp_1.bi6, 
		boxexp_1.br7, boxexp_1.bi7, ich7par6, index22_1.ipar6ch7);
    }
/* ***  Level 7 -> Level 8 */
    if (kp8 != 0) {
	par_to_ch__(&c__16384, &times_1.ds8, &kp8, boxexp_1.br7, boxexp_1.bi7,
		 boxexp_1.br8, boxexp_1.bi8, ich8par7, index22_1.ipar7ch8);
    }
/* ***  Level 8 -> Level 9 */
    if (kp9 != 0) {
	par_to_ch__(&c__65536, &times_1.ds9, &kp9, boxexp_1.br8, boxexp_1.bi8,
		 boxexp_1.br9, boxexp_1.bi9, ich9par8, index22_1.ipar8ch9);
    }
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._. */
/*     C */
/*     STAGE 5 : FOR EACH PARTCLE IN A CHILDLESS BOX                       C */
/*     COMPUTE THE INDUCED VELOCITY FROM THE BOX EXPANSIONS      C */
/*     C */
/*     _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._C */
/* ***  Level 2 */
    if (kchildless2 != 0) {
	box_to_part__(&c__16, &kchildless2, index33_1.ichildless2, 
		centre_1.xc2, centre_1.yc2, index11_1.npb2, boxexp_1.br2, 
		boxexp_1.bi2);
    }
/* ***  Level 3 */
    if (kchildless3 != 0) {
	box_to_part__(&c__64, &kchildless3, index33_1.ichildless3, 
		centre_1.xc3, centre_1.yc3, index11_1.npb3, boxexp_1.br3, 
		boxexp_1.bi3);
    }
/* ***  Level 4 */
    if (kchildless4 != 0) {
	box_to_part__(&c__256, &kchildless4, index33_1.ichildless4, 
		centre_1.xc4, centre_1.yc4, index11_1.npb4, boxexp_1.br4, 
		boxexp_1.bi4);
    }
/* ***  Level 5 */
    if (kchildless5 != 0) {
	box_to_part__(&c__1024, &kchildless5, index33_1.ichildless5, 
		centre_1.xc5, centre_1.yc5, index11_1.npb5, boxexp_1.br5, 
		boxexp_1.bi5);
    }
/* ***  Level 6 */
    if (kchildless6 != 0) {
	box_to_part__(&c__4096, &kchildless6, index33_1.ichildless6, 
		centre_1.xc6, centre_1.yc6, index11_1.npb6, boxexp_1.br6, 
		boxexp_1.bi6);
    }
/* ***  Level 7 */
    if (kchildless7 != 0) {
	box_to_part__(&c__16384, &kchildless7, index33_1.ichildless7, 
		centre_1.xc7, centre_1.yc7, index11_1.npb7, boxexp_1.br7, 
		boxexp_1.bi7);
    }
/* ***  Level 8 */
    if (kchildless8 != 0) {
	box_to_part__(&c__65536, &kchildless8, index33_1.ichildless8, 
		centre_1.xc8, centre_1.yc8, index11_1.npb8, boxexp_1.br8, 
		boxexp_1.bi8);
    }
/* ***  Level 9 */
    if (kp9 != 0) {
	box9_to_part__(&c_b10, &kp9, centre_1.xc9, centre_1.yc9, 
		index11_1.npb9, boxexp_1.br9, boxexp_1.bi9);
    }
    return 0;
} /* condiff_ */

