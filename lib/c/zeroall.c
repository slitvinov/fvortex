/* ../zeroall.f -- translated by f2c (version 20160102).
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
    real xbox[60001], ybox[60001], prbox[480008]	/* was [60001][8] */, 
	    pibox[480008]	/* was [60001][8] */, xt[3450000], yt[3450000]
	    , gt[3450000];
    integer it[3450000];
} tempor_;

#define tempor_1 tempor_

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

/* Subroutine */ int zeroall_(void)
{
    static integer i__, j;

/* ----------------------------------------------------------------------- */
/*     This subroutine zeroes the various arrays used in identifying the tree */
/*     This subroutine zeroes the various arrays used in identifying the tree */
/* ----------------------------------------------------------------------- */
    for (i__ = 1; i__ <= 60001; ++i__) {
	tempor_1.xbox[i__ - 1] = 0.f;
	tempor_1.ybox[i__ - 1] = 0.f;
    }
    for (i__ = 0; i__ <= 7; ++i__) {
	for (j = 1; j <= 60001; ++j) {
	    tempor_1.prbox[j + i__ * 60001 - 1] = 0.f;
	    tempor_1.pibox[j + i__ * 60001 - 1] = 0.f;
	}
    }
    for (i__ = 1; i__ <= 3450000; ++i__) {
	tempor_1.xt[i__ - 1] = 0.f;
	tempor_1.yt[i__ - 1] = 0.f;
	tempor_1.gt[i__ - 1] = 0.f;
	tempor_1.it[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 4; ++i__) {
	centre_1.xc1[i__ - 1] = 0.f;
	centre_1.yc1[i__ - 1] = 0.f;
	index11_1.npb1[i__ - 1] = 0;
	index11_1.npb1[i__ + 3] = 0;
	index11_1.ic1[i__ - 1] = 0;
	index33_1.ichildless1[i__ - 1] = 0;
	index33_1.iparent1[i__ - 1] = 0;
	index33_1.imark1[i__ - 1] = 0;
	index33_1.liststart[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 16; ++i__) {
	centre_1.xc2[i__ - 1] = 0.f;
	centre_1.yc2[i__ - 1] = 0.f;
	index11_1.npb2[i__ - 1] = 0;
	index11_1.npb2[i__ + 15] = 0;
	index11_1.ic2[i__ - 1] = 0;
	index11_1.jc2[i__ - 1] = 0;
	index33_1.ichildless2[i__ - 1] = 0;
	index33_1.iparent2[i__ - 1] = 0;
	index33_1.imark2[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 64; ++i__) {
	centre_1.xc3[i__ - 1] = 0.f;
	centre_1.yc3[i__ - 1] = 0.f;
	index11_1.npb3[i__ - 1] = 0;
	index11_1.npb3[i__ + 63] = 0;
	index11_1.ic3[i__ - 1] = 0;
	index11_1.jc3[i__ - 1] = 0;
	index33_1.ichildless3[i__ - 1] = 0;
	index33_1.iparent3[i__ - 1] = 0;
	index33_1.imark3[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 256; ++i__) {
	centre_1.xc4[i__ - 1] = 0.f;
	centre_1.yc4[i__ - 1] = 0.f;
	index11_1.npb4[i__ - 1] = 0;
	index11_1.npb4[i__ + 255] = 0;
	index11_1.ic4[i__ - 1] = 0;
	index11_1.jc4[i__ - 1] = 0;
	index33_1.ichildless4[i__ - 1] = 0;
	index33_1.iparent4[i__ - 1] = 0;
	index33_1.imark4[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 1024; ++i__) {
	centre_1.xc5[i__ - 1] = 0.f;
	centre_1.yc5[i__ - 1] = 0.f;
	index11_1.npb5[i__ - 1] = 0;
	index11_1.npb5[i__ + 1023] = 0;
	index11_1.ic5[i__ - 1] = 0;
	index11_1.jc5[i__ - 1] = 0;
	index33_1.ichildless5[i__ - 1] = 0;
	index33_1.iparent5[i__ - 1] = 0;
	index33_1.imark5[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 4096; ++i__) {
	centre_1.xc6[i__ - 1] = 0.f;
	centre_1.yc6[i__ - 1] = 0.f;
	index11_1.npb6[i__ - 1] = 0;
	index11_1.npb6[i__ + 4095] = 0;
	index11_1.ic6[i__ - 1] = 0;
	index11_1.jc6[i__ - 1] = 0;
	index33_1.ichildless6[i__ - 1] = 0;
	index33_1.iparent6[i__ - 1] = 0;
	index33_1.imark6[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 16384; ++i__) {
	centre_1.xc7[i__ - 1] = 0.f;
	centre_1.yc7[i__ - 1] = 0.f;
	index11_1.npb7[i__ - 1] = 0;
	index11_1.npb7[i__ + 16383] = 0;
	index11_1.ic7[i__ - 1] = 0;
	index11_1.jc7[i__ - 1] = 0;
	index33_1.ichildless7[i__ - 1] = 0;
	index33_1.iparent7[i__ - 1] = 0;
	index33_1.imark7[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 65536; ++i__) {
	centre_1.xc8[i__ - 1] = 0.f;
	centre_1.yc8[i__ - 1] = 0.f;
	index11_1.npb8[i__ - 1] = 0;
	index11_1.npb8[i__ + 65535] = 0;
	index11_1.ic8[i__ - 1] = 0;
	index11_1.jc8[i__ - 1] = 0;
	index33_1.ichildless8[i__ - 1] = 0;
	index33_1.iparent8[i__ - 1] = 0;
	index33_1.imark8[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 262144; ++i__) {
	centre_1.xc9[i__ - 1] = 0.f;
	centre_1.yc9[i__ - 1] = 0.f;
	index11_1.npb9[i__ - 1] = 0;
	index11_1.npb9[i__ + 262143] = 0;
	index11_1.ic9[i__ - 1] = 0;
	index11_1.jc9[i__ - 1] = 0;
    }
    for (i__ = 1; i__ <= 4; ++i__) {
	for (j = 1; j <= 4; ++j) {
	    index22_1.ipar1ch2[j + (i__ << 2) - 5] = 0;
	}
	for (j = 1; j <= 16; ++j) {
	    index22_1.ipar2ch3[j + (i__ << 4) - 17] = 0;
	}
	for (j = 1; j <= 64; ++j) {
	    index22_1.ipar3ch4[j + (i__ << 6) - 65] = 0;
	}
	for (j = 1; j <= 256; ++j) {
	    index22_1.ipar4ch5[j + (i__ << 8) - 257] = 0;
	}
	for (j = 1; j <= 1024; ++j) {
	    index22_1.ipar5ch6[j + (i__ << 10) - 1025] = 0;
	}
	for (j = 1; j <= 4096; ++j) {
	    index22_1.ipar6ch7[j + (i__ << 12) - 4097] = 0;
	}
	for (j = 1; j <= 16384; ++j) {
	    index22_1.ipar7ch8[j + (i__ << 14) - 16385] = 0;
	}
	for (j = 1; j <= 65536; ++j) {
	    index22_1.ipar8ch9[j + (i__ << 16) - 65537] = 0;
	}
    }
    for (i__ = 0; i__ <= 7; ++i__) {
	for (j = 1; j <= 4; ++j) {
	    poles_1.pr1[j + (i__ << 2) - 1] = 0.f;
	    poles_1.pi1[j + (i__ << 2) - 1] = 0.f;
	}
	for (j = 1; j <= 16; ++j) {
	    poles_1.pr2[j + (i__ << 4) - 1] = 0.f;
	    poles_1.pi2[j + (i__ << 4) - 1] = 0.f;
	}
	for (j = 1; j <= 64; ++j) {
	    poles_1.pr3[j + (i__ << 6) - 1] = 0.f;
	    poles_1.pi3[j + (i__ << 6) - 1] = 0.f;
	}
	for (j = 1; j <= 256; ++j) {
	    poles_1.pr4[j + (i__ << 8) - 1] = 0.f;
	    poles_1.pi4[j + (i__ << 8) - 1] = 0.f;
	}
	for (j = 1; j <= 1024; ++j) {
	    poles_1.pr5[j + (i__ << 10) - 1] = 0.f;
	    poles_1.pi5[j + (i__ << 10) - 1] = 0.f;
	}
	for (j = 1; j <= 4096; ++j) {
	    poles_1.pr6[j + (i__ << 12) - 1] = 0.f;
	    poles_1.pi6[j + (i__ << 12) - 1] = 0.f;
	}
	for (j = 1; j <= 16384; ++j) {
	    poles_1.pr7[j + (i__ << 14) - 1] = 0.f;
	    poles_1.pi7[j + (i__ << 14) - 1] = 0.f;
	}
	for (j = 1; j <= 65536; ++j) {
	    poles_1.pr8[j + (i__ << 16) - 1] = 0.f;
	    poles_1.pi8[j + (i__ << 16) - 1] = 0.f;
	}
	for (j = 1; j <= 262144; ++j) {
	    poles_1.pr9[j + (i__ << 18) - 1] = 0.f;
	    poles_1.pi9[j + (i__ << 18) - 1] = 0.f;
	}
    }
    for (i__ = 1; i__ <= 7; ++i__) {
	for (j = 1; j <= 4; ++j) {
	    boxexp_1.br1[j + (i__ << 2) - 5] = 0.f;
	    boxexp_1.bi1[j + (i__ << 2) - 5] = 0.f;
	}
	for (j = 1; j <= 16; ++j) {
	    boxexp_1.br2[j + (i__ << 4) - 17] = 0.f;
	    boxexp_1.bi2[j + (i__ << 4) - 17] = 0.f;
	}
	for (j = 1; j <= 64; ++j) {
	    boxexp_1.br3[j + (i__ << 6) - 65] = 0.f;
	    boxexp_1.bi3[j + (i__ << 6) - 65] = 0.f;
	}
	for (j = 1; j <= 256; ++j) {
	    boxexp_1.br4[j + (i__ << 8) - 257] = 0.f;
	    boxexp_1.bi4[j + (i__ << 8) - 257] = 0.f;
	}
	for (j = 1; j <= 1024; ++j) {
	    boxexp_1.br5[j + (i__ << 10) - 1025] = 0.f;
	    boxexp_1.bi5[j + (i__ << 10) - 1025] = 0.f;
	}
	for (j = 1; j <= 4096; ++j) {
	    boxexp_1.br6[j + (i__ << 12) - 4097] = 0.f;
	    boxexp_1.bi6[j + (i__ << 12) - 4097] = 0.f;
	}
	for (j = 1; j <= 16384; ++j) {
	    boxexp_1.br7[j + (i__ << 14) - 16385] = 0.f;
	    boxexp_1.bi7[j + (i__ << 14) - 16385] = 0.f;
	}
	for (j = 1; j <= 65536; ++j) {
	    boxexp_1.br8[j + (i__ << 16) - 65537] = 0.f;
	    boxexp_1.bi8[j + (i__ << 16) - 65537] = 0.f;
	}
	for (j = 1; j <= 262144; ++j) {
	    boxexp_1.br9[j + (i__ << 18) - 262145] = 0.f;
	    boxexp_1.bi9[j + (i__ << 18) - 262145] = 0.f;
	}
    }
    return 0;
} /* zeroall_ */

