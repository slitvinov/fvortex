      integer nmax1, nmax2, nmax3, nmax4, nmax5, nmax6, nmax7, nmax8,
     &     nmax9
      parameter(Nmax1=4, Nmax2=16, Nmax3=64, Nmax4=256,
     &     Nmax5=1024, Nmax6=4096, Nmax7=16384,
     &     Nmax8=65536, Nmax9=262144)

      integer kp1
      real ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8, ds9
      common/times/kp1, ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8, ds9

      real xc1(4), yc1(4), xc2(16), yc2(16), xc3(64), yc3(64),
     &     xc4(256), yc4(256), xc5(1024), yc5(1024),
     &     xc6(4096), yc6(4096), xc7(16384), yc7(16384),
     &     xc8(65536), yc8(65536), xc9(262144), yc9(262144)
      common/centre/xc1, yc1, xc2, yc2, xc3, yc3, xc4, yc4, xc5, yc5,
     &     xc6, yc6, xc7, yc7, xc8, yc8, xc9, yc9

      integer npb1(4, 2), ic1(4), jc1(4), npb2(16, 2), ic2(16),
     &     jc2(16),
     &     npb3(64, 2), ic3(64), jc3(64), npb4(256, 2), ic4(256),
     &     jc4(256),
     &     npb5(1024, 2), ic5(1024), jc5(1024),
     &     npb6(4096, 2), ic6(4096), jc6(4096),
     &     npb7(16384, 2), ic7(16384), jc7(16384),
     &     npb8(65536, 2), ic8(65536), jc8(65536),
     &     npb9(262144, 2), ic9(262144), jc9(262144)
      common/index11/npb1, ic1, jc1, npb2, ic2, jc2, npb3, ic3, jc3,
     &     npb4, ic4, jc4, npb5, ic5, jc5, npb6, ic6, jc6,
     &     npb7, ic7, jc7, npb8, ic8, jc8, npb9, ic9, jc9

      integer ipar1ch2(4, 4), ipar2ch3(16, 4), ipar3ch4(64, 4),
     &     ipar4ch5(256, 4), ipar5ch6(1024, 4), ipar6ch7(4096, 4),
     &     ipar7ch8(16384, 4), ipar8ch9(65536, 4)
      common/index22/ipar1ch2, ipar2ch3, ipar3ch4,
     &     ipar4ch5, ipar5ch6, ipar6ch7, ipar7ch8,
     &     ipar8ch9

      integer ichildless8(65536), iparent8(65536), imark8(65536),
     &     ichildless7(16384), iparent7(16384), imark7(16384),
     &     ichildless6(4096), iparent6(4096), imark6(4096),
     &     ichildless5(1024), iparent5(1024), imark5(1024),
     &     ichildless4(256), iparent4(256), imark4(256),
     &     ichildless3(64), iparent3(64), imark3(64),
     &     ichildless2(16), iparent2(16), imark2(16),
     &     ichildless1(4), iparent1(4), imark1(4),
     &     liststart(4)
      common/index33/ichildless8, iparent8, imark8,
     &     ichildless7, iparent7, imark7,
     &     ichildless6, iparent6, imark6,
     &     ichildless5, iparent5, imark5,
     &     ichildless4, iparent4, imark4,
     &     ichildless3, iparent3, imark3,
     &     ichildless2, iparent2, imark2,
     &     ichildless1, iparent1, imark1,
     &     liststart

      real pr9(262144, 0:7), pi9(262144, 0:7),
     &     pr8(65536, 0:7), pi8(65536, 0:7),
     &     pr7(16384, 0:7), pi7(16384, 0:7),
     &     pr6(4096, 0:7), pi6(4096, 0:7), pr5(1024, 0:7),
     &     pi5(1024, 0:7),
     &     pr4(256, 0:7), pi4(256, 0:7), pr3(64, 0:7), pi3(64, 0:7),
     &     pr2(16, 0:7), pi2(16, 0:7), pr1(4, 0:7), pi1(4, 0:7)
      common/poles/pr9, pi9, pr8, pi8, pr7, pi7, pr6, pi6, pr5, pi5,
     &     pr4,
     &     pi4,
     &     pr3, pi3, pr2, pi2, pr1, pi1

      real br9(262144, 7), bi9(262144, 7), br8(65536, 7),
     &     bi8(65536, 7),
     &     br7(16384, 7), bi7(16384, 7), br6(4096, 7), bi6(4096, 7),
     &     br5(1024, 7), bi5(1024, 7), br4(256, 7), bi4(256, 7),
     &     br3(64, 7), bi3(64, 7), br2(16, 7), bi2(16, 7), br1(4, 7),
     &     bi1(4, 7)
      common/boxexp/br9, bi9, br8, bi8, br7, bi7, br6, bi6, br5, bi5,
     &     br4, bi4,
     &     br3, bi3, br2, bi2, br1, bi1
