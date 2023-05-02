      integer nmax1
      integer nmax2
      integer nmax3
      integer nmax4
      integer nmax5
      integer nmax6
      integer nmax7
      integer nmax8
      integer nmax9
      parameter(Nmax1=4)
      parameter(Nmax2=16)
      parameter(Nmax3=64)
      parameter(Nmax4=256)
      parameter(Nmax5=1024)
      parameter(Nmax6=4096)
      parameter(Nmax7=16384)
      parameter(Nmax8=65536)
      parameter(Nmax9=262144)

      integer kp1
      real ds1
      real ds2
      real ds3
      real ds4
      real ds5
      real ds6
      real ds7
      real ds8
      real ds9
      common/times/kp1, ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8, ds9

      real xc1(4)
      real yc1(4)
      real xc2(16)
      real yc2(16)
      real xc3(64)
      real yc3(64)
      real xc4(256)
      real yc4(256)
      real xc5(1024)
      real yc5(1024)
      real xc6(4096)
      real yc6(4096)
      real xc7(16384)
      real yc7(16384)
      real xc8(65536)
      real yc8(65536)
      real xc9(262144)
      real yc9(262144)
      common/centre/xc1, yc1, xc2, yc2, xc3, yc3, xc4, yc4, xc5, yc5,
     $     xc6, yc6, xc7, yc7, xc8, yc8, xc9, yc9

      integer npb1(4,2)
      integer ic1(4)
      integer jc1(4)
      integer npb2(16,2)
      integer ic2(16)
      integer jc2(16)
      integer npb3(64,2)
      integer ic3(64)
      integer jc3(64)
      integer npb4(256,2)
      integer ic4(256)
      integer jc4(256)
      integer npb5(1024,2)
      integer ic5(1024)
      integer jc5(1024)
      integer npb6(4096,2)
      integer ic6(4096)
      integer jc6(4096)
      integer npb7(16384,2)
      integer ic7(16384)
      integer jc7(16384)
      integer npb8(65536,2)
      integer ic8(65536)
      integer jc8(65536)
      integer npb9(262144,2)
      integer ic9(262144)
      integer jc9(262144)
      common/index11/npb1, ic1, jc1, npb2, ic2, jc2, npb3, ic3, jc3,
     $     npb4, ic4, jc4, npb5, ic5, jc5, npb6, ic6, jc6,
     $     npb7, ic7, jc7, npb8, ic8, jc8, npb9, ic9, jc9

      integer ipar1ch2(4,4)
      integer ipar2ch3(16,4)
      integer ipar3ch4(64,4)
      integer ipar4ch5(256,4)
      integer ipar5ch6(1024,4)
      integer ipar6ch7(4096,4)
      integer ipar7ch8(16384,4)
      integer ipar8ch9(65536,4)
      common/index22/ipar1ch2, ipar2ch3, ipar3ch4,
     $     ipar4ch5, ipar5ch6, ipar6ch7, ipar7ch8,
     $     ipar8ch9

      integer ichildless8(65536)
      integer iparent8(65536)
      integer imark8(65536)
      integer ichildless7(16384)
      integer iparent7(16384)
      integer imark7(16384)
      integer ichildless6(4096)
      integer iparent6(4096)
      integer imark6(4096)
      integer ichildless5(1024)
      integer iparent5(1024)
      integer imark5(1024)
      integer ichildless4(256)
      integer iparent4(256)
      integer imark4(256)
      integer ichildless3(64)
      integer iparent3(64)
      integer imark3(64)
      integer ichildless2(16)
      integer iparent2(16)
      integer imark2(16)
      integer ichildless1(4)
      integer iparent1(4)
      integer imark1(4)
      integer liststart(4)
      common/index33/ichildless8, iparent8, imark8,
     $     ichildless7, iparent7, imark7,
     $     ichildless6, iparent6, imark6,
     $     ichildless5, iparent5, imark5,
     $     ichildless4, iparent4, imark4,
     $     ichildless3, iparent3, imark3,
     $     ichildless2, iparent2, imark2,
     $     ichildless1, iparent1, imark1,
     $     liststart

      real pr9(262144,0:7)
      real pi9(262144,0:7)
      real pr8(65536,0:7)
      real pi8(65536,0:7)
      real pr7(16384,0:7)
      real pi7(16384,0:7)
      real pr6(4096,0:7)
      real pi6(4096,0:7)
      real pr5(1024,0:7)
      real pi5(1024,0:7)
      real pr4(256,0:7)
      real pi4(256,0:7)
      real pr3(64,0:7)
      real pi3(64,0:7)
      real pr2(16,0:7)
      real pi2(16,0:7)
      real pr1(4,0:7)
      real pi1(4,0:7)
      common/poles/pr9, pi9, pr8, pi8, pr7, pi7, pr6, pi6, pr5, pi5,
     $     pr4,
     $     pi4,
     $     pr3, pi3, pr2, pi2, pr1, pi1

      real br9(262144,7)
      real bi9(262144,7)
      real br8(65536,7)
      real bi8(65536,7)
      real br7(16384,7)
      real bi7(16384,7)
      real br6(4096,7)
      real bi6(4096,7)
      real br5(1024,7)
      real bi5(1024,7)
      real br4(256,7)
      real bi4(256,7)
      real br3(64,7)
      real bi3(64,7)
      real br2(16,7)
      real bi2(16,7)
      real br1(4,7)
      real bi1(4,7)
      common/boxexp/br9, bi9, br8, bi8, br7, bi7, br6, bi6, br5, bi5,
     $     br4, bi4,
     $     br3, bi3, br2, bi2, br1, bi1
