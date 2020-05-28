integer :: nmax1, nmax2, nmax3, nmax4, nmax5
parameter(Nmax1=4, Nmax2=16, Nmax3=64, Nmax4=256, &
          Nmax5=1024)

integer :: kp1
real :: ds1, ds2, ds3, ds4, ds5
common/times/kp1, ds1, ds2, ds3, ds4, ds5

real :: xc1(4), yc1(4), xc2(16), yc2(16), xc3(64), yc3(64), &
        xc4(256), yc4(256), xc5(1024), yc5(1024)
common/centre/xc1, yc1, xc2, yc2, xc3, yc3, xc4, yc4, xc5, yc5

integer :: npb1(4, 2), ic1(4), jc1(4), npb2(16, 2), ic2(16), jc2(16), &
           npb3(64, 2), ic3(64), jc3(64), npb4(256, 2), ic4(256), jc4(256), &
           npb5(1024, 2), ic5(1024), jc5(1024)
common/index11/npb1, ic1, jc1, npb2, ic2, jc2, npb3, ic3, jc3, &
   npb4, ic4, jc4, npb5, ic5, jc5

integer :: ipar1ch2(4, 4), ipar2ch3(16, 4), ipar3ch4(64, 4), &
           ipar4ch5(256, 4)
common/index22/ipar1ch2, ipar2ch3, ipar3ch4, &
   ipar4ch5

integer :: Ichildless4(256), Iparent4(256), Imark4(256) &
           , Ichildless3(64), Iparent3(64), Imark3(64) &
           , Ichildless2(16), Iparent2(16), Imark2(16) &
           , Ichildless1(4), Iparent1(4), Imark1(4) &
           , liststart(4)
common/index33/Ichildless4, Iparent4, Imark4 &
   , Ichildless3, Iparent3, Imark3 &
   , Ichildless2, Iparent2, Imark2 &
   , Ichildless1, Iparent1, Imark1 &
   , liststart

real :: pr5(1024, 0:7), pi5(1024, 0:7), &
        pr4(256, 0:7), pi4(256, 0:7), pr3(64, 0:7), pi3(64, 0:7), &
        pr2(16, 0:7), pi2(16, 0:7), pr1(4, 0:7), pi1(4, 0:7)
common/poles/pr5, pi5, pr4, pi4, pr3, pi3, pr2, pi2, pr1, pi1

real :: br5(1024, 7), bi5(1024, 7), br4(256, 7), bi4(256, 7), &
        br3(64, 7), bi3(64, 7), br2(16, 7), bi2(16, 7), br1(4, 7), bi1(4, 7)
common/boxexp/br5, bi5, br4, bi4, br3, bi3, br2, bi2, br1, bi1

