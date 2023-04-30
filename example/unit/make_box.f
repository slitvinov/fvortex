      program main
      include 'main_dim.h'
      include 'tree9.h'
      integer ich2Par1(nmax2)
      integer ich3Par2(nmax3)
      integer ich4Par3(nmax4)
      integer ich5Par4(nmax5)
      integer ich6Par5(nmax6)
      integer ich7par6(nmax7)
      integer ich8par7(nmax8)
      integer ich9par8(nmax9)
      integer kchildless1
      integer kchildless2
      integer kchildless3
      integer kchildless4
      integer kchildless5
      integer kchildless6
      integer kchildless7
      integer kchildless8
      integer kp2
      integer kp3
      integer kp4
      integer kp5
      integer kp6
      integer kp7
      integer kp8
      integer kp9
      integer kparent1
      integer kparent2
      integer kparent3
      integer kparent4
      integer kparent5
      integer kparent6
      integer kparent7
      integer kparent8
      integer limpar
      integer npart
      integer stat
      real s0
      real x
      real x0
      real xmax
      real xmin
      real xn(nvort)
      real xp(nvort)
      real y
      real y0
      real ymax
      real ymin
      real yn(nvort)
      real yp(nvort)
      common/vort1/xp, yp
      common/vort2/xn, yn
      common/geom/x0, y0, limpar

      limpar = 8
      npart = 0
      do 100
         read(5, *, iostat = stat) x, y
         if (stat /= 0) exit
         npart = npart + 1
         xp(npart) = x
         yp(npart) = y
 100  continue
      call box_dim(npart, xmin, xmax, ymin, ymax)
      s0 = max(abs(xmax - Xmin), abs(ymax - ymin))
      x0 = xmin - 0.01*s0
      y0 = ymin - 0.01*s0
      s0 = 1.02*s0
      call box_1(npart, s0, xc1, yc1, ic1, jc1, npb1, ds1,
     $  kp1, liststart)
      call print_box(kp1, xc1, yc1, ds1)

c     Level 2 - divide level 1 boxes into four squares
      call make_box(16, ds1, ds2, kp1, kp2, kparent1, kchildless1,
     $  ic1, jc1, npb1, iparent1, imark1, ipar1ch2, ich2par1,
     $  npb2, ic2, jc2, xc2, yc2, ichildless1)
      call print_box(kp2, xc2, yc2, ds2)

c     Level 3
      if (kp2 == 0) goto 1
      call make_box(64, ds2, ds3, kp2, kp3, kparent2, kchildless2,
     $  ic2, jc2, npb2, iparent2, imark2, ipar2ch3, ich3par2,
     $  npb3, ic3, jc3, xc3, yc3, ichildless2)
      call print_box(kp3, xc3, yc3, ds3)

c     Level 4
      if (kp3 == 0) goto 1
      call make_box(256, ds3, ds4, kp3, kp4, kparent3, kchildless3,
     $  ic3, jc3, npb3, iparent3, imark3, ipar3ch4, ich4par3,
     $  npb4, ic4, jc4, xc4, yc4, ichildless3)
      call print_box(kp4, xc4, yc4, ds4)

c     Level 5
      if (kp4 == 0) goto 1
      call make_box(1024, ds4, ds5, kp4, kp5, kparent4, kchildless4,
     $  ic4, jc4, npb4, iparent4, imark4, ipar4ch5, ich5par4,
     $  npb5, ic5, jc5, xc5, yc5, ichildless4)
      call print_box(kp5, xc5, yc5, ds5)

c     Level 6
      if (kp5 == 0) goto 1
      call make_box(4096, ds5, ds6, kp5, kp6, kparent5, kchildless5,
     $  ic5, jc5, npb5, iparent5, imark5, ipar5ch6, ich6par5,
     $  npb6, ic6, jc6, xc6, yc6, ichildless5)
      call print_box(kp6, xc6, yc6, ds6)

c     Level 7
      if (kp6 == 0) goto 1
      call make_box(16384, ds6, ds7, kp6, kp7, kparent6, kchildless6,
     $  ic6, jc6, npb6, iparent6, imark6, ipar6ch7, ich7par6,
     $  npb7, ic7, jc7, xc7, yc7, ichildless6)
      call print_box(kp7, xc7, yc7, ds7)

c     Level 8
      if (kp7 == 0) goto 1
      call make_box(65536, ds7, ds8, kp7, kp8, kparent7, kchildless7,
     $  ic7, jc7, npb7, iparent7, imark7, ipar7ch8, ich8par7,
     $  npb8, ic8, jc8, xc8, yc8, ichildless7)
      call print_box(kp8, xc8, yc8, ds8)

c     Level 9 (finest boxes)
      if (kp8 == 0) goto 1
      call make_box(262144, ds8, ds9, kp8, kp9, kparent8, kchildless8,
     $  ic8, jc8, npb8, iparent8, imark8, ipar8ch9, ich9par8,
     $  npb9, ic9, jc9, xc9, yc9, ichildless8)
      call print_box(kp9, xc9, yc9, ds9)

    1 stop
      end

      subroutine print_box(n, x, y, s)
      integer n
      real x(*)
      real y(*)
      real s
      integer i
      do 110 i = 1, n
         call print_box0(x(i), y(i), s)
 110  continue
      end


      subroutine print_box0(x, y, s)
      real x
      real y
      real s
      real xh
      real xl
      real yh
      real yl

      xl = x - s/2
      xh = x + s/2
      yl = y - s/2
      yh = y + s/2
      write (*, '(1P, SP, 5(E23.16, 1X, E23.16/))') xl, yl, xh, yl,
     $     xh, yh, xl, yh, xl, yl
      end subroutine
