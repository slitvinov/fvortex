      subroutine make_box(nmax, ds1, ds2, kp1, kp2, kparent1,
     $ kchildless1,
     $ ic1, jc1, npb1, iparent1, imark1, ipar1ch2,
     $  ich2par1, npb2, ic2, jc2, xc2, yc2, ichildless1)

C This subroutine takes each box on the previous level and splits it
C into four boxes, creating all the necessary indentification arrays to
C relate the levels.  It mostly parallels the BOX_1 subroutine.
      include 'main_dim.h'

      include 'part.h'

      integer limpar
      real x0
      real y0
      common/geom/x0, y0, Limpar

      integer BF_marker(Nvort)
      real dragBF
      real liftBF
      real momBF
      real xsumBF
      real ysumBF
      real rsumBF
      common/bf/dragBF, liftBF, momBF, xsumBF, ysumBF, rsumBF, BF_marker

      integer nmax
      integer kp1
      integer kp2
      integer kparent1
      integer kchildless1
      integer ic1(nmax/4)
      integer jc1(nmax/4)
      integer npb1(nmax/4,2)
      integer iparent1(nmax/4)
      integer imark1(nmax/4)
      integer ipar1ch2(nmax/4,4)
      integer npb2(nmax,2)
      integer ich2par1(nmax)
      integer ic2(nmax)
      integer jc2(nmax)
      integer ichildless1(nmax/4)
      real ds1
      real ds2
      real xc2(nmax)
      real yc2(nmax)

      integer i
      integer np
      integer kbox
      integer ip
      integer jp
      integer n1
      integer n2
      integer npbox
      integer ipar
      integer jpar
      integer n
      integer ich1
      integer jch1
      integer ich2
      integer jch2
      integer ich3
      integer jch3
      integer ich4
      integer jch4
      integer lx
      integer ly
      integer ibox
      integer jbox
      integer jn
      integer n1m1
      integer nbx
      integer ix
      integer inew
      integer nb1
      integer nb2
      integer nb3
      integer nb4
      integer ixy(Nvort)
      integer idummy(Nvort)
      integer BF_marker_temp(Nvort)
      real ds2inv
      real xst
      real yst
      real si1d
      real sj1d
      real si2d
      real sj2d
      real si3d
      real sj3d
      real si4d
      real sj4d

      do i = 1, nvort
         ixy(i) = 0
         idummy(i) = 0
         BF_marker_temp(i) = BF_marker(i)
      enddo

C Find childless & parent boxes. Subdivide parent boxes in 4 squares.
C (Parent boxes  are those  that contain more than LIMPAR particles )
      ds2 = 0.5*ds1
      ds2inv = 1.0/ds2
      Xst = x0 - 0.5*ds2
      Yst = y0 - 0.5*ds2
      np = 0
      kp2 = 0
      kparent1 = 0
      kchildless1 = 0

      do 2 kbox = 1, kp1
         ip = ic1(kbox)
         jp = jc1(kbox)
         n1 = npb1(kbox, 1)
         n2 = npb1(kbox, 2)
         Npbox = n2 - n1 + 1
         if (Npbox > limpar) then
            kparent1 = kparent1 + 1
            iparent1(kparent1) = kbox
            imark1(kbox) = 1
            ipar = (ip - 1)*2
            jpar = (jp - 1)*2
            ich1 = 1 + ipar
            jch1 = 1 + jpar
            ich2 = ich1
            jch2 = 2 + jpar
            ich3 = 2 + ipar
            jch3 = jch1
            ich4 = ich3
            jch4 = jch2
            si1d = ich1*ds2
            sj1d = jch1*ds2
            si2d = si1d
            sj2d = jch2*ds2
            si3d = ich3*ds2
            sj3d = sj1d
            si4d = si3d
            sj4d = sj2d

            jn = 0
            do 20 n = n1, n2
               lx = (xn(n) - x0)*ds2inv
               ly = (yn(n) - y0)*ds2inv
               ibox = lx + 1
               jbox = ly + 1
               jn = jn + 1
               ixy(jn) = 1
               if ((ibox == ich2) .and. (jbox == jch2)) then
                  ixy(jn) = 2
               else if ((ibox == ich3) .and. (jbox == jch3)) then
                  ixy(jn) = 3
               else if ((ibox == ich4) .and. (jbox == jch4)) then
                  ixy(jn) = 4
               end if
   20       continue

C Find  how  many  particles  are  in  each subbox
C and store  the  particles  in their new sorted  locations
            n1m1 = n1 - 1
            nbx = n1m1

            call wheneq(Npbox, ixy, 1, 1, idummy, nb1)
            do 211 i = 1, nb1
               ix = i + nbx
               inew = idummy(i) + n1m1
               xp(ix) = xn(inew)
               yp(ix) = yn(inew)
               gp(ix) = gn(inew)
               uu(ix) = Uold(inew)
               vv(ix) = Vold(inew)
               Gdiff(ix) = Gdold(inew)
               BF_marker(ix) = BF_marker_temp(inew)
  211       continue

            call wheneq(Npbox, ixy, 1, 2, idummy, nb2)
            nbx = nbx + nb1
            do 212 i = 1, nb2
               ix = i + nbx
               inew = idummy(i) + n1m1
               xp(ix) = xn(inew)
               yp(ix) = yn(inew)
               gp(ix) = gn(inew)
               uu(ix) = Uold(inew)
               vv(ix) = Vold(inew)
               Gdiff(ix) = Gdold(inew)
               BF_marker(ix) = BF_marker_temp(inew)
  212       continue

            call wheneq(Npbox, ixy, 1, 3, idummy, nb3)
            nbx = nbx + nb2
            do 213 i = 1, nb3
               ix = i + nbx
               inew = idummy(i) + n1m1
               xp(ix) = xn(inew)
               yp(ix) = yn(inew)
               gp(ix) = gn(inew)
               uu(ix) = Uold(inew)
               vv(ix) = Vold(inew)
               Gdiff(ix) = Gdold(inew)
               BF_marker(ix) = BF_marker_temp(inew)
  213       continue

            call wheneq(Npbox, ixy, 1, 4, idummy, nb4)
            nbx = nbx + nb3
            do 214 i = 1, nb4
               ix = i + nbx
               inew = idummy(i) + n1m1
               xp(ix) = xn(inew)
               yp(ix) = yn(inew)
               gp(ix) = gn(inew)
               uu(ix) = Uold(inew)
               vv(ix) = Vold(inew)
               Gdiff(ix) = Gdold(inew)
               BF_marker(ix) = BF_marker_temp(inew)
  214       continue

C Box 1
            np = n1 - 1
            if (nb1 > 0) then
               kp2 = kp2 + 1
               ipar1ch2(kbox, 1) = kp2
               ich2par1(kp2) = kbox
               npb2(kp2, 1) = np + 1
               np = np + nb1
               npb2(kp2, 2) = np
               ic2(kp2) = ich1
               jc2(kp2) = jch1
               xc2(kp2) = Xst + si1d
               yc2(kp2) = Yst + sj1d
            else
               IPAR1Ch2(kbox, 1) = 0
            end if

C Box 2
            if (nb2 > 0) then
               kp2 = kp2 + 1
               ipar1ch2(kbox, 2) = kp2
               ich2par1(kp2) = kbox
               npb2(kp2, 1) = np + 1
               np = np + nb2
               npb2(kp2, 2) = np
               ic2(kp2) = ich2
               jc2(kp2) = jch2
               xc2(kp2) = Xst + si2d
               yc2(kp2) = Yst + sj2d
            else
               IPAR1Ch2(kbox, 2) = 0
            end if

C Box 3
            if (nb3 > 0) then
               kp2 = kp2 + 1
               ipar1ch2(kbox, 3) = kp2
               ich2par1(kp2) = kbox
               npb2(kp2, 1) = np + 1
               np = np + nb3
               npb2(kp2, 2) = np
               ic2(kp2) = ich3
               jc2(kp2) = jch3
               xc2(kp2) = Xst + si3d
               yc2(kp2) = Yst + sj3d
            else
               IPAR1Ch2(kbox, 3) = 0
            end if

C Box 4
            if (nb4 > 0) then
               kp2 = kp2 + 1
               ipar1ch2(kbox, 4) = kp2
               ich2par1(kp2) = kbox
               npb2(kp2, 1) = np + 1
               np = np + nb4
               npb2(kp2, 2) = np
               ic2(kp2) = ich4
               jc2(kp2) = jch4
               xc2(kp2) = Xst + si4d
               yc2(kp2) = Yst + sj4d
            else
               IPAR1Ch2(kbox, 4) = 0
            end if

         else
            kchildless1 = Kchildless1 + 1
            ichildless1(kchildless1) = kbox
            imark1(kbox) = 0

            do 22 i = n1, n2
               xp(i) = xn(i)
               yp(i) = yn(i)
               gp(i) = gn(i)
               uu(i) = Uold(i)
               vv(i) = Vold(i)
               Gdiff(i) = Gdold(i)
               BF_marker(i) = BF_marker_temp(i)
   22       continue

         end if

    2 continue

      do i = 1, nvort
         xn(i) = xp(i)
         yn(i) = yp(i)
         gn(i) = gp(i)
         Uold(i) = uu(i)
         Vold(i) = vv(i)
         Gdold(i) = Gdiff(i)
      enddo
      end
