      subroutine box_1(npart, s0, xc1, yc1, ic1, jc1, npb1, ds1, kp1,
     $     liststart)

C This subroutine sorts the particles into four boxes and provides the
C necessary identification arrays for this top level of the interaction
C tree.

      include 'main_dim.h'
      include 'part.h'

      integer limpar
      real x0
      real y0
      common/geom/x0, y0, limpar

      integer BF_marker(nvort)
      real dragBF
      real liftBF
      real momBF
      real xsumBF
      real ysumBF
      real rsumBF
      common/bf/dragBF, liftBF, momBF, xsumBF, ysumBF, rsumBF, BF_marker

      integer npart
      integer kp1
      integer BF_marker_temp(nvort)
      integer ic1(4)
      integer jc1(4)
      integer npb1(4,2)
      integer liststart(4)
      real s0
      real ds1
      real xc1(4)
      real yc1(4)

      integer i
      integer n
      integer np
      integer ibox
      integer jbox
      integer inew
      integer ix
      integer nbx
      integer nb1
      integer nb2
      integer nb3
      integer nb4
      real xst
      real yst
      real ds1inv
      real lx
      real ly

      integer ixy(nvort)
      integer idummy(nvort)

      do i = 1, nvort
         ixy(i) = 0
         idummy(i) = 0
         BF_marker_temp(i) = BF_marker(i)
      enddo
      ds1 = 0.5*s0
      Xst = x0 - 0.5*ds1
      Yst = y0 - 0.5*ds1
      ds1inv = 1.0/ds1

C Identify each particle with one of the boxes
      do 1 n = 1, npart
         lx = (xp(n) - x0)*ds1inv
         ly = (yp(n) - y0)*ds1inv
         ibox = int(lx + 1)          ! indices of the box where the
                                     ! particles
         jbox = int(ly + 1)          ! reside
         ixy(n) = 1             ! IXY(n) has as value
         if ((ibox == 1) .and. (jbox == 2)) then ! the index (1,2,3,4)
            ixy(n) = 2          ! of the box that
         else if ((ibox == 2) .and. (jbox == 1)) then ! particle n is in
            ixy(n) = 3
         else if ((ibox == 2) .and. (jbox == 2)) then
            ixy(n) = 4
         end if
    1 continue

C Find  how  many  particles  are  in  each subbox
C and store  the  particles  in their new sorted  locations

      call wheneq(npart, ixy, 1, 1, idummy, nb1)
      do 211 i = 1, nb1
         inew = idummy(i)
         xn(i) = xp(inew)
         yn(i) = yp(inew)
         gn(i) = gp(inew)
         Uold(i) = uu(inew)
         Vold(i) = vv(inew)
         Gdold(i) = Gdiff(inew)
         BF_marker(i) = BF_marker_temp(inew)
  211 continue

      call wheneq(npart, ixy, 1, 2, idummy, nb2)
      nbx = nb1
      do 212 i = 1, nb2
         ix = i + nbx
         inew = idummy(i)
         xn(ix) = xp(inew)
         yn(ix) = yp(inew)
         gn(ix) = gp(inew)
         Uold(ix) = uu(inew)
         Vold(ix) = vv(inew)
         Gdold(ix) = Gdiff(inew)
         BF_marker(ix) = BF_marker_temp(inew)
  212 continue

      call wheneq(npart, ixy, 1, 3, idummy, nb3)
      nbx = nbx + nb2
      do 213 i = 1, nb3
         ix = i + nbx
         inew = idummy(i)
         xn(ix) = xp(inew)
         yn(ix) = yp(inew)
         gn(ix) = gp(inew)
         Uold(ix) = uu(inew)
         Vold(ix) = vv(inew)
         Gdold(ix) = Gdiff(inew)
         BF_marker(ix) = BF_marker_temp(inew)
  213 continue

      call wheneq(npart, ixy, 1, 4, idummy, nb4)
      nbx = nbx + nb3
      do 214 i = 1, nb4
         ix = i + nbx
         inew = idummy(i)
         xn(ix) = xp(inew)
         yn(ix) = yp(inew)
         gn(ix) = gp(inew)
         Uold(ix) = uu(inew)
         Vold(ix) = vv(inew)
         Gdold(ix) = Gdiff(inew)
         BF_marker(ix) = BF_marker_temp(inew)
  214 continue

C NPBk(kp1,1)  -> index of first particle in box kp1 at level k
C NPBk(kp1,2)  -> index of last particle in box kp1 at level k

C kpi is the number of boxes that contain particles at level i

C Now make necessary identification arrays for the boxes
C Box 1
      kp1 = 0
      do i = 1, 4
         liststart(i) = 0
      enddo
      np = 0
      if (nb1 > 0) then
         kp1 = kp1 + 1
         liststart(kp1) = kp1
         npb1(kp1, 1) = np + 1
         np = nb1
         npb1(kp1, 2) = np
         ic1(kp1) = 1
         jc1(kp1) = 1
         xc1(kp1) = Xst + ds1
         yc1(kp1) = Yst + ds1
      end if

C Box 2
      if (nb2 > 0) then
         kp1 = kp1 + 1
         liststart(kp1) = kp1
         npb1(kp1, 1) = np + 1
         np = np + nb2
         npb1(kp1, 2) = np
         ic1(kp1) = 1
         jc1(kp1) = 2
         xc1(kp1) = Xst + ds1
         yc1(kp1) = Yst + 2.*ds1
      end if

C Box 3
      if (nb3 > 0) then
         kp1 = kp1 + 1
         liststart(kp1) = kp1
         npb1(kp1, 1) = np + 1
         np = np + nb3
         npb1(kp1, 2) = np
         ic1(kp1) = 2
         jc1(kp1) = 1
         xc1(kp1) = Xst + 2.*ds1
         yc1(kp1) = Yst + ds1
      end if

C Box 4
      if (nb4 > 0) then
         kp1 = kp1 + 1
         liststart(kp1) = kp1
         npb1(kp1, 1) = np + 1
         np = np + nb4
         npb1(kp1, 2) = np
         ic1(kp1) = 2
         jc1(kp1) = 2
         xc1(kp1) = Xst + 2.*ds1
         yc1(kp1) = Yst + 2.*ds1
      end if
      end
