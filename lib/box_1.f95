subroutine box_1(Npart, s0, xc1, yc1, ic1, jc1, npb1, ds1, kp1 &
                 , Liststart)

!  This subroutine sorts the particles into four boxes and provides the
!  necessary identification arrays for this top level of the interaction tree.

   implicit none

   include 'main_dim.h'
   include 'part.h'

   integer :: limpar
   real :: x0, y0
   common/geom/x0, y0, limpar

   integer :: BF_marker(Nvort)
   real :: dragBF, liftBF, momBF, xsumBF, ysumBF, rsumBF
   common/bf/dragBF, liftBF, momBF, xsumBF, ysumBF, rsumBF, BF_marker

   integer :: npart, kp1, BF_marker_temp(Nvort)
   integer :: ic1(4), jc1(4), npb1(4, 2), Liststart(4)
   real :: s0, ds1
   real :: xc1(4), yc1(4)

   integer :: i, n, np, ibox, jbox, inew, ix
   integer :: nbx, nb1, nb2, nb3, nb4
   real :: xst, yst, ds1inv, lx, ly

   integer :: ixy(Nvort), idummy(Nvort)
!------------------------------------------------------------------------

   do i = 1, nvort
      ixy(i) = 0
      idummy(i) = 0
      BF_marker_temp(i) = BF_marker(i)
   enddo
   ds1 = 0.5*s0
   Xst = x0 - 0.5*ds1
   Yst = y0 - 0.5*ds1
   ds1inv = 1.0/ds1

!-- Identify each particle with one of the boxes
   do 1 n = 1, Npart
      lx = (Xp(n) - x0)*ds1inv
      ly = (Yp(n) - y0)*ds1inv
      ibox = lx + 1                ! indices of the box where the particles
      jbox = ly + 1                ! reside
      ixy(n) = 1                                  ! IXY(n) has as value
      if ((ibox == 1) .and. (jbox == 2)) then         ! the index (1,2,3,4)
         ixy(n) = 2                                  ! of the box that
      else if ((ibox == 2) .and. (jbox == 1)) then    ! particle n is in
         ixy(n) = 3
      else if ((ibox == 2) .and. (jbox == 2)) then
         ixy(n) = 4
      end if
1  end do

!   Find  how  many  particles  are  in  each subbox
!  and store  the  particles  in their new sorted  locations

   call wheneq(Npart, ixy, 1, 1, idummy, nb1)
   do 211 i = 1, nb1
      inew = idummy(i)
      xn(i) = xp(inew)
      yn(i) = yp(inew)
      gn(i) = gp(inew)
      Uold(i) = uu(inew)
      Vold(i) = vv(inew)
      Gdold(i) = Gdiff(inew)
      BF_marker(i) = BF_marker_temp(inew)
211 end do

   call wheneq(Npart, ixy, 1, 2, idummy, nb2)
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
212 end do

   call wheneq(Npart, ixy, 1, 3, idummy, nb3)
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
213 end do

   call wheneq(Npart, ixy, 1, 4, idummy, nb4)
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
214 end do

!  NPBk(kp1,1)  -> index of first particle in box kp1 at level k
!  NPBk(kp1,2)  -> index of last particle in box kp1 at level k

!  kpi is the number of boxes that contain particles at level i

!  Now make necessary identification arrays for the boxes
! Box 1
   kp1 = 0
   do i = 1, 4
      Liststart(i) = 0
   enddo
   np = 0
   if (nb1 > 0) then
      kp1 = kp1 + 1
      Liststart(kp1) = kp1
      npb1(kp1, 1) = np + 1
      np = nb1
      npb1(kp1, 2) = np
      ic1(kp1) = 1
      jc1(kp1) = 1
      xc1(kp1) = Xst + ds1
      yc1(kp1) = Yst + ds1
   end if

! Box 2
   if (nb2 > 0) then
      kp1 = kp1 + 1
      Liststart(kp1) = kp1
      npb1(kp1, 1) = np + 1
      np = np + nb2
      npb1(kp1, 2) = np
      ic1(kp1) = 1
      jc1(kp1) = 2
      xc1(kp1) = Xst + ds1
      yc1(kp1) = Yst + 2.*ds1
   end if

! Box 3
   if (nb3 > 0) then
      kp1 = kp1 + 1
      Liststart(kp1) = kp1
      npb1(kp1, 1) = np + 1
      np = np + nb3
      npb1(kp1, 2) = np
      ic1(kp1) = 2
      jc1(kp1) = 1
      xc1(kp1) = Xst + 2.*ds1
      yc1(kp1) = Yst + ds1
   end if

! Box 4
   if (nb4 > 0) then
      kp1 = kp1 + 1
      Liststart(kp1) = kp1
      npb1(kp1, 1) = np + 1
      np = np + nb4
      npb1(kp1, 2) = np
      ic1(kp1) = 2
      jc1(kp1) = 2
      xc1(kp1) = Xst + 2.*ds1
      yc1(kp1) = Yst + 2.*ds1
   end if

   return
end subroutine box_1
