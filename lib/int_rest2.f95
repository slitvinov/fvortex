subroutine int_rest2(kp2)

!  This routine figures box-box interactions for level 2 boxes as well as
!  interacting with the particles at higher level childless boxes that
!  interacted as particle-box previously.

   implicit none

   include 'tree_tmp.h'
   include 'main_dim.h'
   include 'part.h'
   include 'tree9.h'

   integer :: limpar
   real :: x0, y0
   common/geom/x0, y0, limpar

   integer :: kp2

   integer :: Listfar(Nhlp), Listclose(Nhlp), Listexam(Nhlp)
   integer :: Listpart(Nhlp), kb, ib, jb, ipar, jpar, i, kexam
   integer :: n4, k, id, n1, n2, np, kbb, kclose, kfar, kpart
   real :: dyopiinv, r21, r11, r22, xb, yb
!---------------------------------------------------------------------

   dyopiinv = 1./(8.*atan(1.))

   r21 = 0.5
   r11 = 1.0
   r22 = 1.0

   do 20 kb = 1, kp2       ! All level 2 boxes, Childless & Parents
      ib = ic2(kb)
      jb = jc2(kb)
      xb = xc2(kb)
      yb = yc2(kb)
      ! Step 1 : Find i,j of your parent.
      ipar = (xb - x0)/ds1 + 1
      jpar = (yb - y0)/ds1 + 1
      do 1 i = 1, kp1
         kexam = kp1
         Listexam(i) = liststart(i)
1     end do

      ! Step 2 : Find boxes adjacent to the parents
      !          ( Start at coarsest  level )

      ! -> 1st level
      call near_far(Nmax1, ipar, jpar, r11, ic1, jc1, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)
      ! Examine boxes that are close for being childless or not.

      call check_box(Nmax1, kclose, Listclose, kexam, Listexam, kpart, &
                     Listpart, Ipar1Ch2, Imark1)
      ! Boxes that are childless and close to the parents are examined to see if
      ! they are close to the box itself.

      call near_far(Nmax1, ib, jb, r21, ic1, jc1, kpart, Listpart, &
                    kfar, Listfar, Kclose, Listclose)

      ! Boxes that are far from the child had interacted particle-box earlier.
      ! Box kb now needs to interact with their particles.

      n4 = 0
      do 21 k = 1, kfar
         id = Listfar(k)
         n1 = npb1(id, 1)
         n2 = npb1(id, 2)
         do 210 np = n1, n2
            n4 = n4 + 1
            xt(n4) = xn(np)
            yt(n4) = yn(np)
            gt(n4) = gn(np)
210      end do
21    end do

      if (n4 == 0) goto 88
      if (n4 > np_max) write (*, *) 'error in rest2b', n4
      call int_box_part(Nmax2, kb, xb, yb, n4, Br2, Bi2)

      !  Now at level of child (box kb), look for boxes far enough away to
      !  interact as box-box

88    call near_far(Nmax2, ib, jb, r22, ic2, jc2, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)

      do 24 kbb = 1, kfar
         id = Listfar(kbb)
         Xbox(kbb) = xc2(id)
         Ybox(kbb) = yc2(id)
         Prbox(kbb, 0) = Pr2(id, 0)
         Pibox(kbb, 0) = Pi2(id, 0)
         Prbox(kbb, 1) = Pr2(id, 1)
         Pibox(kbb, 1) = Pi2(id, 1)
         Prbox(kbb, 2) = Pr2(id, 2)
         Pibox(kbb, 2) = Pi2(id, 2)
         Prbox(kbb, 3) = Pr2(id, 3)
         Pibox(kbb, 3) = Pi2(id, 3)
         Prbox(kbb, 4) = Pr2(id, 4)
         Pibox(kbb, 4) = Pi2(id, 4)
         Prbox(kbb, 5) = Pr2(id, 5)
         Pibox(kbb, 5) = Pi2(id, 5)
         Prbox(kbb, 6) = Pr2(id, 6)
         Pibox(kbb, 6) = Pi2(id, 6)
         Prbox(kbb, 7) = Pr2(id, 7)
         Pibox(kbb, 7) = Pi2(id, 7)
24    end do

      if (kfar > nbox_max) write (*, *) 'error in rest2', kbb
      call int_box(Nmax2, kb, xb, yb, kfar, Br2, Bi2)

20 end do
   return
end subroutine
