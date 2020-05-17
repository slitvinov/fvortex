subroutine int_rest5(kp5)

!  Same as int_rest2 for level 5 boxes.

   implicit none

   include 'tree_tmp.h'
   include 'main_dim.h'
   include 'part.h'
   include 'tree9.h'

   integer :: limpar
   real :: x0, y0
   common/geom/x0, y0, Limpar

   integer :: kp5

   integer :: Listfar(Nhlp), Listclose(Nhlp), Listexam(Nhlp)
   integer :: Listpart(Nhlp), kb, ib, jb, ipar, jpar, i, kexam
   integer :: n4, k, id, n1, n2, np, kbb, kclose, kfar, kpart
   real :: dyopiinv, r41, r42, r43, r44, r51, r52, r53, r54, r55, xb, yb
!---------------------------------------------------------------------

   dyopiinv = 1./(8.*atan(1.))

   r41 = 0.125
   r42 = 0.25
   r43 = 0.50
   r44 = 1.0
   r51 = 0.0625
   r52 = 0.125
   r53 = 0.25
   r54 = 0.50
   r55 = 1.0

   do 20 kb = 1, kp5       ! All boxes Childless & Parents

      ib = ic5(kb)
      jb = jc5(kb)
      xb = xc5(kb)
      yb = yc5(kb)
      ipar = (xb - x0)/ds4 + 1
      jpar = (yb - y0)/ds4 + 1
      do 1 i = 1, kp1
         kexam = kp1
         Listexam(i) = liststart(i)
1     end do

      call near_far(Nmax1, ipar, jpar, r41, ic1, jc1, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)

      call check_box(Nmax1, kclose, Listclose, kexam, Listexam, kpart &
                     , Listpart, Ipar1Ch2, Imark1)

      call near_far(Nmax1, ib, jb, r51, ic1, jc1, kpart, Listpart, &
                    kfar, Listfar, Kclose, Listclose)

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

      call near_far(Nmax2, ipar, jpar, r42, ic2, jc2, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)

      call check_box(Nmax2, kclose, Listclose, kexam, Listexam, kpart &
                     , Listpart, Ipar2Ch3, Imark2)                 ! NT

      call near_far(Nmax2, ib, jb, r52, ic2, jc2, kpart, Listpart, &
                    kfar, Listfar, Kclose, Listclose)

      do 22 k = 1, kfar
         id = Listfar(k)
         n1 = npb2(id, 1)
         n2 = npb2(id, 2)
         do 220 np = n1, n2
            n4 = n4 + 1
            xt(n4) = xn(np)
            yt(n4) = yn(np)
            gt(n4) = gn(np)
220      end do
22    end do

      call near_far(Nmax3, ipar, jpar, r43, ic3, jc3, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)

      call check_box(Nmax3, kclose, Listclose, kexam, Listexam, kpart &
                     , Listpart, Ipar3Ch4, Imark3)                 ! NT

      call near_far(Nmax3, ib, jb, r53, ic3, jc3, kpart, Listpart, &
                    kfar, Listfar, Kclose, Listclose)
      do 23 k = 1, kfar
         id = Listfar(k)
         n1 = npb3(id, 1)
         n2 = npb3(id, 2)
         do 230 np = n1, n2
            n4 = n4 + 1
            xt(n4) = xn(np)
            yt(n4) = yn(np)
            gt(n4) = gn(np)
230      end do
23    end do

      !--------------------------------------------------------------------------
      ! -> 4th level

      ! Close to parents(?)
      call near_far(Nmax4, ipar, jpar, r44, ic4, jc4, Kexam, Listexam, &
                    kfar, Listfar, kclose, Listclose)

      ! Close to parents - Childless(?)
      call check_box(Nmax4, kclose, Listclose, &
                     kexam, Listexam, kpart, Listpart, Ipar4Ch5, Imark4)

      ! Close to parents & childless - close to box(?)
      call near_far(Nmax4, ib, jb, r54, ic4, jc4, Kpart, Listpart, &
                    kfar, Listfar, kclose, Listclose)

      !  Boxes that are far from the child now belong to list 4 of the box

      do 24 k = 1, kfar

         id = Listfar(k)
         n1 = npb4(id, 1)
         n2 = npb4(id, 2)
         do 240 np = n1, n2
            n4 = n4 + 1
            xt(n4) = xn(np)
            yt(n4) = yn(np)
            gt(n4) = gn(np)
240      end do
24    end do

      if (n4 == 0) goto 88
      if (n4 > np_max) write (*, *) 'error in rest5b', n4
      call int_box_part(Nmax5, kb, xb, yb, n4, Br5, Bi5)

88    call near_far(Nmax5, ib, jb, r55, ic5, jc5, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)

      ! CDIR$SHORTLOOP
      do 25 kbb = 1, kfar
         id = Listfar(kbb)
         Xbox(kbb) = xc5(id)
         Ybox(kbb) = yc5(id)
         Prbox(kbb, 0) = Pr5(id, 0)
         Pibox(kbb, 0) = Pi5(id, 0)
         Prbox(kbb, 1) = Pr5(id, 1)
         Pibox(kbb, 1) = Pi5(id, 1)
         Prbox(kbb, 2) = Pr5(id, 2)
         Pibox(kbb, 2) = Pi5(id, 2)
         Prbox(kbb, 3) = Pr5(id, 3)
         Pibox(kbb, 3) = Pi5(id, 3)
         Prbox(kbb, 4) = Pr5(id, 4)
         Pibox(kbb, 4) = Pi5(id, 4)
         Prbox(kbb, 5) = Pr5(id, 5)
         Pibox(kbb, 5) = Pi5(id, 5)
         Prbox(kbb, 6) = Pr5(id, 6)
         Pibox(kbb, 6) = Pi5(id, 6)
         Prbox(kbb, 7) = Pr5(id, 7)
         Pibox(kbb, 7) = Pi5(id, 7)
25    end do

      if (kfar > nbox_max) write (*, *) 'error in rest5', kbb
      call int_box(Nmax5, kb, xb, yb, kfar, Br5, Bi5)

20 end do
   return
end subroutine
