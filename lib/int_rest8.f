      subroutine int_rest8(kp8)

!     Same as int_rest2 for level 8 boxes.

      implicit none

      include 'tree_tmp.h'
      include 'main_dim.h'
      include 'part.h'
      include 'tree9.h'

      integer :: limpar
      real :: x0, y0
      common/geom/x0, y0, limpar

      integer :: kp8

      integer :: Listfar(Nhlp), Listclose(Nhlp), listexam(Nhlp)
      integer :: Listpart(Nhlp), kb, ib, jb, ipar, jpar, i, kexam
      integer :: n4, k, id, n1, n2, np, kbb, kclose, kfar, kpart
      real :: xb, yb, dyopiinv
      real :: r71, r72, r73, r74, r75, r76, r77,
     &     r81, r82, r83, r84, r85, r86
      real :: r87, r88
!---------------------------------------------------------------------

      dyopiinv = 1./(8.*atan(1.))

      r71 = 0.015625
      r72 = 0.03125
      r73 = 0.06250
      r74 = 0.12500
      r75 = 0.25000
      r76 = 0.50000
      r77 = 1.0
      r81 = 0.0078125
      r82 = 0.015625
      r83 = 0.03125
      r84 = 0.06250
      r85 = 0.12500
      r86 = 0.25000
      r87 = 0.50000
      r88 = 1.0

      do 20 kb = 1, kp8         ! All boxes Childless & Parents
         ib = ic8(kb)
         jb = jc8(kb)
         xb = xc8(kb)
         yb = yc8(kb)
         ipar = (xb - x0)/ds7 + 1
         jpar = (yb - y0)/ds7 + 1
         do 1 i = 1, kp1
            kexam = kp1
            listexam(i) = liststart(i)
 1       end do
         call near_far(Nmax1, ipar, jpar, r71, ic1, jc1, kexam, listexam
     &   , kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax1, kclose, Listclose, kexam, listexam, kpart 
     &     , Listpart, ipar1Ch2, imark1) !NT

         call near_far(Nmax1, ib, jb, r81, ic1, jc1, kpart, Listpart, 
     &     kfar, Listfar, Kclose, Listclose)

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
 210        end do
 21      end do

         call near_far(Nmax2, ipar, jpar, r72, ic2, jc2, kexam, listexam
     &    , kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax2, kclose, Listclose, kexam, listexam, kpart 
     &     , Listpart, ipar2Ch3, imark2) ! NT

         call near_far(Nmax2, ib, jb, r82, ic2, jc2, kpart, Listpart, 
     &     kfar, Listfar, Kclose, Listclose)

         do 22 k = 1, kfar
            id = Listfar(k)
            n1 = npb2(id, 1)
            n2 = npb2(id, 2)
            do 220 np = n1, n2
               n4 = n4 + 1
               xt(n4) = xn(np)
               yt(n4) = yn(np)
               gt(n4) = gn(np)
 220        end do
 22      end do

         call near_far(Nmax3, ipar, jpar, r73, ic3, jc3, kexam, listexam
     &   , kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax3, kclose, Listclose, kexam, listexam, kpart 
     &     , Listpart, ipar3Ch4, imark3) ! NT

         call near_far(Nmax3, ib, jb, r83, ic3, jc3, kpart, Listpart, 
     &     kfar, Listfar, Kclose, Listclose)

         do 23 k = 1, kfar
            id = Listfar(k)
            n1 = npb3(id, 1)
            n2 = npb3(id, 2)
            do 230 np = n1, n2
               n4 = n4 + 1
               xt(n4) = xn(np)
               yt(n4) = yn(np)
               gt(n4) = gn(np)
 230        end do
 23      end do

         call near_far(Nmax4, ipar, jpar, r74, ic4, jc4, kexam, listexam
     &   ,  kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax4, kclose, Listclose, kexam, listexam, kpart 
     &     , Listpart, ipar4Ch5, imark4) ! NT

         call near_far(Nmax4, ib, jb, r84, ic4, jc4, kpart, Listpart, 
     &     kfar, Listfar, Kclose, Listclose)

         do 24 k = 1, kfar
            id = Listfar(k)
            n1 = npb4(id, 1)
            n2 = npb4(id, 2)
            do 240 np = n1, n2
               n4 = n4 + 1
               xt(n4) = xn(np)
               yt(n4) = yn(np)
               gt(n4) = gn(np)
 240        end do
 24      end do

         call near_far(Nmax5, ipar, jpar, r75, ic5, jc5, kexam, listexam
     &    , kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax5, kclose, Listclose, kexam, listexam, kpart 
     &     , Listpart, ipar5Ch6, imark5) ! NT

         call near_far(Nmax5, ib, jb, r85, ic5, jc5, kpart, Listpart, 
     &     kfar, Listfar, Kclose, Listclose)

         do 25 k = 1, kfar
            id = Listfar(k)
            n1 = npb5(id, 1)
            n2 = npb5(id, 2)
            do 250 np = n1, n2
               n4 = n4 + 1
               xt(n4) = xn(np)
               yt(n4) = yn(np)
               gt(n4) = gn(np)
 250        end do
 25      end do

         call near_far(Nmax6, ipar, jpar, r76, ic6, jc6, Kexam, listexam
     &   , kfar, Listfar, kclose, Listclose)

         call check_box(Nmax6, kclose, Listclose, 
     &     kexam, listexam, kpart, Listpart, ipar6Ch7, imark6)

         call near_far(Nmax6, ib, jb, r86, ic6, jc6, Kpart, Listpart
     &   , kfar, Listfar, kclose, Listclose)

         do 26 k = 1, kfar
            id = Listfar(k)
            n1 = npb6(id, 1)
            n2 = npb6(id, 2)
            do 260 np = n1, n2
               n4 = n4 + 1
               xt(n4) = xn(np)
               yt(n4) = yn(np)
               gt(n4) = gn(np)
 260        end do
 26      end do

         call near_far(Nmax7, ipar, jpar, r77, ic7, jc7, Kexam, listexam
     &    , kfar, Listfar, kclose, Listclose)

         call check_box(Nmax7, kclose, Listclose, 
     &     kexam, listexam, kpart, Listpart, ipar7Ch8, imark7)

         call near_far(Nmax7, ib, jb, r87, ic7, jc7, Kpart, Listpart
     &   , kfar, Listfar, kclose, Listclose)

         do 27 k = 1, kfar
            id = Listfar(k)
            n1 = npb7(id, 1)
            n2 = npb7(id, 2)
            do 270 np = n1, n2
               n4 = n4 + 1
               xt(n4) = xn(np)
               yt(n4) = yn(np)
               gt(n4) = gn(np)
 270        end do
 27      end do

         if (n4 == 0) goto 88
         if (n4 > np_max) write (*, *) 'error in rest8b', n4
         call int_box_part(Nmax8, kb, xb, yb, n4, Br8, Bi8)

 88      call near_far(Nmax8, ib, jb, r88, ic8, jc8, kexam, listexam, 
     &     kfar, Listfar, Kclose, Listclose)

! CDIR$SHORTLOOP
         do kbb = 1, kfar
            id = Listfar(kbb)
            Xbox(kbb) = xc8(id)
            Ybox(kbb) = yc8(id)

            Prbox(kbb, 0) = Pr8(id, 0)
            Pibox(kbb, 0) = Pi8(id, 0)
            Prbox(kbb, 1) = Pr8(id, 1)
            Pibox(kbb, 1) = Pi8(id, 1)
            Prbox(kbb, 2) = Pr8(id, 2)
            Pibox(kbb, 2) = Pi8(id, 2)
            Prbox(kbb, 3) = Pr8(id, 3)
            Pibox(kbb, 3) = Pi8(id, 3)
            Prbox(kbb, 4) = Pr8(id, 4)
            Pibox(kbb, 4) = Pi8(id, 4)
            Prbox(kbb, 5) = Pr8(id, 5)
            Pibox(kbb, 5) = Pi8(id, 5)
            Prbox(kbb, 6) = Pr8(id, 6)
            Pibox(kbb, 6) = Pi8(id, 6)
            Prbox(kbb, 7) = Pr8(id, 7)
            Pibox(kbb, 7) = Pi8(id, 7)
         enddo

         if (kfar > nbox_max) write (*, *) 'error in rest8', kbb
         call int_box(Nmax8, kb, xb, yb, kfar, Br8, Bi8)

 20   end do
      return
      end subroutine
