subroutine int_rest9(kp9)

!  Same as int_rest2 for level 9 boxes.

   implicit none

   include 'tree_tmp.h'
   include 'main_dim.h'
   include 'part.h'
   include 'tree9.h'

   integer :: limpar
   real :: x0, y0
   common/geom/x0, y0, limpar

   integer :: kp9

   integer :: Listfar(Nhlp), Listclose(Nhlp), listexam(Nhlp)
   integer :: Listpart(Nhlp), kb, ib, jb, ipar, jpar, i, kexam
   integer :: n4, k, id, n1, n2, np, kbb, kclose, kfar, kpart
   real :: xb, yb, dyopiinv
   real :: r81, r82, r83, r84, r85, r86, r87, r88
   real :: r91, r92, r93, r94, r95, r96, r97, r98, r99
!---------------------------------------------------------------------

   dyopiinv = 1./(8.*atan(1.))

   r81 = 0.0078125
   r82 = 0.015625
   r83 = 0.03125
   r84 = 0.06250
   r85 = 0.12500
   r86 = 0.25000
   r87 = 0.50000
   r88 = 1.0
   r91 = 0.00390625
   r92 = 0.0078125
   r93 = 0.015625
   r94 = 0.03125
   r95 = 0.06250
   r96 = 0.12500
   r97 = 0.25000
   r98 = 0.50000
   r99 = 1.0

   do 20 kb = 1, kp9       ! All boxes Childless & Parents
      ib = ic9(kb)
      jb = jc9(kb)
      xb = xc9(kb)
      yb = yc9(kb)
      ipar = (xb - x0)/ds8 + 1
      jpar = (yb - y0)/ds8 + 1
      do 1 i = 1, kp1
         kexam = kp1
         listexam(i) = liststart(i)
1     end do
      call near_far(Nmax1, ipar, jpar, r81, ic1, jc1, kexam, listexam, &
                    kfar, Listfar, Kclose, Listclose)

      call check_box(Nmax1, kclose, Listclose, kexam, listexam, kpart &
                     , Listpart, ipar1Ch2, imark1)               !NT

      call near_far(Nmax1, ib, jb, r91, ic1, jc1, kpart, Listpart, &
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

      call near_far(Nmax2, ipar, jpar, r82, ic2, jc2, kexam, listexam, &
                    kfar, Listfar, Kclose, Listclose)

      call check_box(Nmax2, kclose, Listclose, kexam, listexam, kpart &
                     , Listpart, ipar2Ch3, imark2)                 ! NT

      call near_far(Nmax2, ib, jb, r92, ic2, jc2, kpart, Listpart, &
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

      call near_far(Nmax3, ipar, jpar, r83, ic3, jc3, kexam, listexam, &
                    kfar, Listfar, Kclose, Listclose)

      call check_box(Nmax3, kclose, Listclose, kexam, listexam, kpart &
                     , Listpart, ipar3Ch4, imark3)                 ! NT

      call near_far(Nmax3, ib, jb, r93, ic3, jc3, kpart, Listpart, &
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

      call near_far(Nmax4, ipar, jpar, r84, ic4, jc4, kexam, listexam, &
                    kfar, Listfar, Kclose, Listclose)

      call check_box(Nmax4, kclose, Listclose, kexam, listexam, kpart &
                     , Listpart, ipar4Ch5, imark4)                 ! NT

      call near_far(Nmax4, ib, jb, r94, ic4, jc4, kpart, Listpart, &
                    kfar, Listfar, Kclose, Listclose)

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

      call near_far(Nmax5, ipar, jpar, r85, ic5, jc5, kexam, listexam, &
                    kfar, Listfar, Kclose, Listclose)

      call check_box(Nmax5, kclose, Listclose, kexam, listexam, kpart &
                     , Listpart, ipar5Ch6, imark5)                 ! NT

      call near_far(Nmax5, ib, jb, r95, ic5, jc5, kpart, Listpart, &
                    kfar, Listfar, Kclose, Listclose)

      do 25 k = 1, kfar
         id = Listfar(k)
         n1 = npb5(id, 1)
         n2 = npb5(id, 2)
         do 250 np = n1, n2
            n4 = n4 + 1
            xt(n4) = xn(np)
            yt(n4) = yn(np)
            gt(n4) = gn(np)
250      end do
25    end do

      call near_far(Nmax6, ipar, jpar, r86, ic6, jc6, Kexam, listexam, &
                    kfar, Listfar, kclose, Listclose)

      call check_box(Nmax6, kclose, Listclose, &
                     kexam, listexam, kpart, Listpart, ipar6Ch7, imark6)

      call near_far(Nmax6, ib, jb, r96, ic6, jc6, Kpart, Listpart, &
                    kfar, Listfar, kclose, Listclose)

      do 26 k = 1, kfar
         id = Listfar(k)
         n1 = npb6(id, 1)
         n2 = npb6(id, 2)
         do 260 np = n1, n2
            n4 = n4 + 1
            xt(n4) = xn(np)
            yt(n4) = yn(np)
            gt(n4) = gn(np)
260      end do
26    end do

      call near_far(Nmax7, ipar, jpar, r87, ic7, jc7, Kexam, listexam, &
                    kfar, Listfar, kclose, Listclose)

      call check_box(Nmax7, kclose, Listclose, &
                     kexam, listexam, kpart, Listpart, ipar7Ch8, imark7)

      call near_far(Nmax7, ib, jb, r97, ic7, jc7, Kpart, Listpart, &
                    kfar, Listfar, kclose, Listclose)

      do 28 k = 1, kfar
         id = Listfar(k)
         n1 = npb7(id, 1)
         n2 = npb7(id, 2)
         do 280 np = n1, n2
            n4 = n4 + 1
            xt(n4) = xn(np)
            yt(n4) = yn(np)
            gt(n4) = gn(np)
280      end do
28    end do

      call near_far(Nmax8, ipar, jpar, r88, ic8, jc8, Kexam, listexam, &
                    kfar, Listfar, kclose, Listclose)

      call check_box(Nmax8, kclose, Listclose, &
                     kexam, listexam, kpart, Listpart, ipar8Ch9, imark8)

      call near_far(Nmax8, ib, jb, r98, ic8, jc8, Kpart, Listpart, &
                    kfar, Listfar, kclose, Listclose)

      do 29 k = 1, kfar
         id = Listfar(k)
         n1 = npb8(id, 1)
         n2 = npb8(id, 2)
         do 290 np = n1, n2
            n4 = n4 + 1
            xt(n4) = xn(np)
            yt(n4) = yn(np)
            gt(n4) = gn(np)
290      end do
29    end do

      if (n4 == 0) goto 88
      if (n4 > np_max) write (*, *) 'error in rest9b', n4
      call int_box_part(Nmax9, kb, xb, yb, n4, Br9, Bi9)

88    call near_far(Nmax9, ib, jb, r99, ic9, jc9, kexam, listexam, &
                    kfar, Listfar, Kclose, Listclose)

      ! CDIR$SHORTLOOP
      do 27 kbb = 1, kfar
         id = Listfar(kbb)
         Xbox(kbb) = xc9(id)
         Ybox(kbb) = yc9(id)

         Prbox(kbb, 0) = Pr9(id, 0)
         Pibox(kbb, 0) = Pi9(id, 0)
         Prbox(kbb, 1) = Pr9(id, 1)
         Pibox(kbb, 1) = Pi9(id, 1)
         Prbox(kbb, 2) = Pr9(id, 2)
         Pibox(kbb, 2) = Pi9(id, 2)
         Prbox(kbb, 3) = Pr9(id, 3)
         Pibox(kbb, 3) = Pi9(id, 3)
         Prbox(kbb, 4) = Pr9(id, 4)
         Pibox(kbb, 4) = Pi9(id, 4)
         Prbox(kbb, 5) = Pr9(id, 5)
         Pibox(kbb, 5) = Pi9(id, 5)
         Prbox(kbb, 6) = Pr9(id, 6)
         Pibox(kbb, 6) = Pi9(id, 6)
         Prbox(kbb, 7) = Pr9(id, 7)
         Pibox(kbb, 7) = Pi9(id, 7)
27    end do

      if (kfar > nbox_max) write (*, *) 'error in rest9', kbb
      call int_box(Nmax9, kb, xb, yb, kfar, Br9, Bi9)

20 end do
   return
end subroutine
