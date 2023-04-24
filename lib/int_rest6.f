      subroutine int_rest6(kp6)

C     Same as int_rest2 for level 6 boxes.


      include 'tree_tmp.h'
      include 'main_dim.h'
      include 'part.h'
      include 'tree9.h'

      integer limpar
      real x0, y0
      common/geom/x0, y0, limpar

      integer kp6

      integer Listfar(Nhlp), Listclose(Nhlp), listexam(Nhlp)
      integer Listpart(Nhlp), kb, ib, jb, ipar, jpar, i, kexam
      integer n4, k, id, n1, n2, np, kbb, kclose, kfar, kpart
      real dyopiinv, r51, r52, r53, r54, r55, r61, r62, r63,
     $     r64, r65, r66, xb, yb

      dyopiinv = 1./(8.*atan(1.))

      r51 = 0.06250
      r52 = 0.12500
      r53 = 0.25000
      r54 = 0.50000
      r55 = 1.0
      r61 = 0.03125
      r62 = 0.06250
      r63 = 0.12500
      r64 = 0.25000
      r65 = 0.50000
      r66 = 1.0

      do 20 kb = 1, kp6         ! All boxes Childless & Parents
         ib = ic6(kb)
         jb = jc6(kb)
         xb = xc6(kb)
         yb = yc6(kb)
         ipar = (xb - x0)/ds5 + 1
         jpar = (yb - y0)/ds5 + 1
         do 1 i = 1, kp1
            kexam = kp1
            listexam(i) = liststart(i)
    1    end do

         call near_far(Nmax1, ipar, jpar, r51, ic1, jc1, kexam,
     $        listexam,
     $        kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax1, kclose, Listclose, kexam, listexam, kpart
     $     , Listpart, ipar1Ch2, imark1) !NT

         call near_far(Nmax1, ib, jb, r61, ic1, jc1, kpart, Listpart,
     $     kfar, Listfar, Kclose, Listclose)

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
  210       end do
   21    end do

         call near_far(Nmax2, ipar, jpar, r52, ic2, jc2, kexam,
     $        listexam,
     $        kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax2, kclose, Listclose, kexam, listexam,
     $        kpart,
     $        Listpart, ipar2Ch3, imark2) ! NT

         call near_far(Nmax2, ib, jb, r62, ic2, jc2, kpart, Listpart,
     $     kfar, Listfar, Kclose, Listclose)

         do 22 k = 1, kfar
            id = Listfar(k)
            n1 = npb2(id, 1)
            n2 = npb2(id, 2)
            do 220 np = n1, n2
               n4 = n4 + 1
               xt(n4) = xn(np)
               yt(n4) = yn(np)
               gt(n4) = gn(np)
  220       end do
   22    end do

         call near_far(Nmax3, ipar, jpar, r53, ic3, jc3, kexam,
     $        listexam,
     $        kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax3, kclose, Listclose, kexam, listexam, kpart
     $     , Listpart, ipar3Ch4, imark3) ! NT

         call near_far(Nmax3, ib, jb, r63, ic3, jc3, kpart, Listpart,
     $     kfar, Listfar, Kclose, Listclose)

         do 23 k = 1, kfar
            id = Listfar(k)
            n1 = npb3(id, 1)
            n2 = npb3(id, 2)
            do 230 np = n1, n2
               n4 = n4 + 1
               xt(n4) = xn(np)
               yt(n4) = yn(np)
               gt(n4) = gn(np)
  230       end do
   23    end do

         call near_far(Nmax4, ipar, jpar, r54, ic4, jc4, kexam,
     $        listexam,
     $        kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax4, kclose, Listclose, kexam, listexam, kpart
     $     , Listpart, ipar4Ch5, imark4) ! NT

         call near_far(Nmax4, ib, jb, r64, ic4, jc4, kpart, Listpart,
     $     kfar, Listfar, Kclose, Listclose)

         do 24 k = 1, kfar
            id = Listfar(k)
            n1 = npb4(id, 1)
            n2 = npb4(id, 2)
            do 240 np = n1, n2
               n4 = n4 + 1
               xt(n4) = xn(np)
               yt(n4) = yn(np)
               gt(n4) = gn(np)
  240       end do
   24    end do

         call near_far(Nmax5, ipar, jpar, r55, ic5, jc5, Kexam,
     $        listexam,
     $        kfar, Listfar, kclose, Listclose)

         call check_box(Nmax5, kclose, Listclose,
     $     kexam, listexam, kpart, Listpart, ipar5Ch6, imark5)

         call near_far(Nmax5, ib, jb, r65, ic5, jc5, Kpart, Listpart,
     $     kfar, Listfar, kclose, Listclose)

         do 25 k = 1, kfar
            id = Listfar(k)
            n1 = npb5(id, 1)
            n2 = npb5(id, 2)
            do 250 np = n1, n2
               n4 = n4 + 1
               xt(n4) = xn(np)
               yt(n4) = yn(np)
               gt(n4) = gn(np)
  250       end do
   25    end do

         if (n4 == 0) goto 88
         if (n4 > np_max) write (*, *) 'error in rest6b', n4
         call int_box_part(Nmax6, kb, xb, yb, n4, Br6, Bi6)

   88    call near_far(Nmax6, ib, jb, r66, ic6, jc6, kexam, listexam,
     $     kfar, Listfar, Kclose, Listclose)

C CDIR$SHORTLOOP
         do 26 kbb = 1, kfar
            id = Listfar(kbb)
            Xbox(kbb) = xc6(id)
            Ybox(kbb) = yc6(id)
            Prbox(kbb, 0) = Pr6(id, 0)
            Pibox(kbb, 0) = Pi6(id, 0)
            Prbox(kbb, 1) = Pr6(id, 1)
            Pibox(kbb, 1) = Pi6(id, 1)
            Prbox(kbb, 2) = Pr6(id, 2)
            Pibox(kbb, 2) = Pi6(id, 2)
            Prbox(kbb, 3) = Pr6(id, 3)
            Pibox(kbb, 3) = Pi6(id, 3)
            Prbox(kbb, 4) = Pr6(id, 4)
            Pibox(kbb, 4) = Pi6(id, 4)
            Prbox(kbb, 5) = Pr6(id, 5)
            Pibox(kbb, 5) = Pi6(id, 5)
            Prbox(kbb, 6) = Pr6(id, 6)
            Pibox(kbb, 6) = Pi6(id, 6)
            Prbox(kbb, 7) = Pr6(id, 7)
            Pibox(kbb, 7) = Pi6(id, 7)
   26    end do

         if (kfar > nbox_max) write (*, *) 'error in rest6', kbb
         call int_box(Nmax6, kb, xb, yb, kfar, Br6, Bi6)

   20 end do
      return
      end
