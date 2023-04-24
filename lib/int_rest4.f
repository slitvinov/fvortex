      subroutine int_rest4(kp4)

C     Same as int_rest2 for level 4 boxes.


      include 'tree_tmp.h'
      include 'main_dim.h'
      include 'part.h'
      include 'tree9.h'

      integer limpar
      real x0, y0
      common/geom/x0, y0, limpar

      integer kp4

      integer Listfar(Nhlp), Listclose(Nhlp), listexam(Nhlp)
      integer Listpart(Nhlp), kb, ib, jb, ipar, jpar, i, kexam
      integer n4, k, id, n1, n2, np, kbb, kclose, kfar, kpart
      real dyopiinv, r31, r32, r33, r41, r42, r43, r44, xb, yb
C---------------------------------------------------------------------

      dyopiinv = 1./(8.*atan(1.))

      r31 = 0.25
      r32 = 0.50
      r33 = 1.0
      r41 = 0.125
      r42 = 0.25
      r43 = 0.50
      r44 = 1.0

      do 20 kb = 1, kp4         ! All boxes Childless & Parents
         ib = ic4(kb)
         jb = jc4(kb)
         xb = xc4(kb)
         yb = yc4(kb)
         ipar = int((xb - x0)/ds3 + 1)
         jpar = int((yb - y0)/ds3 + 1)
         do 1 i = 1, kp1
            kexam = kp1
            listexam(i) = liststart(i)
    1    end do

         call near_far(Nmax1, ipar, jpar, r31, ic1, jc1, kexam,
     $        listexam,
     $        kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax1, kclose, Listclose, kexam, listexam, kpart
     $     , Listpart, ipar1Ch2, imark1) !NT

         call near_far(Nmax1, ib, jb, r41, ic1, jc1, kpart, Listpart,
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

         call near_far(Nmax2, ipar, jpar, r32, ic2, jc2, kexam,
     $        listexam,
     $        kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax2, kclose, Listclose, kexam, listexam, kpart
     $     , Listpart, ipar2Ch3, imark2) ! NT

         call near_far(Nmax2, ib, jb, r42, ic2, jc2, kpart, Listpart,
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

         call near_far(Nmax3, ipar, jpar, r33, ic3, jc3, Kexam,
     $        listexam,
     $        kfar, Listfar, kclose, Listclose)

         call check_box(Nmax3, kclose, Listclose,
     $     kexam, listexam, kpart, Listpart, ipar3Ch4, imark3)

         call near_far(Nmax3, ib, jb, r43, ic3, jc3, Kpart, Listpart,
     $     kfar, Listfar, kclose, Listclose)

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

         if (n4 == 0) goto 88
         if (n4 > np_max) write (*, *) 'error in rest4b', n4
         call int_box_part(Nmax4, kb, xb, yb, n4, Br4, Bi4)

   88    call near_far(Nmax4, ib, jb, r44, ic4, jc4, kexam, listexam,
     $     kfar, Listfar, Kclose, Listclose)

C CDIR$SHORTLOOP
         do 25 kbb = 1, kfar
            id = Listfar(kbb)
            Xbox(kbb) = xc4(id)
            Ybox(kbb) = yc4(id)
            Prbox(kbb, 0) = Pr4(id, 0)
            Pibox(kbb, 0) = Pi4(id, 0)
            Prbox(kbb, 1) = Pr4(id, 1)
            Pibox(kbb, 1) = Pi4(id, 1)
            Prbox(kbb, 2) = Pr4(id, 2)
            Pibox(kbb, 2) = Pi4(id, 2)
            Prbox(kbb, 3) = Pr4(id, 3)
            Pibox(kbb, 3) = Pi4(id, 3)
            Prbox(kbb, 4) = Pr4(id, 4)
            Pibox(kbb, 4) = Pi4(id, 4)
            Prbox(kbb, 5) = Pr4(id, 5)
            Pibox(kbb, 5) = Pi4(id, 5)
            Prbox(kbb, 6) = Pr4(id, 6)
            Pibox(kbb, 6) = Pi4(id, 6)
            Prbox(kbb, 7) = Pr4(id, 7)
            Pibox(kbb, 7) = Pi4(id, 7)
   25    end do

         if (kfar > nbox_max) write (*, *) 'error in rest4', kbb
         call int_box(Nmax4, kb, xb, yb, kfar, Br4, Bi4)

   20 end do
      return
      end
