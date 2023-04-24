      subroutine int_rest3(kp3)

C Same as int_rest2 for level 3 boxes.

      include 'tree_tmp.h'
      include 'main_dim.h'
      include 'part.h'
      include 'tree9.h'

      integer limpar
      real x0, y0
      common/geom/x0, y0, limpar

      integer kp3

      integer Listfar(Nhlp), Listclose(Nhlp), listexam(Nhlp)
      integer Listpart(Nhlp), kb, ib, jb, ipar, jpar, i, kexam
      integer n4, k, id, n1, n2, np, kbb, kclose, kfar, kpart
      real dyopiinv, r21, r22, r31, r32, r33, xb, yb

      dyopiinv = 1./(8.*atan(1.))

      r21 = 0.50
      r22 = 1.0
      r31 = 0.25
      r32 = 0.50
      r33 = 1.0

      do 20 kb = 1, kp3         ! All boxes Childless & Parents
         ib = ic3(kb)
         jb = jc3(kb)
         xb = xc3(kb)
         yb = yc3(kb)
         ipar = int((xb - x0)/ds2 + 1)
         jpar = int((yb - y0)/ds2 + 1)
         do 1 i = 1, kp1
            kexam = kp1
            listexam(i) = liststart(i)
    1    end do

         call near_far(Nmax1, ipar, jpar, r21, ic1, jc1, kexam,
     $        listexam,
     $        kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax1, kclose, Listclose, kexam, listexam,
     $        kpart
     $        , Listpart, ipar1Ch2, imark1)

         call near_far(Nmax1, ib, jb, r31, ic1, jc1, kpart, Listpart,
     $        kfar, Listfar, Kclose, Listclose)

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

         call near_far(Nmax2, ipar, jpar, r22, ic2, jc2, kexam,
     $        listexam,
     $        kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax2, kclose, Listclose, kexam, listexam, kpart
     $    , Listpart, ipar2Ch3, imark2)

         call near_far(Nmax2, ib, jb, r32, ic2, jc2, kpart, Listpart,
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

         if (n4 == 0) goto 88
         if (n4 > np_max) write (*, *) 'error in rest3b', n4
         call int_box_part(Nmax3, kb, xb, yb, n4, Br3, Bi3)

   88    call near_far(Nmax3, ib, jb, r33, ic3, jc3, kexam, listexam,
     $     kfar, Listfar, Kclose, Listclose)

C CDIR$SHORTLOOP
         do 25 kbb = 1, kfar
            id = Listfar(kbb)
            Xbox(kbb) = xc3(id)
            Ybox(kbb) = yc3(id)
            Prbox(kbb, 0) = Pr3(id, 0)
            Pibox(kbb, 0) = Pi3(id, 0)
            Prbox(kbb, 1) = Pr3(id, 1)
            Pibox(kbb, 1) = Pi3(id, 1)
            Prbox(kbb, 2) = Pr3(id, 2)
            Pibox(kbb, 2) = Pi3(id, 2)
            Prbox(kbb, 3) = Pr3(id, 3)
            Pibox(kbb, 3) = Pi3(id, 3)
            Prbox(kbb, 4) = Pr3(id, 4)
            Pibox(kbb, 4) = Pi3(id, 4)
            Prbox(kbb, 5) = Pr3(id, 5)
            Pibox(kbb, 5) = Pi3(id, 5)
            Prbox(kbb, 6) = Pr3(id, 6)
            Pibox(kbb, 6) = Pi3(id, 6)
            Prbox(kbb, 7) = Pr3(id, 7)
            Pibox(kbb, 7) = Pi3(id, 7)
   25    end do

         if (kfar > nbox_max) write (*, *) 'error in rest3', kbb
         call int_box(Nmax3, kb, xb, yb, kfar, Br3, Bi3)

   20 end do
      end
