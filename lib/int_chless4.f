      subroutine int_chless4(kp3, kchildless4)

!     Same idea as in int_chless2 but now for level 4 childless boxes.
!     For descriptive comments, go back to int_chless1&2


      include 'tree_tmp.h'
      include 'main_dim.h'
      include 'part.h'
      include 'tree9.h'

      integer limpar
      real x0, y0
      common/geom/x0, y0, limpar

      integer kp3, kchildless4

      integer Listfar(Nhlp), Listclose(Nhlp), listexam(Nhlp)
      integer Listpart(Nhlp), Lclg(10), nns, ipar, jpar, kc,
     &     j, m, ks, km
      integer kexam, kfar, kclose, i, kh, kb, ib, jb
      integer nb1, nb2, k, id, n1, n2, np, level, kfp, nn, kpart, n
      real r41, r42, r43, r44, r45, r46, r47, r48, r49, xnn,
     &     ynn, gnn, dyopiinv
      real up1, vp1, gp1, up2, vp2, gp2, ubox, vbox
!----------------------------------------------------------------------------

      dyopiinv = 1./(8.*atan(1.))

      r41 = 0.125
      r42 = 0.25
      r43 = 0.5
      r44 = 1.0
      r45 = 2.0
      r46 = 4.0
      r47 = 8.0
      r48 = 16.0
      r49 = 32.0

      do 20 kh = 1, Kchildless4
         nns = 0                ! List 1 (same level)
         nn = 0                 ! List 1 (finer levels)
         kfp = 0                ! List 3
         kb = ichildless4(kh)   ! box b index
         ib = ic4(kb)
         jb = jc4(kb)
         nb1 = npb4(kb, 1)
         nb2 = npb4(kb, 2)
         ipar = int((xc4(kb) - x0)/ds3 + 1)
         jpar = int((yc4(kb) - y0)/ds3 + 1)
         kc = 0
         do 21 k = 1, Kp3       ! Loop over boxes in parents level.
            i = ic3(k)
            j = jc3(k)
            if ((iabs(i - ipar) > 1) .or. (iabs(j - jpar) > 1)) goto 21
            kc = kc + 1
            Lclg(kc) = k
   21    end do

         kexam = 0
         do 22 m = 1, 4
            do 23 k = 1, kc
               ks = Lclg(k)
               km = ipar3Ch4(ks, m)
               if (km == 0) goto 23
               kexam = kexam + 1
               listexam(kexam) = km
   23       end do
   22    end do

         call near_far(Nmax4, ib, jb, r44, ic4, jc4, kexam, listexam
     &     , kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax4, kclose, Listclose, kexam, listexam, kpart
     &     , Listpart, ipar4Ch5, imark4)

         do 25 k = 1, kpart
            id = Listpart(k)
            n1 = npb4(id, 1)
            n2 = npb4(id, 2)
            do 250 np = n1, n2
               nns = nns + 1
               xt(nns) = xn(np)
               yt(nns) = yn(np) ! childless boxes same level
               gt(nns) = gn(np)
  250       end do
   25    end do

         if (nns > np_max) write (*, *) 'error in int_chless4', nns
         do 251 n = nb1, nb2
            call int_part1(xn(n), yn(n), gn(n), up1, vp1, gp1, nns)
            uu(n) = uu(n) + up1*dyopiinv
            vv(n) = vv(n) + vp1*dyopiinv
            gdiff(n) = gdiff(n) + gp1
  251    end do

! ____________________
         level = 5
         if (kexam == 0) goto 201

         call near_far(Nmax5, ib, jb, r45, ic5, jc5, kexam, listexam,
     &     kfar, Listfar, kclose, Listclose)

         do 30 k = 1, kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = xc5(id)
            Ybox(kfp) = yc5(id)
            Prbox(kfp, 0) = Pr5(id, 0)
            Pibox(kfp, 0) = Pi5(id, 0)
            Prbox(kfp, 1) = Pr5(id, 1)
            Pibox(kfp, 1) = Pi5(id, 1)
            Prbox(kfp, 2) = Pr5(id, 2)
            Pibox(kfp, 2) = Pi5(id, 2)
            Prbox(kfp, 3) = Pr5(id, 3)
            Pibox(kfp, 3) = Pi5(id, 3)
            Prbox(kfp, 4) = Pr5(id, 4)
            Pibox(kfp, 4) = Pi5(id, 4)
            Prbox(kfp, 5) = Pr5(id, 5)
            Pibox(kfp, 5) = Pi5(id, 5)
            Prbox(kfp, 6) = Pr5(id, 6)
            Pibox(kfp, 6) = Pi5(id, 6)
            Prbox(kfp, 7) = Pr5(id, 7)
            Pibox(kfp, 7) = Pi5(id, 7)
   30    end do

         call check_box(Nmax5, Kclose, Listclose, kexam, listexam, Kpart
     &     , Listpart, ipar5Ch6, imark5)

         do 31 k = 1, kpart
            id = Listpart(k)
            n1 = npb5(id, 1)
            n2 = npb5(id, 2)
            do 310 np = n1, n2
               nn = nn + 1
               xt(nn) = xn(np)
               yt(nn) = yn(np)
               gt(nn) = gn(np)
               it(nn) = np
  310       end do
   31    end do

! ____________________
         level = 6
         if (kexam == 0) goto 201

         call near_far(Nmax6, ib, jb, r46, ic6, jc6, kexam, listexam,
     &     kfar, Listfar, kclose, Listclose)
         do 32 k = 1, kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = xc6(id)
            Ybox(kfp) = yc6(id)
            Prbox(kfp, 0) = Pr6(id, 0)
            Pibox(kfp, 0) = Pi6(id, 0)
            Prbox(kfp, 1) = Pr6(id, 1)
            Pibox(kfp, 1) = Pi6(id, 1)
            Prbox(kfp, 2) = Pr6(id, 2)
            Pibox(kfp, 2) = Pi6(id, 2)
            Prbox(kfp, 3) = Pr6(id, 3)
            Pibox(kfp, 3) = Pi6(id, 3)
            Prbox(kfp, 4) = Pr6(id, 4)
            Pibox(kfp, 4) = Pi6(id, 4)
            Prbox(kfp, 5) = Pr6(id, 5)
            Pibox(kfp, 5) = Pi6(id, 5)
            Prbox(kfp, 6) = Pr6(id, 6)
            Pibox(kfp, 6) = Pi6(id, 6)
            Prbox(kfp, 7) = Pr6(id, 7)
            Pibox(kfp, 7) = Pi6(id, 7)
   32    end do

         call check_box(Nmax6, Kclose, Listclose, kexam, listexam, Kpart
     &     , Listpart, ipar6Ch7, imark6)

         do 33 k = 1, kpart
            id = Listpart(k)
            n1 = npb6(id, 1)
            n2 = npb6(id, 2)
            do 330 np = n1, n2
               nn = nn + 1
               xt(nn) = xn(np)
               yt(nn) = yn(np)
               gt(nn) = gn(np)
               it(nn) = np
  330       end do
   33    end do

! ____________________
         level = 7
         if (kexam == 0) goto 201

         call near_far(Nmax7, ib, jb, r47, ic7, jc7, kexam, listexam,
     &     kfar, Listfar, kclose, Listclose)

         do 34 k = 1, kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = xc7(id)
            Ybox(kfp) = yc7(id)
            Prbox(kfp, 0) = Pr7(id, 0)
            Pibox(kfp, 0) = Pi7(id, 0)
            Prbox(kfp, 1) = Pr7(id, 1)
            Pibox(kfp, 1) = Pi7(id, 1)
            Prbox(kfp, 2) = Pr7(id, 2)
            Pibox(kfp, 2) = Pi7(id, 2)
            Prbox(kfp, 3) = Pr7(id, 3)
            Pibox(kfp, 3) = Pi7(id, 3)
            Prbox(kfp, 4) = Pr7(id, 4)
            Pibox(kfp, 4) = Pi7(id, 4)
            Prbox(kfp, 5) = Pr7(id, 5)
            Pibox(kfp, 5) = Pi7(id, 5)
            Prbox(kfp, 6) = Pr7(id, 6)
            Pibox(kfp, 6) = Pi7(id, 6)
            Prbox(kfp, 7) = Pr7(id, 7)
            Pibox(kfp, 7) = Pi7(id, 7)

   34    end do

         call check_box(Nmax7, Kclose, Listclose, kexam, listexam,
     &        Kpart,
     &        Listpart, ipar7Ch8, imark7)

         do 35 k = 1, kpart
            id = Listpart(k)
            n1 = npb7(id, 1)
            n2 = npb7(id, 2)
            do 350 np = n1, n2
               nn = nn + 1
               xt(nn) = xn(np)
               yt(nn) = yn(np)
               gt(nn) = gn(np)
               it(nn) = np
  350       end do
   35    end do

! ____________________
         level = 8

         call near_far(Nmax8, ib, jb, r48, ic8, jc8, kexam, listexam,
     &     kfar, Listfar, kclose, Listclose)

         do 36 k = 1, kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = xc8(id)
            Ybox(kfp) = yc8(id)
            Prbox(kfp, 0) = Pr8(id, 0)
            Pibox(kfp, 0) = Pi8(id, 0)
            Prbox(kfp, 1) = Pr8(id, 1)
            Pibox(kfp, 1) = Pi8(id, 1)
            Prbox(kfp, 2) = Pr8(id, 2)
            Pibox(kfp, 2) = Pi8(id, 2)
            Prbox(kfp, 3) = Pr8(id, 3)
            Pibox(kfp, 3) = Pi8(id, 3)
            Prbox(kfp, 4) = Pr8(id, 4)
            Pibox(kfp, 4) = Pi8(id, 4)
            Prbox(kfp, 5) = Pr8(id, 5)
            Pibox(kfp, 5) = Pi8(id, 5)
            Prbox(kfp, 6) = Pr8(id, 6)
            Pibox(kfp, 6) = Pi8(id, 6)
            Prbox(kfp, 7) = Pr8(id, 7)
            Pibox(kfp, 7) = Pi8(id, 7)
   36    end do

         call check_box(Nmax8, Kclose, Listclose, kexam, listexam,
     &        Kpart,
     &        Listpart, ipar8Ch9, imark8)

         do 37 k = 1, kpart
            id = Listpart(k)
            n1 = npb8(id, 1)
            n2 = npb8(id, 2)
            do 370 np = n1, n2
               nn = nn + 1
               xt(nn) = xn(np)
               yt(nn) = yn(np)
               gt(nn) = gn(np)
               it(nn) = np
  370       end do
   37    end do

! ____________________
         level = 9

         call near_far(Nmax9, ib, jb, r49, ic9, jc9, kexam, listexam,
     &     kfar, Listfar, kclose, Listclose)

         do k = 1, kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = xc9(id)
            Ybox(kfp) = yc9(id)
            Prbox(kfp, 0) = Pr9(id, 0)
            Pibox(kfp, 0) = Pi9(id, 0)
            Prbox(kfp, 1) = Pr9(id, 1)
            Pibox(kfp, 1) = Pi9(id, 1)
            Prbox(kfp, 2) = Pr9(id, 2)
            Pibox(kfp, 2) = Pi9(id, 2)
            Prbox(kfp, 3) = Pr9(id, 3)
            Pibox(kfp, 3) = Pi9(id, 3)
            Prbox(kfp, 4) = Pr9(id, 4)
            Pibox(kfp, 4) = Pi9(id, 4)
            Prbox(kfp, 5) = Pr9(id, 5)
            Pibox(kfp, 5) = Pi9(id, 5)
            Prbox(kfp, 6) = Pr9(id, 6)
            Pibox(kfp, 6) = Pi9(id, 6)
            Prbox(kfp, 7) = Pr9(id, 7)
            Pibox(kfp, 7) = Pi9(id, 7)
         enddo

         do k = 1, kclose       ! All close boxes are now childless
            id = Listclose(k)
            n1 = npb9(id, 1)
            n2 = npb9(id, 2)
            do np = n1, n2
               nn = nn + 1
               xt(nn) = xn(np)
               yt(nn) = yn(np)
               gt(nn) = gn(np)
               it(nn) = np
            enddo
         enddo

         if (nn > np_max) write (*, *) 'error in int_chless4p', nn
         if (kfp > nbox_max) write (*, *) 'error in int_chless4b', kfp
  201    do 351 n = nb1, nb2
            xnn = xn(n)
            ynn = yn(n)
            gnn = gn(n)
            call int_part2(gnn, xnn, ynn, up2, vp2, gp2, nn)
            call int_part_box(xnn, ynn, ubox, vbox, kfp)
            uu(n) = uu(n) + (up2 + ubox)*dyopiinv
            vv(n) = vv(n) + (vp2 + vbox)*dyopiinv
            gdiff(n) = gdiff(n) + gp2
  351    end do

   20 end do

      return
      end
