      subroutine int_chless8(kp7, kchildless8)

!     Same idea as in int_chless2 but now for level 8 childless boxes.
!     For descriptive comments, go back to int_chless1&2


      include 'tree_tmp.h'
      include 'main_dim.h'
      include 'part.h'
      include 'tree9.h'

      integer limpar
      real x0, y0
      common/geom/x0, y0, limpar

      integer kp7, kchildless8

      integer Listfar(Nhlp), Listclose(Nhlp), listexam(Nhlp)
      integer Listpart(Nhlp), Lclg(10), nns, ipar, jpar, kc,
     $     j, m, ks, km
      integer kexam, kfar, kclose, i, kh, kb, ib, jb
      integer nb1, nb2, k, id, n1, n2, np, level, kfp, nn, kpart, n
      real r88, r89, xnn, ynn, gnn, dyopiinv
      real up1, vp1, gp1, up2, vp2, gp2, ubox, vbox
!----------------------------------------------------------------------------

      dyopiinv = 1./(8.*atan(1.))

      r88 = 1.0
      r89 = 2.0

      do 20 kh = 1, Kchildless8
         nns = 0                ! List 1 (same level)
         nn = 0                 ! List 1 (finer levels)
         kfp = 0                ! List 3
         kb = ichildless8(kh)   ! box b index
         ib = ic8(kb)
         jb = jc8(kb)
         nb1 = npb8(kb, 1)
         nb2 = npb8(kb, 2)
         ipar = int((xc8(kb) - x0)/ds7 + 1)
         jpar = int((yc8(kb) - y0)/ds7 + 1)
         kc = 0
         do 21 k = 1, Kp7       ! Loop over boxes in parents level.
            i = ic7(k)
            j = jc7(k)
            if ((iabs(i - ipar) > 1) .or. (iabs(j - jpar) > 1)) goto 21
            kc = kc + 1
            Lclg(kc) = k
   21    end do

         kexam = 0
         do 22 m = 1, 4
            do 23 k = 1, kc
               ks = Lclg(k)
               km = ipar7Ch8(ks, m)
               if (km == 0) goto 23
               kexam = kexam + 1
               listexam(kexam) = km
   23       end do
   22    end do

         call near_far(Nmax8, ib, jb, r88, ic8, jc8, kexam, listexam
     $     , kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax8, kclose, Listclose, kexam, listexam, kpart
     $     , Listpart, ipar8Ch9, imark8)

         do 25 k = 1, kpart
            id = Listpart(k)
            n1 = npb8(id, 1)
            n2 = npb8(id, 2)
            do 250 np = n1, n2
               nns = nns + 1
               xt(nns) = xn(np)
               yt(nns) = yn(np) ! childless boxes same level
               gt(nns) = gn(np)
  250       end do
   25    end do

         if (nns > np_max) write (*, *) 'error in int_chless8', nns
         do 251 n = nb1, nb2
            call int_part1(xn(n), yn(n), gn(n), up1, vp1, gp1, nns)
            uu(n) = uu(n) + up1*dyopiinv
            vv(n) = vv(n) + vp1*dyopiinv
            gdiff(n) = gdiff(n) + gp1
  251    end do

! ____________________
         level = 9
         if (kexam == 0) goto 201

         call near_far(Nmax9, ib, jb, r89, ic9, jc9, kexam, listexam,
     $     kfar, Listfar, kclose, Listclose)

         do 34 k = 1, kfar
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
   34    end do

         do 37 k = 1, kclose    ! All close boxes are now childless
            id = Listclose(k)
            n1 = npb9(id, 1)
            n2 = npb9(id, 2)
            do 370 np = n1, n2
               nn = nn + 1
               xt(nn) = xn(np)
               yt(nn) = yn(np)
               gt(nn) = gn(np)
               it(nn) = np
  370       end do
   37    end do

         if (nn > np_max) write (*, *) 'error in int_chless6p', nn
         if (kfp > nbox_max) write (*, *) 'error in int_chless6b', kfp
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
