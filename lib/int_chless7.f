      subroutine int_chless7(kp6, kchildless7)

C Same idea as in int_chless2 but now for level 7 childless boxes.
C For descriptive comments, go back to int_chless1&2

      include 'tree_tmp.h'
      include 'main_dim.h'
      include 'part.h'
      include 'tree9.h'

      integer limpar
      real x0
      real y0
      common/geom/x0, y0, limpar

      integer kp6
      integer kchildless7

      integer Listfar(Nhlp)
      integer Listclose(Nhlp)
      integer listexam(Nhlp)
      integer Listpart(Nhlp)
      integer Lclg(10)
      integer nns
      integer ipar
      integer jpar
      integer kc
      integer j
      integer m
      integer ks
      integer km
      integer kexam
      integer kfar
      integer kclose
      integer i
      integer kh
      integer kb
      integer ib
      integer jb
      integer nb1
      integer nb2
      integer k
      integer id
      integer n1
      integer n2
      integer np
      integer level
      integer kfp
      integer nn
      integer kpart
      integer n
      real r77
      real r78
      real r79
      real xnn
      real ynn
      real gnn
      real dyopiinv
      real up1
      real vp1
      real gp1
      real up2
      real vp2
      real gp2
      real ubox
      real vbox

      dyopiinv = 1./(8.*atan(1.))

      r77 = 1.0
      r78 = 2.0
      r79 = 4.0

      do 20 kh = 1, Kchildless7
         nns = 0                ! List 1 (same level)
         nn = 0                 ! List 1 (finer levels)
         kfp = 0                ! List 3
         kb = ichildless7(kh)   ! box b index
         ib = ic7(kb)
         jb = jc7(kb)
         nb1 = npb7(kb, 1)
         nb2 = npb7(kb, 2)
         ipar = int((xc7(kb) - x0)/ds6 + 1)
         jpar = int((yc7(kb) - y0)/ds6 + 1)
         kc = 0
         do 21 k = 1, Kp6       ! Loop over boxes in parents level.
            i = ic6(k)
            j = jc6(k)
            if ((iabs(i - ipar) > 1) .or. (iabs(j - jpar) > 1)) goto 21
            kc = kc + 1
            Lclg(kc) = k
   21    continue

         kexam = 0
         do 22 m = 1, 4
            do 23 k = 1, kc
               ks = Lclg(k)
               km = ipar6Ch7(ks, m)
               if (km == 0) goto 23
               kexam = kexam + 1
               listexam(kexam) = km
   23       continue
   22    continue

         call near_far(Nmax7, ib, jb, r77, ic7, jc7, kexam, listexam
     $     , kfar, Listfar, Kclose, Listclose)

         call check_box(Nmax7, kclose, Listclose, kexam, listexam, kpart
     $     , Listpart, ipar7Ch8, imark7)

         do 25 k = 1, kpart
            id = Listpart(k)
            n1 = npb7(id, 1)
            n2 = npb7(id, 2)
            do 250 np = n1, n2
               nns = nns + 1
               xt(nns) = xn(np)
               yt(nns) = yn(np) ! childless boxes same level
               gt(nns) = gn(np)
  250       continue
   25    continue

         if (nns > np_max) write (*, *) 'error in int_chless7', nns
         do 251 n = nb1, nb2
            call int_part1(xn(n), yn(n), gn(n), up1, vp1, gp1, nns)
            uu(n) = uu(n) + up1*dyopiinv
            vv(n) = vv(n) + vp1*dyopiinv
            gdiff(n) = gdiff(n) + gp1
  251    continue

         level = 8
         if (kexam == 0) goto 201

         call near_far(Nmax8, ib, jb, r78, ic8, jc8, kexam, listexam,
     $     kfar, Listfar, kclose, Listclose)

         do 34 k = 1, kfar
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
   34    continue

         call check_box(Nmax8, kclose, Listclose, kexam, listexam, kpart
     $     , Listpart, ipar8Ch9, imark8)

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
  370       continue
   37    continue

         level = 9
         if (kexam == 0) goto 201

         call near_far(Nmax9, ib, jb, r79, ic9, jc9, kexam, listexam,
     $     kfar, Listfar, kclose, Listclose)

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
  351    continue

   20 continue
      end
