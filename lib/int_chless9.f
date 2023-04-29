      subroutine int_chless9(kp8, kp9)

C Same idea as in int_chless2 but now for level 9 childless boxes.
C For descriptive comments, go back to int_chless1&2

      include 'tree_tmp.h'
      include 'main_dim.h'
      include 'part.h'
      include 'tree9.h'

      integer limpar
      real x0
      real y0
      common/geom/x0, y0, limpar

      integer kp8
      integer kp9

      integer Listfar(Nhlp)
      integer Listclose(Nhlp)
      integer listexam(Nhlp)
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
      integer kfp
      integer nn
      integer n
      real r99
      real dyopiinv
      real up1
      real vp1
      real gp1

      dyopiinv = 1./(8.*atan(1.))

      r99 = 1.0

      do 20 kb = 1, Kp9
         nns = 0                ! List 1 (same level)
         nn = 0                 ! List 1 (finer levels)
         kfp = 0                ! List 3
         ib = ic9(kb)
         jb = jc9(kb)
         nb1 = npb9(kb, 1)
         nb2 = npb9(kb, 2)
         ipar = int((xc9(kb) - x0)/ds8 + 1)
         jpar = int((yc9(kb) - y0)/ds8 + 1)
         kc = 0
         do 21 k = 1, Kp8       ! Loop over boxes in parents level.
            i = ic8(k)
            j = jc8(k)
            if ((iabs(i - ipar) > 1) .or. (iabs(j - jpar) > 1)) goto 21
            kc = kc + 1
            Lclg(kc) = k
   21    continue

         kexam = 0
         do 22 m = 1, 4
            do 23 k = 1, kc
               ks = Lclg(k)
               km = ipar8Ch9(ks, m)
               if (km == 0) goto 23
               kexam = kexam + 1
               listexam(kexam) = km
   23       continue
   22    continue

         call near_far(Nmax9, ib, jb, r99, ic9, jc9, kexam, listexam
     $     , kfar, Listfar, Kclose, Listclose)

         do 25 k = 1, kclose
            id = Listclose(k)
            n1 = npb9(id, 1)
            n2 = npb9(id, 2)
            do 250 np = n1, n2
               nns = nns + 1
               xt(nns) = xn(np)
               yt(nns) = yn(np) ! childless boxes same level
               gt(nns) = gn(np)
  250       continue
   25    continue

         if (nns > np_max) write (*, *) 'error in int_chless8', nns
         do 251 n = nb1, nb2
            call int_part1(xn(n), yn(n), gn(n), up1, vp1, gp1, nns)
            uu(n) = uu(n) + up1*dyopiinv
            vv(n) = vv(n) + vp1*dyopiinv
            gdiff(n) = gdiff(n) + gp1
  251    continue
   20 continue
      end
