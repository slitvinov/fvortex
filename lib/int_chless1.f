      subroutine int_chless1(kchildless1)
C These subroutines determine the hierarchy of interactions and call for
C the interactions. Particle interactions are assigned in the int_chless
C routines as they only happen for childless boxes (otherwise you go to
C the next level in hopes of geting some box interactions). Box
C interactions are taken care of by the int_rest routines. Particle-box
C interactions occur in both, with the particle end of these
C interactions in int_rest and box in int_chless.

C This subroutine figures out the interactions for childless boxes at
C level 1 (the highest level of the tree).  They can interact as
C particle-particle with nearby childless boxes and particle-box with
C finer boxes which are far enough away (bigger box must be considered
C as particles for the smaller box in this case but smaller box as a box
C to bigger box). Box-box interactions cannot happen for a level 1 box
C and are handled by the int_rest arrays rather than int_chless.

      include 'tree_tmp.h'
      include 'main_dim.h'
      include 'part.h'
      include 'tree9.h'

      integer kchildless1

      integer Listfar(Nhlp)
      integer Listclose(Nhlp)
      integer listexam(Nhlp)
      integer Listpart(Nhlp)
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
      integer nns
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
      real r12
      real r13
      real r14
      real r15
      real r16
      real r17
      real r18
      real r19
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
      r12 = 2.0
      r13 = 4.0
      r14 = 8.0
      r15 = 16.0
      r16 = 32.0
      r17 = 64.0
      r18 = 128.0
      r19 = 256.0
      kexam = 0
      kfar = 0
      kclose = 0
      do i = 1, nhlp
         listexam(i) = 0
         Listfar(i) = 0
         Listclose(i) = 0
      enddo

      do 10 kh = 1, Kchildless1
         kb = ichildless1(kh)
         Ib = ic1(kb)
         Jb = jc1(kb)
         nb1 = npb1(kb, 1)
         nb2 = npb1(kb, 2)
         do 1 i = 1, kp1
            kclose = kclose + 1
            Listclose(i) = liststart(i)
    1    continue

C Construct the interaction list with particles and boxes that belong
C to finer levels than the ** 1st **.
C First find all childless boxes on level 1.
C Note that the box will find and interact with itself (as particles)

         call check_box(Nmax1, kclose, Listclose, kexam, listexam,
     $        kpart,
     $        Listpart, ipar1Ch2, imark1)

         nns = 0
         do 11 k = 1, Kpart
            id = Listpart(k)
            n1 = npb1(id, 1)
            n2 = npb1(id, 2)
            do 111 np = n1, n2
               nns = nns + 1
               xt(nns) = xn(np)
               yt(nns) = yn(np)
               gt(nns) = gn(np)
  111       continue
   11    continue

         if (nns > np_max) write (*, *) 'error in int_chless1', nns
         do 251 n = nb1, nb2
            call int_part1(xn(n), yn(n), gn(n), up1, vp1, gp1, nns)
            uu(n) = uu(n) + up1*dyopiinv
            vv(n) = vv(n) + vp1*dyopiinv
            gdiff(n) = gdiff(n) + gp1
  251    continue

         level = 2

C Find which level 2 boxes are far enough away to interact as a box with
C level 1 particles (level 2 boxes are the 4 subdivisions of a level 1
C box).

         call near_far(Nmax2, ib, jb, r12, ic2, jc2, kexam, listexam,
     $     kfar, Listfar, kclose, Listclose)

         kfp = 0
         do 12 k = 1, Kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = xc2(id)
            Ybox(kfp) = yc2(id)
            Prbox(kfp, 0) = Pr2(id, 0)
            Pibox(kfp, 0) = Pi2(id, 0)
            Prbox(kfp, 1) = Pr2(id, 1)
            Pibox(kfp, 1) = Pi2(id, 1)
            Prbox(kfp, 2) = Pr2(id, 2)
            Pibox(kfp, 2) = Pi2(id, 2)
            Prbox(kfp, 3) = Pr2(id, 3)
            Pibox(kfp, 3) = Pi2(id, 3)
            Prbox(kfp, 4) = Pr2(id, 4)
            Pibox(kfp, 4) = Pi2(id, 4)
            Prbox(kfp, 5) = Pr2(id, 5)
            Pibox(kfp, 5) = Pi2(id, 5)
            Prbox(kfp, 6) = Pr2(id, 6)
            Pibox(kfp, 6) = Pi2(id, 6)
            Prbox(kfp, 7) = Pr2(id, 7)
            Pibox(kfp, 7) = Pi2(id, 7)
   12    continue

C Check the remaining level 2 boxes for childless boxes. Since they
C didn't interact as a box above and further subdivisions don't exist
C for the box, it must now interact as particles.

         call check_box(Nmax2, kclose, Listclose, kexam, listexam,
     $        kpart,
     $        Listpart, ipar2Ch3, imark2)

         nn = 0
         do 25 k = 1, kpart
            id = Listpart(k)
            n1 = npb2(id, 1)
            n2 = npb2(id, 2)
            do 250 np = n1, n2
               nn = nn + 1
               xt(nn) = xn(np)
               yt(nn) = yn(np)
               gt(nn) = gn(np)
               it(nn) = np
  250       continue
   25    continue

C All remaining boxes (those which have not yet interacted in some way)
C are parents, thus go to their level 3 children. Process of level 2
C repeats for all subsequent levels.
         level = 3

         call near_far(Nmax3, ib, jb, r13, ic3, jc3, kexam, listexam,
     $     kfar, Listfar, kclose, Listclose)

         do 26 k = 1, kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = xc3(id)
            Ybox(kfp) = yc3(id)
            Prbox(kfp, 0) = Pr3(id, 0)
            Pibox(kfp, 0) = Pi3(id, 0)
            Prbox(kfp, 1) = Pr3(id, 1)
            Pibox(kfp, 1) = Pi3(id, 1)
            Prbox(kfp, 2) = Pr3(id, 2)
            Pibox(kfp, 2) = Pi3(id, 2)
            Prbox(kfp, 3) = Pr3(id, 3)
            Pibox(kfp, 3) = Pi3(id, 3)
            Prbox(kfp, 4) = Pr3(id, 4)
            Pibox(kfp, 4) = Pi3(id, 4)
            Prbox(kfp, 5) = Pr3(id, 5)
            Pibox(kfp, 5) = Pi3(id, 5)
            Prbox(kfp, 6) = Pr3(id, 6)
            Pibox(kfp, 6) = Pi3(id, 6)
            Prbox(kfp, 7) = Pr3(id, 7)
            Pibox(kfp, 7) = Pi3(id, 7)

   26    continue

         call check_box(Nmax3, Kclose, Listclose, kexam, listexam, Kpart
     $     , Listpart, ipar3Ch4, imark3)

         do 27 k = 1, kpart
            id = Listpart(k)
            n1 = npb3(id, 1)
            n2 = npb3(id, 2)
            do 270 np = n1, n2
               nn = nn + 1
               xt(nn) = xn(np)
               yt(nn) = yn(np)
               gt(nn) = gn(np)
               it(nn) = np
  270       continue
   27    continue

         level = 4

         call near_far(Nmax4, ib, jb, r14, ic4, jc4, kexam, listexam,
     $     kfar, Listfar, kclose, Listclose)

         do 28 k = 1, kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = xc4(id)
            Ybox(kfp) = yc4(id)
            Prbox(kfp, 0) = Pr4(id, 0)
            Pibox(kfp, 0) = Pi4(id, 0)
            Prbox(kfp, 1) = Pr4(id, 1)
            Pibox(kfp, 1) = Pi4(id, 1)
            Prbox(kfp, 2) = Pr4(id, 2)
            Pibox(kfp, 2) = Pi4(id, 2)
            Prbox(kfp, 3) = Pr4(id, 3)
            Pibox(kfp, 3) = Pi4(id, 3)
            Prbox(kfp, 4) = Pr4(id, 4)
            Pibox(kfp, 4) = Pi4(id, 4)
            Prbox(kfp, 5) = Pr4(id, 5)
            Pibox(kfp, 5) = Pi4(id, 5)
            Prbox(kfp, 6) = Pr4(id, 6)
            Pibox(kfp, 6) = Pi4(id, 6)
            Prbox(kfp, 7) = Pr4(id, 7)
            Pibox(kfp, 7) = Pi4(id, 7)

   28    continue

         call check_box(Nmax4, Kclose, Listclose, kexam, listexam,
     $        Kpart,
     $        Listpart, ipar4Ch5, imark4)

         do 29 k = 1, kpart
            id = Listpart(k)
            n1 = npb4(id, 1)
            n2 = npb4(id, 2)
            do 290 np = n1, n2
               nn = nn + 1
               xt(nn) = xn(np)
               yt(nn) = yn(np)
               gt(nn) = gn(np)
               it(nn) = np
  290       continue
   29    continue

         level = 5

         call near_far(Nmax5, ib, jb, r15, ic5, jc5, kexam, listexam,
     $     kfar, Listfar, kclose, Listclose)

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

   30    continue

         call check_box(Nmax5, Kclose, Listclose, kexam, listexam,
     $        Kpart,
     $        Listpart, ipar5Ch6, imark5)

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
  310       continue
   31    continue

         level = 6

         call near_far(Nmax6, ib, jb, r16, ic6, jc6, kexam, listexam,
     $     kfar, Listfar, kclose, Listclose)

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

   32    continue

         call check_box(Nmax6, Kclose, Listclose, kexam, listexam,
     $        Kpart,
     $        Listpart, ipar6Ch7, imark6)

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
  330       continue
   33    continue

         level = 7

         call near_far(Nmax7, ib, jb, r17, ic7, jc7, kexam, listexam,
     $     kfar, Listfar, kclose, Listclose)

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
   34    continue

         call check_box(Nmax7, Kclose, Listclose, kexam, listexam,
     $        Kpart,
     $        Listpart, ipar7Ch8, imark7)

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
  350       continue
   35    continue

         level = 8

         call near_far(Nmax8, ib, jb, r18, ic8, jc8, kexam, listexam,
     $     kfar, Listfar, kclose, Listclose)

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
   36    continue

         call check_box(Nmax8, Kclose, Listclose, kexam, listexam,
     $        Kpart,
     $        Listpart, ipar8Ch9, imark8)

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

         call near_far(Nmax9, ib, jb, r19, ic9, jc9, kexam, listexam,
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

         do k = 1, kclose
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

         if (nn > np_max) write (*, *) 'error in int_chless1p', nn
         if (kfp > nbox_max) write (*, *) 'error in int_chless1b', kfp
         do 351 n = nb1, nb2
            xnn = xn(n)
            ynn = yn(n)
            gnn = gn(n)
            call int_part2(gnn, xnn, ynn, up2, vp2, gp2, nn)
            call int_part_box(xnn, ynn, ubox, vbox, kfp)
            uu(n) = uu(n) + (up2 + ubox)*dyopiinv
            vv(n) = vv(n) + (vp2 + vbox)*dyopiinv
            gdiff(n) = gdiff(n) + gp2
  351    continue

   10 continue
      end
