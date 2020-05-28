subroutine build_tree(icheck, xbc, ybc, nn, kfp)

!  This subroutine builds the interaction tree for the boxes and particles
!  effecting the point (xbc,ybc).  It starts at the highest(coarsest)level
!  and descends down, picking out boxes to interact with and particles when
!  boxes are childless until passing all levels, at which point any boxes
!  still too close to interact with must be done on a particle basis.

   implicit none

   include 'main_dim.h'
   include 'tree_tmp.h'
   include 'part.h'
   include 'tree9.h'

   integer :: limpar
   real :: x0, y0
   common/geom/x0, y0, Limpar

   integer :: icheck, nn, kfp
   real :: xbc, ybc

   integer :: Listfar(Nhlp), Listclose(Nhlp), listexam(Nhlp)
   integer :: Listpart(Nhlp), kclose, kfar, kexam, kpart, k, m
   integer :: np1, np2, npt, level, id, n1, n2
   real :: xst, yst
!--------------------------------------------------------
   nn = 0
   kfp = 0
   kclose = 0
   kfar = 0
   kexam = 0
   kpart = 0

!  Start out with the first four boxes
   do 432 k = 1, kp1
      Listclose(k) = liststart(k)
432 end do
   kclose = kp1

!          LEVEL = 1
   call check_box(Nmax1, kclose, Listclose, kexam, listexam, kpart, &
                  Listpart, Ipar1Ch2, Imark1)
   if (Kpart /= 0) then
      do 12 m = 1, Kpart  ! these are all from childless level 1 boxes
         np1 = npb1(Listpart(m), 1)
         np2 = npb1(Listpart(m), 2)
         do 121 npt = np1, np2
            nn = nn + 1
            xt(nn) = xn(npt)   ! will interact with these as particles
            yt(nn) = yn(npt)
            gt(nn) = gn(npt)
121      end do
12    end do
   endif

   level = 2
   if (kexam == 0) goto 9876
   xst = x0 - 0.5*ds2
   yst = y0 - 0.5*ds2
   call far_cls(icheck, NMax2, Xbc, Ybc, ds2, ic2, jc2, kexam, xst, yst, &
                listexam, kfar, Listfar, kclose, Listclose)
   if (kfar /= 0) then   ! boxes far enough away to interact with
      do 211 k = 1, kfar
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
211   end do
   endif

   call check_box(Nmax2, kclose, Listclose, kexam, listexam, &
                  kpart, Listpart, Ipar2Ch3, Imark2)
   if (kpart /= 0) then          ! again childless boxes
      do 22 k = 1, Kpart
         n1 = npb2(Listpart(k), 1)
         n2 = npb2(Listpart(k), 2)
         do 221 npt = n1, n2
            nn = nn + 1
            xt(nn) = xn(npt)
            yt(nn) = yn(npt)
            gt(nn) = gn(npt)
221      end do
22    end do
   endif

   level = 3
   if (kexam == 0) goto 9876
   xst = x0 - 0.5*ds3
   yst = y0 - 0.5*ds3
   call far_cls(icheck, NMax3, Xbc, Ybc, ds3, ic3, jc3, kexam, xst, yst, &
                listexam, kfar, Listfar, kclose, Listclose)

   if (kfar /= 0) then
      do 311 k = 1, kfar
         kfp = kfp + 1
         id = Listfar(k)
         Xbox(kfp) = xc3(id)
         Ybox(kfp) = yc3(id)
         Prbox(kfp, 0) = Pr3(id, 0)
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
311   end do
   endif

   call check_box(Nmax3, kclose, Listclose, kexam, listexam, &
                  Kpart, Listpart, Ipar3Ch4, Imark3)

   if (kpart /= 0) then
      do 32 k = 1, Kpart
         n1 = npb3(Listpart(k), 1)
         n2 = npb3(Listpart(k), 2)
         do 321 npt = n1, n2
            nn = nn + 1
            xt(nn) = xn(npt)
            yt(nn) = yn(npt)
            gt(nn) = gn(npt)
321      end do
32    end do
   endif

   level = 4
   if (kexam == 0) goto 9876
   xst = x0 - 0.5*ds4
   yst = y0 - 0.5*ds4
   call far_cls(icheck, Nmax4, Xbc, Ybc, ds4, ic4, jc4, kexam, xst, yst, &
                listexam, kfar, Listfar, kclose, Listclose)

   if (kfar /= 0) then
      do 411 k = 1, kfar
         kfp = kfp + 1
         id = Listfar(k)
         Xbox(kfp) = xc4(id)
         Ybox(kfp) = yc4(id)
         Prbox(kfp, 0) = Pr4(id, 0)
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
411   end do
   endif

   call check_box(NMax4, kclose, Listclose, kexam, listexam, &
                  kpart, Listpart, Ipar4Ch5, Imark4)

   if (kpart /= 0) then
      do 43 k = 1, Kpart
         n1 = npb4(Listpart(k), 1)
         n2 = npb4(Listpart(k), 2)
         do 431 npt = n1, n2
            nn = nn + 1
            xt(nn) = xn(npt)
            yt(nn) = yn(npt)
            gt(nn) = gn(npt)
431      end do
43    end do
   endif

   level = 5
   if (kexam == 0) goto 9876
   xst = x0 - 0.5*ds5
   yst = y0 - 0.5*ds5
   call far_cls(icheck, Nmax5, xbc, ybc, ds5, ic5, jc5, kexam, xst, yst, &
                listexam, kfar, Listfar, kclose, Listclose)

   if (kfar /= 0) then
      do 511 k = 1, kfar
         kfp = kfp + 1
         id = Listfar(k)
         Xbox(kfp) = xc5(id)
         Ybox(kfp) = yc5(id)
         Prbox(kfp, 0) = Pr5(id, 0)
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
511   end do
   endif

   call check_box(Nmax5, kclose, Listclose, kexam, &
                  listexam, kpart, Listpart, Ipar5Ch6, Imark5)

   if (kpart /= 0) then
      do 52 k = 1, Kpart
         n1 = npb5(Listpart(k), 1)
         n2 = npb5(Listpart(k), 2)
         do 521 npt = n1, n2
            nn = nn + 1
            xt(nn) = xn(npt)
            yt(nn) = yn(npt)
            gt(nn) = gn(npt)
521      end do
52    end do
   endif

   level = 6
   if (kexam == 0) goto 9876
   xst = x0 - 0.5*ds6
   yst = y0 - 0.5*ds6
   call far_cls(icheck, Nmax6, Xbc, Ybc, ds6, ic6, jc6, kexam, xst, yst, &
                listexam, kfar, Listfar, kclose, Listclose)

   if (kfar /= 0) then
      do 611 k = 1, kfar
         kfp = kfp + 1
         id = Listfar(k)
         Xbox(kfp) = xc6(id)
         Ybox(kfp) = yc6(id)
         Prbox(kfp, 0) = Pr6(id, 0)
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
611   end do
   endif

   call check_box(Nmax6, kclose, Listclose, kexam, listexam, &
                  kpart, Listpart, Ipar6Ch7, Imark6)

   if (kpart /= 0) then
      do 62 k = 1, Kpart
         n1 = npb6(Listpart(k), 1)
         n2 = npb6(Listpart(k), 2)
         do 621 npt = n1, n2
            nn = nn + 1
            xt(nn) = xn(npt)
            yt(nn) = yn(npt)
            gt(nn) = gn(npt)
621      end do
62    end do
   endif

   level = 7
   if (kexam == 0) goto 9876
   xst = x0 - 0.5*ds7
   yst = y0 - 0.5*ds7
   call far_cls(icheck, Nmax7, Xbc, Ybc, ds7, ic7, jc7, kexam, xst, yst, &
                listexam, kfar, Listfar, kclose, Listclose)

   if (kfar /= 0) then
      do 711 k = 1, kfar
         kfp = kfp + 1
         id = Listfar(k)
         Xbox(kfp) = xc7(id)
         Ybox(kfp) = yc7(id)
         Prbox(kfp, 0) = Pr7(id, 0)
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
711   end do
   endif

   call check_box(Nmax7, kclose, Listclose, kexam, listexam, &
                  kpart, Listpart, Ipar7Ch8, Imark7)

   if (kpart /= 0) then
      do 72 k = 1, Kpart
         n1 = npb7(Listpart(k), 1)
         n2 = npb7(Listpart(k), 2)
         do 721 npt = n1, n2
            nn = nn + 1
            xt(nn) = xn(npt)
            yt(nn) = yn(npt)
            gt(nn) = gn(npt)
721      end do
72    end do
   endif

   level = 8
   if (kexam == 0) goto 9876
   xst = x0 - 0.5*ds8
   yst = y0 - 0.5*ds8
   call far_cls(icheck, Nmax8, Xbc, Ybc, ds8, ic8, jc8, kexam, xst, yst, &
                listexam, kfar, Listfar, kclose, Listclose)

   if (kfar /= 0) then
      do 811 k = 1, kfar
         kfp = kfp + 1
         id = Listfar(k)
         Xbox(kfp) = xc8(id)
         Ybox(kfp) = yc8(id)
         Prbox(kfp, 0) = Pr8(id, 0)
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
811   end do
   endif

   call check_box(Nmax8, kclose, Listclose, kexam, listexam, &
                  kpart, Listpart, Ipar8Ch9, Imark8)

   if (kpart /= 0) then
      do 82 k = 1, Kpart
         n1 = npb8(Listpart(k), 1)
         n2 = npb8(Listpart(k), 2)
         do 821 npt = n1, n2
            nn = nn + 1
            xt(nn) = xn(npt)
            yt(nn) = yn(npt)
            gt(nn) = gn(npt)
821      end do
82    end do
   endif

   level = 9
   if (kexam == 0) goto 9876
   xst = x0 - 0.5*ds9
   yst = y0 - 0.5*ds9
   call far_cls(icheck, Nmax9, Xbc, Ybc, ds9, ic9, jc9, kexam, xst, yst, &
                listexam, kfar, Listfar, kclose, Listclose)

   if (kfar /= 0) then
      do 911 k = 1, kfar
         kfp = kfp + 1
         id = Listfar(k)
         Xbox(kfp) = xc9(id)
         Ybox(kfp) = yc9(id)
         Prbox(kfp, 0) = Pr9(id, 0)
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
911   end do
   endif

!  No more checking here. All close boxes are cosidered childless

   do 92 k = 1, Kclose
      n1 = npb9(Listclose(k), 1)
      n2 = npb9(Listclose(k), 2)
      do 921 npt = n1, n2
         nn = nn + 1
         xt(nn) = xn(npt)
         yt(nn) = yn(npt)
         gt(nn) = gn(npt)
921   end do
92 end do

9876 continue

   return
end subroutine build_tree
