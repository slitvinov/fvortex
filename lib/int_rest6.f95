SUBROUTINE INT_REST6(kp6)

!  Same as int_rest2 for level 6 boxes.

   implicit none

   include 'tree_tmp.h'
   include 'main_dim.h'
   include 'part.h'
   include 'tree9.h'

   integer :: limpar
   real :: x0, y0
   COMMON/GEOM/X0, Y0, Limpar

   integer :: kp6

   integer :: Listfar(Nhlp), Listclose(Nhlp), Listexam(Nhlp)
   integer :: Listpart(Nhlp), kb, ib, jb, ipar, jpar, i, kexam
   integer :: n4, k, id, n1, n2, np, kbb, kclose, kfar, kpart
   real :: dyopiinv, r51, r52, r53, r54, r55, r61, r62, r63, r64, r65, r66, xb, yb
!---------------------------------------------------------------------

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

   DO 20 kb = 1, kp6       ! All boxes Childless & Parents
      ib = IC6(kb)
      jb = JC6(kb)
      xb = XC6(kb)
      yb = YC6(kb)
      ipar = (xb - X0)/ds5 + 1
      jpar = (yb - Y0)/ds5 + 1
      do 1 i = 1, kp1
         kexam = kp1
         Listexam(i) = Liststart(i)
1     END DO

      CALL near_far(Nmax1, ipar, jpar, r51, IC1, JC1, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)

      CALL check_box(Nmax1, kclose, Listclose, kexam, Listexam, kpart &
                     , Listpart, Ipar1Ch2, Imark1)               !NT

      CALL near_far(Nmax1, ib, jb, r61, IC1, JC1, kpart, Listpart, &
                    kfar, Listfar, Kclose, Listclose)

      n4 = 0
      DO 21 k = 1, kfar
         id = Listfar(k)
         n1 = NPB1(id, 1)
         n2 = NPB1(id, 2)
         DO 210 np = n1, n2
            n4 = n4 + 1
            XT(n4) = XN(np)
            YT(n4) = YN(np)
            GT(n4) = GN(np)
210      END DO
21    END DO

      CALL near_far(Nmax2, ipar, jpar, r52, IC2, JC2, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)

      CALL check_box(Nmax2, kclose, Listclose, kexam, Listexam, kpart &
                     , Listpart, Ipar2Ch3, Imark2)                 ! NT

      CALL near_far(Nmax2, ib, jb, r62, IC2, JC2, kpart, Listpart, &
                    kfar, Listfar, Kclose, Listclose)

      DO 22 k = 1, kfar
         id = Listfar(k)
         n1 = NPB2(id, 1)
         n2 = NPB2(id, 2)
         DO 220 np = n1, n2
            n4 = n4 + 1
            XT(n4) = XN(np)
            YT(n4) = YN(np)
            GT(n4) = GN(np)
220      END DO
22    END DO

      CALL near_far(Nmax3, ipar, jpar, r53, IC3, JC3, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)

      CALL check_box(Nmax3, kclose, Listclose, kexam, Listexam, kpart &
                     , Listpart, Ipar3Ch4, Imark3)                 ! NT

      CALL near_far(Nmax3, ib, jb, r63, IC3, JC3, kpart, Listpart, &
                    kfar, Listfar, Kclose, Listclose)

      DO 23 k = 1, kfar
         id = Listfar(k)
         n1 = NPB3(id, 1)
         n2 = NPB3(id, 2)
         DO 230 np = n1, n2
            n4 = n4 + 1
            XT(n4) = XN(np)
            YT(n4) = YN(np)
            GT(n4) = GN(np)
230      END DO
23    END DO

      CALL near_far(Nmax4, ipar, jpar, r54, IC4, JC4, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)

      CALL check_box(Nmax4, kclose, Listclose, kexam, Listexam, kpart &
                     , Listpart, Ipar4Ch5, Imark4)                 ! NT

      CALL near_far(Nmax4, ib, jb, r64, IC4, JC4, kpart, Listpart, &
                    kfar, Listfar, Kclose, Listclose)

      DO 24 k = 1, kfar
         id = Listfar(k)
         n1 = NPB4(id, 1)
         n2 = NPB4(id, 2)
         DO 240 np = n1, n2
            n4 = n4 + 1
            XT(n4) = XN(np)
            YT(n4) = YN(np)
            GT(n4) = GN(np)
240      END DO
24    END DO

      CALL near_far(Nmax5, ipar, jpar, r55, IC5, JC5, Kexam, Listexam, &
                    kfar, Listfar, kclose, Listclose)

      CALL check_box(Nmax5, kclose, Listclose, &
                     kexam, Listexam, kpart, Listpart, Ipar5Ch6, Imark5)

      CALL near_far(Nmax5, ib, jb, r65, IC5, JC5, Kpart, Listpart, &
                    kfar, Listfar, kclose, Listclose)

      DO 25 k = 1, kfar
         id = Listfar(k)
         n1 = NPB5(id, 1)
         n2 = NPB5(id, 2)
         DO 250 np = n1, n2
            n4 = n4 + 1
            XT(n4) = XN(np)
            YT(n4) = YN(np)
            GT(n4) = GN(np)
250      END DO
25    END DO

      IF (n4 == 0) GOTO 88
      if (n4 > np_max) write (*, *) 'error in rest6b', n4
      CALL int_box_part(Nmax6, kb, xb, yb, n4, Br6, Bi6)

88    CALL near_far(Nmax6, ib, jb, r66, IC6, JC6, kexam, Listexam, &
                    kfar, Listfar, Kclose, Listclose)

      ! CDIR$SHORTLOOP
      DO 26 kbb = 1, kfar
         id = Listfar(kbb)
         Xbox(kbb) = XC6(id)
         Ybox(kbb) = YC6(id)
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
26    END DO

      if (kfar > nbox_max) write (*, *) 'error in rest6', kbb
      CALL int_box(Nmax6, kb, xb, yb, kfar, Br6, Bi6)

20 END DO
   RETURN
END SUBROUTINE
