      subroutine par_to_ch(nmax, dch, kp, br2, bi2, br3, bi3,
     $  ich3par2, ipar2ch3)

C This subroutine computes the MULTIPOLE EXPANSIONS of
C the CHILDREN boxes from the multipole expansions of their
C parents.


      integer nmax, kp, ich3par2(4*nmax), ipar2ch3(nmax, 4)
      real dch, br2(nmax, 7), bi2(nmax, 7), br3(4*nmax, 7),
     $     bi3(4*nmax, 7)

      integer nb, km, m1, m2, m3, m4
      real p1, p2, p3, p4, p5, p6, p7, p22, p32, p44, p54, p68, p78
      real p12, p26, p38, p420, p524, p656, p764
      real p13, p212, p320, p460, p584, p6224
      real p14, p220, p340, p4140, p5224
      real p15, p230, p370, p4280, p16, p242, p3112, p17, p256, p18
      real r1, r2, r3, r4, r5, r6, r7, f1, f2, f3, f4, f5, f6, f7
      p1 = 0.5*dch
      p2 = p1*p1
      p3 = p1*p2
      p4 = p2*p2
      p5 = p2*p3
      p6 = p3*p3
      p7 = p3*p4

C Other constants

      p78 = 8.*p7
      p68 = 8.*p6
      p54 = 4.*p5
      p44 = 4.*p4
      p32 = 2.*p3
      p22 = 2.*p2

      p764 = 64.*p7
      p656 = 56.*p6
      p524 = 24.*p5
      p420 = 20.*p4
      p38 = 8.*p3
      p26 = 6.*p2
      p12 = 2.*p1

      p6224 = 224.*p6
      p584 = 84.*p5
      p460 = 60.*p4
      p320 = 20.*p3
      p212 = 12.*p2
      p13 = 3.*p1

      p5224 = 224.*p5
      p4140 = 140.*p4
      p340 = 40.*p3
      p220 = 20.*p2
      p14 = 4.*p1

      p4280 = 280.*p4
      p370 = 70.*p3
      p230 = 30.*p2
      p15 = 5.*p1

      p3112 = 112.*p3
      p242 = 42.*p2
      p16 = 6.*p1

      p256 = 56.*p2
      p17 = 7.*p1

      p18 = 8.*p1

      do 20 nb = 1, kp

         km = ich3par2(nb)
C REAL VALUES
         r1 = Br2(km, 1)
         r2 = Br2(km, 2)
         r3 = Br2(km, 3)
         r4 = Br2(km, 4)
         r5 = Br2(km, 5)
         r6 = Br2(km, 6)
         r7 = Br2(km, 7)

C IMAGINARY VALUES
         f1 = Bi2(km, 1)
         f2 = Bi2(km, 2)
         f3 = Bi2(km, 3)
         f4 = Bi2(km, 4)
         f5 = Bi2(km, 5)
         f6 = Bi2(km, 6)
         f7 = Bi2(km, 7)

C Contribution to 1st Child (if any)

         m1 = ipar2ch3(km, 1) ! index of child (if any) box 1 at level i
         m2 = ipar2ch3(km, 2) ! index of child (if any) box 2 at level i
         m3 = ipar2ch3(km, 3) ! index of child (if any) box 3 at level i
         m4 = ipar2ch3(km, 4) ! index of child (if any) box 4 at level i

         if (nb == m1) then
C z
C --------
C REAL and IMAGINARY PART OF EXPANSIONS
C --------
C 1 - Order
            Br3(nb, 1) = Br3(nb, 1) - p68*f7
     $        + p54*(r6 + f6) - p44*r5 + p32*(r4 - f4)
     $        + p22*f3 - p1*(r2 + f2) + r1
            Bi3(nb, 1) = Bi3(nb, 1) + p68*r7
     $        + p54*(f6 - r6) - p44*f5 + p32*(f4 + r4)
     $        - p22*r3 - p1*(f2 - r2) + f1
C 2 - Order
            Br3(nb, 2) = Br3(nb, 2)
     $        + p524*(r7 + f7) - p420*r6 + p38*(r5 - f5)
     $        + p26*f4 - p12*(r3 + f3) + r2
            Bi3(nb, 2) = Bi3(nb, 2)
     $        + p524*(f7 - r7) - p420*f6 + p38*(f5 + r5)
     $        - p26*r4 - p12*(f3 - r3) + f2

C 3 - Order
            Br3(nb, 3) = Br3(nb, 3) - p460*r7
     $        + p320*(r6 - f6) + p212*f5 - p13*(r4 + f4) + r3
            Bi3(nb, 3) = Bi3(nb, 3) - p460*f7
     $        + p320*(f6 + r6) - p212*r5 - p13*(f4 - r4) + f3
C 4 - Order
            Br3(nb, 4) = Br3(nb, 4)
     $        + p340*(r7 - f7) + p220*f6 - p14*(r5 + f5) + r4
            Bi3(nb, 4) = Bi3(nb, 4)
     $        + p340*(f7 + r7) - p220*r6 - p14*(f5 - r5) + f4
C 5 - Order
            Br3(nb, 5) = Br3(nb, 5) + p230*f7
     $        - p15*(r6 + f6) + r5
            Bi3(nb, 5) = Bi3(nb, 5) - p230*r7
     $        - p15*(f6 - r6) + f5
C 6 - Order
            Br3(nb, 6) = Br3(nb, 6)
     $        - p16*(r7 + f7) + r6
            Bi3(nb, 6) = Bi3(nb, 6)
     $        - p16*(f7 - r7) + f6
C 7 - Order
            Br3(nb, 7) = Br3(nb, 7) + r7
            Bi3(nb, 7) = Bi3(nb, 7) + f7

         else if (nb == m2) then

C 
C *
C Contribution to 2nd Child (if any)                      *
C *
C 

C --------
C REAL and IMAGINARY PART OF EXPANSIONS
C --------
C 1 - Order
            Br3(nb, 1) = Br3(nb, 1) + p68*f7
     $        + p54*(r6 - f6) - p44*r5 + p32*(r4 + f4)
     $        - p22*f3 - p1*(r2 - f2) + r1
            Bi3(nb, 1) = Bi3(nb, 1) - p68*r7
     $        + p54*(f6 + r6) - p44*f5 + p32*(f4 - r4)
     $        + p22*r3 - p1*(f2 + r2) + f1
C 2 - Order
            Br3(nb, 2) = Br3(nb, 2)
     $        + p524*(r7 - f7) - p420*r6 + p38*(r5 + f5)
     $        - p26*f4 - p12*(r3 - f3) + r2
            Bi3(nb, 2) = Bi3(nb, 2)
     $        + p524*(f7 + r7) - p420*f6 + p38*(f5 - r5)
     $        + p26*r4 - p12*(f3 + r3) + f2

C 3 - Order
            Br3(nb, 3) = Br3(nb, 3) - p460*r7
     $        + p320*(r6 + f6) - p212*f5 - p13*(r4 - f4) + r3
            Bi3(nb, 3) = Bi3(nb, 3) - p460*f7
     $        + p320*(f6 - r6) + p212*r5 - p13*(f4 + r4) + f3
C 4 - Order
            Br3(nb, 4) = Br3(nb, 4)
     $        + p340*(r7 + f7) - p220*f6 - p14*(r5 - f5) + r4
            Bi3(nb, 4) = Bi3(nb, 4) +
     $        p340*(f7 - r7) + p220*r6 - p14*(f5 + r5) + f4
C 5 - Order
            Br3(nb, 5) = Br3(nb, 5) - p230*f7
     $        - p15*(r6 - f6) + r5
            Bi3(nb, 5) = Bi3(nb, 5) + p230*r7
     $        - p15*(f6 + r6) + f5
C 6 - Order
            Br3(nb, 6) = Br3(nb, 6)
     $        - p16*(r7 - f7) + r6
            Bi3(nb, 6) = Bi3(nb, 6)
     $        - p16*(f7 + r7) + f6
C 7 - Order
            Br3(nb, 7) = Br3(nb, 7) + r7
            Bi3(nb, 7) = Bi3(nb, 7) + f7

         else if (nb == m3) then
C 
C *
C Contribution to 3rd Child (if any)                      *
C *
C 

C --------
C REAL and IMAGINARY PART OF EXPANSIONS
C --------
C 1 - Order
            Br3(nb, 1) = Br3(nb, 1) + p68*f7
     $        - p54*(r6 - f6) - p44*r5 - p32*(r4 + f4)
     $        - p22*f3 + p1*(r2 - f2) + r1
            Bi3(nb, 1) = Bi3(nb, 1) - p68*r7
     $        - p54*(f6 + r6) - p44*f5 - p32*(f4 - r4)
     $        + p22*r3 + p1*(f2 + r2) + f1
C 2 - Order
            Br3(nb, 2) = Br3(nb, 2)
     $        - p524*(r7 - f7) - p420*r6 - p38*(r5 + f5)
     $        - p26*f4 + p12*(r3 - f3) + r2
            Bi3(nb, 2) = Bi3(nb, 2)
     $        - p524*(f7 + r7) - p420*f6 - p38*(f5 - r5)
     $        + p26*r4 + p12*(f3 + r3) + f2
C 3 - Order
            Br3(nb, 3) = Br3(nb, 3) - p460*r7
     $        - p320*(r6 + f6) - p212*f5 + p13*(r4 - f4) + r3
            Bi3(nb, 3) = Bi3(nb, 3) - p460*f7
     $        - p320*(f6 - r6) + p212*r5 + p13*(f4 + r4) + f3
C 4 - Order
            Br3(nb, 4) = Br3(nb, 4)
     $        - p340*(r7 + f7) - p220*f6 + p14*(r5 - f5) + r4
            Bi3(nb, 4) = Bi3(nb, 4)
     $        - p340*(f7 - r7) + p220*r6 + p14*(f5 + r5) + f4
C 5 - Order
            Br3(nb, 5) = Br3(nb, 5) - p230*f7
     $        + p15*(r6 - f6) + r5
            Bi3(nb, 5) = Bi3(nb, 5) + p230*r7
     $        + p15*(f6 + r6) + f5
C 6 - Order
            Br3(nb, 6) = Br3(nb, 6)
     $        + p16*(r7 - f7) + r6
            Bi3(nb, 6) = Bi3(nb, 6)
     $        + p16*(f7 + r7) + f6
C 7 - Order
            Br3(nb, 7) = Br3(nb, 7) + r7
            Bi3(nb, 7) = Bi3(nb, 7) + f7

         else if (nb == m4) then

C 
C *
C Contribution to 4th Child (if any)                      *
C *
C 

C --------
C REAL and IMAGINARY PART OF EXPANSIONS
C --------
C 1 - Order
            Br3(nb, 1) = Br3(nb, 1) - p68*f7
     $        - p54*(r6 + f6) - p44*r5 - p32*(r4 - f4)
     $        + p22*f3 + p1*(r2 + f2) + r1
            Bi3(nb, 1) = Bi3(nb, 1) + p68*r7
     $        - p54*(f6 - r6) - p44*f5 - p32*(f4 + r4)
     $        - p22*r3 + p1*(f2 - r2) + f1
C 2 - Order
            Br3(nb, 2) = Br3(nb, 2)
     $        - p524*(r7 + f7) - p420*r6 - p38*(r5 - f5)
     $        + p26*f4 + p12*(r3 + f3) + r2
            Bi3(nb, 2) = Bi3(nb, 2)
     $        - p524*(f7 - r7) - p420*f6 - p38*(f5 + r5)
     $        - p26*r4 + p12*(f3 - r3) + f2
C 3 - Order
            Br3(nb, 3) = Br3(nb, 3) - p460*r7
     $        - p320*(r6 - f6) + p212*f5 + p13*(r4 + f4) + r3
            Bi3(nb, 3) = Bi3(nb, 3) - p460*f7
     $        - p320*(f6 + r6) - p212*r5 + p13*(f4 - r4) + f3
C 4 - Order
            Br3(nb, 4) = Br3(nb, 4)
     $        - p340*(r7 - f7) + p220*f6 + p14*(r5 + f5) + r4
            Bi3(nb, 4) = Bi3(nb, 4)
     $        - p340*(f7 + r7) - p220*r6 + p14*(f5 - r5) + f4
C 5 - Order
            Br3(nb, 5) = Br3(nb, 5) + p230*f7
     $        + p15*(r6 + f6) + r5
            Bi3(nb, 5) = Bi3(nb, 5) - p230*r7
     $        + p15*(f6 - r6) + f5
C 6 - Order
            Br3(nb, 6) = Br3(nb, 6)
     $        + p16*(r7 + f7) + r6
            Bi3(nb, 6) = Bi3(nb, 6)
     $        + p16*(f7 - r7) + f6
C 7 - Order
            Br3(nb, 7) = Br3(nb, 7) + r7
            Bi3(nb, 7) = Bi3(nb, 7) + f7

         end if

   20 end do
      end
