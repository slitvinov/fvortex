      subroutine par_to_ch(nmax, dch, kp, br2, bi2, br3, bi3,
     $  ich3par2, ipar2ch3)

C This subroutine computes the MULTIPOLE EXPANSIONS of
C the CHILDREN boxes from the multipole expansions of their
C parents.

      integer nmax
      integer kp
      integer ich3par2(4*nmax)
      integer ipar2ch3(nmax,4)
      real dch
      real br2(nmax,7)
      real bi2(nmax,7)
      real br3(4*nmax,7)
      real bi3(4*nmax,7)

      integer nb
      integer km
      integer m1
      integer m2
      integer m3
      integer m4
      real p1
      real p2
      real p3
      real p4
      real p5
      real p6
      real p7
      real p22
      real p32
      real p44
      real p54
      real p68
      real p78
      real p12
      real p26
      real p38
      real p420
      real p524
      real p656
      real p764
      real p13
      real p212
      real p320
      real p460
      real p584
      real p6224
      real p14
      real p220
      real p340
      real p4140
      real p5224
      real p15
      real p230
      real p370
      real p4280
      real p16
      real p242
      real p3112
      real p17
      real p256
      real p18
      real r1
      real r2
      real r3
      real r4
      real r5
      real r6
      real r7
      real f1
      real f2
      real f3
      real f4
      real f5
      real f6
      real f7
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

         m1 = ipar2ch3(km, 1)
         m2 = ipar2ch3(km, 2)
         m3 = ipar2ch3(km, 3)
         m4 = ipar2ch3(km, 4)

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

   20 continue
      end
