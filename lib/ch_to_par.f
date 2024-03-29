      subroutine ch_to_par(Nmax1, dch, kparent, iparent, IPARiCHj,
     $     Pr, Pi, Pgr, Pgi, Gr, Gi)

C This subroutine computes the MULTIPOLE EXPANSIONS of
C the PARENT boxes once the multipole expansions of their
C children has been computed.

      integer nmax1
      integer kparent
      real dch
      integer IPARiCHj(Nmax1/4,4)
      integer iparent(Nmax1/4)
      real pr(Nmax1,0:7)
      real pi(Nmax1,0:7)
      real pgr(Nmax1/4,0:7)
      real pgi(Nmax1/4,0:7)
      real gr(Nmax1/4,0:7)
      real gi(Nmax1/4,0:7)

      integer km
      integer m
      integer k
      integer kb
      integer nb
      real p1
      real p2
      real p3
      real p4
      real p5
      real p6
      real p7
      real p32
      real p26
      real p13
      real p44
      real p38
      real p212
      real p14
      real p54
      real p420
      real p320
      real p220
      real p15
      real p68
      real p524
      real p460
      real p340
      real p230
      real p16
      real p78
      real p656
      real p584
      real p4140
      real p370
      real p242
      real p17
      real r0
      real r1
      real r2
      real r3
      real r4
      real r5
      real r6
      real r7
      real f0
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

C 3-order
      p32 = 2.*p3
      p26 = 6.*p2
      p13 = 3.*p1
C 4-order
      p44 = 4.*p4
      p38 = 8.*p3
      p212 = 12.*p2
      p14 = 4.*p1
C 5-order
      p54 = 4.*p5
      p420 = 20.*p4
      p320 = 20.*p3
      p220 = 20.*p2
      p15 = 5.*p1
C 6-order
      p68 = 8.*p6
      p524 = 24.*p5
      p460 = 60.*p4
      p340 = 40.*p3
      p230 = 30.*p2
      p16 = 6.*p1
C 7-order
      p78 = 8.*p7
      p656 = 56.*p6
      p584 = 84.*p5
      p4140 = 140.*p4
      p370 = 70.*p3
      p242 = 42.*p2
      p17 = 7.*p1

      do 20 nb = 1, kparent
         Gr(nb, 0) = 0.
         Gr(nb, 1) = 0.
         Gr(nb, 2) = 0.
         Gr(nb, 3) = 0.
         Gr(nb, 4) = 0.
         Gr(nb, 5) = 0.
         Gr(nb, 6) = 0.
         Gr(nb, 7) = 0.
         Gi(nb, 0) = 0.
         Gi(nb, 1) = 0.
         Gi(nb, 2) = 0.
         Gi(nb, 3) = 0.
         Gi(nb, 4) = 0.
         Gi(nb, 5) = 0.
         Gi(nb, 6) = 0.
         Gi(nb, 7) = 0.

C Contribution of 1st Child (if any)
         km = iparent(nb)
         m = ipariChj(km, 1)

         if (m == 0) goto 2

C REAL VALUES
         r0 = Pr(m, 0)
         r1 = Pr(m, 1)
         r2 = Pr(m, 2)
         r3 = Pr(m, 3)
         r4 = Pr(m, 4)
         r5 = Pr(m, 5)
         r6 = Pr(m, 6)
         r7 = Pr(m, 7)

C IMAGINARY VALUES
         f0 = Pi(m, 0)
         f1 = Pi(m, 1)
         f2 = Pi(m, 2)
         f3 = Pi(m, 3)
         f4 = Pi(m, 4)
         f5 = Pi(m, 5)
         f6 = Pi(m, 6)
         f7 = Pi(m, 7)

C REAL and IMAGINARY PART OF EXPANSIONS

C 0 - Order
         Gr(nb, 0) = r0
         Gi(nb, 0) = 0.
C 1 - Order
         Gr(nb, 1) = -p1*r0 + r1
         Gi(nb, 1) = p1*r0 + f1
C 2 - Order
         Gr(nb, 2) = -2.*p1*(r1 + f1) + r2
         Gi(nb, 2) = 2.*(-p2*r0 + p1*(r1 - f1)) + f2
C 3 - Order
         Gr(nb, 3) = p32*r0 + p26*f1 - p13*(r2 + f2) + r3
         Gi(nb, 3) = p32*r0 - p26*r1 + p13*(r2 - f2) + f3
C 4 - Order
         Gr(nb, 4) = -p44*r0 + p38*(r1 - f1) + p212*f2
     $        - p14*(r3 + f3) + r4
         Gi(nb, 4) = p38*(r1 + f1) - p212*r2 + p14*(r3 - f3) + f4
C 5 - Order
         Gr(nb, 5) = p54*r0 - p420*r1 + p320*(r2 - f2)
     $        + p220*f3 - p15*(r4 + f4) + r5
         Gi(nb, 5) = -p54*r0 - p420*f1 + p320*(r2 + f2)
     $        - p220*r3 + p15*(r4 - f4) + f5
C 6 - Order
         Gr(nb, 6) = p524*(r1 + f1) - p460*r2 + p340*(r3 - f3)
     $        + p230*f4 - p16*(r5 + f5) + r6
         Gi(nb, 6) = p68*r0 + p524*(f1 - r1) - p460*f2
     $        + 40*p3*(r3 + f3) - p230*r4 + p16*(r5 - f5) + f6
C 7 - Order
         Gr(nb, 7) = -p78*r0 - p656*f1 + p584*(r2 + f2)
     $        - p4140*r3 + p370*(r4 - f4) + p242*f5
     $        - p17*(r6 + f6) + r7
         Gi(nb, 7) = -p78*r0 + p656*r1 + p584*(f2 - r2)
     $        - p4140*f3 + p370*(r4 + f4) - p242*r5
     $        + p17*(r6 - f6) + f7

C -  Contribution of 2nd Child (if any)

    2    m = ipariChj(km, 2)

         if (m == 0) goto 3

C REAL VALUES
         r0 = Pr(m, 0)
         r1 = Pr(m, 1)
         r2 = Pr(m, 2)
         r3 = Pr(m, 3)
         r4 = Pr(m, 4)
         r5 = Pr(m, 5)
         r6 = Pr(m, 6)
         r7 = Pr(m, 7)

C IMAGINARY VALUES
         f0 = Pi(m, 0)
         f1 = Pi(m, 1)
         f2 = Pi(m, 2)
         f3 = Pi(m, 3)
         f4 = Pi(m, 4)
         f5 = Pi(m, 5)
         f6 = Pi(m, 6)
         f7 = Pi(m, 7)

C REAL and IMAGINARY PARTS OF EXPANSIONS

C 0 - Order
         Gr(nb, 0) = Gr(nb, 0) + r0
         Gi(nb, 0) = 0.
C 1 - Order
         Gr(nb, 1) = Gr(nb, 1) - p1*r0 + r1
         Gi(nb, 1) = Gi(nb, 1) - p1*r0 + f1
C 2 - Order
         Gr(nb, 2) = Gr(nb, 2) + 2.*p1*(f1 - r1) + r2
         Gi(nb, 2) = Gi(nb, 2) + 2.*(p2*r0 - p1*(r1 + f1)) + f2
C 3 - Order
         Gr(nb, 3) = Gr(nb, 3) + p32*r0 - p26*f1
     $        + p13*(f2 - r2) + r3
         Gi(nb, 3) = Gi(nb, 3) - p32*r0 + p26*r1
     $        - p13*(f2 + r2) + f3
C 4 - Order
         Gr(nb, 4) = Gr(nb, 4) - p44*r0 + p38*(r1 + f1)
     $        - p212*f2 + p14*(f3 - r3) + r4
         Gi(nb, 4) = Gi(nb, 4) + p38*(f1 - r1) + p212*r2
     $        - p14*(r3 + f3) + f4
C 5 - Order
         Gr(nb, 5) = Gr(nb, 5) + p54*r0 - p420*r1
     $        + p320*(r2 + f2) - p220*f3 + p15*(f4 - r4) + r5
         Gi(nb, 5) = Gi(nb, 5) + p54*r0 - p420*f1 + p320*(f2 - r2)
     $        + p220*r3 - p15*(r4 + f4) + f5
C 6 - Order
         Gr(nb, 6) = Gr(nb, 6) + p524*(r1 - f1) - p460*r2
     $        + p340*(r3 + f3) - p230*f4 + p16*(f5 - r5) + r6
         Gi(nb, 6) = Gi(nb, 6) - p68*r0 + p524*(f1 + r1) - p460*f2
     $        + 40*p3*(f3 - r3) + p230*r4 - p16*(r5 + f5) + f6
C 7 - Order
         Gr(nb, 7) = Gr(nb, 7) - p78*r0 + p656*f1 + p584*(r2 - f2)
     $        - p4140*r3 + p370*(r4 + f4) - p242*f5
     $        + p17*(f6 - r6) + r7
         Gi(nb, 7) = Gi(nb, 7) + p78*r0 - p656*r1 + p584*(f2 + r2)
     $        - p4140*f3 + p370*(f4 - r4) + p242*r5
     $        - p17*(r6 + f6) + f7

C -  Contribution of 3rd Child (if any)
    3    m = ipariChj(km, 3)

         if (m == 0) goto 4

C REAL VALUES
         r0 = Pr(m, 0)
         r1 = Pr(m, 1)
         r2 = Pr(m, 2)
         r3 = Pr(m, 3)
         r4 = Pr(m, 4)
         r5 = Pr(m, 5)
         r6 = Pr(m, 6)
         r7 = Pr(m, 7)

C IMAGINARY VALUES
         f0 = Pi(m, 0)
         f1 = Pi(m, 1)
         f2 = Pi(m, 2)
         f3 = Pi(m, 3)
         f4 = Pi(m, 4)
         f5 = Pi(m, 5)
         f6 = Pi(m, 6)
         f7 = Pi(m, 7)

C REAL and IMAGINARY PARTS OF EXPANSIONS

C 0 - Order
         Gr(nb, 0) = Gr(nb, 0) + r0
         Gi(nb, 0) = 0.
C 1 - Order
         Gr(nb, 1) = Gr(nb, 1) + p1*r0 + r1
         Gi(nb, 1) = Gi(nb, 1) + p1*r0 + f1
C 2 - Order
         Gr(nb, 2) = Gr(nb, 2) - 2.*p1*(f1 - r1) + r2
         Gi(nb, 2) = Gi(nb, 2) + 2.*(p2*r0 + p1*(r1 + f1)) + f2
C 3 - Order
         Gr(nb, 3) = Gr(nb, 3) - p32*r0 - p26*f1
     $        - p13*(f2 - r2) + r3
         Gi(nb, 3) = Gi(nb, 3) + p32*r0 + p26*r1
     $        + p13*(f2 + r2) + f3
C 4 - Order
         Gr(nb, 4) = Gr(nb, 4) - p44*r0 - p38*(r1 + f1)
     $        - p212*f2 - p14*(f3 - r3) + r4
         Gi(nb, 4) = Gi(nb, 4) - p38*(f1 - r1) + p212*r2
     $        + p14*(r3 + f3) + f4
C 5 - Order
         Gr(nb, 5) = Gr(nb, 5) - p54*r0 - p420*r1
     $        - p320*(r2 + f2) - p220*f3 - p15*(f4 - r4) + r5
         Gi(nb, 5) = Gi(nb, 5) - p54*r0 - p420*f1 - p320*(f2 - r2)
     $        + p220*r3 + p15*(r4 + f4) + f5
C 6 - Order
         Gr(nb, 6) = Gr(nb, 6) - p524*(r1 - f1) - p460*r2
     $        - p340*(r3 + f3) - p230*f4 - p16*(f5 - r5) + r6
         Gi(nb, 6) = Gi(nb, 6) - p68*r0 - p524*(f1 + r1) - p460*f2
     $        - 40*p3*(f3 - r3) + p230*r4 + p16*(r5 + f5) + f6
C 7 - Order
         Gr(nb, 7) = Gr(nb, 7) + p78*r0 + p656*f1 - p584*(r2 - f2)
     $        - p4140*r3 - p370*(r4 + f4) - p242*f5
     $        - p17*(f6 - r6) + r7
         Gi(nb, 7) = Gi(nb, 7) - p78*r0 - p656*r1 - p584*(f2 + r2)
     $        - p4140*f3 - p370*(f4 - r4) + p242*r5
     $        + p17*(r6 + f6) + f7

C -   Contribution of 4th Child (if any)

    4    m = ipariChj(km, 4)

         if (m == 0) goto 20

C REAL VALUES
         r0 = Pr(m, 0)
         r1 = Pr(m, 1)
         r2 = Pr(m, 2)
         r3 = Pr(m, 3)
         r4 = Pr(m, 4)
         r5 = Pr(m, 5)
         r6 = Pr(m, 6)
         r7 = Pr(m, 7)

C IMAGINARY VALUES
         f0 = Pi(m, 0)
         f1 = Pi(m, 1)
         f2 = Pi(m, 2)
         f3 = Pi(m, 3)
         f4 = Pi(m, 4)
         f5 = Pi(m, 5)
         f6 = Pi(m, 6)
         f7 = Pi(m, 7)

C REAL and IMAGINARY PART OF EXPANSIONS

C 0 - Order
         Gr(nb, 0) = Gr(nb, 0) + r0
         Gi(nb, 0) = 0.
C 1 - Order
         Gr(nb, 1) = Gr(nb, 1) + p1*r0 + r1
         Gi(nb, 1) = Gi(nb, 1) - p1*r0 + f1
C 2 - Order
         Gr(nb, 2) = Gr(nb, 2) + 2.*p1*(r1 + f1) + r2
         Gi(nb, 2) = Gi(nb, 2) + 2.*(-p2*r0 - p1*(r1 - f1)) + f2
C 3 - Order
         Gr(nb, 3) = Gr(nb, 3) - p32*r0 + p26*f1
     $        + p13*(r2 + f2) + r3
         Gi(nb, 3) = Gi(nb, 3) - p32*r0 - p26*r1
     $        - p13*(r2 - f2) + f3
C 4 - Order
         Gr(nb, 4) = Gr(nb, 4) - p44*r0 - p38*(r1 - f1)
     $        + p212*f2 + p14*(r3 + f3) + r4
         Gi(nb, 4) = Gi(nb, 4) - p38*(r1 + f1) - p212*r2
     $        - p14*(r3 - f3) + f4
C 5 - Order
         Gr(nb, 5) = Gr(nb, 5) - p54*r0 - p420*r1 - p320*(r2 - f2)
     $        + p220*f3 + p15*(r4 + f4) + r5
         Gi(nb, 5) = Gi(nb, 5) + p54*r0 - p420*f1 - p320*(r2 + f2)
     $        - p220*r3 - p15*(r4 - f4) + f5
C 6 - Order
         Gr(nb, 6) = Gr(nb, 6) - p524*(r1 + f1) - p460*r2
     $        - p340*(r3 - f3) + p230*f4 + p16*(r5 + f5) + r6
         Gi(nb, 6) = Gi(nb, 6) + p68*r0 - p524*(f1 - r1) - p460*f2
     $        - 40*p3*(r3 + f3) - p230*r4 - p16*(r5 - f5) + f6
C 7 - Order
         Gr(nb, 7) = Gr(nb, 7) + p78*r0 - p656*f1 - p584*(r2 + f2)
     $        - p4140*r3 - p370*(r4 - f4) + p242*f5
     $        + p17*(r6 + f6) + r7
         Gi(nb, 7) = Gi(nb, 7) + p78*r0 + p656*r1 - p584*(f2 - r2)
     $        - p4140*f3 - p370*(r4 + f4) - p242*r5
     $        - p17*(r6 - f6) + f7

   20 continue

      do 30 k = 1, Kparent
         kb = iparent(k)
         Pgr(kb, 0) = Gr(k, 0)
         Pgr(kb, 1) = Gr(k, 1)
         Pgr(kb, 2) = Gr(k, 2)
         Pgr(kb, 3) = Gr(k, 3)
         Pgr(kb, 4) = Gr(k, 4)
         Pgr(kb, 5) = Gr(k, 5)
         Pgr(kb, 6) = Gr(k, 6)
         Pgr(kb, 7) = Gr(k, 7)
         Pgi(kb, 0) = Gi(k, 0)
         Pgi(kb, 1) = Gi(k, 1)
         Pgi(kb, 2) = Gi(k, 2)
         Pgi(kb, 3) = Gi(k, 3)
         Pgi(kb, 4) = Gi(k, 4)
         Pgi(kb, 5) = Gi(k, 5)
         Pgi(kb, 6) = Gi(k, 6)
         Pgi(kb, 7) = Gi(k, 7)
   30 continue
      end
