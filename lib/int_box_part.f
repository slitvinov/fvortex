      subroutine int_box_part(Nmax, kb, xb, yb, k4, Br, Bi)

C This subroutine computes the influence of the particles of childless
C boxes on far boxes of a lower level, as these are particle-box
C interactions.

      include 'tree_tmp.h'

      integer nmax
      integer kb
      integer k4
      real Br(nmax,7)
      real Bi(Nmax,7)
      real xb
      real yb

      integer nb
      real br1
      real bi1
      real br2
      real bi2
      real br3
      real bi3
      real br4
      real bi4
      real br5
      real bi5
      real br6
      real bi6
      real br7
      real bi7
      real s0
      real t0
      real s1
      real t1
      real s2
      real t2
      real s3
      real t3
      real s4
      real t4
      real s5
      real t5
      real s6
      real t6
      real g
      real xx
      real yy
      real p
      real f
      real r2inv
      Br1 = 0.
      Bi1 = 0.
      Br2 = 0.
      Bi2 = 0.
      Br3 = 0.
      Bi3 = 0.
      Br4 = 0.
      Bi4 = 0.
      Br5 = 0.
      Bi5 = 0.
      Br6 = 0.
      Bi6 = 0.
      Br7 = 0.
      Bi7 = 0.

      do 1 nb = 1, k4
         g = gt(nb)
         xx = xb - xt(nb)
         yy = yb - yt(nb)
         r2inv = 1.0/(xx*xx + yy*yy)
         p = xx*r2inv
         f = yy*r2inv

C level = 1
         s0 = p*g
         t0 = f*g
C level = 2
         s1 = s0*p - t0*f
         t1 = s0*f + t0*p
C level = 3
         s2 = s1*p - t1*f
         t2 = s1*f + t1*p
C level = 4
         s3 = s2*p - t2*f
         t3 = s2*f + t2*p
C level = 5
         s4 = s3*p - t3*f
         t4 = s3*f + t3*p
C level = 6
         s5 = s4*p - t4*f
         t5 = s4*f + t4*p
C level = 7
         s6 = s5*p - t5*f
         t6 = s5*f + t5*p

         Br1 = Br1 + s0
         Bi1 = Bi1 + t0
         Br2 = Br2 - s1
         Bi2 = Bi2 - t1
         Br3 = Br3 + s2
         Bi3 = Bi3 + t2
         Br4 = Br4 - s3
         Bi4 = Bi4 - t3
         Br5 = Br5 + s4
         Bi5 = Bi5 + t4
         Br6 = Br6 - s5
         Bi6 = Bi6 - t5
         Br7 = Br7 + s6
         Bi7 = Bi7 + t6
    1 continue

      Br(kb, 1) = Br(kb, 1) + br1
      Bi(kb, 1) = Bi(kb, 1) + bi1
      Br(kb, 2) = Br(kb, 2) + br2
      Bi(kb, 2) = Bi(kb, 2) + bi2
      Br(kb, 3) = Br(kb, 3) + Br3
      Bi(kb, 3) = Bi(kb, 3) + Bi3
      Br(kb, 4) = Br(kb, 4) + Br4
      Bi(kb, 4) = Bi(kb, 4) + Bi4
      Br(kb, 5) = Br(kb, 5) + Br5
      Bi(kb, 5) = Bi(kb, 5) + Bi5
      Br(kb, 6) = Br(kb, 6) + Br6
      Bi(kb, 6) = Bi(kb, 6) + Bi6
      Br(kb, 7) = Br(kb, 7) + Br7
      Bi(kb, 7) = Bi(kb, 7) + Bi7
      end
