      subroutine box9_to_part(nmax, kchildless, xc, yc, npb, br, bi)

C This subroutine calculates the velocities induced by a box
C on its own particles from the box's interactions (level 9).

      include 'main_dim.h'
      include 'part.h'

      integer nmax
      integer kchildless
      integer npb(nmax,2)
      real xc(nmax)
      real yc(nmax)
      real br(nmax,7)
      real bi(nmax,7)

      integer id
      integer n1
      integer n2
      integer n
      real dyopiinv
      real xb
      real yb
      real xx
      real yy
      real p
      real f
      real cr
      real ci
      real brb(7)
      real bib(7)
      real c1r
      real c1i
      real r1
      real f1
      real c2r
      real c2i
      real r2
      real f2
      real c3r
      real c3i
      real r3
      real f3
      real c4r
      real c4i
      real r4
      real f4
      real c5r
      real c5i
      real r5
      real f5
      real c6r
      real c6i
      real r6
      real f6
      real c7r
      real c7i

      dyopiinv = 1./(8.*atan(1.))

      do 90 id = 1, kchildless
         xb = xc(id)
         yb = yc(id)
         n1 = npb(id, 1)
         n2 = npb(id, 2)
         Brb(1) = Br(id, 1)
         Bib(1) = Bi(id, 1)
         Brb(2) = Br(id, 2)
         Bib(2) = Bi(id, 2)
         Brb(3) = Br(id, 3)
         Bib(3) = Bi(id, 3)
         Brb(4) = Br(id, 4)
         Bib(4) = Bi(id, 4)
         Brb(5) = Br(id, 5)
         Bib(5) = Bi(id, 5)
         Brb(6) = Br(id, 6)
         Bib(6) = Bi(id, 6)
         Brb(7) = Br(id, 7)
         Bib(7) = Bi(id, 7)

         do 2 n = n1, n2
            xx = xn(n) - xb
            yy = yb - yn(n)

C Use multipole expansions to compute the forces on the box

            p = Brb(1)
            f = Bib(1)
            c1r = p
            c1i = f

C level = 2
            r1 = xx
            f1 = yy
            p = Brb(2)
            f = Bib(2)
            c2r = r1*p - f1*f
            c2i = r1*f + f1*p

C level = 3
            r2 = r1*r1 - f1*f1
            f2 = r1*f1 + f1*r1
            p = Brb(3)
            f = Bib(3)
            c3r = r2*p - f2*f
            c3i = r2*f + f2*p

C level = 4
            r3 = r2*r1 - f2*f1
            f3 = r2*f1 + f2*r1
            p = Brb(4)
            f = Bib(4)
            c4r = r3*p - f3*f
            c4i = r3*f + f3*p

C level = 5
            r4 = r3*r1 - f3*f1
            f4 = r3*f1 + f3*r1
            p = Brb(5)
            f = Bib(5)
            c5r = r4*p - f4*f
            c5i = r4*f + f4*p

C level = 6
            r5 = r4*r1 - f4*f1
            f5 = r4*f1 + f4*r1
            p = Brb(6)
            f = Bib(6)
            c6r = r5*p - f5*f
            c6i = r5*f + f5*p

C level = 7
            r6 = r5*r1 - f5*f1
            f6 = r5*f1 + f5*r1
            p = Brb(7)
            f = Bib(7)
            c7r = r6*p - f6*f
            c7i = r6*f + f6*p

C Sum all the terms in the series
            cr = c1r + c2r + c3r + c4r + c5r + c6r + c7r
            ci = c1i + c2i + c3i + c4i + c5i + c6i + c7i

C Calculate the velocity induced by the group "k" on particle "i"

            uu(n) = uu(n) - ci*dyopiinv
            vv(n) = vv(n) + cr*dyopiinv

    2    continue
   90 continue
      end
