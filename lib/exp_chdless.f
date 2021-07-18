      subroutine exp_chdless(nmax, kchildless, xc, yc, ichildless, 
     &  npb, Pr, Pi)

!     This subroutine computes the MULTIPOLE EXPANSIONS of
!     the childless boxes at a level.

      implicit none

      include 'main_dim.h'
      include 'part.h'

      integer nmax, kchildless
      integer ichildless(nmax), npb(nmax, 2)
      real xc(nmax), yc(nmax), pr(nmax, 0:7), pi(nmax, 0:7)

      integer k, nb, n1, n2, j
      real xm, ym, x, y
      real tr0, ti0, tr1, ti1, tr2, ti2, tr3, ti3, tr4, ti4
      real tr5, ti5, tr6, ti6, tr7, ti7
      real re0, re1, fa1, re2, fa2, re3, fa3, re4, fa4
      real re5, fa5, re6, fa6, re7, fa7
!-----------------------------------------------------------------

      do 10 k = 1, kchildless
         nb = ichildless(k)
         n1 = npb(nb, 1)
         n2 = npb(nb, 2)
         Xm = xc(nb)
         Ym = yc(nb)
         Tr0 = 0.
         Ti0 = 0.
         Tr1 = 0.
         Ti1 = 0.
         Tr2 = 0.
         Ti2 = 0.
         Tr3 = 0.
         Ti3 = 0.
         Tr4 = 0.
         Ti4 = 0.
         Tr5 = 0.
         Ti5 = 0.
         Tr6 = 0.
         Ti6 = 0.
         Tr7 = 0.
         Ti7 = 0.

         do 100 j = n1, n2
            x = xn(j) - Xm
            y = Ym - yn(j)
            re0 = gn(j)
            re1 = re0*x
            fa1 = re0*y
            re2 = re1*x - fa1*y
            fa2 = re1*y + fa1*x
            re3 = re2*x - fa2*y
            fa3 = re2*y + fa2*x
            re4 = re3*x - fa3*y
            fa4 = re3*y + fa3*x
            re5 = re4*x - fa4*y
            fa5 = re4*y + fa4*x
            re6 = re5*x - fa5*y
            fa6 = re5*y + fa5*x
            re7 = re6*x - fa6*y
            fa7 = re6*y + fa6*x

            Tr0 = Tr0 + re0
            Tr1 = Tr1 + re1
            Ti1 = Ti1 + fa1
            Tr2 = Tr2 + re2
            Ti2 = Ti2 + fa2
            Tr3 = Tr3 + re3
            Ti3 = Ti3 + fa3
            Tr4 = Tr4 + re4
            Ti4 = Ti4 + fa4
            Tr5 = Tr5 + re5
            Ti5 = Ti5 + fa5
            Tr6 = Tr6 + re6
            Ti6 = Ti6 + fa6
            Tr7 = Tr7 + re7
            Ti7 = Ti7 + fa7
 100     end do
         Pr(nb, 0) = Tr0
         Pi(nb, 0) = Ti0
         Pr(nb, 1) = Tr1
         Pi(nb, 1) = Ti1
         Pr(nb, 2) = Tr2
         Pi(nb, 2) = Ti2
         Pr(nb, 3) = Tr3
         Pi(nb, 3) = Ti3
         Pr(nb, 4) = Tr4
         Pi(nb, 4) = Ti4
         Pr(nb, 5) = Tr5
         Pi(nb, 5) = Ti5
         Pr(nb, 6) = Tr6
         Pi(nb, 6) = Ti6
         Pr(nb, 7) = Tr7
         Pi(nb, 7) = Ti7
 10   end do

      return
      end
