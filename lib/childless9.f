      subroutine childless9(kp9)

C This subroutine computes the MULTIPOLE EXPANSIONS of
C the childless boxes on level 9.

      include 'main_dim.h'
      include 'part.h'
      include 'tree9.h'

      integer kp9

      integer nb, n1, n2, j
      real xm, ym, x, y
      real tr0, ti0, re0, tr1, ti1, re1, fa1, tr2, ti2, re2, fa2
      real tr3, ti3, re3, fa3, tr4, ti4, re4, fa4, tr5, ti5, re5, fa5
      real tr6, ti6, re6, fa6, tr7, ti7, re7, fa7

      do 10 nb = 1, kp9
         n1 = npb9(nb, 1)
         n2 = npb9(nb, 2)
         Xm = xc9(nb)
         Ym = yc9(nb)
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
  100    end do
         Pr9(nb, 0) = Tr0
         Pi9(nb, 0) = Ti0
         Pr9(nb, 1) = Tr1
         Pi9(nb, 1) = Ti1
         Pr9(nb, 2) = Tr2
         Pi9(nb, 2) = Ti2
         Pr9(nb, 3) = Tr3
         Pi9(nb, 3) = Ti3
         Pr9(nb, 4) = Tr4
         Pi9(nb, 4) = Ti4
         Pr9(nb, 5) = Tr5
         Pi9(nb, 5) = Ti5
         Pr9(nb, 6) = Tr6
         Pi9(nb, 6) = Ti6
         Pr9(nb, 7) = Tr7
         Pi9(nb, 7) = Ti7

   10 end do
      end
