!     gfortran -std=legacy ppm.f
      program main
      implicit none
      integer k
      integer m
      integer n
      integer ox
      integer oy
      integer r
      integer x
      integer x0
      integer y
      integer y0
      character zero
      parameter(m = 200, n = 100, k = 3 * m * n, zero = char(0))
      character a(3, m, n)
      data a/k * zero /

      ox = 9*m/10
      oy = 9*n/10
      r = 20
      do 10 x = -r, r
         do 20 y = -r, r
            if (x*x + y * y <= r * r) then
               x0 = ox + x
               y0 = oy + y
               if (1 <= x0 .and. x0 <= m .and. 1 <= y0 .and. y0 <= n)
     &              then
                  a(1, x0, y0) = char(255)
               end if
            end if
   20    end do
   10 end do


      open(unit=2, file='a.ppm')
      write(2, '(A / I0 X I0 / A)') 'P6', m, n, '255'
      close(2)
      open(unit=2, file='a.ppm', FORM='UNFORMATTED', ACCESS='APPEND')
      write(2) a
      close(2)
      end
