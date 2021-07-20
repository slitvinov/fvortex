!     gfortran -std=legacy ppm.f
      program main
      implicit none
      integer m, n, k
      integer x, y, ox, oy, r
      character zero
      parameter(m = 200, n = 100, k = 3 * m * n, zero = char(0))
      character a(3, m, n)
      data a/k * zero /

      ox = m/2
      oy = n/4
      r = 20
      do 10 x = -r, r
         do 20 y = -r, r
            if (x*x + y * y <= r * r) then
               a(1, ox + x, oy + y) = char(255)
            end if
 20      end do
 10   end do


      open(unit=2, file='a.ppm')
      write(2, '(A / I0 X I0 / A)') 'P6', m, n, '255'
      close(2)
      open(unit=2, file='a.ppm', FORM='UNFORMATTED', ACCESS='APPEND')
      write(2) a
      close(2)
      end
