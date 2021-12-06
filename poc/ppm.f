!     gfortran -std=legacy ppm.f
      program main
      integer m, n, k, i, j
      character zero
      parameter(m = 200, n = 100, k = 3 * m * n, zero = char(0))
      character a(3, m, n)
      data a/k * zero /

      do 10 i = 1, 10
         do 20 j = 1, n
            a(1, i, j) = char(255)
            a(2, i + 10, j) = char(255)
            a(3, i + 20, j) = char(255)
   20    end do
   10 end do

      open(unit=2, file='a.ppm')
      write(2, '(A / I0 X I0 / A)') 'P6', m, n, '255'
      close(2)
      open(unit=2, file='a.ppm', FORM='UNFORMATTED', ACCESS='APPEND')
      write(2) a
      close(2)
      end
