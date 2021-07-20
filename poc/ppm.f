      program main
      implicit none
      integer m
      integer n
      character*1 a

c$$$      open(unit=2, file='a.ppm', form='formatted')
c$$$      m = 600
c$$$      n = 800
c$$$      write(2, '(A / I0 X I0 / A)') 'P6', m, n, '255'
c$$$      close(2)

      open(unit=2, file='a.ppm', form='unformatted')
      write(2) a
      close(2)
      end
