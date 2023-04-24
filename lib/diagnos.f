      subroutine diagnos(iframe)

C Calculates the linear/angular impulse and circulation of the flow.
C Differentiation of the impulse will give drag and lift.

      include 'main_dim.h'
      include 'part.h'

      character partfile*256
      integer i
      integer iframe
      integer n
      integer np
      real ang
      real circ
      real dt
      real g
      real gnu
      real ovrlp
      real s2
      real Time
      real x
      real xmom
      real y
      real ymom

      common/params/n, Time, dt
      common/part/Np, s2, ovrlp, gnu

      write (partfile, '(A, I8.8, A)') 'p.', iframe, '.dat'
      open (1, file=partfile, status='REPLACE')
      write (partfile, '(A, I8.8, A)') 'p.', iframe, '.raw'
      open (2, file=partfile, status='REPLACE', access='STREAM')
      xmom = 0.
      ymom = 0.
      ang = 0.
      do 2 i = 1, Np
         x = xp(i)
         y = yp(i)
         g = gp(i)
         circ = circ + g
         xmom = xmom - g*y
         ymom = ymom + g*x
         ang = ang + 0.5*(x*x + y*y + s2)*g
         write (1, '(SP, 2P E23.16, 1X, E23.16, 1X, E23.16)') x, y, g
         write (2) x, y, g
    2 end do
      close (1)
      close (2)      

      write (10, 100) Time, xmom
      write (11, 100) Time, ymom
      write (12, 100) Time, Circ
      close(10)
      close(11)
      close(12)

  100 format(f10.4, 2x, e13.6)
      end
