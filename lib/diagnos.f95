subroutine diagnos

!  Calculates the linear/angular impulse and circulation of the flow.
!  Differentiation of the impulse will give drag and lift.

   implicit none

   include 'main_dim.h'

   include 'part.h'

   integer :: n
   real :: Time, dt
   common/params/n, Time, dt

   integer :: np
   real :: s2, ovrlp, gnu
   common/part/Np, s2, ovrlp, gnu

   integer :: i
   real :: pi, circ, xmom, ymom, g, x_acc, y_acc, w_acc, ang, x, y
!-----------------------------------------------------

   pi = 4.*atan(1.)
   circ = 0.
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
      write (21, *) x, y
2  end do
   close (21)

   write (10, 100) Time, xmom
   write (11, 100) Time, ymom
   write (12, 100) Time, Circ

100 format(f10.4, 2x, e13.6)

   return
end subroutine
