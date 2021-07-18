      subroutine mv_ab(irk)

!     Advance the particle positions using an Adams Bashforth scheme
!     based on velocities calculated in CONDIFF and VEL_EXT.

      implicit none

      include 'main_dim.h'

      include 'part.h'

      integer n
      real time, dt
      common/params/n, Time, dt

      integer np
      real s2, ovrlp, gnu
      common/part/Np, s2, ovrlp, gnu

      integer irk

      integer in, i
      real pi, const, c1, c2
!--------------------------------------------------------------------

      in = 0
      pi = 4.*atan(1.)
      const = gnu*ovrlp**2/(pi*s2)

      if (irk == 0) then
         c1 = 1.5
         c2 = 0.5
      else
         c1 = 2.
         c2 = 1.
      endif
      do i = 1, Np
         xp(i) = xn(i) + dt*(c1*uu(i) - c2*Uold(i))
         yp(i) = yn(i) + dt*(c1*vv(i) - c2*Vold(i))

         gp(i) = gn(i) + dt*const*(c1*gdiff(i) - c2*gdold(i))
      enddo

      return
      end
