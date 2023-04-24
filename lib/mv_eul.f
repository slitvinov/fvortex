      subroutine mv_eul()

C     Advance the particle positions using an Euler step
C     based on velocities calculated in CONDIFF and VEL_EXT.


      include 'main_dim.h'

      include 'part.h'

      integer n
      real time, dt
      common/params/n, Time, dt

      integer np
      real s2, ovrlp, gnu
      common/part/Np, s2, ovrlp, gnu

      integer in, i
      real pi, const
C--------------------------------------------------------------------

      in = 0
      pi = 4.*atan(1.)
      const = gnu*ovrlp**2/(pi*s2)

      do 1 i = 1, Np
         xp(i) = xn(i) + dt*uu(i)
         yp(i) = yn(i) + dt*vv(i)
         gp(i) = gn(i) + dt*const*gdiff(i)
    1 end do

      return
      end
