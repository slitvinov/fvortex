      subroutine mv_eul()

C Advance the particle positions using an Euler step
C based on velocities calculated in CONDIFF and VEL_EXT.

      include 'main_dim.h'

      include 'part.h'

      integer n
      real time
      real dt
      common/params/n, Time, dt

      integer np
      real s2
      real ovrlp
      real gnu
      common/part/Np, s2, ovrlp, gnu

      integer in
      integer i
      real pi
      real const

      in = 0
      pi = 4.*atan(1.)
      const = gnu*ovrlp**2/(pi*s2)

      do 1 i = 1, Np
         xp(i) = xn(i) + dt*uu(i)
         yp(i) = yn(i) + dt*vv(i)
         gp(i) = gn(i) + dt*const*gdiff(i)
    1 continue
      end
