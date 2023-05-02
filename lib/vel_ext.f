      subroutine vel_ext(tm)

C This routine adds the irrotational components of the velocity field
C (which are not represented by the vorticity field) such as a free
C stream.

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

      real tm

      integer i
      real alpha
      real x_vel
      real y_vel
      real xni
      real yni
      real arn2
      real uext
      real vext

      real velocity_x
      real velocity_y
      real omega

      alpha = omega(tm)
      x_vel = velocity_x(tm)
      y_vel = velocity_y(tm)

      do 1 i = 1, Np
         xni = xn(i)
         yni = yn(i)
         Arn2 = alpha/(xni*xni + yni*yni)
         Uext = -yni*Arn2 + x_vel
         Vext = xni*Arn2 + y_vel
         uu(i) = uu(i) + Uext
         vv(i) = vv(i) + Vext
    1 continue
      end
