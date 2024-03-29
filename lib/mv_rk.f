      subroutine mv_rk(visc_rmax)

C Advance the particle positions using a Runge-Kutta scheme.
C Must do after a remesh rather than Adams Bashforth because
C velocities from the previous step are not available.

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

      integer xfree
      integer yfree
      integer wfree
      real xmass
      real xdamp
      real xspring
      real xforce
      real x_const
      real x_amp
      real x_freq
      real x_phase
      real ymass
      real ydamp
      real yspring
      real yforce
      real y_amp
      real y_freq
      real y_phase
      real wmass
      real wdamp
      real wspring
      real w_fixed
      real w_const
      real w_amp
      real w_freq
      real w_phase
      real last_x
      real last_u
      real last_udot
      real last_y
      real last_v
      real last_vdot
      real last_th
      real last_w
      real last_wdot
      common/motion/xfree, xmass, xdamp, xspring, xforce,
     $  x_const, x_amp, x_freq, x_phase,
     $  yfree, ymass, ydamp, yspring, yforce,
     $  y_amp, y_freq, y_phase,
     $  wfree, wmass, wdamp, wspring,
     $  w_fixed, w_const, w_amp, w_freq, w_phase,
     $  last_x, last_u, last_udot,
     $  last_y, last_v, last_vdot,
     $  last_th, last_w, last_wdot

      real visc_rmax

      integer in
      integer i
      real pi
      real const
      in = 0
      pi = 4.*atan(1.)
      const = gnu*ovrlp**2/(pi*s2)

C 1st Substep, predictor step, goes to dt/2
      do 1 i = 1, Np
         xp(i) = xn(i) + 0.5*dt*uu(i)
         yp(i) = yn(i) + 0.5*dt*vv(i)

         gp(i) = gn(i) + 0.5*dt*const*gdiff(i)
    1 continue

C If any free motion, need to update now
      if ((xfree == 1) .or. (yfree == 1) .or. (wfree == 1)) then
C Rebuild the tree
         call condiff(Np, 0, 9999., 0)
         call update_position(0.5*dt)
      endif

C 2nd Substep, corrector step, uses dt/2 values to get to dt

      in = 0
C Need to rebuild the interaction tree
      call condiff(Np, 1, visc_rmax, 0)
      call vel_ext(time + 0.5*dt)
      do 2 i = 1, Np
         xp(i) = xn(i) + dt*(uu(i) - 0.5*Uold(i))
         yp(i) = yn(i) + dt*(vv(i) - 0.5*Vold(i))

         gp(i) = gn(i) + dt*const*(gdiff(i) - 0.5*gdold(i))
    2 continue
      end
