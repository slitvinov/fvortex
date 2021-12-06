      subroutine mv_rk(visc_rmax)

!     Advance the particle positions using a Runge-Kutta scheme.
!     Must do after a remesh rather than Adams Bashforth because
!     velocities from the previous step are not available.

      implicit none

      include 'main_dim.h'
      include 'part.h'

      integer n
      real time, dt
      common/params/n, Time, dt

      integer np
      real s2, ovrlp, gnu
      common/part/Np, s2, ovrlp, gnu

      integer xfree, yfree, wfree
      real xmass, xdamp, xspring, xforce
      real x_const, x_amp, x_freq, x_phase
      real ymass, ydamp, yspring, yforce
      real y_amp, y_freq, y_phase
      real wmass, wdamp, wspring
      real w_fixed, w_const, w_amp, w_freq, w_phase
      real last_x, last_u, last_udot
      real last_y, last_v, last_vdot
      real last_th, last_w, last_wdot
      common/motion/xfree, xmass, xdamp, xspring, xforce,
     &  x_const, x_amp, x_freq, x_phase,
     &  yfree, ymass, ydamp, yspring, yforce,
     &  y_amp, y_freq, y_phase,
     &  wfree, wmass, wdamp, wspring,
     &  w_fixed, w_const, w_amp, w_freq, w_phase,
     &  last_x, last_u, last_udot,
     &  last_y, last_v, last_vdot,
     &  last_th, last_w, last_wdot

      real visc_rmax

      integer in, i
      real pi, const
!------------------------------------------------------------------
      in = 0
      pi = 4.*atan(1.)
      const = gnu*ovrlp**2/(pi*s2)

!---  1st Substep, predictor step, goes to dt/2
      do 1 i = 1, Np
         xp(i) = xn(i) + 0.5*dt*uu(i)
         yp(i) = yn(i) + 0.5*dt*vv(i)

         gp(i) = gn(i) + 0.5*dt*const*gdiff(i)
    1 end do

!--   If any free motion, need to update now
      if ((xfree == 1) .or. (yfree == 1) .or. (wfree == 1)) then
!-- Rebuild the tree
         call condiff(Np, 0, 9999., 0)
         call update_position(0.5*dt)
      endif

!---  2nd Substep, corrector step, uses dt/2 values to get to dt

      in = 0
!--   Need to rebuild the interaction tree
      call condiff(Np, 1, visc_rmax, 0)
      call vel_ext(time + 0.5*dt)
      do 2 i = 1, Np
         xp(i) = xn(i) + dt*(uu(i) - 0.5*Uold(i))
         yp(i) = yn(i) + dt*(vv(i) - 0.5*Vold(i))

         gp(i) = gn(i) + dt*const*(gdiff(i) - 0.5*gdold(i))
    2 end do

      return
      end
