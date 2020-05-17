SUBROUTINE MV_RK(visc_rmax)

!  Advance the particle positions using a Runge-Kutta scheme.
!  Must do after a remesh rather than Adams Bashforth because
!  velocities from the previous step are not available.

   implicit none

   include 'main_dim.h'
   include 'part.h'

   integer :: n
   real :: time, dt, slip_frac
   COMMON/PARAMS/n, Time, dt, slip_frac

   integer :: np
   real :: s2, ovrlp, gnu
   COMMON/PART/Np, s2, ovrlp, gnu

   integer :: xfree, yfree, wfree
   real :: xmass, xdamp, xspring, xforce
   real :: x_const, x_amp, x_freq, x_phase
   real :: ymass, ydamp, yspring, yforce
   real :: y_amp, y_freq, y_phase
   real :: wmass, wdamp, wspring
   real :: w_fixed, w_const, w_amp, w_freq, w_phase
   real :: last_x, last_u, last_udot
   real :: last_y, last_v, last_vdot
   real :: last_th, last_w, last_wdot
   common/motion/xfree, xmass, xdamp, xspring, xforce, &
      x_const, x_amp, x_freq, x_phase, &
      yfree, ymass, ydamp, yspring, yforce, &
      y_amp, y_freq, y_phase, &
      wfree, wmass, wdamp, wspring, &
      w_fixed, w_const, w_amp, w_freq, w_phase, &
      last_x, last_u, last_udot, &
      last_y, last_v, last_vdot, &
      last_th, last_w, last_wdot

   real :: visc_rmax

   integer :: in, i
   real :: pi, const, xpi, ypi
   real :: theta, u_vel, v_vel, vel_tan
!------------------------------------------------------------------
   in = 0
   pi = 4.*ATAN(1.)
   const = gnu*ovrlp**2/(pi*s2)

!--- 1st Substep, predictor step, goes to dt/2
   DO 1 i = 1, Np
      xp(i) = XN(i) + 0.5*dt*UU(i)
      yp(i) = YN(i) + 0.5*dt*VV(i)

      gp(i) = gn(i) + 0.5*dt*const*gdiff(i)
1  END DO

!-- If any free motion, need to update now
   if ((xfree == 1) .OR. (yfree == 1) .OR. (wfree == 1)) then
      !-- Rebuild the tree
      CALL CONDIFF(Np, 0, 9999., 0)
      call update_position(0.5*dt)
   endif

!--- 2nd Substep, corrector step, uses dt/2 values to get to dt

   in = 0
!-- Need to rebuild the interaction tree
   CALL CONDIFF(Np, 1, visc_rmax, 0)
   CALL VEL_EXT(time + 0.5*dt)
   DO 2 i = 1, Np
      xp(i) = XN(i) + dt*(UU(i) - 0.5*Uold(i))
      yp(i) = YN(i) + dt*(VV(i) - 0.5*Vold(i))

      gp(i) = gn(i) + dt*const*(gdiff(i) - 0.5*gdold(i))
2  END DO

   RETURN
END SUBROUTINE
