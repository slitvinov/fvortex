function velocity_y(time)

!  Gives  the freestream velocity in the y-direction at time

   implicit none

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

   real :: velocity_y, time
!------------------------------------------------------

   if (yfree == 1) then
      velocity_y = last_v
   else
      velocity_y = y_amp*SIN(y_freq*Time + y_phase)
   endif

   return
end function velocity_y
