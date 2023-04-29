      function velocity_y(time)

C Gives  the freestream velocity in the y-direction at time

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

      real velocity_y
      real time

      if (yfree == 1) then
         velocity_y = last_v
      else
         velocity_y = y_amp*sin(y_freq*Time + y_phase)
      endif
      end
