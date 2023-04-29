      subroutine update_position(delta_t)

C This routine updates the motion of the body in all directions in
C which it is not considered fixed.
C Signs get a bit messy becuase u,v,udot,vdot are for the freestream,
C which is (-) the body motion.

      real last_cdp
      real last_cdf
      real last_clp
      real last_clf
      real last_cm
      common/force/last_cdp, last_cdf, last_clp, last_clf, last_cm

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

      real delta_t

      real cd
      real cl
      real old_sheet
      real pi
      real x_tilde
      real u_tilde
      real y_tilde
      real v_tilde
      real th_tilde
      real w_tilde

      pi = 4.*atan(1.)

      if (xfree == 1) then
         x_tilde = last_x - delta_t*last_u
         u_tilde = last_u + delta_t*last_udot
         old_sheet = -2.*pi*last_udot
         cd = last_cdp + last_cdf + old_sheet
         last_udot = -(1./(xmass + pi))*(cd + (xmass - pi)*xforce
     $     + xdamp*u_tilde - xspring*x_tilde)
         last_u = last_u + delta_t*last_udot
         last_x = last_x - delta_t*last_u
      endif
      if (yfree == 1) then
         y_tilde = last_y - delta_t*last_v
         v_tilde = last_v + delta_t*last_vdot
         old_sheet = -2.*pi*last_vdot
         cl = last_clp + last_clf + old_sheet
         last_vdot = -(1./(ymass + pi))*(cl + (ymass - pi)*yforce
     $     + ydamp*v_tilde - yspring*y_tilde)
         last_v = last_v + delta_t*last_vdot
         last_y = last_y - delta_t*last_v
      endif
      if (wfree == 1) then
         th_tilde = last_th + delta_t*last_w
         w_tilde = last_w + delta_t*last_wdot
         last_wdot = (1./wmass)*(last_cm
     $     - wdamp*w_tilde - wspring*(th_tilde - w_fixed))
         last_w = last_w + delta_t*last_wdot
         last_th = last_th + delta_t*last_w
      endif
      end

