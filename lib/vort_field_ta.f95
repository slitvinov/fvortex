subroutine VORT_FIELD_ta

!   This subroutine outputs the vorticity field on a grid for time averaging.

   implicit none

   include 'main_dim.h'

   include 'part.h'

   include 'measure.h'

   integer :: n
   real :: time, dt, slip_frac
   common/params/n, Time, dt, slip_frac

   integer :: np
   real :: s2, ovrlp, gnu
   common/part/Np, s2, ovrlp, gnu

   integer :: nxavg, nyavg
   real :: Xmaxavg, Xminavg, Ymaxavg, Yminavg
   common/vort_avg/Nxavg, Nyavg, Xmaxavg, Xminavg, Ymaxavg, Yminavg

   integer :: ivelpts
   real :: Xstr(Nsx), Ystr(Nsy)
   real :: Psi(Nsx*Nsy), xg(nsx*nsy), yg(nsx*nsy), gg(nsx*nsy)
   real :: u90(ivel_pts), v90(ivel_pts), u270(ivel_pts), v270(ivel_pts)
   real :: x90(ivel_pts), y90(ivel_pts), x270(ivel_pts), y270(ivel_pts)
   common/time_avg/Xstr, Ystr, Psi, ivelpts, &
      u90, v90, u270, v270, x90, y90, x270, y270, xg, yg, gg

   integer :: ny, j, ix, iy, m0, m, nx, ngrid
   real :: pi, twopiinv, dx, dy, x0, y0, s2inv2, s2piinv, dxinv, dyinv
   real :: g, x, y, r2s, c
!----------------------------------------------------------------

   pi = 4.0*atan(1.0)
   twopiinv = 0.5/pi

   dx = abs((xmaxavg - xminavg)/nxavg)
   dy = abs((ymaxavg - yminavg)/nyavg)
   x0 = Xminavg
   y0 = Yminavg
   ny = nyavg
   nx = nxavg
   ngrid = nx*ny

!- Determination of  the  circulation  of each particle

   s2inv2 = 0.5/s2
   s2piinv = twopiinv/s2
   dxinv = 1./dx
   dyinv = 1./dy

   do 20 j = 1, Np
      g = gp(j)*s2piinv
      x = xp(j)
      y = yp(j)
      ix = nint((x - x0)*dxinv) + 1
      iy = nint((y - y0)*dyinv) + 1
      if ((ix > -3) .and. (iy > -3) .and. (ix < (nx + 3)) .and. &
          (iy < (ny + 3))) then

         !-- loop over grid in expnaded code in order to vectorize
         !     ---- Points on column IX
         m0 = (ix - 1)*Ny + iy
         m = m0
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif

         !     ---- Points on column IX - 3
         m0 = (ix - 4)*Ny + iy
         m = m0
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif

         !     ---- Points on column IX - 2
         m0 = (ix - 3)*Ny + iy
         m = m0
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif

         !     ---- Points on column IX - 1
         m0 = (ix - 2)*Ny + iy
         m = m0
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif

         !     ---- Points on column IX + 1
         m0 = (ix)*Ny + iy
         m = m0
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif

         !     ---- Points on column IX + 2
         m0 = (ix + 1)*Ny + iy
         m = m0
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif

         !     ---- Points on column IX + 3
         m0 = (ix + 2)*Ny + iy
         m = m0
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 - 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 1
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 2
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
         m = m0 + 3
         if ((m > 0) .and. (m < (ngrid + 1))) then
            r2s = ((xg(m) - x)**2 + (yg(m) - y)**2)*s2inv2
            c = exp(-r2s)
            gg(m) = gg(m) + g*c
         endif
      endif
20 end do

   return
end subroutine
