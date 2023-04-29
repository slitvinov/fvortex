      subroutine vort_field(iframe)

C outputs the vorticity field on a grid.

      include 'main_dim.h'
      include 'part.h'

      character vortoutfile*256
      integer i
      integer iframe
      integer in
      integer ix
      integer iy
      integer j
      integer m
      integer m0
      integer n
      integer ngrid
      integer np
      integer nx
      integer ny
      real c
      real dt
      real dx
      real dxinv
      real dy
      real dyinv
      real g
      real gg(nxmaxavg*nymaxavg)
      real gnu
      real ovrlp
      real pi
      real r2s
      real s2
      real s2inv2
      real s2piinv
      real time
      real twopiinv
      real x
      real x0
      real xg(nxmaxavg*nymaxavg)
      real xmax
      real xmin
      real xx
      real y
      real y0
      real yg(nxmaxavg*nymaxavg)
      real ymax
      real ymin
      
      common/vort_avg/nx, ny, xmax, xmin, ymax, ymin
      common/part/Np, s2, ovrlp, gnu
      common/params/n, Time, dt
      pi = 4.0*atan(1.0)
      twopiinv = 0.5/pi

      dx = abs((xmax - Xmin)/nx)
      dy = abs((ymax - ymin)/ny)
      x0 = Xmin
      y0 = ymin

      in = 0
      do 1 ix = 1, nx
         do 2 iy = 1, ny
            in = in + 1
            gg(in) = 0.
    2    continue
    1 continue

      in = 0
      do 10 ix = 1, nx
         xx = x0 + (ix - 1)*dx
         do 11 iy = 1, ny
            in = in + 1
            xg(in) = xx
            yg(in) = y0 + (iy - 1)*dy
   11    continue
   10 continue

      Ngrid = nx*ny

C Determination of  the  circulation  of each particle

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
         if ((ix > -3) .and. (iy > -3) .and. (ix < (nx + 3)) .and.
     $     (iy < (ny + 3))) then

C loop over grid in expnaded code in order to vectorize
C ---- Points on column IX
            m0 = (ix - 1)*ny + iy
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

C ---- Points on column IX - 3
            m0 = (ix - 4)*ny + iy
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

C ---- Points on column IX - 2
            m0 = (ix - 3)*ny + iy
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

C ---- Points on column IX - 1
            m0 = (ix - 2)*ny + iy
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

C ---- Points on column IX + 1
            m0 = (ix)*ny + iy
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

C ---- Points on column IX + 2
            m0 = (ix + 1)*ny + iy
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

C ---- Points on column IX + 3
            m0 = (ix + 2)*ny + iy
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
   20 continue
      write (vortoutfile, '(A, I8.8, A)') 'w.', iframe, '.dat'
      open (1, file=vortoutfile, status='replace')
      write (1, '(A, /, A, I8, A, I8)') 'variables=x,y,w',
     $     'zone i=', nx, ', j=', ny
      in = 0
      do j = 1, nx
         do i = 1, ny
            in = in + 1
            write (1, '(SP, E23.16, 1X, E23.16, 1X, E23.16)')
     $           xg(in), yg(in), gg(in)
         continue
      continue
      close (1)
      end
