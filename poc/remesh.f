      call remesh
      end

      subroutine remesh
      integer nx_max
      integer ny_max
      integer nx_min
      integer ny_min
      integer ngrid
      parameter(nx_max=800)
      parameter(nx_min=-800)
      parameter(ny_max=800)
      parameter(ny_min=-800)
      parameter(ngrid=(nx_max - nx_min + 1)*(ny_max - ny_min + 1))

      integer nvort
      parameter(nvort=5000000)

      real xp(nvort)
      real yp(nvort)
      real gp(nvort)
      common/vort1/xp, yp, gp

      integer np
      real s2
      real ovrlp
      common/part/Np, s2, ovrlp

      integer n
      real time
      real dt
      common/params/n, Time, dt

      real vortlim
      common/rems/vortlim

      integer nx_r
      integer nx_l
      integer ny_t
      integer ny_b
      integer ig
      integer ix
      integer iy
      integer i
      integer in
      integer ifar
      integer iback
      integer ix0
      integer ix1
      integer ix2
      integer iy0
      integer iy1
      integer iy2
      integer nmesh
      integer indx(nx_min:nx_max,ny_min:ny_max)
      integer k00
      integer k10
      integer k20
      integer k01
      integer k11
      integer k21
      integer k02
      integer k12
      integer k22
      real pi
      real twopi
      real twopiinv
      real dh
      real circ
      real xmax
      real xmin
      real ymax
      real ymin
      real xr
      real xl
      real yt
      real yb
      real dhhaf
      real xx
      real yy
      real cold
      real cx
      real cutoff
      real cut_far
      real dhinv
      real g
      real x
      real y
      real ndist
      real sdist
      real u
      real v
      real fy0
      real fy1
      real fy2
      real fx0
      real fx1
      real fx2
      real ag
      real cnew
      real xg(ngrid)
      real yg(ngrid)
      real gg(ngrid)
      real strength
      real deltax
      real denom
      real ell_x
      real ell_y
      real h2
      real r_arg
      real rmax
      integer Nmx


      Rmax = 1.1
      ell_x = 6.0
      ell_y = 1.0
      s2 = 1e-4
      ovrlp = 0.9

      pi = 4.0*atan(1.0)
      twopi = 2.*pi
      twopiinv = 1./twopi
      dh = 2.*ovrlp*sqrt(s2/pi)
      dhhaf = 0.5*dh
      dhinv = 1./dh

      h2 = s2*ovrlp**2
      deltax = sqrt(h2)
      h2 = deltax*deltax
      Nmx = 2*Rmax/deltax + 1
      denom = 1.0/(0.1*Rmax)**2
      in = 0
      do 101 ix = 1, Nmx
         do 102 iy = 1, Nmx
            x = -Rmax + deltax*(ix - 0.5)
            y = -Rmax + deltax*(iy - 0.5)
            r_arg = (x/ell_x)**2 + (y/ell_y)**2
            strength = denom*h2*exp(-r_arg*denom)
            in = in + 1
            xp(in) = x
            yp(in) = y
            gp(in) = strength
  102    continue
 101  continue

      Np = in
      write (*, *) 'initial number of Particles ', Np

      xmin = xp(1)
      xmax = xp(1)
      ymin = yp(1)
      ymax = yp(1)
      do 1001 i = 1, np
         x = xp(i)
         y = yp(i)
         xmin = amin1(xmin, x)
         xmax = amax1(xmax, x)
         ymin = amin1(ymin, y)
         ymax = amax1(ymax, y)
 1001 continue

      xr = xmax + 5.*dh
      xl = xmin - 5.*dh
      yt = ymax + 5.*dh
      yb = ymin - 5.*dh
      nx_r = nint(Xr/dh)
      nx_l = nint(Xl/dh)
      ny_t = nint(Yt/dh)
      ny_b = nint(Yb/dh) - 1

C Establish the new grid for the remeshed field

      ig = 0
      do 110 ix = nx_l, nx_r
         xx = dhhaf + ix*dh
         do 111 iy = ny_b, ny_t
            yy = dhhaf + iy*dh
            ig = ig + 1
            xg(ig) = xx
            yg(ig) = yy
            gg(ig) = 0.0
            indx(ix, iy) = ig
 111     continue
 110  continue

      Nmesh = ig

C check diagnostics, pre-remesh - they should be conserved through
C remesh

      cold = 0.0
      cx = 0.0
      do 71 i = 1, Np
         cold = cold + gp(i)
         cx = cx + gp(i)*yp(i)
   71 continue
      write (*, *) 'pre-remesh, circulation:', cold, '  x-impulse: ', cx

C set cutoff values to throw out particles

      cutoff = vortlim*s2*ovrlp**2
      cut_far = 10.*cutoff
C NEW GRID IN PLACE, REMESH OLD FIELD NOW

      in = 0
      ifar = 0
      do 40 i = 1, Np

         g = gp(i)
         x = xp(i)
         y = yp(i)
         ndist = y
         sdist = x
         ix = nint(sdist*dhinv - 0.5)
         iy = nint(ndist*dhinv - 0.5)

C ---- Do not remesh the particles outside the established grid

         if ((ix > nx_r) .or. (ix < nx_l) .or. (iy > ny_t) .or.
     $     (iy < ny_b)) then
            if (abs(g) >= cut_far) then
               ifar = ifar + 1
               xp(ifar) = x
               yp(ifar) = y
               gp(ifar) = g
            endif

C all other particles are in the inner grid
         else if ((ix == nx_r) .or. (ix == nx_l) .or.
     $        (iy == ny_t) .or. (iy == ny_b)) then
C Category 0_1 : PARTICLES at the FAR interface (NGP remeshing)
            in = in + 1
            ig = indx(ix, iy)
            gg(ig) = gg(ig) + g

         else
C Category 0_1 : ALL other PARTICLES in the domain
            in = in + 1
            ix0 = ix
            ix1 = ix - 1
            ix2 = ix + 1
            iy0 = iy
            iy1 = iy - 1
            iy2 = iy + 1
            k00 = indx(ix0, iy0)
            k10 = indx(ix1, iy0)
            k20 = indx(ix2, iy0)
            k01 = indx(ix0, iy1)
            k11 = indx(ix1, iy1)
            k21 = indx(ix2, iy1)
            k02 = indx(ix0, iy2)
            k12 = indx(ix1, iy2)
            k22 = indx(ix2, iy2)
            u = (sdist - xg(k00))*dhinv
            v = (ndist - yg(k00))*dhinv
            Fy0 = 1.0 - v*v
            Fy1 = 0.5*v*(v - 1.)
            Fy2 = 0.5*v*(v + 1.)
            Fx0 = g*(1.-u*u)
            Fx1 = g*(0.5*u*(u - 1.))
            Fx2 = g*(0.5*u*(u + 1.))
            gg(k00) = gg(k00) + Fx0*Fy0
            gg(k01) = gg(k01) + Fx0*Fy1
            gg(k02) = gg(k02) + Fx0*Fy2
            gg(k10) = gg(k10) + Fx1*Fy0
            gg(k11) = gg(k11) + Fx1*Fy1
            gg(k12) = gg(k12) + Fx1*Fy2
            gg(k20) = gg(k20) + Fx2*Fy0
            gg(k21) = gg(k21) + Fx2*Fy1
            gg(k22) = gg(k22) + Fx2*Fy2
         endif
   40 continue

C put remeshed particles into arrays, using cutoffs determined earlier

      iback = ifar
      do 29 i = 1, Nmesh
         g = gg(i)
         ag = abs(g)
C only cutoff if below threshold AND away from domain center
         if ((ag > cutoff) .or. ((abs(yg(i))*abs(yg(i)) +
     $     abs(xg(i))*abs(xg(i))) < 1.)) then
            iback = iback + 1
            xp(iback) = xg(i)
            yp(iback) = yg(i)
            gp(iback) = g
         endif
   29 continue
      Np = iback
      cnew = 0.0
      cx = 0.0
      do 72 i = 1, Np
         circ = gp(i)
         cnew = cnew + circ
         cx = cx + circ*yp(i)
 72   continue
      write (*, *) 'post-remesh, circulation:', cnew,
     $     '  x-impulse: ', cx

      write (*, 89) Np, iback - ifar, ifar

   89 format(3x, 'New Total :', i8, 3x, 'INSIDE :', i8, 2x,
     $     'OUTSIDE :', i8, 2x)
      end
