      subroutine remesh()

C This subroutine remeshes the vortex field onto a uniform Cartesian
C grid.

      include 'remesh_cartesian.h'
      include 'main_dim.h'
      include 'part.h'

      integer np
      real s2, ovrlp, gnu
      common/part/Np, s2, ovrlp, gnu

      integer n
      real time, dt
      common/params/n, Time, dt

      real vortlim
      common/rems/vortlim

      integer nx_r, nx_l, ny_t, ny_b, ig, ix, iy, i, in, ifar, iback
      integer ix0, ix1, ix2, iy0, iy1, iy2, nmesh
      integer indx(nx_min:nx_max, ny_min:ny_max)
      integer k00, k10, k20, k01, k11, k21, k02, k12, k22
      real pi, twopi, twopiinv, dh, circ
      real xmax, xmin, ymax, ymin, xr, xl, yt, yb, dhhaf, xx, yy,
     $     cold, cx
      real cutoff, cut_far, dhinv, g, x, y, ndist, sdist
      real u, v, fy0, fy1, fy2, fx0, fx1, fx2, ag, cnew
      real xg(ngrid), yg(ngrid), gg(ngrid)

      pi = 4.0*atan(1.0)
      twopi = 2.*pi
      twopiinv = 1./twopi
      dh = 2.*ovrlp*sqrt(s2/pi)
      dhinv = 1./dh

C FIRST ESTABLISH THE NEW GRID TO BE MAPPED INTO

C Find the edge of the grid of particles

      call box_dim(np, xmin, xmax, ymin, ymax)
      xr = xmax + 5.*dh
      xl = xmin - 5.*dh
      yt = ymax + 5.*dh
      yb = ymin - 5.*dh
      nx_r = nint(Xr/dh)
      nx_l = nint(Xl/dh)
      ny_t = nint(Yt/dh)
      ny_b = nint(Yb/dh) - 1

      write (*, *) 'nx_l,nx_r', nx_l, nx_r
      write (*, *) 'ny_t,ny_b', ny_t, ny_b
      if (nx_r > NX_max) then
         write (*, *) 'PROBLEM :nx_right =', nx_r, ' nx_max = ', nx_max
      end if
      if (nx_l < NX_min) then
         write (*, *) 'PROBLEM :nx_left =', nx_l, ' nx_min=', nx_min
      end if
      if (ny_t > ny_max) then
         write (*, *) 'PROBLEM :ny_top =', ny_t, 'ny_max=', ny_max
      end if
      if (ny_b < ny_min) then
         write (*, *) 'PROBLEM :ny_bottom =', ny_b, 'ny_min=', ny_min
      end if

      if ((nx_r > NX_max) .or. (nx_l < NX_min) .or.
     $  (ny_t > ny_max) .or. (ny_b < ny_min)) then
         stop
      end if

C Establish the new grid for the remeshed field

      dhhaf = 0.5*dh
      ig = 0
      do 10 ix = nx_l, nx_r, 1
         xx = dhhaf + ix*dh
         do 11 iy = ny_b, ny_t, 1
            yy = dhhaf + iy*dh
            ig = ig + 1
            xg(ig) = xx
            yg(ig) = yy
            gg(ig) = 0.0
            indx(ix, iy) = ig   ! We can avoid this big array
   11    end do
   10 end do

      Nmesh = ig

      write (*, *) 'Nmesh = ', Nmesh
      if (nmesh > ngrid) then   ! overran array dimensions
         write (*, *) 'nmesh too large in remesh, stopping'
         stop
      endif

C check diagnostics, pre-remesh - they should be conserved through
C remesh

      cold = 0.0
      cx = 0.0
      do 71 i = 1, Np
         cold = cold + gp(i)    ! total circulation
         cx = cx + gp(i)*yp(i)  ! x-impulse
   71 end do
      write (*, *) 'pre-remesh, circulation:', cold, '  x-impulse: ', cx

C set cutoff values to throw out particles

      cutoff = vortlim*s2*ovrlp**2
      cut_far = 10.*cutoff
C NEW GRID IN PLACE, REMESH OLD FIELD NOW

      in = 0
      ifar = 0
      do 40 i = 1, Np           ! loop through, mapping particles to
                                ! mesh
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
            gg(ig) = gg(ig) + g    ! NGP   Interpolation

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
   40 end do

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
   29 end do

C check diagnostics

      Np = iback
      cnew = 0.0
      cx = 0.0
      do 72 i = 1, Np
         circ = gp(i)
         cnew = cnew + circ
         cx = cx + circ*yp(i)
   72 end do
      write (*, *) 'post-remesh, circulation:', cnew,
     $     '  x-impulse: ', cx

      write (*, 89) Np, iback - ifar, ifar

   89 format(3x, 'New Total :', i8, 3x, 'INSIDE :', i8, 2x,
     $     'OUTSIDE :', i8, 2x)
      end
