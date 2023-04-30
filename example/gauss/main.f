      program go

      include 'main_dim.h'
      include 'part.h'

      integer icase
      integer idiags
      integer irk
      integer istepping
      integer ivalue
      integer n
      integer np
      integer Nrem
      integer Nrestart
      integer Nsteps
      integer Ntree
      integer Nvf
      logical  lremesh
      real dt
      real ell_x
      real ell_y
      real gnu
      real ovrlp
      real Rmax
      real s2
      real time
      real visc_rmax
      real vortlim
      common/params/n, time, dt
      common/part/Np, s2, ovrlp, gnu
      common/rems/vortlim

      irk = 0
      lremesh = .false.
      call input(icase, idiags, istepping, Nsteps, Nrem, Nrestart, Nvf,
     $     Ntree, Rmax, ell_x, ell_y, visc_rmax)

c---  tabulate the gaussian for use as diffusion kernel

      call gaussian()

c---  old data for continuation run
      if (icase == 0) then
         call read_restart(time, np, s2, ovrlp, nvort, xp, yp, gp)

c     -- NEW run
      else
         call initial(Rmax, ell_x, ell_y)
         time = 0.0
         irk = 0
         if (idiags == 1) then
            call diagnos(ivalue)
         endif
      endif
      ivalue = 0
      call vort_field(ivalue)

      call condiff(Np, 0, 9999., 0)
      do 1 n = 1, Nsteps

c--   compute vortex interactions with the FAST MULTIPOLE METHOD
         if (mod(n, Ntree) == 0) then
            call condiff(Np, 1, visc_rmax, 1)
         else
            call condiff(np, 1, visc_rmax, 0)
         endif
         call vel_ext(time)

c---  Move the particles

         if ((n == 1) .or. (lremesh)) then
            lremesh = .false.
            if (istepping == 2) then
               call mv_rk(visc_rmax)
               irk = 1
            else
               call mv_eul
               irk = 0
            endif
         else
            call mv_ab(irk)
            irk = 0
         endif

         time = time + dt
         write (*, 102) n, time
c--   remesh every few steps to regularize particle locations

         if (mod(n, Nrem) == 0) then
            call remesh
            lremesh = .true.
         endif

         if (idiags == 1) then
            call diagnos(ivalue)
         endif

c--   save data for restart, if desired

         if (mod(n, Nrestart) == 0) then
            ivalue = n/Nrestart
            call write_restart(time, np, s2, ovrlp, nvort, xp, yp, gp)
         endif

c--   take measurements if desired
         call condiff(Np, 0, 9999., 0)
         if (mod(n, Nvf) == 0) then
            ivalue = n/Nvf
            call vort_field(ivalue)
         endif

c--   end of loop

    1 continue

  102 format(10x, ' Time Step :', i9, 6x, 'Time :', f8.4)
  103 format(10x, ' Particles :', i9, 6x, 'Time :', f8.4)

      end

      subroutine initial(Rmax, ell_x, ell_y)

c     Computes the initial positions for the particles in a new run and
c     assigns circulation based on initial time desired.
c     Rmax is used as the half-width of the square grid.
c     Initial time is shifted backwards for accurate discretization
c     using initially point vortex diffused to desired core size.


      include 'main_dim.h'
      include 'part.h'

      common/part/Np, s2, ovrlp, gnu
      integer in
      integer ix
      integer iy
      integer Nmx
      integer np
      real deltax
      real denom
      real ell_x
      real ell_y
      real gnu
      real h2
      real ovrlp
      real r_arg
      real rmax
      real s2
      real strength
      real x
      real y
      h2 = s2*ovrlp**2
      deltax = sqrt(h2)
      h2 = deltax*deltax
      Nmx = 2*Rmax/deltax + 1
      denom = 1.0/(0.1*Rmax)**2

c---  generate the grid
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
  101 continue

      Np = in
      write (*, *) 'initial number of Particles ', Np
      end

      subroutine input(icase, idiags, istepping, Nsteps, Nrem, Nrestart,
     $     Nvf, Ntree, Rmax, ell_x, ell_y, visc_rmax)

c     In this subroutine parameters for the computation of the
c     Lamb-Oseen (initially point) vortex are input.


      include 'main_dim.h'

      integer n
      real time
      real dt
      common/params/n, Time, dt

      integer np
      real s2
      real ovrlp
      real gnu
      common/part/Np, s2, ovrlp, gnu

      real vortlim
      common/rems/vortlim

      integer limpar
      real x0
      real y0
      common/geom/x0, y0, Limpar

      integer nxavg
      integer nyavg
      real xmaxavg
      real xminavg
      real ymaxavg
      real yminavg
      common/vort_avg/nxavg, nyavg, xmaxavg, xminavg, ymaxavg, yminavg

      integer icase
      integer idiags
      integer nsteps
      integer nrem
      integer nrestart
      integer nvf
      integer ntree
      integer istepping
      real Rmax
      real ell_x
      real ell_y
      real visc_rmax
c-----read in various parameters for the computation

      open (1, file='input.dat', status='OLD', err = 101)
      read (1, *)
      read (1, *) dt, Nsteps
      read (1, *)
      read (1, *) gnu
      read (1, *)
      read (1, *) s2, ovrlp
      read (1, *)
      read (1, *) Limpar, vortlim
      read (1, *)
      read (1, *) Nrem, visc_rmax
      read (1, *)
      read (1, *) Rmax, ell_x, ell_y
      read (1, *)
      read (1, *) istepping
      read (1, *)
      read (1, *) Nvf
      read (1, *)
      read (1, *) nxavg, nyavg, xminavg, xmaxavg, yminavg, ymaxavg
      read (1, *)
      read (1, *) idiags
      read (1, *)
      read (1, *) Ntree
      read (1, *)
      read (1, *) icase, Nrestart
      close (1)

c     dt = time step length   Nsteps = # of time steps
c     gnu = kinematic viscosity
c     s2 = particle core area
c     ovrlp = ratio of grid to particle core size

c     Limpar = minimum particles per box in tree decompositions
c     vortlim = cutoff vorticity for a particle

c     Nrem = frequency of remeshing
c     visc_rmax = maximum radius at which do account for diffusion

c     Rmax = outer radius of remesh grid
c
c     istepping = type of time stepping after remesh
c     (1=Euler (1st order), 2=Runge Kutta 2nd order)
c
c     Nvf = frequency of vorticity field measurement
c
c     nxavg,nyavg = points in grid for vorticity time averaging
c     xmaxavg,xminavg,ymaxavg,yminavg = boundaries of this grid
c
c     idiags = 0 for no force measurement, 1 for momentum, 2 for both
c
c     ntree = frequency for tree stats output
c
c     icase = 0 for a continuation run
c     Nrestart = frequency with which to write restart files
      return
  101 write (*, '(''gauss: error: needs input.dat file'')')
      stop 1

      end
