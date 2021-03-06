program go
   implicit none

   interface
      include 'petros.h'
   end interface
   include 'main_dim.h'
   include 'part.h'

   integer :: np
   real :: s2, ovrlp, gnu
   common/part/Np, s2, ovrlp, gnu

   integer :: n
   real :: time, dt
   common/params/n, time, dt

   real :: vortlim, t1, t2
   common/rems/vortlim

   integer :: irk, ivalue, istepping
   integer :: icase, idiags
   integer :: Nsteps, Nrem, Nrestart
   integer :: Nvf, Ntree
   real :: Rmax, ell_x, ell_y, visc_rmax
   logical ::  lremesh

!---------------------------------------------------------------------------

   irk = 0
   lremesh = .false.

   call input(icase, idiags, istepping, &
              Nsteps, Nrem, Nrestart, &
              Nvf, Ntree, &
              Rmax, ell_x, ell_y, visc_rmax)

!---  tabulate the gaussian for use as diffusion kernel

   call gaussian

!---  old data for continuation run
   if (icase == 0) then
      call read_restart(time, np, s2, ovrlp, nvort, xp, yp, gp)

      !     -- NEW run
   else
      call initial(Rmax, ell_x, ell_y)
      time = 0.0
      irk = 0
      call diagnos           ! get initial impulse and circulation
   endif
   ivalue = 0
   call vort_field(ivalue)

   call condiff(Np, 0, 9999., 0) ! rebuild the interaction tree

!     call vel_error  ! absolute error in a vel. profile relative to exact

   do 1 n = 1, Nsteps

      !--   compute vortex interactions with the FAST MULTIPOLE METHOD
      call cpu_time(t1)

      if (mod(n, Ntree) == 0) then
         call condiff(Np, 1, visc_rmax, 1)
      else
         call condiff(np, 1, visc_rmax, 0)
      endif

      call cpu_time(t2)
      write (*, 103) Np, t2 - t1
      call vel_ext(time)     ! Add irrotational velocities

      !---  Move the particles

      if ((n == 1) .or. (lremesh)) then ! first step or first after remesh
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
      !--   remesh every few steps to regularize particle locations

      if (mod(n, Nrem) == 0) then
         call remesh
         lremesh = .true.
      endif

      if (idiags == 1) then
         call diagnos        ! flow momentum and circulation
      endif

      !--   save data for restart, if desired

      if (mod(n, Nrestart) == 0) then
         ivalue = n/Nrestart
         call write_restart(time, np, s2, ovrlp, nvort, xp, yp, gp)
      endif

      !--   take measurements if desired

      call condiff(Np, 0, 9999., 0) ! rebuild the interaction tree

      !     call vel_error  ! absolute error in a vel. profile relative to exact

      if (mod(n, Nvf) == 0) then
         ivalue = n/Nvf
         call vort_field(ivalue)
      endif

      !--   end of loop

1  end do

102 format(10x, ' Time Step :', i9, 6x, 'Time :', f8.4)
103 format(10x, ' Particles :', i9, 6x, 'Time :', f8.4)

   stop
end program

subroutine initial(Rmax, ell_x, ell_y)

!     Computes the initial positions for the particles in a new run and
!     assigns circulation based on initial time desired.
!     Rmax is used as the half-width of the square grid.
!     Initial time is shifted backwards for accurate discretization
!     using initially point vortex diffused to desired core size.

   implicit none

   include 'main_dim.h'
   include 'part.h'

   integer :: np
   real :: s2, ovrlp, gnu
   common/part/Np, s2, ovrlp, gnu
   real :: rmax, ell_x, ell_y, r_arg
   integer :: Nmx, in, ix, iy
   real :: h2, deltax, denom, x, y, strength
!-----------------------------------------------------------------------
   h2 = s2*ovrlp**2
   deltax = sqrt(h2)  ! grid spacing
   h2 = deltax*deltax ! actual cell area
   Nmx = 2*Rmax/deltax + 1
   denom = 1.0/(0.1*Rmax)**2

!--- generate the grid
   in = 0
   do 101 ix = 1, Nmx
      do 102 iy = 1, Nmx
         x = -Rmax + deltax*(ix - 0.5)
         y = -Rmax + deltax*(iy - 0.5)
         r_arg = (x/ell_x)**2 + (y/ell_y)**2 ! elliptic vortex
         strength = denom*h2*exp(-r_arg*denom)
         in = in + 1
         xp(in) = x
         yp(in) = y
         gp(in) = strength
102   end do
101 end do

   Np = in           ! the initial number of particles
   write (*, *) 'initial number of Particles ', Np

   return
end subroutine initial

subroutine input(icase, idiags, istepping, &
                 Nsteps, Nrem, Nrestart, &
                 Nvf, Ntree, &
                 Rmax, ell_x, ell_y, visc_rmax)

! In this subroutine parameters for the computation of the
! Lamb-Oseen (initially point) vortex are input.

   implicit none

   include 'main_dim.h'

   integer :: n
   real :: time, dt
   common/params/n, Time, dt

   integer :: np
   real :: s2, ovrlp, gnu
   common/part/Np, s2, ovrlp, gnu

   real :: vortlim
   common/rems/vortlim

   integer :: limpar
   real :: x0, y0
   common/geom/x0, y0, Limpar

   integer :: nxavg, nyavg
   real :: xmaxavg, xminavg, ymaxavg, yminavg
   common/vort_avg/nxavg, nyavg, xmaxavg, Xminavg, ymaxavg, yminavg

   integer :: icase, idiags, nsteps, nrem, nrestart, nvf
   integer :: ntree, istepping
   real :: Rmax, ell_x, ell_y, visc_rmax
!---------------------------------------------------------------------------
!-----read in various parameters for the computation

   open (1, file='input.dat', status='OLD')
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

!~~~~~~~~~~~~~~~~~~~~ GLOSSARY OF INPUT PARAMETERS ~~~~~~~~~~~~~~~~~~~~~~~~

!       dt = time step length   Nsteps = # of time steps
!       gnu = kinematic viscosity
!            s2 = particle core area
!       ovrlp = ratio of grid to particle core size

!       Limpar = minimum particles per box in tree decompositions
!       vortlim = cutoff vorticity for a particle

!       Nrem = frequency of remeshing
!            visc_rmax = maximum radius at which do account for diffusion

!       Rmax = outer radius of remesh grid
!
!            istepping = type of time stepping after remesh
!                (1=Euler (1st order), 2=Runge Kutta 2nd order)
!
!       Nvf = frequency of vorticity field measurement
!
!       nxavg,nyavg = points in grid for vorticity time averaging
!       xmaxavg,xminavg,ymaxavg,yminavg = boundaries of this grid
!
!       idiags = 0 for no force measurement, 1 for momentum, 2 for both
!
!            ntree = frequency for tree stats output
!
!       icase = 0 for a continuation run
!       Nrestart = frequency with which to write restart files

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   return
end subroutine input
