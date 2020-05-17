subroutine input(icase, ipath, idiags, istepping, &
                 Nsteps, Nrem, Nrestart, &
                 Nvf, Nvel, Ntree, &
                 Rmax, gamma_0, ell_x, ell_y, time_0, visc_rmax, &
                 i_time_avg, n_avg_start, n_avg_times, n_avg_interval)

! In this subroutine parameters for the computation of the
! Lamb-Oseen (initially point) vortex are input.

   implicit none

   include 'main_dim.h'
   include 'measure.h'

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
   common/vort_avg/Nxavg, Nyavg, Xmaxavg, Xminavg, Ymaxavg, Yminavg

   real :: vel_rmax, vel_points
   common/vel_avg/vel_rmax, vel_points

   integer :: icase, ipath, idiags, nsteps, nrem, nrestart, nvf
   integer :: nvel, ntree, istepping
   integer :: i_time_avg, n_avg_start, n_avg_times, n_avg_interval
   real :: Rmax, gamma_0, ell_x, ell_y, time_0, visc_rmax
!---------------------------------------------------------------------------
!-----read in various parameters for the computation

   open (1, file='ubd_input.dat', status='OLD')
   read (1, *)
   read (1, *) dt, Nsteps
   read (1, *)
   read (1, *) gamma_0, time_0, gnu
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
   read (1, *) Nvf, Nvel
   read (1, *)
   read (1, *) i_time_avg, n_avg_start, n_avg_times, n_avg_interval
   read (1, *)
   read (1, *) nxavg, nyavg, xminavg, xmaxavg, yminavg, ymaxavg
   read (1, *)
   read (1, *) vel_rmax, vel_points
   read (1, *)
   read (1, *) ipath
   read (1, *)
   read (1, *) idiags
   read (1, *)
   read (1, *) Ntree
   read (1, *)
   read (1, *) icase, Nrestart
   close (1)

!~~~~~~~~~~~~~~~~~~~~ GLOSSARY OF INPUT PARAMETERS ~~~~~~~~~~~~~~~~~~~~~~~~

!       dt = time step length   Nsteps = # of time steps

!            Lamb vortex IC: omega(r)=(gamma_0/(4.*gnu*time_0*pi))exp(-r*r/4.*gnu*time_0)
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

!       Nvf = frequency of vorticity field measurement
!       Nvel = frequency of velocity field measurement

!       i_time_avg = whether to do time averaged measurements
!       n_avg_start = step on which to start averaging
!       n_avg_times = number of times to do averaging measurements
!       n_avg_interval = freqency of doing a measurement used in the average

!       nxavg,nyavg = points in grid for vorticity time averaging
!       xmaxavg,xminavg,ymaxavg,yminavg = boundaries of this grid

!       vel_rmax = outer radius to measure time averaged velocity to
!       vel_points = points along the ray to measure velocity

!       ipath = 0 to do pathlines throughout the run

!       idiags = 0 for no force measurement, 1 for momentum, 2 for both

!            ntree = frequency for tree stats output

!       icase = 0 for a continuation run
!       Nrestart = frequency with which to write restart files

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   return
end subroutine input
