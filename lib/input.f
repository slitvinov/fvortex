c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE INPUT(icase,ipath,idiags,istepping,
     &     Nsteps,Nrem,Nrestart,
     &     Nvf,Nvel,Ntree,
     &     Rmax,gamma_0,ell_x,ell_y,time_0,visc_rmax,
     &     i_time_avg,n_avg_start,n_avg_times,n_avg_interval)

C In this subroutine parameters for the computation of the 
c Lamb-Oseen (initially point) vortex are input.

        implicit none

            include 'main_dim.h'
        include 'measure.h'

        integer n
        real time,dt,slip_frac
        COMMON/PARAMS/n,Time,dt,slip_frac

        integer np
        real s2,ovrlp,gnu
        COMMON/PART/Np,s2,ovrlp,gnu

        real vortlim
        COMMON/REMS/vortlim

        integer limpar
        real x0,y0
        COMMON/GEOM/X0,Y0,Limpar

        integer nxavg,nyavg
        real xmaxavg,xminavg,ymaxavg,yminavg
        COMMON/VORT_AVG/Nxavg,Nyavg,Xmaxavg,Xminavg,Ymaxavg,Yminavg

        real vel_rmax,vel_points
        COMMON/vel_avg/vel_rmax,vel_points

        integer icase,ipath,idiags,nsteps,nrem,nrestart,nvf
        integer nvel,ntree,istepping
        integer i_time_avg,n_avg_start,n_avg_times,n_avg_interval
        real Rmax,gamma_0,ell_x,ell_y,time_0,visc_rmax
c---------------------------------------------------------------------------
c-----read in various parameters for the computation

        OPEN(1,FILE='ubd_input.dat',STATUS = 'OLD')
        READ(1,*)
        READ(1,*)dt,Nsteps
        READ(1,*)
        READ(1,*)gamma_0,time_0,gnu
        READ(1,*)
        READ(1,*)s2,ovrlp
        READ(1,*)
        READ(1,*)Limpar,vortlim
        READ(1,*)
        READ(1,*)Nrem,visc_rmax
        READ(1,*)
        READ(1,*)Rmax,slip_frac,ell_x,ell_y
        READ(1,*)
        READ(1,*)istepping
        READ(1,*)
        READ(1,*)Nvf,Nvel
        READ(1,*)
        READ(1,*)i_time_avg,n_avg_start,n_avg_times,n_avg_interval
        READ(1,*)
        READ(1,*)nxavg,nyavg,xminavg,xmaxavg,yminavg,ymaxavg
        READ(1,*)
        READ(1,*)vel_rmax,vel_points
        READ(1,*)
        READ(1,*)ipath
        READ(1,*)
        READ(1,*)idiags
        READ(1,*)
            READ(1,*)Ntree
            READ(1,*)
        READ(1,*)icase,Nrestart
        close(1)

c~~~~~~~~~~~~~~~~~~~~ GLOSSARY OF INPUT PARAMETERS ~~~~~~~~~~~~~~~~~~~~~~~~


c       dt = time step length   Nsteps = # of time steps

c            Lamb vortex IC: omega(r)=(gamma_0/(4.*gnu*time_0*pi))exp(-r*r/4.*gnu*time_0)
c       gnu = kinematic viscosity       

c            s2 = particle core area
c       ovrlp = ratio of grid to particle core size

c       Limpar = minimum particles per box in tree decompositions
c       vortlim = cutoff vorticity for a particle

c       Nrem = frequency of remeshing
c            visc_rmax = maximum radius at which do account for diffusion

c       Rmax = outer radius of remesh grid
c       slip_frac = fractional time during step when slip is given

c            istepping = type of time stepping after remesh
c                (1=Euler (1st order), 2=Runge Kutta 2nd order)

c       Nvf = frequency of vorticity field measurement
c       Nvel = frequency of velocity field measurement

c       i_time_avg = whether to do time averaged measurements
c       n_avg_start = step on which to start averaging
c       n_avg_times = number of times to do averaging measurements
c       n_avg_interval = freqency of doing a measurement used in the average

c       nxavg,nyavg = points in grid for vorticity time averaging
c       xmaxavg,xminavg,ymaxavg,yminavg = boundaries of this grid

c       vel_rmax = outer radius to measure time averaged velocity to
c       vel_points = points along the ray to measure velocity

c       ipath = 0 to do pathlines throughout the run

c       idiags = 0 for no force measurement, 1 for momentum, 2 for both

c            ntree = frequency for tree stats output

c       icase = 0 for a continuation run
c       Nrestart = frequency with which to write restart files

c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        RETURN
        END
