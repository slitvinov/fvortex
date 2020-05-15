        SUBROUTINE INITIAL(Rmax,gamma_0,ell_x,ell_y,time_0)

c     Computes the initial positions for the particles in a new run and
c     assigns circulation based on initial time desired.
c     Rmax is used as the half-width of the square grid.
c     Initial time is shifted backwards for accurate discretization
c     using initially point vortex diffused to desired core size.

        implicit none

            include 'main_dim.h'
                include 'part.h'

        integer n
        real time,dt,slip_frac
        COMMON/PARAMS/n,Time,dt,slip_frac

        integer np
        real s2,ovrlp,gnu
        COMMON/PART/Np,s2,ovrlp,gnu

        real vortlim
        COMMON/REMS/vortlim

        real rmax,gamma_0,ell_x,ell_y,time_0,r_arg

        integer Nmx,in,ix,iy
        real pi,h2,deltax,denom,front,x,y,r,strength,t_shift
c-----------------------------------------------------------------------
        pi=4.*ATAN(1.0)
        h2 = s2*ovrlp**2
        deltax = SQRT(h2)  ! grid spacing
            h2 = deltax*deltax ! actual cell area
        Nmx = 2*Rmax/deltax + 1

        denom= 1.0/(0.1*Rmax)**2! 1./(4.*gnu*(time_0-t_shift))
        front=gamma_0*denom

c--- generate the grid
        in = 0
        DO 101 ix = 1,Nmx
            DO 102 iy = 1,Nmx
               x = -Rmax + deltax*(ix-0.5)
               y = -Rmax + deltax*(iy-0.5)
                           r_arg = (x/ell_x)**2 + (y/ell_y)**2 ! elliptic vortex
               strength=front*h2*exp(-r_arg*denom)
               in = in + 1
               xp(in) = x
               yp(in) = y
               gp(in) = strength
102         CONTINUE
101     CONTINUE

        Np=in           ! the initial number of particles
                write(*,*)'initial number of Particles ', Np

        RETURN
        END
