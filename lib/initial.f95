subroutine initial(Rmax, gamma_0, ell_x, ell_y)

!     Computes the initial positions for the particles in a new run and
!     assigns circulation based on initial time desired.
!     Rmax is used as the half-width of the square grid.
!     Initial time is shifted backwards for accurate discretization
!     using initially point vortex diffused to desired core size.

   implicit none

   include 'main_dim.h'
   include 'part.h'

   integer :: n

   integer :: np
   real :: s2, ovrlp, gnu
   common/part/Np, s2, ovrlp, gnu
   real :: rmax, gamma_0, ell_x, ell_y, r_arg
   integer :: Nmx, in, ix, iy
   real :: pi, h2, deltax, denom, front, x, y, strength
!-----------------------------------------------------------------------
   pi = 4.*atan(1.0)
   h2 = s2*ovrlp**2
   deltax = sqrt(h2)  ! grid spacing
   h2 = deltax*deltax ! actual cell area
   Nmx = 2*Rmax/deltax + 1

   denom = 1.0/(0.1*Rmax)**2
   front = gamma_0*denom

!--- generate the grid
   in = 0
   do 101 ix = 1, Nmx
      do 102 iy = 1, Nmx
         x = -Rmax + deltax*(ix - 0.5)
         y = -Rmax + deltax*(iy - 0.5)
         r_arg = (x/ell_x)**2 + (y/ell_y)**2 ! elliptic vortex
         strength = front*h2*exp(-r_arg*denom)
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
