subroutine max_rad(Np, rmax)

!  This subroutine finds the distance of the particle farthest from (0,0).

   implicit none

   include 'main_dim.h'
   include 'part.h'

   integer :: np
   real :: rmax

   integer :: i
   real :: x, y, r
!----------------------------------------------------------------------

   x = xp(1)
   y = yp(1)
   rmax = sqrt(x*x + y*y)
   do 11 i = 2, Np
      x = xp(i)
      y = yp(i)
      r = sqrt(x*x + y*y)
      rmax = amax1(rmax, r)
11 end do

   return
end subroutine max_rad
