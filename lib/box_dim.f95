subroutine box_dim(Num, Xmin, Xmax, Ymin, Ymax)

!  This subroutine finds the bounding x and y coordinates in the domain.

   implicit none

   include 'main_dim.h'
   include 'part.h'

   integer :: num
   real :: xmin, xmax, ymin, ymax, x, y

   integer :: i
!------------------------------------------------------------------

   Xmin = xp(1)
   Xmax = xp(1)
   Ymin = yp(1)
   Ymax = yp(1)
   do 10 i = 1, Num
      x = xp(i)
      y = yp(i)
      Xmin = amin1(Xmin, x)
      Xmax = amax1(Xmax, x)
      Ymin = amin1(Ymin, y)
      Ymax = amax1(Ymax, y)
10 end do

   return
end subroutine box_dim
