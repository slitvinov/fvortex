      subroutine box_dim(npart, xmin, xmax, ymin, ymax)
!     This subroutine finds the bounding x and y coordinates in the domain.
      implicit none

      include 'main_dim.h'
      real xp(nvort), yp(nvort)
      common/vort1/xp, yp
      integer npart
      real xmax
      real xmin
      real ymax
      real ymin

      real x
      real y
      integer i
!------------------------------------------------------------------

      xmin = xp(1)
      xmax = xp(1)
      ymin = yp(1)
      ymax = yp(1)
      do 10 i = 1, npart
         x = xp(i)
         y = yp(i)
         xmin = amin1(xmin, x)
         xmax = amax1(xmax, x)
         ymin = amin1(ymin, y)
         ymax = amax1(ymax, y)
 10   end do

      return
      end
