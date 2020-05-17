SUBROUTINE BOX_DIM(Num, Xmin, Xmax, Ymin, Ymax)

!  This subroutine finds the bounding x and y coordinates in the domain.

   implicit none

   include 'main_dim.h'
   include 'part.h'

   integer :: num
   real :: xmin, xmax, ymin, ymax, x, y

   integer :: i
!------------------------------------------------------------------

   Xmin = XP(1)
   Xmax = XP(1)
   Ymin = YP(1)
   Ymax = YP(1)
   DO 10 I = 1, Num
      x = XP(i)
      y = YP(i)
      Xmin = AMIN1(Xmin, x)
      Xmax = AMAX1(Xmax, x)
      Ymin = AMIN1(Ymin, y)
      Ymax = AMAX1(Ymax, y)
10 END DO

   RETURN
END SUBROUTINE BOX_DIM
