SUBROUTINE POT_PART(xtest, ytest, kpart, up)

!  This subroutine finds the velocity potential induced at (xtest,ytest)
!  by the particles in XT,YT.

   implicit none

   include 'tree_tmp.h'

   integer :: kpart
   real :: xtest, ytest, up

   integer :: m
   real :: xa, ya, xv, yv, gv, radius
!------------------------------------------------------------------------------

   Up = 0.0
   radius = 1.

   DO 4 m = 1, kpart
      gv = GT(m)
      xv = XT(m)
      yv = YT(m)
      xa = xv - xtest
      ya = yv - ytest
      IF ((abs(yv) <= radius) .AND. (xv <= 0.)) THEN
         ! the panels intersect
         ! the branch cut of the vortex at pi,-pi
         xa = -xa
         ya = -ya
      ENDIF
      up = up + gv*ATAN2(ya, xa)
4  END DO

   RETURN
END SUBROUTINE POT_PART

