SUBROUTINE STR_PART(xtest, ytest, kpart, sp)

!  This subroutine finds the streamfunction value at (xtest,ytest) induced
!  by the vorticies in the list XT,YT,GT.

   implicit none

   include 'tree_tmp.h'

   integer :: kpart
   real :: xtest, ytest, sp

   integer :: m
   real :: xa, ya, xv, yv, gv, r2a
!------------------------------------------------------------------------------

   Sp = 0.0

   DO 4 m = 1, kpart
      gv = GT(m)
      xv = XT(m)
      yv = YT(m)
      xa = xtest - xv
      ya = ytest - yv
      r2a = xa*xa + ya*ya
      Sp = Sp - gv*ALOG(r2a)
4  END DO

   Sp = 0.5*Sp

   RETURN
END SUBROUTINE STR_PART
