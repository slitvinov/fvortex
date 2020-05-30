subroutine str_part(xtest, ytest, kpart, sp)

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

   do 4 m = 1, kpart
      gv = gt(m)
      xv = xt(m)
      yv = yt(m)
      xa = xtest - xv
      ya = ytest - yv
      r2a = xa*xa + ya*ya
      Sp = Sp - gv*alog(r2a)
4  end do

   Sp = 0.5*Sp

   return
end subroutine str_part
