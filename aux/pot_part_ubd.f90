subroutine pot_part_ubd(xtest, ytest, kpart, up)

!  This subroutine finds the velocity potential induced at (xtest,ytest)
!  by the particles in XT,YT.

   implicit none

   include 'tree_tmp.h'

   integer :: kpart
   real :: xtest, ytest, up

   integer :: m
   real :: xa, ya, xv, yv, gv
!------------------------------------------------------------------------------

   Up = 0.0

   do 4 m = 1, kpart
      gv = gt(m)
      xv = xt(m)
      yv = yt(m)
      xa = xtest - xv
      ya = ytest - yv

      up = up + gv*atan2(ya, xa)
4  end do

   return
end subroutine pot_part_ubd

