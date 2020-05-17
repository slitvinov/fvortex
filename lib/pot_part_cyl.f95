subroutine pot_part(xtest, ytest, kpart, up)

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

   do 4 m = 1, kpart
      gv = gt(m)
      xv = xt(m)
      yv = yt(m)
      xa = xv - xtest
      ya = yv - ytest
      if ((abs(yv) <= radius) .and. (xv <= 0.)) then
         ! the panels intersect
         ! the branch cut of the vortex at pi,-pi
         xa = -xa
         ya = -ya
      endif
      up = up + gv*atan2(ya, xa)
4  end do

   return
end subroutine pot_part

