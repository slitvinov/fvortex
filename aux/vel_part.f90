subroutine vel_part(xtest, ytest, upart, vpart, kpart)

!  This  subroutine calculates the velocities induced on the
!  particle located at *xtest*,*ytest*, by the particles in its
!  interaction list *XT,YT,GT*.

   implicit none

   include 'tree_tmp.h'

   integer :: np
   real :: s2, ovrlp, gnu
   common/part/Np, s2, ovrlp, gnu

   integer :: kpart
   real :: xtest, ytest, upart, vpart

   integer :: m
   real :: eps, xx, yy, r2, fc, s2inv2
!------------------------------------------------------------------------------

! Compute now the direct particle-particle interactions

   Upart = 0.0
   Vpart = 0.0
   eps = 0.000001
   s2inv2 = 0.5/s2

   do 4 m = 1, kpart
      xx = xtest - xt(m)
      yy = yt(m) - ytest
      r2 = xx*xx + yy*yy
      fc = gt(m)*(1.-exp(-r2*s2inv2))/(r2 + eps)
      Upart = Upart + yy*fc
      Vpart = Vpart + xx*fc
4  end do

   return
end subroutine vel_part
