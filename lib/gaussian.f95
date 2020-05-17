SUBROUTINE GAUSSIAN

!    Computes a table of values for the gaussian, which is used in the
!    formulas for convection and diffusion between particles. Table just
!    has the Guassian for argument -r**2/s2, starting at 0 and going out
!    to whatever cutoff value we choose for particle interactions.

   implicit none

   include 'main_dim.h'

   real :: gdelta, gauss(ngauss)
   COMMON/GAUSS_TABLE/gdelta, GAUSS

   integer :: i
   real :: x
!------------------------------------------------------------------

   gdelta = gauss_cut/(Ngauss - 1)

   do 1 i = 1, Ngauss
      x = (i - 1)*gdelta
      gauss(i) = exp(-x)
1  END DO

   return
END SUBROUTINE GAUSSIAN
