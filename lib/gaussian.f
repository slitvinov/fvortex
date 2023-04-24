      subroutine gaussian()

C Computes a table of values for the gaussian, which is used in the
C formulas for convection and diffusion between particles. Table just
C has the Guassian for argument -r**2/s2, starting at 0 and going out
C to whatever cutoff value we choose for particle interactions.

      include 'main_dim.h'

      real gdelta, gauss(ngauss)
      common/gauss_table/gdelta, gauss

      integer i
      real x

      gdelta = gauss_cut/(ngauss - 1)

      do 1 i = 1, ngauss
         x = (i - 1)*gdelta
         gauss(i) = exp(-x)
    1 end do
      end
