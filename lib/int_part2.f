      subroutine int_part2(gtest, xtest, ytest, upart, vpart, gpart,
     &     kpart)

!     This  subroutine calculates the velocities induced on the
!     particle located at *xtest*,*ytest*, by the particles in its
!     interaction list *XT,YT,GT*; it also does the reverse influence of
!     xtest,ytest on the particles XT,YT. Thus it is only used when xtest,
!     ytest is in a higher level box than XT,YT to keep things straight.

      implicit none

      include 'main_dim.h'
      include 'tree_tmp.h'
      include 'part.h'

      integer :: np
      real :: s2, ovrlp, gnu
      common/part/Np, s2, ovrlp, gnu

      real :: gdelta, gauss(ngauss)
      common/gauss_table/gdelta, gauss

      real :: visc_cutoff
      common/cutoff/visc_cutoff

      integer :: kpart
      real :: gtest, xtest, ytest, upart, vpart, gpart

      integer :: m, i, n
      real :: s2inv2, gg, xx, yy, r2, arg, c, svl, fm, fn
      real :: rad1, rad2, a, dyopiinv
!------------------------------------------------------------------------

      dyopiinv = 1./(8.*atan(1.))
      s2inv2 = 0.5/s2
      Upart = 0.0
      Vpart = 0.0
      gg = gtest*dyopiinv
      gpart = 0.0

!     FPP$PERMUTATION(IT)
      do 4 m = 1, kpart
         xx = xtest - xt(m)
         yy = yt(m) - ytest
         r2 = xx*xx + yy*yy
         arg = r2*s2inv2
         if (arg < gauss_cut) then
            i = arg/gdelta + 1
            c = gauss(i)*(1 + i*gdelta - arg) ! includes first error term
         else
            c = 0.
         endif
         svl = (1.-c)/r2
         fm = gt(m)*svl
         Upart = Upart + yy*fm  ! velocity on particle LP
         Vpart = Vpart + xx*fm
! Calculation is made symmetric here
         n = it(m)
         fn = gg*svl
         uu(n) = uu(n) - yy*fn
         vv(n) = vv(n) - xx*fn
!*          rad1 = xtest*xtest + ytest*ytest
!*          rad2 = xt(m)*xt(m) + yt(m)*yt(m)
!*          if((rad1.lt.visc_cutoff).and.(rad2.lt.visc_cutoff))then
         a = (gt(m) - gtest)*c
         gpart = gpart + a
         Gdiff(n) = Gdiff(n) - a ! Strength of n
!*         endif
 4    end do

      return
      end subroutine int_part2
